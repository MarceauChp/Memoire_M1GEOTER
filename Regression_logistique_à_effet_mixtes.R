# ==============================================================================
# Script : Calcul du GLMM
# Auteur : Chapon Marceau - Master 1 GEOTER
# ==============================================================================

library(dplyr)
library(readr)
library(car)
library(lme4)
library(lmerTest)
library(performance)
library(broom.mixed)
library(pROC)
library(ggplot2)
library(corrplot)

# Paramètres globaux

Chemin_BD_batiment = "C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Traitements/Données HLM/BD_Batiment.csv"
Chemin_indices    = "C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Traitements/Données HLM/Indices_bi_scalaires_complets.csv"
Chemin_export     = "C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Traitements/Données HLM/resultatsats_carto_modele_GLMM.csv"
Chemin_figures     = "C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Graphiques/GLMM"

SEUIL_COR  = 0.49   # Corrélation inter-variables max toléré
SEUIL_VIF  = 5      # VIF max toléré
SEED       = 28     # Reproductibilité

# Création automatique du dossier d'export des figures
if (!dir.exists(Chemin_figures )) dir.create(Chemin_figures , recursive = TRUE)



# Étape 1 : Importation et fusion
BD_Batiment = read_csv(Chemin_BD_batiment, show_col_types = FALSE)
BD_Batiment$ID_Bat = 1:nrow(BD_Batiment)

Indices_Multi = read_csv(Chemin_indices, show_col_types = FALSE)

BD_Merge = BD_Batiment %>%
  left_join(Indices_Multi, by = "ID_Bat")



# ÉTAPE 2 : CONSTRUCTION DE LA BASE ANALYTIQUE


BD_Merge$ROW_ID = 1:nrow(BD_Merge)

NOM_COMMUNE = "Code.Commune"

BD_Full = BD_Merge %>%
  mutate(
    Y_VEFA          = ifelse(Type_achat == "HLM VEFA", 1, 0),
    Poids_Logements = NB_LOGE,
    Log_Dist        = log(Distance + 1),
    Annee_Exp       = as.numeric(Année.expiration.de.la.convention),
    # Facteur commune pour l'effet aléatoire (obligatoirement un facteur)
    Commune         = as.factor(.data[[NOM_COMMUNE]])
  )

# Vérification du nombre de communes dans l'analyse
n_communes = n_distinct(BD_Full$Commune)
cat("Communes distinctes :", n_communes, "\n")
if (n_communes < 10) warning("Moins de 10 communes : les effets aléatoires seront instables !")

# Variables du modèle
variables_multiscalaires = names(Indices_Multi)[-1]   # Tout sauf l'ID
variables_rpls = c("Log_Dist", "Annee_Exp")
variables_predictives = c(variables_multiscalaires, variables_rpls)

# Sélection nettoyage et standardisation Z-score des prédicteurs
BD_Full = BD_Full %>%
  dplyr::select(ROW_ID, Y_VEFA, Poids_Logements, Commune, all_of(variables_predictives)) %>%
  na.omit()

# Récupération coordonnées pour export QGIS
BD_Coordonnees = BD_Batiment %>%
  mutate(ROW_ID = 1:nrow(BD_Batiment)) %>%
  filter(ROW_ID %in% BD_Full$ROW_ID) %>%
  dplyr::select(ROW_ID, X, Y)

# Standardisation Z-score des variables (les facteurs et ID restent inchangés)
BD_Analytique = BD_Full %>%
  mutate(across(all_of(variables_predictives), ~ scale(.)[, 1]))

cat("Base analytique finale :", nrow(BD_Analytique), "bâtiments\n")
cat("VEFA :", sum(BD_Analytique$Y_VEFA), "| MOD :", sum(BD_Analytique$Y_VEFA == 0), "\n")
cat("Variables prédictives :", length(variables_predictives), "\n")
cat("  dont bi-scalaires :", length(variables_multiscalaires),
    "(", length(variables_multiscalaires) / 2, "indicateurs × 2 échelles : 500m + 2000m)\n")
cat("  dont RPLS :", length(variables_rpls), "\n")




# Étape 3 la présélection 

# A. Flitrage par corrélation

donnees_filtre = BD_Analytique %>% dplyr::select(Y_VEFA, all_of(variables_predictives))
variables_candidates = variables_predictives

# Corrélation de chaque prédicteur avec Y_VEFA (critère de conservation)
cor_avec_Y = sapply(variables_candidates, function(v) {
  abs(cor(donnees_filtre[[v]], donnees_filtre$Y_VEFA, use = "complete.obs"))
})

mat_cor_abs = abs(cor(donnees_filtre %>% dplyr::select(all_of(variables_candidates)),
                       use = "complete.obs"))
diag(mat_cor_abs) = 0

variables_retenues  = variables_candidates
n_retraits_cor = 0

repeat {
  sous_mat = mat_cor_abs[variables_retenues, variables_retenues]
  max_cor  = max(sous_mat, na.rm = TRUE)
  if (max_cor < SEUIL_COR) break

  idx = which(sous_mat == max_cor, arr.ind = TRUE)[1, ]
  v1  = rownames(sous_mat)[idx[1]]
  v2  = colnames(sous_mat)[idx[2]]

  if (cor_avec_Y[v1] >= cor_avec_Y[v2]) {
    cat(sprintf("  [r=%.3f] Retrait: %-35s | Conservé: %s\n", max_cor, v2, v1))
    variables_retenues = variables_retenues[variables_retenues != v2]
  } else {
    cat(sprintf("  [r=%.3f] Retrait: %-35s | Conservé: %s\n", max_cor, v1, v2))
    variables_retenues = variables_retenues[variables_retenues != v1]
  }
  n_retraits_cor = n_retraits_cor + 1
}



# B. Filtrage par le VIF (VIF > SEUIL_VIF)
# Note : le VIF est calculé sur un glm() simple (sans effet aléatoire)

variables_apres_vif = variables_retenues
n_retraits_vif = 0

repeat {
  donnees_vif = BD_Analytique %>% dplyr::select(Y_VEFA, all_of(variables_apres_vif))

  modele_vif = tryCatch(
    glm(Y_VEFA ~ ., data = donnees_vif, family = "binomial",
        weights = BD_Analytique$Poids_Logements),
    error = function(e) { cat("Erreur GLM VIF :", e$message, "\n"); NULL }
  )
  if (is.null(modele_vif)) break

  vif_valeurs = tryCatch(vif(modele_vif), error = function(e) NULL)
  if (is.null(vif_valeurs) || length(vif_valeurs) == 0) break

  max_vif = max(vif_valeurs, na.rm = TRUE)
  if (max_vif <= SEUIL_VIF) break

  var_exclure    = names(which.max(vif_valeurs))
  cat(sprintf("  VIF=%.2f → Retrait : %s\n", max_vif, var_exclure))
  variables_apres_vif = variables_apres_vif[variables_apres_vif != var_exclure]
  n_retraits_vif = n_retraits_vif + 1
}


  
# Visualisation de la matrice de corrélation après le filtre
corrplot(
  cor(BD_Analytique %>% dplyr::select(all_of(variables_apres_vif)), use = "complete.obs"),
  method = "color", type = "upper",
  tl.col = "black", tl.srt = 45, tl.cex = 0.65,
  addCoef.col = "black", number.cex = 0.5,
  title = "Matrice après filtrage corrélation + VIF (entrée GLMM)",
  mar   = c(0, 0, 2, 0)
)

# Export PNG de la matrice de colinéarité finale (variables entrant dans le GLMM)
fichier_matrice = file.path(Chemin_figures , "Matrice_Colinearite_Finale_GLMM.png")
png(fichier_matrice, width = 2400, height = 2000, res = 250)
corrplot(
  cor(BD_Analytique %>% dplyr::select(all_of(variables_apres_vif)), use = "complete.obs"),
  method = "color", type = "upper",
  tl.col = "black", tl.srt = 45, tl.cex = 0.65,
  addCoef.col = "black", number.cex = 0.5,
  title = "Matrice de colinéarité — Variables du modèle final (GLMM)",
  mar   = c(0, 0, 2, 0)
)
dev.off()



# Étape 4 : Modèle GLMM avec effet aléatoire des communes

formule_glmm = as.formula(
  paste("Y_VEFA ~", paste(variables_apres_vif, collapse = " + "), "+ (1 | Commune)")
)

set.seed(SEED)
modele_glmm = glmer(
  formule_glmm,
  data    = BD_Analytique,
  family  = binomial(link = "logit"),
  weights = Poids_Logements,
  # bobyqa est l'optimiseur le plus stable pour les GLMM binomiaux
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

print(summary(modele_glmm))



# ÉTAPE 5 : sélèction des variables significatives par le LRT
# C'est l'équivalent pour un GLMM du stepwise backward AIC d'une régression logistique classique

modele_step = modele_glmm
variables_step   = variables_apres_vif
n_step      = 0

# Création d'un modèle avec toutes les variables filtrées (sans doublon)
# À chaque itération identification de la variable qui dégrade le moins significativement le modèle (p-valeurue LRT la plus haute)
# Si p-value > 0.05, on la retire et on re-crée un modèle sans elle
# La boucle s'arrête quand toutes les variables restantes sont significatives (p-value LRT <= 0.05)
# Le BIC est affiché en début et en fin pour vérifier que la simplification est raisonnable
repeat {
  lrt_table = drop1(modele_step, test = "Chisq")

  lrt_variables = lrt_table[rownames(lrt_table) != "<none>", ]
  if (nrow(lrt_variables) == 0) break

  p_valeurs  = lrt_variables[["Pr(>Chi)"]]
  max_p   = max(p_valeurs, na.rm = TRUE)

  if (max_p <= 0.05) break

  var_suppr = rownames(lrt_variables)[which.max(p_valeurs)]
  cat(sprintf("  p=%.4f → Retrait : %s\n", max_p, var_suppr))
  variables_step = variables_step[variables_step != var_suppr]

  formule_step = as.formula(
    paste("Y_VEFA ~", paste(variables_step, collapse = " + "), "+ (1 | Commune)")
  )
  modele_step = glmer(
    formule_step,
    data    = BD_Analytique,
    family  = binomial(link = "logit"),
    weights = Poids_Logements,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
  n_step = n_step + 1
}

variables_finales_glmm = variables_step
modele_final_glmm      = modele_step

cat("\n→", n_step, "variables retirées par backward LRT\n")
cat("→", length(variables_finales_glmm), "variables dans le modèle final\n")
cat("BIC final :", round(BIC(modele_final_glmm), 1), "\n\n")
print(summary(modele_final_glmm))




# Érape 6 : La mesure de "l'effet communal" (ICC, Intra-Class Correlation)

# Interprétation :
# ICC < 0.05 = effet communal trop faible, le GLMM n'est pas justifié
# ICC compris entre [0.05, 0.15] = GLMM justifié, effet communal modéré
# ICC > 0.15 = effet communal important

ICC_resultats = icc(modele_final_glmm)
print(ICC_resultats)

ICC_valeur = as.numeric(ICC_resultats$ICC_adjusted)
if (ICC_valeur < 0.05) {
  cat("  ICC =", round(ICC_valeur, 3), "→ Effet communal absent.\n")
  cat("  Le modèle glmm n'est pas justfié, l'hypothèse n'est pas validée.\n")
} else if (ICC_valeur < 0.15) {
  cat("  ICC =", round(ICC_valeur, 3), "→ Effet communal modéré.\n")
  cat("  Le GLMM améliore la rigueur statistique de l'analyse.\n")
} else {
  cat("  ICC =", round(ICC_valeur, 3), "→ Effet communal important.\n")
  cat("  Les communes structurent fortement la géographie du logement social\n")
}


# Pseudo-R² de Nakagawa (2013)
# R²m (marginal) soit la variance expliquée par les variables uniquement
# R²c (conditionnel) soit la variance expliquée par les variables et les communes
r2_valeurs = r2_nakagawa(modele_final_glmm)
cat("\nPseudo-R² (Nakagawa & Schielzeth) :\n")
cat("  R²m (effets fixes seuls)       :", round(r2_valeurs$R2_marginal,     3), "\n")
cat("  R²c (fixes + commune)          :", round(r2_valeurs$R2_conditional,  3), "\n")
cat("  Part commune [R²c - R²m]       :",
    round(r2_valeurs$R2_conditional - r2_valeurs$R2_marginal, 3), "\n")




# Étape 7 : odds ratios conditionnels et visualisation

tidy_glmm = tidy(modele_final_glmm, effects = "fixed", exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  rename(Variable = term) %>%
  mutate(
    Concept = gsub("_(500m|2000m)$", "", Variable),
    Echelle = ifelse(
      grepl("_(500m|2000m)$", Variable),
      regmatches(Variable, regexpr("(500m|2000m)$", Variable)),
      "RPLS (pas d'échelle)"
    ),
    Significatif = p.value < 0.05
  ) %>%
  arrange(Concept, Echelle)

print(tidy_glmm %>%
        dplyr::select(Concept, Echelle, estimate, conf.low, conf.high, p.value) %>%
        mutate(across(where(is.numeric), ~ round(.x, 4))),
      row.names = FALSE)

print(table(tidy_glmm$Echelle))

# Forest plot des odds ratios conditionnels
plot_or_glmm = ggplot(
  tidy_glmm,
  aes(x      = reorder(Variable, estimate),
      y      = estimate,
      ymin   = conf.low,
      ymax   = conf.high,
      color  = Significatif)
) +
  geom_pointrange(size = 0.9) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 1) +
  scale_color_manual(values = c("TRUE" = "#E66100", "FALSE" = "#888888"),
                     labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
                     name   = "Significativité") +
  coord_flip() +
  labs(
    title    = "Portrait socio-spatial des opérations en VEFA HLM",
    subtitle = "OR conditionnels GLMM — contrôle de l'effet communal | IC 95%",
    x        = NULL,
    y        = "Odds Ratio conditionnel (IC 95%)"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(face = "bold", size = 10))

print(plot_or_glmm)

# Export en PNG du forest plot
fichier_forest = file.path(Chemin_figures , "Forest_Plot_OR_GLMM.png")
ggsave(fichier_forest, plot = plot_or_glmm,
       width = 10, height = max(4, nrow(tidy_glmm) * 0.5 + 2),
       dpi = 300, bg = "white")




# Étape 8 : évaluation des performanes avec la ROC et la AUC

probabilités_glmm = predict(modele_final_glmm, type = "response")

roc_glmm  = roc(BD_Analytique$Y_VEFA, probabilités_glmm, quiet = TRUE)
seuil_opt = as.numeric(coords(roc_glmm, "best", ret = "threshold")[1])

cat("AUC (GLMM, toute la base) :", round(auc(roc_glmm), 4), "\n")
cat("Note : le GLMM est calculée in-sample (données d'entraînement), donc le résultat est optimiste.\n")
cat("Seuil optimal :", round(seuil_opt, 4), "\n")

plot_roc = ggroc(roc_glmm, colour = "#2E9FDF", size = 1.2) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "red") +
  labs(
    title    = "Courbe ROC — GLMM VEFA HLM (contrôle communal)",
    subtitle = paste("AUC =", round(auc(roc_glmm), 4)),
    x        = "Spécificité",
    y        = "Sensibilité"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
print(plot_roc)

# Export en PNG de la courbe ROC
fichier_roc = file.path(Chemin_figures , "Courbe_ROC_GLMM.png")
ggsave(fichier_roc, plot = plot_roc, width = 8, height = 6, dpi = 300, bg = "white")



# Étape 9 : exportation des données en geopackage


BD_Carto = BD_Analytique %>%
  mutate(
    Coord_X      = BD_Coordonnees$X,
    Coord_Y      = BD_Coordonnees$Y,
    Proba_VEFA   = probabilités_glmm,
    Prediction   = ifelse(Proba_VEFA >= seuil_opt, 1, 0),
    Statut_Carto = case_when(
      Y_VEFA == 1 & Prediction == 1 ~ "1. Vrai Positif  (VEFA bien prédite)",
      Y_VEFA == 0 & Prediction == 0 ~ "2. Vrai Négatif  (MOD bien prédite)",
      Y_VEFA == 0 & Prediction == 1 ~ "3. Faux Positif  (Le modèle croyait VEFA)",
      Y_VEFA == 1 & Prediction == 0 ~ "4. Faux Négatif  (Le modèle croyait MOD)"
    ),
    Residu       = Y_VEFA - Proba_VEFA
  )

write_csv(BD_Carto, Chemin_export)
cat(" Variables finales :", length(variables_finales_glmm), "sur",
    length(variables_predictives), "testées.\n")
cat(" ICC :", round(ICC_valeur, 3),
    "= Part de variance liée à l'effet commune.\n")
cat("R²m :",r2_valeurs[[2]],
    "= Part de la variance liée aux variables locales.\n")
