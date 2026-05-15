# ==============================================================================
# Script : Filtre du RPLS, préparation des données et statistiques descritpives
# Auteur : Chapon Marceau - Master 1 GEOTER
# ==============================================================================

library(dplyr)
library(readr)
library(FactoMineR)
library(ggplot2)

setwd('C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire')

# 1. Chargement et nettoyage des colonnes
Donnees_logements_Aix_Mar = read.csv2("C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Data/Donnees_logements_Aix-Marseille-csv.csv", header = TRUE, sep = ";")

Donnees_logements_Aix_Mar = Donnees_logements_Aix_Mar[-1,-1:-11]
Donnees_logements_Aix_Mar = Donnees_logements_Aix_Mar[-2]
Donnees_logements_Aix_Mar = Donnees_logements_Aix_Mar[-3:-10]
Donnees_logements_Aix_Mar = Donnees_logements_Aix_Mar[-4]
Donnees_logements_Aix_Mar = Donnees_logements_Aix_Mar[-8]
Donnees_logements_Aix_Mar = Donnees_logements_Aix_Mar[-10:-13]
Donnees_logements_Aix_Mar = Donnees_logements_Aix_Mar[-12:-17]
Donnees_logements_Aix_Mar = Donnees_logements_Aix_Mar[-13:-19]
Donnees_logements_Aix_Mar = Donnees_logements_Aix_Mar[-16:-22]
Donnees_logements_Aix_Mar = Donnees_logements_Aix_Mar[-19:-27]

Donnees_logements_Aix_Mar$ID = 1:nrow(Donnees_logements_Aix_Mar)

# 2. Traitements des variables de sélection
Donnees_logements_Aix_Mar$Type_achat = ifelse(
  Donnees_logements_Aix_Mar$Origine.de.l.entrée.dans.le.patrimoine == 1 |
    Donnees_logements_Aix_Mar$Origine.de.l.entrée.dans.le.patrimoine == 2 |
    Donnees_logements_Aix_Mar$Origine.de.l.entrée.dans.le.patrimoine == 3,
  "HLM non VEFA", "HLM VEFA")

Donnees_logements_Aix_Mar$Année.d.entrée.dans.le.patrimoine = as.numeric(as.character(Donnees_logements_Aix_Mar$Année.d.entrée.dans.le.patrimoine))
Donnees_logements_Aix_Mar$Année.expiration.de.la.convention = as.numeric(as.character(Donnees_logements_Aix_Mar$Année.expiration.de.la.convention))

# Regroupement des arrondissements de Marseille
Donnees_logements_Aix_Mar = Donnees_logements_Aix_Mar %>%
  mutate(
    Code.Commune = ifelse(grepl("Marseille", Libéllé.Commune, ignore.case = TRUE), 13055, Code.Commune),
    Libéllé.Commune = ifelse(grepl("Marseille", Libéllé.Commune, ignore.case = TRUE), "Marseille", Libéllé.Commune)
  )


# 3. Tri de la BD
# Dataframe de tout les logements après 2000
HLM_Aix_Mar_Brut = subset(Donnees_logements_Aix_Mar,Année.d.entrée.dans.le.patrimoine >= 2000)

# Dataframe des logements toujours conventionné en 2026 et géolocalisé avec la meilleure qualité (Données de base pour les traitements futurs)
HLM_Aix_Mar = subset(Donnees_logements_Aix_Mar, is.na(Année.expiration.de.la.convention) | Année.expiration.de.la.convention >= 2025)
HLM_Aix_Mar = HLM_Aix_Mar[HLM_Aix_Mar$Qualité.de.la.géolocalisation == "11",]
HLM_Aix_Mar = HLM_Aix_Mar[HLM_Aix_Mar$Année.d.entrée.dans.le.patrimoine >= 2000 & HLM_Aix_Mar$Année.d.entrée.dans.le.patrimoine<=2024,]
HLM_Aix_Mar = HLM_Aix_Mar %>% filter(!is.na(Coordonnée.X) & !is.na(Coordonnée.Y))

# 4. Découpage temporel
# Découpage en 4 périodes (Pré-crise, Post-crise/essor VEFA, ALUR/Densification, ZAN)
HLM_Aix_Mar = HLM_Aix_Mar %>%
  mutate(
    Periode = cut(
      Année.d.entrée.dans.le.patrimoine,
      breaks = c(2000, 2008, 2014, 2021, 2027), # On a ajouté 2008 ici
      labels = c("2000-2008", "2008-2014", "2014-2021", "2021-2026"),
      include.lowest = TRUE, right = FALSE
    )
  )


# 5. Filtre pour la représentativité statistique

# Backup
HLM_Avant_Filtre = HLM_Aix_Mar

# 1er filtre : on supprime les communes qui ont construit moins de logements 20 entre 2000 et 2026
HLM_Aix_Mar = HLM_Aix_Mar %>%
  add_count(Code.Commune, name = "Total_Commune_Global") %>%
  filter(Total_Commune_Global >= 20) %>%
  select(-Total_Commune_Global) 


# 2ème filtre : on supprime les périodes trop faibles au sein d'une commune (moins de 5 logements)
HLM_Aix_Mar = HLM_Aix_Mar %>%
  add_count(Code.Commune, Periode, name = "Total_Commune_Periode") %>%
  filter(Total_Commune_Periode >= 5) %>%
  select(-Total_Commune_Periode)


# isolement de toutes les logements supprimés
logements_supprimes = anti_join(HLM_Avant_Filtre, HLM_Aix_Mar, by = "ID")

nb_supprimes = nrow(logements_supprimes)
nb_conserves = nrow(HLM_Aix_Mar)
communes_concernees = sort(unique(logements_supprimes$Libéllé.Commune))

cat(">>> BILAN DU NETTOYAGE :\n")
cat(paste("- Logements exclus de l'analyse :", nb_supprimes, "\n"))
cat(paste("- Logements conservés pour les cartes :", nb_conserves, "\n"))
cat("- Communes ayant subi des suppressions (totales ou partielles) :\n")
print(communes_concernees)

# Exportation de ce qui a été supprimé
write.csv(logements_supprimes, file = "C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Traitements/Données HLM/Filtre/Logements_exclus_du_filtrage.csv", row.names = FALSE)

# Exportation du fichier filtré qui servira au Script 2
write.csv(HLM_Aix_Mar, file = "C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Traitements/Données HLM/Filtre/HLM_Aix_Mar_2000_filtre.csv", row.names = FALSE)

#Exportation du fichier sans les filtres (backup)
write.csv(HLM_Avant_Filtre, file = "C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Traitements/Données HLM/Filtre/HLM_Aix_Mar_2000_avant_filtre.csv", row.names = FALSE)



# 6. Statistiques descriptives de la base de données non filtré

# Calcul des effectifs par année et type d'acquisition
tableau_annuel_type = HLM_Aix_Mar_Brut %>%
  group_by(Année.d.entrée.dans.le.patrimoine, Type_achat) %>%
  summarise(Nb = n(), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from  = Type_achat,
    values_from = Nb,
    values_fill = 0
  ) %>%
  rename(Année = Année.d.entrée.dans.le.patrimoine)

# Sécurité : s'assurer que les deux colonnes existent même si une catégorie est absente
if (!"HLM VEFA"     %in% names(tableau_annuel_type)) tableau_annuel_type$`HLM VEFA`     = 0L
if (!"HLM non VEFA" %in% names(tableau_annuel_type)) tableau_annuel_type$`HLM non VEFA` = 0L

# Calcul du total et des parts (%)
tableau_annuel_type = tableau_annuel_type %>%
  mutate(
    Total         = `HLM VEFA` + `HLM non VEFA`,
    Part_VEFA_pct = round(`HLM VEFA`     / Total * 100, 1),
    Part_MOD_pct  = round(`HLM non VEFA` / Total * 100, 1)
  ) %>%
  arrange(Année) %>%
  select(Année, `HLM VEFA`, Part_VEFA_pct, `HLM non VEFA`, Part_MOD_pct, Total)

# Renommage pour plus de lisibilité dans la console
names(tableau_annuel_type) = c(
  "Année",
  "Nb VEFA", "Part VEFA (%)",
  "Nb MOD",  "Part MOD (%)",
  "Total"
)

# Affichage dans la console
cat("\n>>> PART DE VEFA ET DE MOD PAR ANNÉE (2000-2024) :\n")
print(tableau_annuel_type, n = Inf)

# Export CSV
write.csv(
  tableau_annuel_type,
  file      = "C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Traitements/Données HLM/Filtre/Stats descriptive BD brut/Tableau_VEFA_MOD_par_annee.csv",
  row.names = FALSE
)


# Diagramme en anneau
donut_data = as.data.frame(table(HLM_Aix_Mar_Brut$Type_achat))
colnames(donut_data) = c("Type", "Count")
donut_data$Percentage = donut_data$Count / sum(donut_data$Count) * 100

Donuts = ggplot(donut_data, aes(x = 2, y = Count, fill = Type)) +
  geom_bar(stat = "identity", width = 1) + coord_polar(theta = "y", start = 0) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5), color = "white", size = 4) +
  xlim(0.5, 2.5) + labs(title = "Répartition des types d'acquisition", subtitle = "Logements non filtrés") + theme_void()
print(Donuts)
ggsave("C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Graphiques/Stats descriptive BD brut/Donuts.png", plot = Donuts, width = 12, height = 8, dpi = 300, bg = "lightgrey")


# Histogramme des logements entrée au patrimoine
NB_loge_annee_entree = ggplot(HLM_Aix_Mar_Brut, aes(x = Année.d.entrée.dans.le.patrimoine, fill = Type_achat)) +
  geom_histogram(binwidth = 1, color = "white", position = "stack") +
  labs(
    title = "Nombre de logements entrés dans le patrimoine par année et type",
    subtitle = "Tous logements 2000-2025 (non filtré)",
    x = "Année d'entrée", y = "Logements", fill = "Type d'acquisition"
  )
print(NB_loge_annee_entree)
ggsave("C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Graphiques/Stats descriptive BD brut/histogramme_annee_entree.png", plot = NB_loge_annee_entree, width = 12, height = 8, dpi = 300, bg = "grey")


# Histogramme de la date d'expiration des conventions selon les types d'acquisition
Expiration_data = HLM_Aix_Mar_Brut %>% filter(!is.na(Année.expiration.de.la.convention))
Graph_Expiration = ggplot(Expiration_data, aes(x = Année.expiration.de.la.convention, fill = Type_achat)) +
  geom_histogram(binwidth = 1, color = "white", position = "stack") +
  labs(
    title = "Année d'expiration des conventions HLM",
    subtitle = "Pour le parc entré entre 2000 et 2025 (logements non filtrés)",
    x = "Année d'expiration", 
    y = "Nombre de logements",
    fill = "Type d'acquisition"
  ) +
  coord_cartesian(xlim = c(2025, 2100)) + 
  theme_minimal()
print(Graph_Expiration)
ggsave("C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Camille/Graphiques/Histogramme_Expiration.png", plot = Graph_Expiration, width = 12, height = 8, dpi = 300, bg = "white")


# Boites à moustache de la date d'entrée au patrimoine par type d'acquisition
Moustache = ggplot(HLM_Aix_Mar_Brut, aes(x = Type_achat, y = Année.d.entrée.dans.le.patrimoine)) + 
  geom_boxplot(aes(color = Type_achat)) + labs(title = "Entrée aux patrimoines", x = "Type", y = "Année")
print(Moustache)
ggsave("C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Graphiques/Stats descriptive BD brut/Boite_moustache_type_acquisition.png", plot = Moustache, width = 12, height = 8, dpi = 300, bg = "grey")


# Courbe de densité d'entrée au patrimoine par type d'acquisition
Densite = ggplot(HLM_Aix_Mar_Brut, aes(x = Année.d.entrée.dans.le.patrimoine, fill = Type_achat, color = Type_achat)) +
  geom_density(alpha = 0.4, size = 1) + labs(
    title = "Densité de l'entrée aux patrimoines entre 2000 et 2025", 
    subtitle = "Logements non filtrés",
    x = "Année", 
    y = "Densité")
print(Densite)
ggsave("C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Graphiques/Stats descriptive BD brut/Courbe de densité par type d'achat.png", plot = Densite, width = 12, height = 8, dpi = 300, bg = "grey")




# 7. Statistiques descriptives de la base de données filtré


# Calcul des effectifs par année et type d'acquisition
tableau_annuel_type = HLM_Aix_Mar %>%
  group_by(Année.d.entrée.dans.le.patrimoine, Type_achat) %>%
  summarise(Nb = n(), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from  = Type_achat,
    values_from = Nb,
    values_fill = 0
  ) %>%
  rename(Année = Année.d.entrée.dans.le.patrimoine)

# Sécurité : s'assurer que les deux colonnes existent même si une catégorie est absente
if (!"HLM VEFA"     %in% names(tableau_annuel_type)) tableau_annuel_type$`HLM VEFA`     = 0L
if (!"HLM non VEFA" %in% names(tableau_annuel_type)) tableau_annuel_type$`HLM non VEFA` = 0L

# Calcul du total et des parts (%)
tableau_annuel_type = tableau_annuel_type %>%
  mutate(
    Total         = `HLM VEFA` + `HLM non VEFA`,
    Part_VEFA_pct = round(`HLM VEFA`     / Total * 100, 1),
    Part_MOD_pct  = round(`HLM non VEFA` / Total * 100, 1)
  ) %>%
  arrange(Année) %>%
  select(Année, `HLM VEFA`, Part_VEFA_pct, `HLM non VEFA`, Part_MOD_pct, Total)

# Renommage pour plus de lisibilité dans la console
names(tableau_annuel_type) = c(
  "Année",
  "Nb VEFA", "Part VEFA (%)",
  "Nb MOD",  "Part MOD (%)",
  "Total"
)

# Affichage dans la console
cat("\n>>> Part de la VEFA et de la MOD entre 2000 et 2025 :\n")
print(tableau_annuel_type, n = Inf)

# Export CSV
write.csv(
  tableau_annuel_type,
  file      = "C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Traitements/Données HLM/Filtre/Tableau_VEFA_MOD_par_annee.csv",
  row.names = FALSE
)



# Diagramme en anneau
donut_data = as.data.frame(table(HLM_Aix_Mar$Type_achat))
colnames(donut_data) = c("Type", "Count")
donut_data$Percentage = donut_data$Count / sum(donut_data$Count) * 100

Donuts = ggplot(donut_data, aes(x = 2, y = Count, fill = Type)) +
  geom_bar(stat = "identity", width = 1) + coord_polar(theta = "y", start = 0) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5), color = "white", size = 4) +
  xlim(0.5, 2.5) + labs(title = "Répartition des types d'acquisition dans la métrpole AMP entre 2000 et 2025", subtitle = "Logements conventionnés et flitrés") + theme_void()
print(Donuts)
ggsave("C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Graphiques/Stats descriptive/Donuts.png", plot = Donuts, width = 12, height = 8, dpi = 300, bg = "lightgrey")


# Diagrammes des logements exclus par communes
if(nrow(logements_supprimes) > 0)
  # On compte le nombre de logements supprimés par commune
  decompte_supprimes = logements_supprimes %>%
    group_by(Libéllé.Commune) %>%
    summarise(Nb_exclus = n()) %>%
    arrange(desc(Nb_exclus))
  
  # Création du graphique en barres horizontales
  Graph_exclus = ggplot(decompte_supprimes, aes(x = reorder(Libéllé.Commune, Nb_exclus), y = Nb_exclus)) +
    geom_bar(stat = "identity", fill = "#F8766D", color = "grey") +
    coord_flip() + # Met le graphique à l'horizontale pour lire les noms des communes
    labs(
      title = "Nombre de logements exclus par commune lors du filtrage",
      subtitle = "Communes n'ayant pas atteint les seuils de représentativité",
      x = "Commune", 
      y = "Nombre de logements écartés"
    ) +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white", color = NA))
  
  print(Graph_exclus)
  
  ggsave(
    filename = "C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Graphiques/Stats descriptive/Logements_exclus_par_commune.png",
    plot = Graph_exclus, width = 10, height = 8, dpi = 300, bg = "white"
  )


# Histogramme des logements entrée au patrimoine
NB_loge_annee_entree = ggplot(HLM_Aix_Mar, aes(x = Année.d.entrée.dans.le.patrimoine, fill = Type_achat)) +
  geom_histogram(binwidth = 1, color = "white", position = "stack") +
  labs(
    title = "Nombre de logements entrés dans le patrimoine par année et type",
    subtitle = "Tous logements 2000-2025 (conventionnés et filtrés)",
    x = "Année d'entrée", y = "Logements", fill = "Type d'acquisition"
  )
print(NB_loge_annee_entree)
ggsave("C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Graphiques/Stats descriptive/histogramme_annee_entree.png", plot = NB_loge_annee_entree, width = 12, height = 8, dpi = 300, bg = "grey")


# Histogramme de la date d'expiration des conventions selon les types d'acquisition
Expiration_data = HLM_Aix_Mar %>% filter(!is.na(Année.expiration.de.la.convention))
Graph_Expiration = ggplot(Expiration_data, aes(x = Année.expiration.de.la.convention, fill = Type_achat)) +
  geom_histogram(binwidth = 1, color = "white", position = "stack") +
  labs(
    title = "Année d'expiration des conventions HLM",
    subtitle = "Pour le parc entré entre 2000 et 2025 (conventionnés et filtrés)",
    x = "Année d'expiration", 
    y = "Nombre de logements",
    fill = "Type d'acquisition"
  ) +
  coord_cartesian(xlim = c(2025, 2100)) + 
  theme_minimal()
print(Graph_Expiration)
ggsave("C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Camille/Graphiques/Stats descriptive/Histogramme_Expiration.png", plot = Graph_Expiration, width = 12, height = 8, dpi = 300, bg = "white")


# Boites à moustache de la date d'entrée au patrimoine par type d'acquisition
Moustache = ggplot(HLM_Aix_Mar, aes(x = Type_achat, y = Année.d.entrée.dans.le.patrimoine)) + 
  geom_boxplot(aes(color = Type_achat)) + labs(
    title = "Entrée aux patrimoines entre 2000 et 2025",
    subtitle = "Logements conventionnés et flitrés",
    x = "Type", 
    y = "Année")
print(Moustache)
ggsave("C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Graphiques/Stats descriptive/Boite_moustache_type_acquisition.png", plot = Moustache, width = 12, height = 8, dpi = 300, bg = "grey")


# Courbe de densité d'entrée au patrimoine par type d'acquisition
Densite = ggplot(HLM_Aix_Mar, aes(x = Année.d.entrée.dans.le.patrimoine, fill = Type_achat, color = Type_achat)) +
  geom_density(alpha = 0.4, size = 1) + labs(
    title = "Densité de l'entrée aux patrimoines", 
    subtitle = "Logements conventionnés et flitrés",
    x = "Année", 
    y = "Densité")
print(Densite)
ggsave("C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Graphiques/Stats descriptive/Courbe de densité par type d'achat.png", plot = Densite, width = 12, height = 8, dpi = 300, bg = "grey")
