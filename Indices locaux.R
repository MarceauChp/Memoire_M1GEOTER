# ==============================================================================
# Script : Calcul des indices locaux pour GLMM
# Auteur : Chapon Marceau - Master 1 GEOTER
# ==============================================================================

library(sf)
library(dplyr)
library(purrr) 


# 1. Bâtiments
BD_Batiment = read.csv("C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Traitements/Données HLM/BD_Batiment.csv") %>%
  st_as_sf(coords = c("X", "Y"), crs = 2154) 
BD_Batiment$ID_Bat = 1:nrow(BD_Batiment)

# 2. Carreaux INSEE
Carreaux_INSEE = st_read("C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Traitements/Filosofi/Carreaux_2019_AMP.gpkg", quiet = TRUE) %>%
  st_transform(2154) %>%
  mutate(Surf_Init = as.numeric(st_area(.)))

# Liste des données à re-calculer
colonnes_brutes = c("ind", "ind_snv", "men", "men_pauv", "men_prop", 
                     "men_fmp", "men_1ind", "men_5ind", "men_mais", 
                     "log_av45", "log_45_70", "log_70_90", "log_ap90", 
                     "ind_0_3", "ind_4_5", "ind_6_10", "ind_11_17", 
                     "ind_18_24", "ind_25_39", "ind_40_54", "ind_55_64", "ind_65_79")

# Deux échelles retenues, une micro (500m) et une macro (2000m)
distances = c(500, 2000)
liste_resultats = list()

for (d in distances) {
  
  Tampon = st_buffer(BD_Batiment, dist = d)
  Inter  = st_intersection(Tampon, Carreaux_INSEE)
  
  Indices_D = Inter %>%
    # Pondération par la part du carreau intersectée
    mutate(Ratio = as.numeric(st_area(geometry)) / Surf_Init) %>%
    mutate(across(all_of(colonnes_brutes), ~ .x * Ratio)) %>%
    st_drop_geometry() %>%
    group_by(ID_Bat) %>%
    summarise(across(all_of(colonnes_brutes), ~ sum(.x, na.rm = TRUE))) %>%
    
    # Recalcul des indicateurs socio-spatiaux (taux et parts)
    mutate(
      Taux_Pauvrete  = ifelse(men > 0, men_pauv / men, NA),
      Niv_Vie_Moyen  = ifelse(ind > 0, ind_snv  / ind, NA),
      Part_Proprio   = ifelse(men > 0, men_prop  / men, NA),
      Part_Monop     = ifelse(men > 0, men_fmp   / men, NA),
      Part_1_Indiv   = ifelse(men > 0, men_1ind  / men, NA),
      Part_5_Indiv   = ifelse(men > 0, men_5ind  / men, NA),
      
      Total_Logements  = log_av45 + log_45_70 + log_70_90 + log_ap90,
      Total_Logements  = ifelse(Total_Logements == 0, NA, Total_Logements),
      Part_Maisons     = ifelse(men > 0, men_mais  / men, NA),
      Part_Log_45_70   = log_45_70 / Total_Logements,
      Part_Log_70_90   = log_70_90 / Total_Logements,
      Part_Log_ap90    = log_ap90  / Total_Logements,
      
      Part_0_3   = ifelse(ind > 0, ind_0_3   / ind, NA),
      Part_4_5   = ifelse(ind > 0, ind_4_5   / ind, NA),
      Part_6_10  = ifelse(ind > 0, ind_6_10  / ind, NA),
      Part_11_17 = ifelse(ind > 0, ind_11_17 / ind, NA),
      Part_18_24 = ifelse(ind > 0, ind_18_24 / ind, NA),
      Part_25_39 = ifelse(ind > 0, ind_25_39 / ind, NA),
      Part_40_54 = ifelse(ind > 0, ind_40_54 / ind, NA),
      Part_55_64 = ifelse(ind > 0, ind_55_64 / ind, NA),
      Part_65_79 = ifelse(ind > 0, ind_65_79 / ind, NA),
      
      # Densité ajustée à la surface du tampon
      Surf_km2 = (pi * d^2) / 1000000,
      Densite  = ind / Surf_km2
    ) %>%
    
    # Suppression des volumes bruts et variables intermédiaires
    dplyr::select(-all_of(colonnes_brutes), -Total_Logements, -Surf_km2)
  
  # Suffixe de distance ajouté à tous les noms de colonnes (sauf ID_Bat)(Taux_Pauvrete → Taux_Pauvrete_500m)
  colnames(Indices_D)[-1] = paste0(colnames(Indices_D)[-1], "_", d, "m")
  liste_resultats[[as.character(d)]] = Indices_D
}



# 3. Fusion et export

# Jointure avec ID_Bat : les indices de 500m à gauche, et ceux de 2000m à droite
# Structure finale : ID_Bat | [18 indices]_500m | [18 indices]_2000m → 37 colonnes
Tableau_Bi_Scalaire = liste_resultats %>% 
  reduce(left_join, by = "ID_Bat")


write.csv(Tableau_Bi_Scalaire,
          "C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Traitements/Données HLM/Indices_bi_scalaires_complets.csv",
          row.names = FALSE)