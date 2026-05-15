# ==============================================================================
# Script : Calcul des distances, ellipses d'écart type et coordonnées moyennes
# Auteur : Chapon Marceau - Master 1 GEOTER
# ==============================================================================

library(dplyr)
library(tidyr) 
library(readr)
library(phonTools)
library(ggplot2)
library(sf)

setwd("C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Traitements/Données HLM/SCE_distance_centroide")

# 1. Chargement des logements filtré (script filtre)
HLM_Aix_Mar = read_csv("C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Traitements/Données HLM/Filtre/HLM_Aix_Mar_2000_filtre.csv", show_col_types = FALSE)



# 2. Préparation des données, création des catégories (ensemble)
HLM_Aix_Mar_ensemble = HLM_Aix_Mar %>% mutate(Type_achat = "Ensemble")
HLM_Aix_Mar_combine = bind_rows(HLM_Aix_Mar, HLM_Aix_Mar_ensemble)



# 3. Moyennes communales
Construction_par_commune_date_type = HLM_Aix_Mar %>%
  group_by(Code.Commune, Libéllé.Commune, Periode, Type_achat) %>%
  summarise(Nb = n(), Moy_X = mean(Coordonnée.X, na.rm = TRUE), Moy_Y = mean(Coordonnée.Y, na.rm = TRUE), .groups = 'drop')
write_csv(Construction_par_commune_date_type, "Construction_par_commune_date_type.csv")



# 4. Caclul des l'éllipses d'écart type
calc_ellipse_phontools = function(data) {
  if(nrow(data) < 3) return(data.frame(Superficie=NA, Orientation=NA))
  coords = cbind(data$Coordonnée.X, data$Coordonnée.Y)
  ellipse_pts = tryCatch({sdellipse(coords, stdev = 1, show = FALSE, density = 0.1)}, error = function(e) return(NULL), warning = function(w) return(NULL))
  if(is.null(ellipse_pts) || anyNA(ellipse_pts)) return(data.frame(Superficie=NA, Orientation=NA))
  x = ellipse_pts[,1]; y = ellipse_pts[,2]
  area = 0.5 * abs(sum(x * c(y[-1], y[1]) - y * c(x[-1], x[1])))
  eigen_dec = eigen(cov(coords))
  vec_major = eigen_dec$vectors[,1]
  angle = atan2(vec_major[2], vec_major[1]) * (180 / pi)
  if (angle < 0) angle = angle + 180
  return(data.frame(Superficie = area, Orientation = angle))
}



# 5. Exportatio en gpkg pour QGIS (Ellipses par période et globales)

# A. Ellipses par Période
res_ellipses_periode = HLM_Aix_Mar_combine %>% group_by(Code.Commune, Libéllé.Commune, Periode, Type_achat) %>% do(calc_ellipse_phontools(.)) %>% ungroup()
res_ellipses_periode$Superficie_Km2 = res_ellipses_periode$Superficie / 1000000
write_csv(res_ellipses_periode, "tableau_ellipses_par_periode.csv")

liste_ellipses_sf <- list()
for(grp in HLM_Aix_Mar_combine %>% group_by(Code.Commune, Libéllé.Commune, Periode, Type_achat) %>% group_split()) {
  if(nrow(grp) < 3) next
  coords = cbind(grp$Coordonnée.X, grp$Coordonnée.Y)
  pts = tryCatch({ sdellipse(coords, stdev = 1, show = FALSE, density = 0.1) }, error = function(e) return(NULL), warning = function(w) return(NULL))
  if(is.null(pts) || anyNA(pts)) next
  pts = rbind(pts, pts[1,]); poly = st_polygon(list(pts)) 
  x = pts[,1]; y = pts[,2]; area = 0.5 * abs(sum(x * c(y[-1], y[1]) - y * c(x[-1], x[1])))
  eigen_dec = eigen(cov(coords)); vec_major = eigen_dec$vectors[,1]
  angle = atan2(vec_major[2], vec_major[1]) * (180 / pi); if(angle < 0) angle = angle + 180
  
  liste_ellipses_sf[[length(liste_ellipses_sf) + 1]] <- st_sf(
    Commune = grp$Libéllé.Commune[1], Periode = grp$Periode[1], Type_achat = grp$Type_achat[1], Nombre_Logements = nrow(grp), 
    Superficie_km2 = area / 1000000, Orientation = angle, geometry = st_sfc(poly, crs = 2154)
  )
}

if(length(liste_ellipses_sf) > 0) {
  couche_ellipses_QGIS <- do.call(rbind, liste_ellipses_sf)
  for(type in unique(couche_ellipses_QGIS$Type_achat)) {
    st_write(obj = couche_ellipses_QGIS %>% filter(Type_achat == type), dsn = "Ellipses_HLM_Evolution.gpkg", layer = paste0("ellipses_periodes_", gsub(" ", "_", type)), delete_layer = TRUE, quiet = TRUE)
  }
}

# B. Ellipses Globales
liste_ellipses_globales_sf <- list()
for(grp in HLM_Aix_Mar_combine %>% group_by(Code.Commune, Libéllé.Commune, Type_achat) %>% group_split()) {
  if(nrow(grp) < 3) next
  coords = cbind(grp$Coordonnée.X, grp$Coordonnée.Y)
  pts = tryCatch({ sdellipse(coords, stdev = 1, show = FALSE, density = 0.1) }, error = function(e) return(NULL), warning = function(w) return(NULL))
  if(is.null(pts) || anyNA(pts)) next
  pts = rbind(pts, pts[1,]); poly = st_polygon(list(pts)) 
  x = pts[,1]; y = pts[,2]; area = 0.5 * abs(sum(x * c(y[-1], y[1]) - y * c(x[-1], x[1])))
  eigen_dec = eigen(cov(coords)); vec_major = eigen_dec$vectors[,1]
  angle = atan2(vec_major[2], vec_major[1]) * (180 / pi); if(angle < 0) angle = angle + 180
  
  liste_ellipses_globales_sf[[length(liste_ellipses_globales_sf) + 1]] <- st_sf(
    Commune = grp$Libéllé.Commune[1], Type_achat = grp$Type_achat[1], Nombre_Logements_Total = nrow(grp), 
    Superficie_km2 = area / 1000000, Orientation = angle, geometry = st_sfc(poly, crs = 2154)
  )
}

if(length(liste_ellipses_globales_sf) > 0) {
  couche_globales_QGIS <- do.call(rbind, liste_ellipses_globales_sf)
  for(type in unique(couche_globales_QGIS$Type_achat)) {
    st_write(obj = couche_globales_QGIS %>% filter(Type_achat == type), dsn = "Ellipses_HLM_Evolution.gpkg", layer = paste0("ellipses_GLOBALE_", gsub(" ", "_", type)), delete_layer = TRUE, quiet = TRUE)
  }
}

# C. Centroïdes et Distances
centroides_globaux = HLM_Aix_Mar_combine %>% group_by(Code.Commune, Libéllé.Commune, Type_achat) %>% summarise(X_moy = mean(Coordonnée.X, na.rm = TRUE), Y_moy = mean(Coordonnée.Y, na.rm = TRUE), .groups = 'drop')
st_write(obj = st_as_sf(centroides_globaux, coords = c("X_moy", "Y_moy"), crs = 2154), dsn = "Ellipses_HLM_Evolution.gpkg", layer = "centroides_globaux", delete_layer = TRUE, quiet = TRUE)

distances_centroides = centroides_globaux %>% filter(Type_achat %in% c("HLM VEFA", "HLM non VEFA")) %>% select(Commune = Libéllé.Commune, Type_achat, X_moy, Y_moy) %>% pivot_wider(names_from = Type_achat, values_from = c(X_moy, Y_moy)) %>% mutate(Distance_VEFA_MOD_metres = sqrt((`X_moy_HLM VEFA` - `X_moy_HLM non VEFA`)^2 + (`Y_moy_HLM VEFA` - `Y_moy_HLM non VEFA`)^2)) %>% arrange(desc(Distance_VEFA_MOD_metres))
write_csv(distances_centroides, "distances_centroides_vefa_mod.csv")



# 6. Statistiques descritpives (orientation et dispersion)
Distrib_orientation_ellipses = ggplot(res_ellipses_periode, aes(x = Orientation, fill = Type_achat)) +
  geom_histogram(bins = 10, color = "black", show.legend = FALSE) + facet_wrap(~ Type_achat, ncol = 1) + 
  scale_fill_manual(values = c("Ensemble" = "grey", "HLM non VEFA" = "#00BFC4", "HLM VEFA" = "#F8766D")) +
  labs(title = "Distribution de l'orientation", x = "Angle (degrés)", y = "Fréquence") + theme_minimal() + theme(plot.background = element_rect(fill = "white", color = NA), strip.text = element_text(size = 12, face = "bold"))
ggsave("C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Graphiques/SCE et centroïdes/Distrib_orientation_ellipses_par_type.png", plot = Distrib_orientation_ellipses, width = 10, height = 10, dpi = 300, bg = "white")

Distrib_etalement_ellipses = ggplot(res_ellipses_periode, aes(x = Superficie_Km2, fill = Type_achat)) +
  geom_histogram(bins = 10, color = "black", show.legend = FALSE) + facet_wrap(~ Type_achat, ncol = 1, scales = "free_y") + 
  scale_fill_manual(values = c("Ensemble" = "grey", "HLM non VEFA" = "#00BFC4", "HLM VEFA" = "#F8766D")) +
  labs(title = "Distribution de l'étalement", x = "Superficie (Km²)", y = "Fréquence") + theme_minimal() + theme(plot.background = element_rect(fill = "white", color = NA), strip.text = element_text(size = 12, face = "bold"))
ggsave("C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Graphiques/SCE et centroïdes/Distrib_etalements_ellipses_par_type.png", plot = Distrib_etalement_ellipses, width = 10, height = 10, dpi = 300, bg = "white")


# Graphique centroïdes
distances_propres <- distances_centroides %>% 
  filter(!is.na(Distance_VEFA_MOD_metres))

if(nrow(distances_propres) > 0)
  
  # 1. Histogramme de la distribution des distances
  Distrib_distances = ggplot(distances_propres, aes(x = Distance_VEFA_MOD_metres)) +
    geom_histogram(bins = 15, fill = "orchid4", color = "grey") +
    labs(
      title = "Distribution des écarts spatiaux entre production VEFA et MOD",
      subtitle = "Distance en mètres entre les centres de gravité par commune",
      x = "Distance entre les centroides (mètres)", 
      y = "Nombre de communes"
    ) +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white", color = NA))
  
  print(Distrib_distances)
  
  ggsave("C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Graphiques/SCE et centroïdes/Distrib_distances_centroides.png", 
         plot = Distrib_distances, width = 10, height = 8, dpi = 300, bg = "white")
  
  # 2. Les 15 communes avec la plus grande distance
  Top15_distances = distances_propres %>% head(15)
  
  Graph_top_distances = ggplot(Top15_distances, aes(x = reorder(Commune, Distance_VEFA_MOD_metres), y = Distance_VEFA_MOD_metres)) +
    geom_col(fill = "tan2", color = "grey") +
    coord_flip() +
    labs(
      title = "Top 15 des communes avec le plus fort décalage spatial VEFA / MOD",
      subtitle = "Toutes périodes confondues",
      x = "Commune", 
      y = "Distance séparant le barycentre VEFA du barycentre MOD (mètres)"
    ) +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white", color = NA))
  
  print(Graph_top_distances)
  
  ggsave("C:/Users/thema/Desktop/Master 1 GEOTER/Mémoire/Graphiques/SCE et centroïdes/Top15_distances_centroides.png", 
         plot = Graph_top_distances, width = 10, height = 8, dpi = 300, bg = "white")