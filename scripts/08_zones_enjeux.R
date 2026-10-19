#%%%%%%%%%%%%%%% Cartographie simplfiiée des zones à enjeux %%%%%%%%%%%%%%%%%%%%

####-------------------Récupérer les données--------------------------------####
cli::cli_h1("Récupérer les prédictions cours d'eau") 

ce_predict <- st_read("processed_data/cours_eau_predict.gpkg")


cli::cli_h1("Récupérer les données présence APP") 

presence_APP <- st_read("processed_data/data_APP.gpkg")

cli::cli_h1("Récupérer la grille nationale 10x10km") 


grille_10 <- st_read("assets/grille_10.shp")

grille_10 <- st_transform(grille_10, st_crs(presence_APP))

####---------------------Croiser les données--------------------------------####
cli::cli_h1("Croisement grille et présence APP") 

poids_APP <- presence_APP %>%
  mutate(poids = case_when(
    Date >= "2014" & Date < "2023" ~ 3,
    Date >= "2023" ~ 5,
    TRUE ~ 1))


intersections <- st_intersects(poids_APP, grille_10)

id_maille <- vapply(
  intersections,
  function(x) {
    if(length(x) == 0) {
      NA_integer_ 
    } else {
        x[1]}},
  integer(1))

poids_APP$ID_maille <- grille_10$cd_sig[id_maille]

comptage_APP <- poids_APP %>%
  st_drop_geometry() %>%
  filter(!is.na(ID_maille)) %>%
  group_by(ID_maille) %>%
  summarise(nb_points = n(),
            total_pondere = sum(poids, na.rm = TRUE), .groups = "drop")



grille_APP <- grille_10 %>%
  left_join(comptage_APP,
            by = c("cd_sig" = "ID_maille")) %>%
  mutate(nb_points = coalesce(nb_points, 0L),
         total_pondere = coalesce(total_pondere, 0L))



cli::cli_h1("Croisement grille et prédictions cours d'eau") 

troncons_ce <- ce_predict %>%
  st_intersection(grille_10) %>%
  filter(as.numeric(st_length(geom)) > 0)

troncons_ce <- troncons_ce %>%
  mutate(poids = case_when(
    classe_habitat == "Très favorable" ~ 5,
    classe_habitat == "Favorable" ~ 5,
    classe_habitat == "Moyen" ~ 5,
    TRUE ~ 0),
    
    longueur_m = round(as.numeric(st_length(geom)),2))


score_ce <- troncons_ce %>%
  st_drop_geometry() %>%
  filter(!is.na(poids),
         !is.na(longueur_m)) %>%
  group_by(cd_sig) %>%
  summarise(longueur_totale_m = sum(longueur_m),
            longueur_ponderee = sum(longueur_m * poids),
            score = round(longueur_ponderee / longueur_totale_m, 2), .groups = "drop")


grille_ce <- grille_10 %>%
  left_join(score_ce,
            by = "cd_sig") %>%
  mutate(score = coalesce(score, 0),
         longueur_totale_m = coalesce(longueur_totale_m, 0))


####---------------------Normaliser les deux grilles------------------------####
cli::cli_h1("Rassembler les 2 scores") 

score_APP <- grille_APP %>%
  st_drop_geometry() %>%
  select(cd_sig,
         total_pondere)

score_CE <- grille_ce %>%
  st_drop_geometry() %>%
  select(cd_sig,
         score)

grille_finale <- grille_10 %>%
  left_join(score_APP,
            by = "cd_sig") %>%
  left_join(score_CE,
            by = "cd_sig") %>%
  mutate(pts_norm = (total_pondere - min(total_pondere, na.rm = TRUE)) / 
           (max(total_pondere, na.rm = TRUE) - min(total_pondere, na.rm = TRUE)),
         
         ce_norm = (score - min(score, na.rm = TRUE)) / 
           (max(score, na.rm = TRUE) - min(score, na.rm = TRUE)),
         
         score_global = round(0.6 * pts_norm + 0.4 * ce_norm,2))



####---------------------Sauvegarder les données----------------------------####

st_write(grille_APP, "processed_data/grille_APP.gpkg",
         append = FALSE,
         driver = "GPKG")

st_write(grille_ce, "processed_data/grille_ce.gpkg",
         append = FALSE,
         driver = "GPKG")


st_write(grille_finale, "processed_data/grille_finale.gpkg",
         append = FALSE,
         driver = "GPKG")



