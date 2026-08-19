#%%%%%%%%%%%%%%%%%%%%%%%% Préparation pour Qgis %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###---------------------------------------------------------#
cli::cli_h1("Lire le fichier format .gpkg") 

bdd_ecrevisse <- st_read("processed_data/bdd_ecrevisse.gpkg")


###---------------------------------------------------------#
cli::cli_h1("Fichiers concernant les contributeurs") 

data_fournisseur_inf_2015 <- bdd_ecrevisse %>%
  st_point_on_surface() %>%
  st_cast("POINT") %>%
  filter(Date < "2015") 

data_fournisseur_2015 <- bdd_ecrevisse %>%
  st_point_on_surface() %>%
  st_cast("POINT") %>%
  filter(Date >= "2015" & Date <= "2020") 

data_fournisseur_2020 <- bdd_ecrevisse %>%
  st_point_on_surface() %>%
  st_cast("POINT") %>%
  filter(Date > "2020") 

data_fournisseur <- bdd_ecrevisse %>%
  st_point_on_surface() %>%
  st_cast("POINT") %>%
  mutate(Periode = case_when(
    Date >= "2015" & Date < "2020" ~ "2015 - 2020",
    Date >= "2020" ~ "2020 - 2025",
    TRUE ~ "Avant_2015")) %>%
  filter(Periode != "Avant_2015")

st_write(data_fournisseur_inf_2015, "processed_data/data_fournisseur_inf_2015.gpkg", 
         append = FALSE,
         driver = "GPKG")


st_write(data_fournisseur_2015, "processed_data/data_fournisseur_2015.gpkg", 
         append = FALSE,
         driver = "GPKG")



st_write(data_fournisseur_2020, "processed_data/data_fournisseur_2020.gpkg", 
         append = FALSE,
         driver = "GPKG")

st_write(data_fournisseur, "processed_data/data_fournisseur.gpkg", 
         append = FALSE,
         driver = "GPKG")

###---------------------------------------------------------#
cli::cli_h1("Fichiers concernant les statuts d'espèces") 

data_statut_inf_2015 <- bdd_ecrevisse %>%
  st_point_on_surface() %>%
  st_cast("POINT") %>%
  filter(Date < "2015") 

data_statut_2015 <- bdd_ecrevisse %>%
  st_point_on_surface() %>%
  st_cast("POINT") %>%
  filter(Date >= "2015" & Date <= "2020") 

data_statut_2020 <- bdd_ecrevisse %>%
  st_point_on_surface() %>%
  st_cast("POINT") %>%
  filter(Date > "2020") 


st_write(data_statut_inf_2015, "processed_data/data_statut_inf_2015.gpkg", 
         append = FALSE,
         driver = "GPKG")


st_write(data_statut_2015, "processed_data/data_statut_2015.gpkg", 
         append = FALSE,
         driver = "GPKG")



st_write(data_statut_2020, "processed_data/data_statut_2020.gpkg", 
         append = FALSE,
         driver = "GPKG")


###---------------------------------------------------------#
cli::cli_h1("Populations APP après 2020") 

cours_eau <- st_read("assets/cours_deau_NA.gpkg")

geom_type <- st_geometry_type(bdd_ecrevisse)

data_APP <- bdd_ecrevisse %>%
  filter(Cdnom == "18437",
         geom_type %in% c("POINT", "MULTIPOINT")) %>%
  mutate(periode = if_else(Date >= "2020" & Presence == "Présent", "Apres_2020", "Avant_2020")) %>%
  st_centroid()


cours_eau <- cours_eau %>%
  st_transform(2154) %>%
  st_cast("LINESTRING") %>%
  st_buffer(dist = 500) %>%
  select(cleabs,
         code_hydrographique,
         toponyme)

id_proche <- st_nearest_feature(data_APP, cours_eau)

data_APP$Cours_eau <- cours_eau$toponyme[id_proche]

data_APP_ce <- data_APP %>%
  filter(!is.na(Cours_eau)) %>%
  group_by(Cours_eau) %>%
  group_modify(~ regrouper_points(.x, distance = 500)) %>%
  ungroup()

data_APP_na <- data_APP %>%
  filter(is.na(Cours_eau)) %>%
  regrouper_points(distance = 500)

data_APP_groupe <- rbind(data_APP_ce,
                         data_APP_na)

data_APP_final <- data_APP_groupe %>%
  mutate(site = ifelse(
    is.na(Cours_eau),
    paste0("NA_", site_local),
    paste0(Cours_eau, "_", site_local))) %>%
  group_by(site) %>%
  slice(1) %>%
  filter(!is.na(periode))




st_write(data_APP_final, "processed_data/data_APP_final.gpkg", 
         append = FALSE,
         driver = "GPKG")

