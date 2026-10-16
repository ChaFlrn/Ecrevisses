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
  filter(Date < "2015",
         Presence == "Présent") 

data_statut_2015 <- bdd_ecrevisse %>%
  st_point_on_surface() %>%
  st_cast("POINT") %>%
  filter(Date >= "2015" & Date <= "2020",
         Presence == "Présent") 

data_statut_2020 <- bdd_ecrevisse %>%
  st_point_on_surface() %>%
  st_cast("POINT") %>%
  filter(Date > "2020",
         Presence == "Présent") 


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
         geom_type %in% c("POINT", "MULTIPOINT"),
         Presence == "Présent") %>%
  mutate(periode = case_when(
    Date < "2014" ~ "Avant 2014",
    Date >= "2014" & Date < "2024" ~ "Entre 2014 et 2023",
    Date > "2023" ~ "Après 2023")) %>%
  st_centroid()


st_write(data_APP, "processed_data/data_APP.gpkg", 
         append = FALSE,
         driver = "GPKG")




