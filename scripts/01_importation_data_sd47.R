#%%%%%%%%%%%%%%%%%%%%% Importation données SD 47 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###---------------------------------------------------------#
cli::cli_h1("Lire le fichier format .csv") 

data_sd47 <- read.csv2(file = "raw_data/SD47/APP_recent.csv",
                       header = TRUE,
                       fileEncoding = "Windows-1252")

###---------------------------------------------------------#
cli::cli_h1("Trier le fichier")

data_sd47 <- data_sd47 %>%
  st_as_sf(coords = c("X_coord", "Y_coord"),
           crs = 2154) %>%
  mutate(Date_precis = as.Date(Date_precis, format = "%d/%m/%y")) %>%
  select(Id,
         Date,
         Date_precis,
         Cdnom,
         Nom_vernaculaire,
         Nom_scientifique,
         Effectif,
         Presence,
         Departement,
         Fiabilite,
         Fournisseur,
         Source,
         Geometrie = geometry)



###---------------------------------------------------------#
cli::cli_h1("Sauvegarder le fichier")
st_write(data_sd47, "processed_data/data_sd47.gpkg", 
         append = FALSE,
         driver = "GPKG")
