#%%%%%%%%%%%%%%%%%%%%% Importation données SD 19 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###---------------------------------------------------------#
cli::cli_h1("Lire le fichier format .shp") 

data_19_98 <- st_read("raw_data/SD19/CSP recensement 1998.shp",
                      options = "ENCODING=CP1252")

data_19_2000 <- st_read("raw_data/SD19/CSP recensement 2000.shp",
                      options = "ENCODING=CP1252")

data_19_APP <- st_read("raw_data/SD19/patte blanche 2.shp",
                      options = "ENCODING=CP1252")


###---------------------------------------------------------#
cli::cli_h1("Gérer les géométrie")

data_19_2000_pts <- data_19_2000 %>%
  st_point_on_surface()

data_19_98_pts <- data_19_98 %>%
  st_point_on_surface()


###---------------------------------------------------------#
cli::cli_h1("Assembler les fichiers")

data_19_APP <- data_19_APP %>%
  select(id,
         Année = annnÃ.e,
         Source,
         Abondance,
         Espèce = EspÃ.ce,
         Cours.eau)

data_sd19 <- rbind(data_19_98_pts,
                   data_19_2000_pts,
                   data_19_APP)

###---------------------------------------------------------#
cli::cli_h1("Trier le fichier")

data_sd19_propre <- data_sd19 %>%
  filter(!is.na(Espèce)) %>%
  mutate(Date_precis = as.Date(paste0(Année, "-01-01")),
         Id = paste0("SD19_", row_number()),
         Fournisseur = Source,
         Effectif = ifelse(is.na(Abondance), "Non renseigné", Abondance),
         Source = "SD 19",
         Departement = "19",
         Fiabilite = "Valide",
         Presence = "Présent",
         
         Cdnom = case_when(
           Espèce == "APP" ~ "18437",
           Espèce == "OCL" ~ "17646",
           Espèce == "PFL" ~ "162667"),
         
         Nom_scientifique = case_when(
           Cdnom == "162667" ~ "Pacifastacus leniusculus",
           Cdnom == "17646" ~ "Faxonius limosus",
           Cdnom == "18437" ~ "Austropotamobius pallipes"),
         
         Nom_vernaculaire = case_when(
           Cdnom == "162667" ~ "Ecrevisse de Californie",
           Cdnom == "17646" ~ "Ecrevisse americaine",
           Cdnom == "18437" ~ "Ecrevisse a pieds blancs"),
         
         Fournisseur = dplyr::recode(Fournisseur,
                                     "Roqueplot GuÃ©net" = "Roqueplot Guénet",
                                     "Village du Midi CorrÃ©zien" = "Village du Midi Corrézien",
                                     "OFB EF JD" = "OFB",
                                     "OFB EF LH" = "OFB")) %>%
  
  st_transform(crs = 2154) %>%
  
  
  select(Id,
         Date = Année,
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
st_write(data_sd19_propre, "processed_data/data_sd19.gpkg", 
         append = FALSE,
         driver = "GPKG")
