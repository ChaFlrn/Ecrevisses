#%%%%%%%%%%%%%%%%%%%% Importation données NAIADES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###---------------------------------------------------------#
cli::cli_h1("Lire le fichier format .csv") 

data_naiades <- read.table("raw_data/Naiades/ListesFauneFlore.CSV", 
                                   header = TRUE, 
                                   sep = ";", 
                                   fill = TRUE, 
                                   row.name = NULL)

codes_sandre <- c("2963", "873", "2028","871","68137","866","868","869","31843","31844")


data_naiades <- data_naiades %>%
  filter(CdAppelTaxon %in% codes_sandre) %>%
  mutate(Id = paste0("Naiades_", row_number()),
         Date = substr(DateDebutOperationPrelBio, 1,4),
         
         Cdnom = recode(CdAppelTaxon,
                        "2963" = "162666",
                        "68137" = "17646",
                        "873" = "162667",
                        "2028" = "162668",
                        "871" = "17646",
                        "866" = "18432",
                        "868" = "18437",
                        "869" = "159447",
                        "31843" = "534582",
                        "31844" = "320575"),
         
         Nom_vernaculaire = case_when(
           Cdnom == "162666" ~ "Ecrevisse a pattes greles",
           Cdnom == "162667" ~ "Ecrevisse de Californie",
           Cdnom == "162668" ~ "Ecrevisse de Louisiane",
           Cdnom == "17646" ~ "Ecrevisse americaine",
           Cdnom == "18432" ~ "Ecrevisse a pattes rouges",
           Cdnom == "18437" ~ "Ecrevisse a pieds blancs",
           Cdnom == "159447" ~ "Ecrevisse des torrents",
           Cdnom == "534582" ~ "Ecrevisse juvenile",
           Cdnom == "320575" ~ "Ecrevisse calicot"),
         
         Presence = "Présent",
         
         Effectif = ifelse(is.na(RsTaxRep), "Non renseigné", RsTaxRep),
         
         Source = "Naiades", 
         
         Fiabilite = "Valide") %>%
           
  select(Id,
         Date,
         Date_precis = DateDebutOperationPrelBio,
         Cdnom,
         Nom_vernaculaire,
         Nom_scientifique = NomLatinAppelTaxon,
         Effectif,
         Presence,
         Fournisseur = NomProducteur,
         Fiabilite,
         Source,
         Cdstation = CdStationMesureEauxSurface)

###---------------------------------------------------------#
cli::cli_h1("Récupérer les données spatiales") 

data_geo_naiades <- read.table("raw_data/Naiades/Stations.CSV", 
                           header = TRUE, 
                           sep = ";", 
                           fill = TRUE, 
                           row.name = NULL)

data_geo_naiades <- data_geo_naiades %>%
  select(Cdstation = CdStationMesureEauxSurface,
         InseeCom = CodeCommune,
         Commune = LbCommune,
         Departement = CodeDepartement,
         CoordX = CoordXStationMesureEauxSurface,
         CoordY = CoordYStationMesureEauxSurface)

data_geo_naiades <- st_as_sf(data_geo_naiades, coords = c("CoordX", "CoordY"), crs = 2154)


###---------------------------------------------------------#
cli::cli_h1("Joindre les données spatiales") 

data_naiades <- data_naiades %>%
  left_join(data_geo_naiades,
            by = "Cdstation") %>%
  mutate(Date_precis = as.Date(Date_precis)) %>%
  select(Id,
         Date,
         Date_precis,
         Cdnom,
         Nom_vernaculaire,
         Nom_scientifique,
         Effectif,
         Presence,
         Departement,
         InseeCom,
         Fiabilite,
         Fournisseur,
         Source,
         Geometrie = geometry)

###---------------------------------------------------------#
cli::cli_h1("Homogénéiser les variables") 

data_naiades <- data_naiades %>%
  mutate(Fournisseur = recode(Fournisseur,
                       "OFFICE FRANCAIS DE LA BIODIVERSITE - OFB DIRECTION REGIONALE NOUVELLE AQUITAINE (OFB)" = "OFB",
                       "OFFICE FRANCAIS DE LA BIODIVERSITE - OFB DIRECTION REGIONALE CENTRE VAL LOIRE (OFB)" = "OFB"))

###---------------------------------------------------------#
cli::cli_h1("Sauvegarder le fichier")

st_write(data_naiades, "processed_data/data_naiades.gpkg", 
         append = FALSE,
         driver = "GPKG")
