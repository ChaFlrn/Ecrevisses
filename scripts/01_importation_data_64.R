#%%%%%%%%%%%%%%%%%%%%% Importation données SD 64 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###---------------------------------------------------------#
cli::cli_h1("Lire le fichier format .gpkg") 

data_64 <- st_read("raw_data/SD64/data64.shp")


###---------------------------------------------------------#
cli::cli_h1("Assembler et trier le fichier")

data_64 <- data_64 %>%
  mutate(Date_precis = as.Date(Date),
         Date = substr(Date, 1,4),
         Cdnom = recode(Espèce,
                        "PCC" = "162668",
                        "OCL" = "17646",
                        "PFL" = "162667",
                        "ASL" = "162666",
                        "ASA" = "18432"),
         
         Nom_vernaculaire = case_when(
           Cdnom == "162666" ~ "Ecrevisse a pattes greles",
           Cdnom == "162667" ~ "Ecrevisse de Californie",
           Cdnom == "162668" ~ "Ecrevisse de Louisiane",
           Cdnom == "17646" ~ "Ecrevisse americaine",
           Cdnom == "18432" ~ "Ecrevisse a pattes rouges"),
         
         Nom_scientifique = case_when(
           Cdnom == "162666" ~ "Astacus leptodactylus",
           Cdnom == "162667" ~ "Pacifastacus leniusculus",
           Cdnom == "162668" ~ "Procambarus clarkii",
           Cdnom == "17646" ~ "Faxonius limosus",
           Cdnom == "18432" ~ "Astacus astacus"),
         
         Fournisseur = Source,
         
         Departement = "64",
         Fiabilite = "Valide",
         Source = "FDAAPPMA 64") %>%
  rowwise() %>%
  mutate(
    Effectif = if (Espèce %in% names(cur_data())) {
      cur_data()[[Espèce]]
    } else {
      0
    },
    Presence = ifelse(Effectif == 0, "Absent", "Présent")) %>%
  ungroup() %>%
  select(Id = id,
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
cli::cli_h1("Vérifier les doublons")

data_64 <- data_64[!duplicated(data_64$Id), ]

###---------------------------------------------------------#
cli::cli_h1("Sauvegarder le fichier")

st_write(data_64, "processed_data/data_64.gpkg", 
         append = FALSE,
         driver = "GPKG")
