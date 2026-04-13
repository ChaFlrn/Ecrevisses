#%%%%%%%%%%%%%%%%%%%%% Importation données ASTAQUITAINE %%%%%%%%%%%%%%%%%%%%%%%%

###---------------------------------------------------------#
cli::cli_h1("Lire le fichier format .gpkg") 

data_astaq <- st_read("raw_data/Astaquitaine/data_ecrevisse.gpkg")


###---------------------------------------------------------#
cli::cli_h1("Assembler et trier le fichier")

data_astaq <- data_astaq %>%
  mutate(Date = substr(date, 1,4),
         Date_precis = as.Date(date),
         Cdnom = recode(releve_ecrevisse_espece,
                        "102" = "162666",
                        "104" = "17646",
                        "108" = "162667",
                        "106" = "162668",
                        "103" = "18432",
                        "110" = "18437",
                        "105" = "320575"),
         
         Nom_vernaculaire = case_when(
           Cdnom == "162666" ~ "Ecrevisse a pattes greles",
           Cdnom == "162667" ~ "Ecrevisse de Californie",
           Cdnom == "162668" ~ "Ecrevisse de Louisiane",
           Cdnom == "17646" ~ "Ecrevisse americaine",
           Cdnom == "18432" ~ "Ecrevisse a pattes rouges",
           Cdnom == "18437" ~ "Ecrevisse a pieds blancs",
           Cdnom == "320575" ~ "Ecrevisse calicot"),
         
         Nom_scientifique = case_when(
           Cdnom == "162666" ~ "Astacus leptodactylus",
           Cdnom == "162667" ~ "Pacifastacus leniusculus",
           Cdnom == "162668" ~ "Procambarus clarkii",
           Cdnom == "17646" ~ "Faxonius limosus",
           Cdnom == "18432" ~ "Astacus astacus",
           Cdnom == "18437" ~ "Austropotamobius pallipes",
           Cdnom == "320575" ~ "Orconectes immunis"),
         
         Fournisseur = ifelse(grepl("ONEMA", organisme), "OFB", organisme),

         
         Effectif = ifelse(is.na(releve_ecrevisse_quantite), "Non renseigné", releve_ecrevisse_quantite),
         
         Presence = "Présent",
         
         Departement = substr(departement_saisie, 2,3),
         Fiabilite = "Valide",
         Source = "Astaquitaine") %>%

  select(Id = num_fiche,
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
       Geometrie = geom)

###---------------------------------------------------------#
cli::cli_h1("Vérifier les doublons")

data_astaq <- data_astaq[!duplicated(data_astaq$Id), ]

###---------------------------------------------------------#
cli::cli_h1("Sauvegarder le fichier")

st_write(data_astaq, "processed_data/data_astaq.gpkg", 
         append = FALSE,
         driver = "GPKG")







