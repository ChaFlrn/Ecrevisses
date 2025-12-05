#%%%%%%%%%%%%%%%%%%%%%%%% Assemblage des bases %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###---------------------------------------------------------#
cli::cli_h1("Lire les fichiers format .gpkg") 

data_oison <- st_read("processed_data/data_oison.gpkg")

data_fauna <- st_read("processed_data/data_fauna.gpkg")


###---------------------------------------------------------#
cli::cli_h1("Supprimer les doublons OFB") 

# Vérifier dans la base FAUNA la date des dernières remontées de données OFB
# Supprimer ces données dans le fichier OISON pour éviter les doublons

data_oison <- data_oison %>%
  filter(Date < 1984 | Date > 2021)


###---------------------------------------------------------#
cli::cli_h1("Assembler les bases")

bdd_ecrevisse <- rbind(data_oison,
                       data_fauna)

###---------------------------------------------------------#
cli::cli_h1("Nettoyage du fichier")

bdd_ecrevisse <- bdd_ecrevisse %>%
  mutate(Cdnom = recode(Cdnom,
                        "983403" = "162666",
                        "853999" = "17646"),
         
         Nom_vernaculaire = case_when(
    Cdnom == "162666" ~ "Ecrevisse a pattes greles",
    Cdnom == "162667" ~ "Ecrevisse de Californie",
    Cdnom == "162668" ~ "Ecrevisse de Louisiane",
    Cdnom == "17646" ~ "Ecrevisse americaine",
    Cdnom == "18432" ~ "Ecrevisse a pattes rouges",
    Cdnom == "18437" ~ "Ecrevisse a pieds blancs"),
    
        Fournisseur = recode(Fournisseur,
                             "OFFICE FRANCAIS DE LA BIODIVERSITE - OFB DIRECTION REGIONALE NOUVELLE AQUITAINE (OFB" = "OFB",
                             "OFFICE FRANCAIS DE LA BIODIVERSITE - OFB DIRECTION REGIONALE CENTRE VAL LOIRE (OFB" = "OFB",
                             "OFFICE FRANCAIS DE LA BIODIVERSITE - OFB DIRECTION REGIONALE OCCITANIE (OFB" = "OFB"),
        Nom_scientifique = str_remove(Nom_scientifique, "\\s*\\([^\\)]*\\)")) %>%
  filter(Cdnom != "65899") # Supression de l'écrevisse de terre (courtillère)
  

###---------------------------------------------------------#
cli::cli_h1("Sauvegarder le fichier")

st_write(bdd_ecrevisse, "processed_data/bdd_ecrevisse.gpkg", 
         append = FALSE,
         driver = "GPKG")



