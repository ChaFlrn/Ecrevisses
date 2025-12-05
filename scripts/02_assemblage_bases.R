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
cli::cli_h1("Sauvegarder le fichier")

st_write(bdd_ecrevisse, "processed_data/bdd_ecrevisse.gpkg", 
         append = FALSE,
         driver = "GPKG")



