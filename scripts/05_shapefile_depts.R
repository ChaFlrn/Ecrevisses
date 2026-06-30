#%%%%%%%%%%%%%%%%% Création shapefile par département %%%%%%%%%%%%%%%%%%%%%%%%%%

###---------------------------------------------------------#
cli::cli_h1("Lire le fichier format .gpkg") 

bdd_ecrevisse <- st_read("processed_data/bdd_ecrevisse.gpkg")

###---------------------------------------------------------#
cli::cli_h1("Simplification du fichier") 

bdd_ecrevisse_simple <- bdd_ecrevisse %>%
  select(Id,
         Date,
         Cdnom,
         Nom_vernaculaire,
         Nom_scientifique,
         Presence,
         Departement,
         Fournisseur)

###---------------------------------------------------------#
cli::cli_h1("Séparation des départements") 

dir.create("output/gpkg", showWarnings = FALSE)

for (dept in unique(bdd_ecrevisse_simple$Departement)) {
  data_dept <- bdd_ecrevisse %>%
    filter(Departement == dept)
  st_write(data_dept,
           paste0("output/gpkg/departement_", dept,".gpkg"),
           delete_layer = TRUE)}



