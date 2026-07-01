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
cli::cli_h1("Création des géopackages") 

dir.create("output/gpkg", showWarnings = FALSE)

for (dept in unique(bdd_ecrevisse_simple$Departement)) {
  
  gpkg <- paste0("output/gpkg/departement_", dept, ".gpkg")
  
  # Couche des observations
  data_dept <- bdd_ecrevisse %>%
    filter(Departement == dept)
  
  st_write(
    data_dept,
    gpkg,
    layer = "ecrevisses",
    delete_layer = TRUE,
    quiet = TRUE
  )}




