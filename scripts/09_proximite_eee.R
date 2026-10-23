#%%%%%%%%%%%%%%%%% Cartographie proximité des EEE et APP %%%%%%%%%%%%%%%%%%%%%%%

####-------------------Récupérer les données--------------------------------####
cli::cli_h1("Récupérer les données écrevisses") 

bdd_ecrevisse <- st_read("processed_data/bdd_ecrevisse.gpkg")


####-------------------Récupérer les données--------------------------------####
cli::cli_h1("Détecter les espèces dans un rayon de 3km") 

data_APP <- bdd_ecrevisse %>%
  filter(Cdnom == "18437" & Presence == "Présent")

data_EEE <- bdd_ecrevisse %>%
  filter(Cdnom == "117646" | Cdnom == "162667" | Cdnom == "162668",
         Presence == "Présent")

proximite_eee <- st_is_within_distance(data_APP,
                                       data_EEE,
                                       dist = 3000)

resultats_APP_EEE <- data_APP %>%
  mutate(PFL = sapply(proximite_eee, function(i) {
    any(data_EEE$Cdnom[i] == "162667")}),
    
    PCC = sapply(proximite_eee, function(i) {
      any(data_EEE$Cdnom[i] == "162668")}),
    
    OCL = sapply(proximite_eee, function(i) {
      any(data_EEE$Cdnom[i] == "17646")})) %>%
  
  mutate(proximite = case_when(
    PFL ~ "PFL",
    PCC ~ "PCC",
    OCL ~ "OCL",
    TRUE ~ "Aucune"))


####-------------------Enregistrer les données------------------------------####

st_write(resultats_APP_EEE, "processed_data/resultats_APP_EEE.gpkg",
         append = FALSE,
         driver = "GPKG") 



