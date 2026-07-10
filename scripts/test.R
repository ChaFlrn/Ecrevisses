rf_final <- randomForest(
  factor(Presence_bin) ~ .,
  data = data_acp,
  importance = TRUE,
  ntree = 1000)

proba <- predict(
  rf_final,
  newdata = cours_troncons,
  type = "prob")


cours_troncons$proba_presence <- proba[,"1"]

summary(cours_troncons$proba_presence)

hist(cours_troncons$proba_presence)


##### Préparation fichier pour carte #####

seuil <- 0.337 #seuil défini par les caractéristiques du modèle 

cours_troncons$predic <- ifelse(cours_troncons$proba_presence >= seuil, 1, 0)

cours_troncons$classe_habitat <- cut(
  cours_troncons$proba_presence,
  breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
  include.lowest = TRUE,
  labels = c(
    "Très faible",
    "Faible",
    "Moyen",
    "Favorable",
    "Très favorable"))


##### Carte simple #####
ggplot(cours_troncons) +
  geom_sf(aes(color = proba_presence))




####----------------------------Sauvegarde---------------------------------####
cli::cli_h1("Sauvegarde du fichier pour QGIS")

st_write(cours_troncons, "processed_data/cours_troncons.gpkg",
         append = FALSE,
         driver = "GPKG")





