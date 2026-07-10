#%%%%%%%%%%%%%%%%%%%%%%%% Prédictions présence APP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load(file = "processed_data/data_APP_propre.RData")

####--------------------Analyse Composantes Principales---------------------####
cli::cli_h1("ACP")

data_acp <- data_APP_propre %>%
  st_set_geometry(NULL) %>%
  mutate(Presence_bin = ifelse(Presence_APP == "Absence", 0, 1),
         CdNatureMa = as.numeric(CdNatureMa),
         across(where(is.numeric), ~ replace_na(.x, 0)),
         classe_bio = replace_na(classe, "0"),
         classe_bio = if_else(classe_bio == "U", "0", classe_bio),
         classe_bio = as.numeric(classe_bio)) %>%
  select(Presence_bin,
         surface_m,
         altitude_moy,
         temp_moy,
         pente_pct,
         ox_dis_moy,
         classe_bio)


acp <- dudi.pca(data_acp %>%
                  select(-Presence_bin),
                scale = TRUE,
                scannf = FALSE,
                nf = 2)

100*acp$eig / sum(acp$eig)

# Cercle de corrélations
s.corcircle(acp$co,
            clabel = 0.8)


# Contribution des axes
acp$co


#### Matrice de corrélation ####

cor_mat <- cor(
  data_acp %>%
    select(-Presence_bin),
  use = "complete.obs"
)

round(cor_mat, 2)

####--------------------Modèles prédiction---------------------####
cli::cli_h1("Modèles de prédiction")

##### Modèle GLM #####

data_std <- data_acp
data_std[, -c(1)] <- scale(data_std[, -c(1)])

mod_glm <- glm(
  Presence_bin ~ altitude_moy +
    pente_pct +
    temp_moy +
    ox_dis_moy +
    surface_m +
    classe_bio,
  family = binomial,
  data = data_std
)

exp(coef(mod_glm))
summary(mod_glm)$coefficients

set.seed(123)

id_train <- sample(
  seq_len(nrow(data_std)),
  size = 0.7 * nrow(data_std)
)

train <- data_std[id_train, ]
test  <- data_std[-id_train, ]

glm_mod <- glm(
  Presence_bin ~ .,
  family = binomial,
  data = train
)

prob_glm <- predict(
  glm_mod,
  newdata = test,
  type = "response"
)


roc_glm <- roc(
  test$Presence_bin,
  prob_glm
)

auc(roc_glm)

##### Modèle Random Forest #####

rf <- randomForest(
  factor(Presence_bin) ~ .,
  data = data_acp,
  importance = TRUE
)

print(rf)
importance(rf)
varImpPlot(rf)

# Robustesse modèle
set.seed(123)

id_train <- sample(
  1:nrow(data_acp),
  size = 0.7 * nrow(data_acp)
)

train <- data_acp[id_train, ]
test  <- data_acp[-id_train, ]

rf_train <- randomForest(
  factor(Presence_bin) ~ .,
  data = train,
  importance = TRUE
)

prob_test <- predict(
  rf_train,
  newdata = test,
  type = "prob"
)[,2]

roc_test <- roc(test$Presence_bin, prob_test)

auc(roc_test)

# Choix du modèle en fonction du meilleur résultat de l'AUC


##### Probabilité absence/présence #####
coords(roc_test, 
       "best",
       best.method = "youden")


pred_class <- ifelse(prob_test > 0.337, 1, 0)

confusionMatrix(
  factor(pred_class, levels = c(0,1)),
  factor(test$Presence_bin, levels = c(0,1)),
  positive = "1"
)

####--------------------Prédiction régionale---------------------####
cli::cli_h1("Prédiction régionale")

cours_troncons <- st_read("processed_data/cours_eau_final.gpkg")


##### Traitement des NA #####
vars_rf <- c(
  "surface_m",
  "altitude_moy",
  "temp_moy",
  "pente_pct",
  "ox_dis_moy",
  "classe_bio")


troncons_ok <- cours_troncons %>%
  filter(if_all(all_of(vars_rf), ~ !is.na(.))) %>%
  st_drop_geometry(NULL)


##### Prédictions #####

rf_final <- randomForest(
  factor(Presence_bin) ~ .,
  data = data_acp,
  importance = TRUE,
  ntree = 1000)

proba <- predict(
  rf_final,
  newdata = troncons_ok,
  type = "prob")[,"1"]


troncons_ok$proba_presence <- proba


summary(troncons_ok$proba_presence)

hist(troncons_ok$proba_presence)


##### Préparation fichier pour carte #####

seuil <- 0.337 #seuil défini par les caractéristiques du modèle 

troncons_ok$predic <- ifelse(troncons_ok$proba_presence >= seuil, 1, 0)

table(troncons_ok$predic)


# Seuils naturels (Jenks) en fonction du seuil de Youden

groupe_bas <- proba[proba < seuil]
groupe_haut <- proba[proba >= seuil]
 
jenks_bas <- classIntervals(groupe_bas, n = 2, style = "jenks") # détermination des seuils de classes "très faible" et "faible"
jenks_haut <- classIntervals(groupe_haut, n = 3, style = "jenks") # classes "moyen", "favorable", "très favorable"

troncons_ok$classe_habitat <- cut(
  troncons_ok$proba_presence,
  breaks = c(0, 0.168, 0.337, 0.495, 0.669, 1),
  include.lowest = TRUE,
  labels = c(
    "Très faible",
    "Faible",
    "Moyen",
    "Favorable",
    "Très favorable"))



####----------------------------Sauvegarde---------------------------------####
cli::cli_h1("Sauvegarde du fichier pour QGIS")

cours_eau_predict <- troncons_ok %>%
  left_join(cours_troncons %>%
              select(cleabs,
                     geom),
            by = "cleabs")


st_write(cours_eau_predict, "processed_data/cours_eau_predict.gpkg",
         append = FALSE,
         driver = "GPKG")





