#%%%%%%%%%%%%%% Préparation fichier prédictions présence APP %%%%%%%%%%%%%%%%%%%

####-------------------Base de données--------------------------------------####
cli::cli_h1("Récupérer la base de données") 

bdd_ecrevisse <- st_read("processed_data/bdd_ecrevisse.gpkg")

####----------------------Préparation fichier-------------------------------####
cli::cli_h1("Préparer le fichier APP")

data_APP <- bdd_ecrevisse %>%
  mutate(
    Ecrevisse = case_when(
      Cdnom == "18437" ~ "APP",
      Cdnom == "18432" ~ "ASA ou ASL",
      Cdnom == "162666" ~ "ASA ou ASL",
      TRUE ~ "EEE"),
    
    Presence_APP = case_when(
      Ecrevisse == "APP" & Presence == "Présent" ~ "Presence",
      Ecrevisse == "APP" & Presence == "Absent" ~ "Absence",
      Ecrevisse == "EEE" & Presence == "Présent" ~ "Absence",
      TRUE ~ "NSP")) %>%
  
  filter(Presence_APP != "NSP")
  
####------------------------Tri stations------------------------------------####
cli::cli_h1("Tri des stations à conserver")

cours_eau <- st_read("assets/cours_deau_NA.gpkg")

data_APP <- st_transform(data_APP, 2154)

cours_eau <- cours_eau %>%
  st_transform(2154) %>%
  st_cast("LINESTRING")

cours_eau$id_ligne <- 1:nrow(cours_eau)


# Conversion des géométries polygones et lignes en points

geom_type <- st_geometry_type(data_APP)

app_pts <- data_APP[geom_type %in% c("POINT", "MULTIPOINT"),]

app_autres <- data_APP[!geom_type %in% c("POINT", "MULTIPOINT"),]

voisins_rivieres <- st_nearest_feature(app_autres, cours_eau)

nearest_lines <- st_nearest_points(
  app_autres,
  cours_eau[voisins_rivieres, ],
  pairwise = TRUE
)

nearest_points <- st_cast(nearest_lines, "POINT")
source_points <- nearest_points[seq(1, length(nearest_points), by = 2)]

app_autres <- st_set_geometry(app_autres, source_points)

data_APP_pts <- rbind(app_pts,
                      app_autres)

data_APP_pts <- st_cast(data_APP_pts, "POINT")


# Eviter plusieurs points au même endroit (restriction 20m)

coords <- st_coordinates(data_APP_pts)

cluster_20 <-dbscan(coords, eps = 20, minPts = 1)

data_APP_pts$cluster_id <- cluster_20$cluster



# Sélection des points selon 3 filtres : espèce, effectif, date

data_APP_final <- data_APP_pts %>%
  mutate(priorite_esp = ifelse(Cdnom == "18437", 1, 0)) %>%
  group_by(cluster_id) %>%
  arrange(desc(priorite_esp),
          desc(Effectif),
          desc(Date_precis)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-cluster_id,
         -priorite_esp)


save(data_APP_final, file = "processed_data/data_APP_final.RData")
load(file = "processed_data/data_APP_final.RData")


####-------------------Données environnementales----------------------------####
cli::cli_h1("Récupérer les données environnementales")

##### Données alitude et pente #####
departements <- st_read("assets/departements.gpkg")

# Téléchargement DEM, peut prendre du temps en fonction de la taille du territoire
#mnt <- get_elev_raster(
  #locations = departements,
  #z = 12,
  #clip = "locations")

#mnt <- rast(mnt)

#writeRaster(
  #mnt,
  #"processed_data/mnt_departements.tif",
  #overwrite = TRUE)

mnt <- rast("processed_data/mnt_departements.tif")
plot(mnt)


# Associer chaque point au cours d'eau le plus proche

idx <- st_nearest_feature(data_APP_final, cours_eau)
data_APP_final$riviere_id <- cours_eau$id_ligne[idx]

resultats <- list()

for(i in 1:nrow(data_APP_final)){
  
  cat(
    "Station :",
    i,
    "/",
    nrow(data_APP_final),
    "\n"
  )
  
  # station courante
  station <- data_APP_final[i, ]
  
  # rivière associée
  ligne <- cours_eau[
    station$riviere_id,
  ]
  
  # sécurité
  if(nrow(ligne) == 0){
    next
  }
  
  # extraction tronçon
  troncon <- tryCatch(
    
    extraire_troncon_buffer(
      point = station,
      ligne = ligne,
      distance = 200
    ),
    
    error = function(e){
      return(NULL)
    }
  )
  
  if(is.null(troncon)){
    next
  }
  
  # calcul pente
  res <- tryCatch(
    
    calcul_pente_mnt(
      troncon,
      mnt
    ),
    
    error = function(e){
      return(NULL)
    }
  )
  
  if(is.null(res)){
    next
  }
  
  # stockage résultats
  resultats[[i]] <- data.frame(
    
    id_station = station$Id,
    
    altitude_moy = res$altitude_moy,
    
    altitude_min = res$altitude_min,
    
    altitude_max = res$altitude_max,
    
    z_debut = res$z_debut,
    
    z_fin = res$z_fin,
    
    pente_m_m = res$pente_m_m,
    
    pente_pct = res$pente_pct
  )
}

# Assemblage final
data_station_alti_pente <- bind_rows(resultats)

save(data_station_alti_pente, file = "processed_data/data_station_alti_pente.RData")
load(file = "processed_data/data_station_alti_pente.RData")

##### Température de l'eau et Qualité physico-chimique #####

url <- paste0(
  "https://hubeau.eaufrance.fr/api/v2/qualite_rivieres/station_pc?code_region=75",
  "&size=20000"
)

req <- GET(url)

txt <- content(
  req,
  "text",
  encoding = "UTF-8"
)

stations_na <- fromJSON(txt)$data

stations_na <- stations_na %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>%
  st_transform(crs = 2154)

save(stations_na, file = "processed_data/stations_na.RData")
load(file = "processed_data/stations_na.RData" )


idx <- st_nearest_feature(data_APP_final,
                          stations_na)

data_APP_final$code_station_hubeau <- stations_na$code_station[idx]

data_APP_final$nom_station_hubeau <- stations_na$libelle_station[idx]

distances <- st_distance(
  data_APP_final,
  stations_na[idx, ],
  by_element = TRUE
)

data_APP_final$distance_hubeau_m <- as.numeric(distances)

codes <- unique(data_APP_final$code_station_hubeau)

temp_ox <- map_dfr(
  
  codes,
  
  function(code){
    
    cat("Téléchargement :", code, "\n")
    
    tryCatch(
      
      telecharger_temperature(code),
      
      error = function(e) NULL
    )
  }
)


save(temp_ox, file = "processed_data/temp_ox.RData")
load("processed_data/temp_ox.RData")

# Ne garder que les paramètres voulus et leurs créer une colonne

stations_quali <- temp_ox %>%
  filter(code_parametre %in% c("1301","1311")) %>%
  group_by(code_station) %>%
  summarise(temp_min = min(resultat[code_parametre == "1301"], na.rm = TRUE),
         temp_max = max(resultat[code_parametre == "1301"], na.rm = TRUE),
         temp_moy = mean(resultat[code_parametre == "1301"], na.rm = TRUE),
         
         ox_dis_min = min(resultat[code_parametre == "1311"], na.rm = TRUE),
         ox_dis_max = max(resultat[code_parametre == "1311"], na.rm = TRUE),
         ox_dis_moy = mean(resultat[code_parametre == "1311"], na.rm = TRUE),
         
         n = n(), .groups = "drop")

save(stations_quali, file = "processed_data/stations_quali.RData")
load(file = "processed_data/stations_quali.RData")


##### Bassins versants topographiques, Rang de Strahler et Anthropisation des masses d'eau #####

bv_topo <- st_read("assets/bv_na.gpkg")

masse_eau <- st_read("assets/masses_eau_na.gpkg")

masse_eau_bv <- masse_eau %>%
  st_transform(crs = st_crs(bv_topo)) %>%
  st_intersection(bv_topo) %>%
  mutate(intersection = st_length(geom)) %>%
  group_by(gid) %>%
  slice_max(intersection,
            n = 1,
            with_ties = FALSE) %>%
  ungroup() %>%
  st_set_geometry(NULL) %>%
  select(id_masse = gid,
         NomMasseDE,
         CdNatureMa,
         CategorieG,
         StrahlMax,
         StrahlMin,
         cdoh)

bv_topo_me <- bv_topo %>%
  left_join(masse_eau_bv, by = "cdoh")


cours_eau_bv <- cours_eau %>%
  st_intersection(bv_topo_me) %>%
  mutate(intersection = st_length(geom)) %>%
  group_by(id_ligne) %>%
  slice_max(intersection,
            n = 1,
            with_ties = FALSE) %>%
  ungroup() %>%
  st_set_geometry(NULL)


save(cours_eau_bv, file = "processed_data/cours_eau_bv.RData")
load(file = "processed_data/cours_eau_bv.RData")


##### Etat biologique/écologique des masses d'eau ######
# Bassin Adour-Garonne
data_adour_garonne <- read.csv2(file = "assets/etats.csv",
                                fileEncoding = "latin1")

data_adour_garonne <- data_adour_garonne %>%
  filter(code_alteration == "B_BIO") %>%
  group_by(station) %>%
  slice(1) %>%
  ungroup() %>%
  select(station,
         nom_station,
         classe)

# Bassin Loire-Bretagne
data_loire_bzh <- read.csv2(file = "assets/etat_bio_lb_2023.csv",
                            fileEncoding = "latin1")

data_loire_bzh <- data_loire_bzh %>%
  select(station = Code.station,
         nom_station = Libellé.station,
         classe = Etat_bio)

# Assemblage fichiers

data_age <- rbind(data_adour_garonne,
                  data_loire_bzh)

data_age <- data_age %>%
  mutate(station = as.character(station),
         station = paste0("0", station))

save(data_age, file = "processed_data/data_age.RData")

####-------------------Assemblage données----------------------------####
cli::cli_h1("Récupérer les données environnementales")

data_APP_propre <- data_APP_final %>%
  left_join(cours_eau_bv %>%
              select(id_ligne,
                     surface_m,
                     StrahlMax,
                     StrahlMin,
                     CdNatureMa),
            by = c("riviere_id" = "id_ligne")) %>%
  left_join(data_station_alti_pente %>%
              select(id_station,
                     altitude_moy,
                     altitude_min,
                     altitude_max,
                     pente_pct),
            by = c("Id" = "id_station")) %>%
  left_join(stations_quali %>%
              select(-n),
            by = c("code_station_hubeau" = "code_station")) %>%
  left_join(data_age,
            by = c("code_station_hubeau" = "station"))


save(data_APP_propre, file = "processed_data/data_APP_propre.RData")
load(file = "processed_data/data_APP_propre.RData")




####-------------------Créer fichier général régional----------------------------####
cli::cli_h1("Construire fichier régional des cours d'eau")

##### Altitude, pente #####
mnt <- rast("processed_data/mnt_departements.tif")

cours_eau <- st_read("assets/cours_deau_NA.gpkg")

pente_pct <- tan(terrain(mnt, "slope", unit = "radians")) * 100

cours_eau_v <- vect(cours_eau)

alt <- extract(
  mnt,
  cours_eau_v,
  fun = mean,
  na.rm = TRUE
)

save(alt, file = "processed_data/alt.RData")
load("processed_data/alt.RData")

pent <- extract(
  pente_pct,
  cours_eau_v,
  fun = mean,
  na.rm = TRUE
)

save(pent, file = "processed_data/pent.RData")
load("processed_data/pent.RData")

cours_eau$altitude_moy <- alt[,2]
cours_eau$pente_pct <- pent[,2]


##### Surface BV, rang de Strahler, artificialisation #####

bv_topo <- st_read("assets/bv_na.gpkg")

masse_eau <- st_read("assets/masses_eau_na.gpkg")

masse_eau_bv <- masse_eau %>%
  st_transform(crs = st_crs(bv_topo)) %>%
  st_intersection(bv_topo) %>%
  mutate(intersection = st_length(geom)) %>%
  group_by(gid) %>%
  slice_max(intersection,
            n = 1,
            with_ties = FALSE) %>%
  ungroup() %>%
  st_set_geometry(NULL) %>%
  select(id_masse = gid,
         NomMasseDE,
         CdNatureMa,
         CategorieG,
         StrahlMax,
         StrahlMin,
         cdoh)

bv_topo_me <- bv_topo %>%
  left_join(masse_eau_bv, by = "cdoh")


cours_eau_bv <- cours_eau %>%
  st_intersection(bv_topo_me) %>%
  mutate(intersection = st_length(geom)) %>%
  group_by(cleabs) %>%
  slice_max(intersection,
            n = 1,
            with_ties = FALSE) %>%
  ungroup()


st_write(cours_eau_bv, "processed_data/cours_eau_ind.gpkg",
         append = FALSE,
         driver = "GPKG")

cours_eau <- st_read("processed_data/cours_eau_ind.gpkg")


##### Température et oxygène dissous #####

station_ox <- get_qualite_rivieres_analyse(
  code_region = "75",
  code_parametre = "1311",
  date_debut_prelevement = "2024-01-01",
  date_fin_prelevement = Sys.Date())


station_temp <- get_qualite_rivieres_analyse(
  code_region = "75",
  code_parametre = "1301",
  date_debut_prelevement = "2024-01-01",
  date_fin_prelevement = Sys.Date())

quali_na <- bind_rows(station_ox,
                      station_temp)

quali_na <- quali_na %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(2154) %>%
  group_by(code_station) %>%
  summarise(temp_min = min(resultat[code_parametre == "1301"], na.rm = TRUE),
            temp_max = max(resultat[code_parametre == "1301"], na.rm = TRUE),
            temp_moy = mean(resultat[code_parametre == "1301"], na.rm = TRUE),
            
            ox_dis_min = min(resultat[code_parametre == "1311"], na.rm = TRUE),
            ox_dis_max = max(resultat[code_parametre == "1311"], na.rm = TRUE),
            ox_dis_moy = mean(resultat[code_parametre == "1311"], na.rm = TRUE),
            
            .groups = "drop")


save(quali_na, file = "processed_data/quali_na.RData")
load("processed_data/quali_na.RData")
st_write(quali_na, "processed_data/quali_na.gpkg",
         append = FALSE,
         driver = "GPKG")

load(file = "processed_data/data_age.RData")

quali_bio_na <- quali_na %>%
  left_join(data_age,
            by = c("code_station" = "station"))



##### Fichier cours d'eau final #####

cours_troncons <- cours_eau %>%
  st_join(quali_bio_na,
          join = st_nearest_feature) %>%
  mutate(classe_bio = replace_na(classe, "0"),
         classe_bio = if_else(classe_bio == "U", "0", classe_bio),
         classe_bio = as.numeric(classe_bio)) %>%
  select(-classe)



st_write(cours_troncons, "processed_data/cours_eau_final.gpkg",
         append = FALSE,
         driver = "GPKG")







