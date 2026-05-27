#%%%%%%%%%%%%%%%%%%%%%%% Prédictions présence APP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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


data_esp <- data_APP_final %>%
  filter(Presence == "Présent") %>%
  select(-Presence_APP)


####-------------------Données environnementales----------------------------####
cli::cli_h1("Récupérer les données environnementales")

##### Données alitude et pente #####
departements <- st_read("assets/departements.gpkg")


# emprise des stations
emprise <- st_bbox(data_APP_final)

# téléchargement DEM
mnt <- get_elev_raster(
  locations = departements,
  z = 12,
  clip = "locations"
)

mnt <- rast(mnt)

plot(mnt)

# Associer chaque point au cours d'eau le plus proche

idx <- st_nearest_feature(data_APP_final, cours_eau)
data_APP_final$riviere_id <- idx

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

# assemblage final
data_station_alti_pente <- bind_rows(resultats)


plot(
  st_geometry(data_APP_final),
  col = "black",
  pch = 16,
  cex = 0.3,
  add = TRUE
)

# Sauvegarde du fichier

save(data_station_alti_pente, file = "processed_data/data_station_alti_pente.RData")

bdd_APP_alti <- data_APP_final %>%
  left_join(data_station_alti_pente,
            by = c("Id" = "id_station"))

save(bdd_APP_alti, file = "processed_data/bdd_APP_alti.RData")


##### Bassins versants topographiques #####

bv_topo <- st_read("assets/bv_na.gpkg")

cours_eau_bv <- cours_eau %>%
  st_intersection(bv_topo) %>%
  mutate(intersection = st_length(geom)) %>%
  group_by(id_ligne) %>%
  slice_max(intersection,
            n = 1,
            with_ties = FALSE) %>%
  ungroup() %>%
  st_set_geometry(NULL)

bdd_APP_alti_bv <- bdd_APP_alti %>%
  left_join(cours_eau_bv %>%
              select(id_ligne,
                     toponyme,
                     caractere_permanent,
                     topooh,
                     surface_m),
            by = c("riviere_id" = "id_ligne"))

save(bdd_APP_alti_bv, file = "processed_data/bdd_APP_alti_bv.RData")


##### Température de l'eau #####

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

idx <- st_nearest_feature(data_APP_final,
                          stations_na)

data_APP_final$code_station_hubeau <-
  stations_na$code_station[idx]

data_APP_final$nom_station_hubeau <-
  stations_na$libelle_station[idx]

distances <- st_distance(
  data_APP_final,
  stations_na[idx, ],
  by_element = TRUE
)

data_APP_final$distance_hubeau_m <-
  as.numeric(distances)

codes <- unique(
  data_APP_final$code_station_hubeau
)

temperature_all <- map_dfr(
  
  codes,
  
  function(code){
    
    cat("Téléchargement :", code, "\n")
    
    tryCatch(
      
      telecharger_temperature(code),
      
      error = function(e) NULL
    )
  }
)

stations_temp <- temperature_all %>%
  filter(code_parametre == "1301") %>%
  group_by(code_station) %>%
  summarise(temp_min = min(resultat, na.rm = TRUE),
         temp_max = max(resultat, na.rm = TRUE),
         temp_moy = mean(resultat, na.rm = TRUE),
         n = n())


data_APP_hubeau <- data_APP_final %>%
  left_join(stations_temp,
            by = c("code_station_hubeau" = "code_station")) %>%
  st_set_geometry(NULL)


bdd_APP <- bdd_APP_alti_bv %>%
  left_join(data_APP_hubeau %>%
              select(Id,
                     code_station_hubeau,
                     temp_min.y,
                     temp_max.y,
                     temp_moy.y),
            by = "Id")

save(bdd_APP, file = "processed_data/bdd_APP.RData")


##### Qualité de l'eau #####



##### Occupation du sol #####






