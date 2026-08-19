
# Extraction tronçons 200m

extraire_troncon_buffer <- function(point, ligne, distance = 200){
  
  buf <- st_buffer(point, distance)
  
  troncon <- st_intersection(ligne, buf)
  
  if(nrow(troncon) == 0){
    return(NULL)
  }
  
  longueurs <- st_length(troncon)
  
  troncon <- troncon[which.max(longueurs), ]
  
  return(troncon)
}



# Calcul pente

calcul_pente_mnt <- function(troncon, mnt){
  
  # résolution raster
  pas <- res(mnt)[1]
  
  # longueur tronçon
  longueur <- as.numeric(
    st_length(troncon)
  )
  
  # sécurité
  if(longueur < pas * 2){
    return(NULL)
  }
  
  # nombre de points
  npts <- floor(longueur / pas)
  
  # points réguliers
  pts <- st_line_sample(
    troncon,
    n = npts,
    type = "regular"
  )
  
  pts_sf <- st_sf(
    geometry = pts
  )
  
  # extraction altitudes
  z <- terra::extract(
    mnt,
    vect(pts_sf)
  )[,2]
  
  # retirer NA
  ok <- !is.na(z)
  
  z <- z[ok]
  
  if(length(z) < 3){
    return(NULL)
  }
  
  # distances réelles
  distances <- seq(
    0,
    longueur,
    length.out = length(z)
  )
  
  # régression longitudinale
  modele <- lm(z ~ distances)
  
  pente <- abs(coef(modele)[2])
  
  return(list(
    altitude_moy = mean(z),
    altitude_min = min(z),
    altitude_max = max(z),
    z_debut = z[1],
    z_fin = z[length(z)],
    pente_m_m = pente,
    pente_pct = pente * 100
  ))
}


# Chercher les stations Hubeau

chercher_station_hubeau <- function(lon,
                                    lat,
                                    rayon = 5000){
  
  url <- paste0(
    "https://hubeau.eaufrance.fr/api/v2/qualite_rivieres/station_pc?",
    "longitude=", lon,
    "&latitude=", lat,
    "&distance=", rayon,
    "&size=1"
  )
  
  cat(url, "\n")
  
  req <- httr::GET(url)
  
  txt <- httr::content(
    req,
    "text",
    encoding = "UTF-8"
  )
  
  dat <- jsonlite::fromJSON(txt)
  
  if(nrow(dat$data) == 0){
    return(NULL)
  }
  
  return(dat$data[1, ])
}



# Télécharger les données température de l'eau


telecharger_temperature <- function(code_station){
  
  url <- paste0(
    "https://hubeau.eaufrance.fr/api/v2/qualite_rivieres/analyse_pc?",
    "code_station=", code_station,
    "&code_parametre=1301,1311",
    "&size=20000"
  )
  
  req <- httr::GET(url)
  
  txt <- httr::content(
    req,
    "text",
    encoding = "UTF-8"
  )
  
  dat <- jsonlite::fromJSON(txt)
  
  if(is.null(dat$data)){
    return(NULL)
  }
  
  dat$data
}


# Regrouper les sites APP

regrouper_points <- function(data, distance = 500) {
  
  if (nrow(data) == 1) {
    data$site_local <- 1
    return(data)
  }
  
  voisins <- st_is_within_distance(
    data,
    data,
    dist = distance
  )
  
  edges <- do.call(
    rbind,
    lapply(seq_along(voisins), function(i) {
      
      j <- voisins[[i]]
      j <- j[j > i]
      
      if (length(j) > 0) {
        cbind(i, j)
      }
    })
  )
  
  g <- make_empty_graph(
    n = nrow(data),
    directed = FALSE
  )
  
  if (!is.null(edges) && nrow(edges) > 0) {
    g <- add_edges(
      g,
      as.vector(t(edges))
    )
  }
  
  data$site_local <- components(g)$membership
  
  data
}
