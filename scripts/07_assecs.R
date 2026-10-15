#%%%%%%%%%%%%%%%%%% Analyses des assecs dans la région %%%%%%%%%%%%%%%%%%%%%%%%%

####-------------------Base de données--------------------------------------####
cli::cli_h1("Récupérer la base de données") 

url_onde_2012_2014 <- "https://hubeau.eaufrance.fr/api/v1/ecoulement/observations?code_region=75&date_observation_max=2015-01-01&size=20000&format=geojson"

data_assec_2012_2014 <- st_read(url_onde_2012_2014, quiet = TRUE)


url_onde_2015_2017 <- "https://hubeau.eaufrance.fr/api/v1/ecoulement/observations?code_region=75&date_observation_min=2015-01-01&date_observation_max=2018-01-01&size=20000&format=geojson"

data_assec_2015_2017 <- st_read(url_onde_2015_2017, quiet = TRUE)


url_onde_2018_2020 <- "https://hubeau.eaufrance.fr/api/v1/ecoulement/observations?code_region=75&date_observation_min=2018-01-01&date_observation_max=2021-01-01&size=20000&format=geojson"

data_assec_2018_2020 <- st_read(url_onde_2018_2020, quiet = TRUE)


url_onde_2021_2022 <- "https://hubeau.eaufrance.fr/api/v1/ecoulement/observations?code_region=75&date_observation_min=2021-01-01&date_observation_max=2023-01-01&size=20000&format=geojson"

data_assec_2021_2022 <- st_read(url_onde_2021_2022, quiet = TRUE)


url_onde_2023_2024 <- "https://hubeau.eaufrance.fr/api/v1/ecoulement/observations?code_region=75&date_observation_min=2023-01-01&date_observation_max=2025-01-01&size=20000&format=geojson"

data_assec_2023_2024 <- st_read(url_onde_2023_2024, quiet = TRUE)


url_onde_2025_2026 <- "https://hubeau.eaufrance.fr/api/v1/ecoulement/observations?code_region=75&date_observation_min=2025-01-01&size=20000&format=geojson"

data_assec_2025_2026 <- st_read(url_onde_2025_2026, quiet = TRUE)


data_assec_2012_2026 <- rbind(data_assec_2012_2014,
                              data_assec_2015_2017,
                              data_assec_2018_2020,
                              data_assec_2021_2022,
                              data_assec_2023_2024,
                              data_assec_2025_2026)

save(data_assec_2012_2026, file = "processed_data/data_assec.RData")
load("processed_data/data_assec.RData")


####-------------------Créer le fichier d'analyse--------------------------------------####
cli::cli_h1("Trier et filtrer la base de données") 

data_assec <- data_assec_2012_2026 %>%
  mutate(annee = year(date_observation),
         mois = month(date_observation)) %>%
  arrange(code_station, annee, mois) %>%
  group_by(code_station, annee, mois) %>%
  summarise(statut = if_else(any(code_ecoulement == "3"), "assec", "non_assec"),
            .groups = "drop") %>%
  group_by(code_station, annee) %>%
  complete(mois = 1:12) %>%                      # <- correction 1 : mai à novembre
  mutate(groupe = cumsum(replace_na(statut != "assec", TRUE))) %>%
  ungroup() %>%
  filter(statut == "assec") %>%                  # <- correction 2 : ne garder QUE les épisodes d'assec
  group_by(code_station, annee, groupe) %>%
  summarise(mois_debut = min(mois),
            mois_fin = max(mois),
            nb_mois_consecutif_assec = n(),
            .groups = "drop") %>%
  select(-groupe)

save(data_assec, file = "processed_data/data_assec.RData")

data_assec_select <- data_assec %>%
  filter(nb_mois_consecutif_assec >= 2) %>%
  group_by(code_station) %>%
  mutate(derniere_annee = max(annee)) %>%
  arrange(code_station, derniere_annee) %>%
  slice(1) %>%
  left_join(data_assec_2012_2026 %>%
              select(code_station,
                     libelle_station,
                     code_cours_eau,
                     libelle_cours_eau,
                     code_departement,
                     geometry) %>%
              distinct(code_station, .keep_all = TRUE),
            by = "code_station")



st_write(data_assec_select, "processed_data/data_assec_select.gpkg", 
         append = FALSE,
         driver = "GPKG")







