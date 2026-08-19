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
  mutate(annee = as.Date(substr(date_observation, 1,4), "%Y")) %>%
  arrange(code_cours_eau, annee, date_observation) %>%
  group_by(code_cours_eau, annee) %>%
  mutate(jour_prec = lag(date_observation),
         nouveau_groupe = is.na(jour_prec) | date_observation != jour_prec +1 | code_ecoulement != 3,
         groupe = cumsum(nouveau_groupe)) %>%
  filter(code_ecoulement == 3) %>%
  group_by(code_cours_eau, annee, groupe) %>%
  summarise(date_debut_assec = min(date_observation),
            date_fin_assec = max(date_observation),
            nb_jours = as.numeric(date_fin_assec - date_debut_assec) + 1, .groups = "drop")








departements <- st_read("assets/departements.gpkg")

ggplot(data_assec) +
  geom_sf(data = departements,
          fill = "grey95", color = "black", size = 0.3) +
  geom_sf(aes(geometry = geometry, fill = libelle_ecoulement),
          color = "black",
          alpha = 0.85, 
          size = 3, 
          shape = 21) +
  theme_void() +
  theme(text = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))







