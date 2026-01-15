#%%%%%%%%%%%%%%%%%%%%% Analyses données %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###---------------------------------------------------------#
cli::cli_h1("Lire le fichier format .gpkg") 

bdd_ecrevisse <- st_read("processed_data/bdd_ecrevisse.gpkg")



# Qualité des données -> taux de validation des données

data_valid <- bdd_ecrevisse %>%
  st_set_geometry(NULL) %>%
  group_by(Fiabilite) %>%
  summarise(nb_saisies = n(), .groups = "drop") %>%
  mutate(pourcentage = round((nb_saisies / sum(nb_saisies))*100, 2)) %>%
  arrange(desc(nb_saisies))


## Caractérisation des espèces
carac_esp <- bdd_ecrevisse %>%
  st_set_geometry(NULL) %>%
  group_by(Statut) %>%
  summarise(nb_saisies = n(), .groups = "drop") %>%
  mutate(pourcentage = round(nb_saisies / sum(nb_saisies)*100,2)) %>%
  arrange(desc(nb_saisies))


carac_esp_spe <- bdd_ecrevisse %>%
  st_set_geometry(NULL) %>%
  mutate(Periode = case_when(
    Date <= "2005" ~ "2005",
    Date > "2005" & Date <= "2015" ~ "2005_2015",
    Date > "2015" ~ "2015")) %>%
  group_by(Nom_vernaculaire, Periode) %>%
  summarise(nb = n(), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = Periode,
    values_from = nb,
    values_fill = 0,
    names_prefix = "nb_obs_"
  ) %>%
  mutate(pour_2005_2015 = round((nb_obs_2005_2015 - nb_obs_2005) / (nb_obs_2005_2015 + nb_obs_2005) *100,2),
         pourc_2015_2025 = round((nb_obs_2015 - nb_obs_2005_2015) / (nb_obs_2015 + nb_obs_2005_2015) *100,2))


## Analyse des dynamiques écologiques

colors_statut <- c(
  "Espèce envahissante" = "#e67e22",
  "Espèce représentée" = "#4895ef",
  "Espèce autochtone" = "#8ccb3f")


pop_eee_2005 <- bdd_ecrevisse %>%
  filter(Date <= "2005" & Statut == "Espèce envahissante") %>%
  st_point_on_surface()

pop_eee_2015 <- bdd_ecrevisse %>%
  filter(Date > "2005" & Date <= "2015" & Statut == "Espèce envahissante") %>%
  st_point_on_surface()

pop_eee_2025 <- bdd_ecrevisse %>%
  filter(Date > "2015" & Statut == "Espèce envahissante") %>%
  st_point_on_surface()

carte_eee_2005 <- ggplot(pop_eee_2005) +
  geom_sf(data = departements,
          fill = "grey95", color = "black", size = 0.3) +
  geom_sf(aes(geometry = geom, fill = Statut),
          color = "black",
          alpha = 0.85, 
          size = 3, 
          shape = 21) +
  scale_fill_manual(values = colors_statut) +
  labs(title = "Avant 2005") +
  theme_void(base_size = 9) +
  theme(text = element_text(size = 6),
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        legend.position = "none")

carte_eee_2015 <- ggplot(pop_eee_2015) +
  geom_sf(data = departements,
          fill = "grey95", color = "black", size = 0.3) +
  geom_sf(aes(geometry = geom, fill = Statut),
          color = "black",
          alpha = 0.85, 
          size = 3, 
          shape = 21) +
  scale_fill_manual(values = colors_statut) +
  labs(title = "De 2005 à 2015") +
  theme_void(base_size = 9) +
  theme(text = element_text(size = 6),
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        legend.position = "none")

carte_eee_2025 <- ggplot(pop_eee_2025) +
  geom_sf(data = departements,
          fill = "grey95", color = "black", size = 0.3) +
  geom_sf(aes(geometry = geom, fill = Statut),
          color = "black",
          alpha = 0.85, 
          size = 3, 
          shape = 21) +
  scale_fill_manual(values = colors_statut) +
  labs(title = "Depuis 2015") +
  theme_void(base_size = 9) +
  theme(text = element_text(size = 6),
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        legend.position = "none")

# Assemblage des cartes
(carte_eee_2005 | carte_eee_2015 | carte_eee_2025) +
  plot_annotation(
    title = "Evolution du nombre d'observations des populations d'écrevisses envahissantes",
    subtitle = "Écrevisse de Californie, Écrevisse de Lousiane, Écrevisse américaine",
    theme = theme(
      plot.title = element_text(size = 10),
      plot.subtitle = element_text(size = 8)
    ))
