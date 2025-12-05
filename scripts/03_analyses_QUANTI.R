#%%%%%%%%%%%%%%%%%%%%% Analyses données %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###---------------------------------------------------------#
cli::cli_h1("Lire le fichier format .gpkg") 

bdd_ecrevisse <- st_read("processed_data/bdd_ecrevisse.gpkg")

colors_esp <- c(
  "Ecrevisse a pattes greles" = "#8DD3C7",
  "Ecrevisse a pattes rouges" = "#1F65C1",
  "Ecrevisse a pieds blancs" = "#4895EF",
  "Ecrevisse de Californie" = "#E67E22",
  "Ecrevisse de Louisiane" = "#E41A1C",
  "Ecrevisse americaine" = "#B03A2E"
)

esp_legend <- bdd_ecrevisse %>%
  st_set_geometry(NULL) %>%
  select(Cdnom, Nom_vernaculaire) %>%
  distinct() %>%
  arrange(factor(Cdnom, levels = names(colors_esp)))

###---------------------------------------------------------#
cli::cli_h1("Analyse quantitative")

# Tableau de synthèse temporel : observation, nombre d'espèces, année


tablo_synt_esp <- bdd_ecrevisse %>%
  st_set_geometry(NULL) %>%
  group_by(Cdnom, Nom_vernaculaire) %>%
  summarise(nb_saisies = n(), .groups = "drop") %>%
  mutate(total = sum(nb_saisies),
         pourcentage = round((nb_saisies / total)*100,2)) %>%
  select(-total) %>%
  arrange(desc(pourcentage))

tablo_contrib <- bdd_ecrevisse %>%
  st_set_geometry(NULL) %>%
  group_by(Fournisseur) %>%
  summarise(nb_saisies = n(), .groups = "drop") %>%
  mutate(total = sum(nb_saisies),
         pourcentage = round((nb_saisies / total)*100,2)) %>%
  select(- total) %>%
  arrange(desc(pourcentage))


# Calcul d'indice : richesse spécifique, indice de shannon,...

# Graphique nombre de saisies par année et par département
histo_synt_saisies <- bdd_ecrevisse %>%
  st_set_geometry(NULL) %>%
  group_by(Date) %>%
  summarise(nb_saisies = n(), .groups = "drop") %>%
  mutate(Date = as.numeric(Date),
         Type = "Total saisies")
  
contrib_ofb <- bdd_ecrevisse %>%
  st_set_geometry(NULL) %>%
  filter(Fournisseur == "OFB") %>%
  group_by(Date, Fournisseur) %>%
  summarise(nb_saisies = n(), .groups = "drop") %>%
  mutate(Date = as.numeric(Date),
         Type = "Saisies OFB")

ggplot() +
  geom_col(data = contrib_ofb, aes(x = Date, y = nb_saisies, fill = Type)) +
  geom_line(data = histo_synt_saisies, aes(x = Date, y = nb_saisies, color = Type), linewidth = 1) +
  geom_point(data = histo_synt_saisies, aes(x = Date, y = nb_saisies, color = Type), size = 1.5) +
  scale_y_continuous(breaks = seq(0, max(histo_synt_saisies$nb_saisies), by = 200)) +
  scale_x_continuous(breaks = seq(min(histo_synt_saisies$Date), max(histo_synt_saisies$Date), by = 5)) +
  scale_fill_manual(values = c("Saisies OFB" = "#003A76")) +
  scale_color_manual(values = c("Total saisies" = "#A6CEE3")) +
  labs(
    title = "Evolution des saisies de l'OFB par rapport au total annuel",
    x = "Année",
    y = "Nombre de saisies",
    fill = "",
    color = "") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank()
  )


# Graphique évolution du nombre de saisies par espèces et par années

histo_synt_esp <- bdd_ecrevisse %>%
  st_set_geometry(NULL) %>%
  group_by(Date,Cdnom, Nom_vernaculaire) %>%
  summarise(nb_saisies = n(), .groups = "drop")

ggplot(histo_synt_esp) +
  aes(
    x = Date,
    y = nb_saisies,
    colour = Nom_vernaculaire,
    group = Nom_vernaculaire) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    x = "Années",
    y = "Nombre de saisies",
    title = "Evolution du nombre de saisies par espèces") +
  scale_color_manual(values = colors_esp,
                     name = "Espèces") +
  scale_y_continuous(breaks = seq(0, max(histo_synt_esp$nb_saisies), by = 50)) +
  scale_x_continuous(breaks = seq(min(histo_synt_esp$Date), max(histo_synt_esp$Date), by = 5)) +
  theme_minimal(base_size = 14)+
  theme(text = element_text(size = 9),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5))


# Graphique nombre de saisies par contributeurs

# Carte de répartition par nombre d'bservation
# Cartes d'abondance/intensité graduée selon volume de données


###---------------------------------------------------------#
cli::cli_h1("Analyse qualitative")

# Qualité des données -> taux de validation des données
# Identification des epsèces observées (autochtones/invasives)
# Analyse des modes de collecte : prospection, protocole,...

# Etude des expansions des espèces EE
nombre individus
# Analyse des co-occurence entre espèces
# Identification zones sensibles ou prioritaires dans la gestion

# Carte décrivant les types d'habitats
# Carte montrant les enjeux : présence EEE +++



