#%%%%%%%%%%%%%%%%%%%%% Analyses données %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###---------------------------------------------------------#
cli::cli_h1("Lire le fichier format .gpkg") 

bdd_ecrevisse <- st_read("processed_data/bdd_ecrevisse.gpkg")


###---------------------------------------------------------#
cli::cli_h1("Analyse quantitative")

# Tableau de synthèse temporel : observation, nombre d'espèces, année
# Calcul d'indice : richesse spécifique, indice de shannon,...

# Graphique nombre de saisies par année et par département
# Graphique nombre de saisies par contributeurs

# Carte de répartition par nombre d'bservation
# Cartes d'abondance/intensité graduée selon volume de données


###---------------------------------------------------------#
cli::cli_h1("Analyse qualitative")

# Qualité des données -> taux de validation des données
# Identification des epsèces observées (autochtones/invasives)
# Analyse des modes de collecte : prospection, protocole,...

# Etude des expansions des espèces EE
# Analyse des co-occurence entre espèces
# Identification zones sensibles ou prioritaires dans la gestion

# Carte décrivant les types d'habitats
# Carte montrant les enjeux : présence EEE +++



