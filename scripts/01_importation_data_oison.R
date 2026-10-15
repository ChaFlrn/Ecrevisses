#%%%%%%%%%%%%%%%%%%%%% Importation données OISON %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


###---------------------------------------------------------#
cli::cli_h1("Extraire automatiquement les données") 
geom_NA <-
  COGiter::regions_geo %>%
  # filtre sur NA
  dplyr::filter(REG == 75) %>%
  sf::st_as_sfc() %>%
  # convert to text
  sf::st_as_text()

string_geom_exemple <- paste0(substr(geom_NA,1,100), '....')


# a) connexion
bdd_oison <- start_sql_connexion()

# b) requete
oison_NA <-
  get_table_taxon_sql(conn = bdd_oison,
                      geometrie = geom_NA)

# d) stop de la connexion
stop_sql_connexion(conn = bdd_oison)


# Filtrage
oison_NA_ecrevisse <-
  oison_NA %>%
  # detection pattern 'creviss' et filtre
  dplyr::filter(grepl('creviss', nom_vernaculaire))


sf::write_sf(oison_NA_ecrevisse,
             "raw_data/oison_ecrevisse.gpkg",
             driver = "GPKG")


###---------------------------------------------------------#
cli::cli_h1("Lire le fichier format .gpkg") 

data_oison <- st_read("raw_data/oison_ecrevisse.gpkg")


###---------------------------------------------------------#
cli::cli_h1("Ajouter les communes et départements") 

communes <- st_read("assets/communes.gpkg") %>%
  select(insee_com = code,
         code_postal = Code_postal,
         nom_com = nom,
         insee_dep = departement,
         geom) %>%
  st_transform(2154)



###---------------------------------------------------------#
cli::cli_h1("Assembler et trier le fichier")

data_oison <- data_oison %>%
  st_join(communes, join = st_nearest_feature) %>%
  mutate(Date = substr(date, 1,4),
         Fournisseur = "OFB",
         Observateur = paste(nom,prenom),
         Effectif = ifelse(is.na(nombre_individu), "Non renseigné", nombre_individu),
         Source = "OISON") %>%
  select(Id = observation_id,
         Date,
         Date_precis = date,
         Cdnom = cd_nom,
         Nom_vernaculaire = nom_vernaculaire,
         Nom_scientifique = nom_scientifique,
         Effectif,
         Presence = presence,
         Departement = insee_dep,
         Fiabilite = status,
         Fournisseur,
         Source,
         Geometrie = geom)


###---------------------------------------------------------#
cli::cli_h1("Vérifier les doublons")

data_oison <- data_oison[!duplicated(data_oison$Id), ]


###---------------------------------------------------------#
cli::cli_h1("Sauvegarder le fichier")

st_write(data_oison, "processed_data/data_oison.gpkg", 
         append = FALSE,
         driver = "GPKG")





