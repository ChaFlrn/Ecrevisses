#%%%%%%%%%%%%%%%%%%%%% Importation données OISON %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
         InseeCom = insee_com,
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





