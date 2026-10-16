#%%%%%%%%%%%%%%%%%%%%% Importation données FAUNA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###---------------------------------------------------------#
cli::cli_h1("Lire les fichiers format .csv") 

data_points <- read.csv2("raw_data/Point_16_10_26.csv",
                          header = TRUE,
                          sep = "\t")

data_meta <- read.csv2("raw_data/Metadonnees_16_10.csv",
                        header = TRUE,
                        sep = "\t")


###---------------------------------------------------------#
cli::cli_h1("Rassembler les fichiers") 

data_points <- st_as_sf(data_points, wkt = "GeomWkt")


###---------------------------------------------------------#
cli::cli_h1("Récupérer les contributeurs et trier la base")

data_fauna <- data_points %>%
  left_join(data_meta, by = c("IdJdd" = "IdJeuDonnees")) %>%
  mutate(Date = substr(DateDebut, 1,4),
         Date_precis = as.Date(DateDebut),
         Cdnom = as.character(CdNomCite),
         Fournisseur = case_when(
           is.na(Fournisseur) ~ str_extract(Observer, "(?<=\\().+?(?=\\))"),
           TRUE ~ Fournisseur),
         Effectif = coalesce(
           as.character(DenbrMax),
           as.character(DenbrMin),
           "Non renseigné"),
         Source = "FAUNA") %>%
  select(Id = IdRegional,
         Date,
         Date_precis,
         Cdnom,
         Nom_vernaculaire = TaxNomVern,
         Nom_scientifique = TaxNomVal,
         Effectif,
         Presence = StatPresen,
         Departement = CodeDpt,
         Fiabilite = NivValReg,
         Fournisseur,
         Source,
         Geometrie = GeomWkt)

###---------------------------------------------------------#
cli::cli_h1("Vérifier les doublons")

data_fauna <- data_fauna[!duplicated(data_fauna$Id), ]


###---------------------------------------------------------#
cli::cli_h1("Sauvegarder le fichier")

st_write(data_fauna, "processed_data/data_fauna.gpkg", 
         append = FALSE,
         driver = "GPKG")

