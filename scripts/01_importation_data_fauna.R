#%%%%%%%%%%%%%%%%%%%%% Importation données FAUNA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###---------------------------------------------------------#
cli::cli_h1("Lire les fichiers format .csv") 

data_lignes <- read_delim("raw_data/Linestring.csv",
                          delim = NULL)

data_points <- read_delim("raw_data/Point.csv",
                          delim = NULL)

data_polygones <- read_delim("raw_data/Polygon.csv",
                         delim = NULL)

data_meta <- read_delim("raw_data/Metadonnees.csv",
                        delim = NULL)

###---------------------------------------------------------#
cli::cli_h1("Rassembler les fichiers") 

data_lignes <- st_as_sf(data_lignes, wkt = "GeomWkt")
data_points <- st_as_sf(data_points, wkt = "GeomWkt")
data_polygones <- st_as_sf(data_polygones, wkt = "GeomWkt")

data_fauna <- rbind(data_lignes,
                    data_points,
                    data_polygones)


###---------------------------------------------------------#
cli::cli_h1("Récupérer les contributeurs et trier la base")

data_fauna <- data_fauna %>%
  left_join(data_meta, by = c("IdJdd" = "IdJeuDonnees")) %>%
  mutate(Date = substr(DateDebut, 1,4),
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
         Date_precis = DateDebut,
         Cdnom,
         Nom_vernaculaire = TaxNomVern,
         Nom_scientifique = TaxNomVal,
         Effectif,
         Presence = StatPresen,
         Departement = CodeDpt,
         InseeCom,
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

