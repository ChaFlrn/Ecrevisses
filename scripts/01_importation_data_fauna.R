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
cli::cli_h1("Trier la base de données") 

data_fauna <- data_fauna %>%
  mutate(Date = substr(DateDebut, 1,4),
         Observateur = sub(".*\\(([^()]*)\\).*", "\\1", Observer),
         Effectif = (DenbrMin + DenbrMax)/2)

###---------------------------------------------------------#
cli::cli_h1("Récupérer les contributeurs")

data_fauna <- data_fauna %>%
  left_join(data_meta, by = c("IdCadreAc" = "IdCadre")) %>%
  select(Id = IdRegional,
         Date,
         Cdnom = CdNomCite,
         Nom_vernaculaire = TaxNomVern,
         Nom_scientifique = TaxNomVal,
         Classe,
         Ordre,
         Famille,
         Observateur,
         Effectif,
         Departement = CodeDpt,
         Commune = NomCom,
         InseeCom,
         Fiabilite = NivValReg,
         Fournisseur,
         GeomWkt,
         TypeGeom,
         IdCadreAc)

unique(data_fauna$IdRegional)  
