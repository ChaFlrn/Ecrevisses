#%%%%%%%%%%%%%%%%%%%%% Importation données SD 64 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###---------------------------------------------------------#
cli::cli_h1("Lire le fichier format .gpkg") 

data_64 <- st_read("raw_data/SD64/donnees_64.gpkg",
                   layer = "donnes_ecrevisses_64_propres")

###---------------------------------------------------------#
cli::cli_h1("Assembler et trier le fichier")

cols_especes <- c("APP", "ASA", "ASL", "OCL", "PCC", "PFL")

data_long <- data_64 %>%
  mutate(id_ligne = row_number()) %>%
  pivot_longer(
    cols = all_of(cols_especes),
    names_to = "code_espece",
    values_to = "effectif") %>%
  rowwise() %>%
  mutate(presence_brut = get(
    paste0("Presence_", code_espece)),
    
    presence = case_when(
    presence_brut == 1 ~ "Présent",
    TRUE ~ "NSP")) %>%
  
  ungroup() %>%
  filter(presence == "Présent")


data_64 <- data_long %>%
  mutate(Date_precis = as.Date(Date),
         Date = substr(Date, 1,4),
         Cdnom = recode(code_espece,
                        "PCC" = "162668",
                        "OCL" = "17646",
                        "PFL" = "162667",
                        "ASL" = "162666",
                        "ASA" = "18432",
                        "APP" = "18437"),
         
         Nom_vernaculaire = case_when(
           Cdnom == "162666" ~ "Ecrevisse a pattes greles",
           Cdnom == "162667" ~ "Ecrevisse de Californie",
           Cdnom == "162668" ~ "Ecrevisse de Louisiane",
           Cdnom == "17646" ~ "Ecrevisse americaine",
           Cdnom == "18432" ~ "Ecrevisse a pattes rouges",
           Cdnom == "18437" ~ "Ecrevisse a pieds blancs"),
         
         Nom_scientifique = case_when(
           Cdnom == "162666" ~ "Astacus leptodactylus",
           Cdnom == "162667" ~ "Pacifastacus leniusculus",
           Cdnom == "162668" ~ "Procambarus clarkii",
           Cdnom == "17646" ~ "Faxonius limosus",
           Cdnom == "18432" ~ "Astacus astacus",
           Cdnom == "18437" ~ "Austropotamobius pallipes"),
         
         Fournisseur = Source,
         
         Departement = "64",
         Fiabilite = "Valide",
         Source = "FDAAPPMA 64") %>%
  
  select(Id = id,
         Date,
         Date_precis,
         Cdnom,
         Nom_vernaculaire,
         Nom_scientifique,
         Effectif = effectif,
         Presence = presence,
         Departement,
         Fiabilite,
         Fournisseur,
         Source,
         Geometrie = geom)

###---------------------------------------------------------#
cli::cli_h1("Vérifier les doublons")

data_64 <- data_64[!duplicated(data_64$Id), ]

###---------------------------------------------------------#
cli::cli_h1("Sauvegarder le fichier")

st_write(data_64, "processed_data/data_64.gpkg", 
         append = FALSE,
         driver = "GPKG")
