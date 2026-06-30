#%%%%%%%%%%%%%%%%%%%%% Importation données SD 86 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###---------------------------------------------------------#
cli::cli_h1("Lire le fichier format .xls") 

fichier_excel <- "raw_data/SD86/base sd86.xlsx"
feuilles <- c("APP","OCL","PFL","PCC","ASL","ASA")

data_sd86 <- lapply(feuilles, function(feuille){
  read_excel(fichier_excel, sheet = feuille) %>%
    select(-Date)}) %>%
  bind_rows()


###---------------------------------------------------------#
cli::cli_h1("Trier le fichier")

data_sd86_propre <- data_sd86 %>%
  filter(!is.na(Lambert_93_X)) %>%
  st_as_sf(coords = c("Lambert_93_X", "Lambert_93_Y"), 
           crs = 2154) %>%
  mutate(Date_precis = paste0(Année, "-01-01"),
         Id = paste0("SD86_", row_number()),
         Fournisseur = "SD 86 et Partenaires",
         Effectif = "Non renseigné",
         Source = "SD 86",
         Departement = ifelse(Département == "Charente", "16", "86"),
         Fiabilite = "Valide",
         
         Cdnom = case_when(
           Espèce == "APP" ~ "18437",
           Espèce == "OCL" ~ "17646",
           Espèce == "PFL" ~ "162667",
           Espèce == "PCC" ~ "162668",
           Espèce == "ASL" ~ "162666",
           Espèce == "ASA " ~ "18432"),
         
         Nom_scientifique = case_when(
           Cdnom == "162666" ~ "Astacus leptodactylus",
           Cdnom == "162667" ~ "Pacifastacus leniusculus",
           Cdnom == "162668" ~ "Procambarus clarkii",
           Cdnom == "17646" ~ "Faxonius limosus",
           Cdnom == "18432" ~ "Astacus astacus",
           Cdnom == "18437" ~ "Austropotamobius pallipes"),
           
         Nom_vernaculaire = case_when(
           Cdnom == "162666" ~ "Ecrevisse a pattes greles",
           Cdnom == "162667" ~ "Ecrevisse de Californie",
           Cdnom == "162668" ~ "Ecrevisse de Louisiane",
           Cdnom == "17646" ~ "Ecrevisse americaine",
           Cdnom == "18432" ~ "Ecrevisse a pattes rouges",
           Cdnom == "18437" ~ "Ecrevisse a pieds blancs")) %>%
  select(Id,
         Date = Année,
         Date_precis,
         Cdnom,
         Nom_vernaculaire,
         Nom_scientifique,
         Effectif,
         Presence = Présence,
         Departement,
         Fiabilite,
         Fournisseur,
         Source,
         Geometrie = geometry)



###---------------------------------------------------------#
cli::cli_h1("Sauvegarder le fichier")
st_write(data_sd86_propre, "processed_data/data_sd86.gpkg", 
         append = FALSE,
         driver = "GPKG")
