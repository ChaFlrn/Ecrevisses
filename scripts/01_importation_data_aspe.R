#%%%%%%%%%%%%%%%%%%%%% Importation données ASPE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###---------------------------------------------------------#
cli::cli_h1("Extraire les données de la base") 

aspe_db <- imp_connecter_aspe_idg()

imp_importer_aspe_idg(aspe_db)
export_tables_rdata()

###---------------------------------------------------------#
cli::cli_h1("Créer un jeu de données") 

aspe_na <- 
  mef_creer_passerelle() %>%
  mef_ajouter_ope_date() %>%
  mef_ajouter_dept() %>%
  mef_ajouter_lots() %>%
  mef_ajouter_esp() %>%
  mef_ajouter_mei() %>%
  left_join(point_prelevement %>%
              select(pop_id,
                     typ_id = pop_typ_id,
                     pop_coordonnees_x,
                     pop_coordonnees_y),
            by = "pop_id") %>%
  left_join(ref_espece %>%
              select(esp_code_sandre,
                     esp_code_alternatif),
            by = "esp_code_alternatif") %>%
  left_join(station %>%
              select(sta_id,
                     sta_com_code_insee),
            by = "sta_id") %>%
  left_join(ref_type_projection %>%
              select(typ_id,
                     typ_code_epsg),
            by = "typ_id") %>%
  filter(dept %in% c(16, 17, 19, 23, 24, 33, 40, 47, 64, 79, 86, 87),
         str_detect(esp_nom_commun, "Ecrevisse"))

###---------------------------------------------------------#
cli::cli_h1("Transformer en objet spatial")
data_aspe <- st_as_sf(aspe_na, coords = c("pop_coordonnees_x", "pop_coordonnees_y"), crs = 2154)


###---------------------------------------------------------#
cli::cli_h1("Harmoniser le fichier")

data_aspe <- data_aspe %>%
  mutate(Id = paste0("Aspe_", row_number()),
         
         Cdnom = recode(esp_code_sandre,
                                "2963" = "162666",
                                "68137" = "17646",
                                "873" = "162667",
                                "2028" = "162668",
                                "866" = "18432",
                                "868" = "18437"),
         
         Nom_vernaculaire = case_when(
           Cdnom == "162666" ~ "Ecrevisse a pattes greles",
           Cdnom == "162667" ~ "Ecrevisse de Californie",
           Cdnom == "162668" ~ "Ecrevisse de Louisiane",
           Cdnom == "17646" ~ "Ecrevisse americaine",
           Cdnom == "18432" ~ "Ecrevisse a pattes rouges",
           Cdnom == "18437" ~ "Ecrevisse a pieds blancs"),
         
         Presence = "Présent",
         Date_precis = substr(ope_date, 1,10),
         Fiabilite = "Valide",
         Source = "Aspe",
         Fournisseur = "OFB") %>%

  select(Id,
         Date = annee,
         Date_precis,
         Cdnom,
         Nom_vernaculaire,
         Nom_scientifique = esp_nom_latin,
         Effectif = lop_effectif,
         Presence,
         Departement = dept,
         InseeCom = sta_com_code_insee,
         Fiabilite,
         Fournisseur,
         Source,
         Geometrie = geometry)




###---------------------------------------------------------#
cli::cli_h1("Sauvegarder le fichier")

st_write(data_aspe, "processed_data/data_aspe.gpkg", 
         append = FALSE,
         driver = "GPKG")
