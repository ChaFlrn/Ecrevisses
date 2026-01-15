#%%%%%%%%%%%%%%%%%%%%%%%% Assemblage des bases %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###---------------------------------------------------------#
cli::cli_h1("Lire les fichiers format .gpkg") 

data_oison <- st_read("processed_data/data_oison.gpkg")

data_fauna <- st_read("processed_data/data_fauna.gpkg")

data_naiades <- st_read("processed_data/data_naiades.gpkg")

data_aspe <- st_read("processed_data/data_aspe.gpkg")


###---------------------------------------------------------#
cli::cli_h1("Supprimer les doublons OFB") 

oison <- data_oison %>%
  mutate(geom_wkt = st_as_text(geom)) %>%
  st_set_geometry(NULL) %>%
  as.data.frame()

fauna <- data_fauna %>%
  mutate(geom_wkt = st_as_text(geom)) %>% 
  st_set_geometry(NULL) %>%
  as.data.frame()


aspe <- data_aspe %>%
  mutate(geom_wkt = st_as_text(geom)) %>% 
  st_set_geometry(NULL) %>%
  as.data.frame()

naiades <- data_naiades %>%
  mutate(geom_wkt = st_as_text(geom)) %>% 
  st_set_geometry(NULL) %>%
  as.data.frame()



data_doublons_f_o <- fauna %>%
  inner_join(oison,
             by = c("Date_precis", "Cdnom", "geom_wkt")) %>%
  mutate(doublon = "oui")

data_doublons_a_n <- aspe %>%
  mutate(Date_precis = as.Date(Date_precis)) %>%
  inner_join(naiades,
             by = c("Date_precis", "Cdnom", "geom_wkt")) %>%
  mutate(doublon = "oui")


data_doublons <- rbind(data_doublons_f_o,
                       data_doublons_a_n)


###---------------------------------------------------------#
cli::cli_h1("Assembler les bases")

bdd_ecrevisse <- rbind(data_oison,
                       data_fauna,
                       data_naiades,
                       data_aspe)

###---------------------------------------------------------#
cli::cli_h1("Nettoyage du fichier")

bdd_ecrevisse <- bdd_ecrevisse %>%
  left_join(data_doublons %>% 
              select(Id.x, doublon),
            by = c("Id" = "Id.x")) %>%
  filter(is.na(doublon) | doublon != "oui") %>%
  select(-doublon) %>%
  mutate(Cdnom = recode(Cdnom,
                        "983403" = "162666",
                        "853999" = "17646"),
         
         Nom_vernaculaire = case_when(
           Cdnom == "162666" ~ "Ecrevisse a pattes greles",
           Cdnom == "162667" ~ "Ecrevisse de Californie",
           Cdnom == "162668" ~ "Ecrevisse de Louisiane",
           Cdnom == "17646" ~ "Ecrevisse americaine",
           Cdnom == "18432" ~ "Ecrevisse a pattes rouges",
           Cdnom == "18437" ~ "Ecrevisse a pieds blancs"),
         
         Fournisseur = recode(Fournisseur,
                              "OFFICE FRANCAIS DE LA BIODIVERSITE - OFB DIRECTION REGIONALE NOUVELLE AQUITAINE (OFB" = "OFB",
                              "OFFICE FRANCAIS DE LA BIODIVERSITE - OFB DIRECTION REGIONALE CENTRE VAL LOIRE (OFB" = "OFB",
                              "OFFICE FRANCAIS DE LA BIODIVERSITE - OFB DIRECTION REGIONALE OCCITANIE (OFB" = "OFB"),
         Nom_scientifique = str_remove(Nom_scientifique, "\\s*\\([^\\)]*\\)"),
         
         Presence = recode(Presence,
                           "présent" = "Présent")) %>%
  filter(Cdnom != "65899") # Supression de l'écrevisse de terre (courtillère)
  

###---------------------------------------------------------#
cli::cli_h1("Jointure statut EEE")
fr_esp_envahissantes <- read.table("assets/fr_especes_envahissantes.csv", 
                                   header = TRUE, 
                                   sep = ";", 
                                   fill = TRUE, 
                                   row.name = NULL)

fr_esp_envahissantes <- fr_esp_envahissantes %>%
  select(Cdnom = CD_NOM,
         Nom_vernaculaire = Nom.vernaculaire) %>%
  mutate(Statut = "Envahissante",
         Cdnom = as.character(Cdnom))


bdd_ecrevisse <- bdd_ecrevisse %>%
  left_join(fr_esp_envahissantes %>% 
              select(Cdnom,
                     Statut),
            by = "Cdnom") %>%
  mutate(Statut = case_when(
    Statut == "Envahissante" ~ "Espèce envahissante",
    Cdnom == "162666" ~ "Espèce représentée",
    TRUE ~ "Espèce autochtone")) %>%
  filter(Departement %in% c("16","17","19","23", "24", "33", "40", "47", "64", "79", "86", "87"))



###---------------------------------------------------------#
cli::cli_h1("Sauvegarder le fichier")

st_write(bdd_ecrevisse, "processed_data/bdd_ecrevisse.gpkg", 
         append = FALSE,
         driver = "GPKG")




