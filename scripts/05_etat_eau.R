#%%%%%%%%%%%%%%%%%%%%% Etude des milieux aquatiques %%%%%%%%%%%%%%%%%%%%%%%%%%%%

###---------------------------------------------------------#
cli::cli_h1("Téléchargement des stations") 

dept_NA <- c("16","17","19","23","24","33","40","47","64","79","86","87")

stations_na <- get_qualite_rivieres_station(
  code_departement = dept_NA,
  size = 5000
)

# Nettoyage
stations_na <- stations_na %>%
  distinct() %>%
  filter(!is.na(longitude), !is.na(latitude))


###---------------------------------------------------------#
cli::cli_h1("Téléchargement les analyses physico-chimiques")

analyses_na <- get_qualite_rivieres_analyse(
  code_departement = dept_NA,
  )

head(analyses_na)





###---------------------------------------------------------#
cli::cli_h1("Téléchargement des données Etat écologique calculées")


etat_ecolo <- get_etat_corps_eau(
  code_dept = dept_NA,
  size = 5000
)

etat_ecolo <- etat_ecolo %>%
  select(code_masse_eau, annee, etat_ecologique, etat_chimique)




get_masses_dept <- function(code_dept, size = 5000) {
  
  url <- "https://hubeau.eaufrance.fr/api/v1/referentiel/masse_eau"
  
  res <- GET(url, query = list(
    code_departement = code_dept,
    size = size
  ))
  
  stop_for_status(res)
  
  data <- fromJSON(content(res, "text", encoding = "UTF-8"))
  
  return(data$data)
}










