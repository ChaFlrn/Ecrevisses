##%######################################################%##
#                                                          #
####      Etude sur les populations d'écrevisses           ####
###       Direction régionale Nouvelle-Aquitaine           ###
#                                                          #
##%------------------------------------------------------%##
##%------------------------------------------------------%##  
####                   C. FLORIN                           ####
###         Chargée de mission analyse de données          ###
##                 Service Connaissance                    ##
##%------------------------------------------------------%##
##                                                         ##
#                                                          #
##%######################################################%##

##----------------------------------------------------------------------------##
## 1. Lancement des scripts

source("scripts/00_packages.R")
source("scripts/01_importation_data_aspe.R")
source("scripts/01_importation_data_fauna.R")
source("scripts/01_importation_data_naiades.R")
source("scripts/01_importation_data_oison.R")
source("scripts/02_assemblage_bases.R")

##----------------------------------------------------------------------------##
## 2. Lancement du rapport Word

dir.create("output")

rmarkdown::render(input = "templates/etude_ecrevisse.Rmd",
                  output_file = paste0("../output/Bilan_Régional_Ecrevisses.docx"))




