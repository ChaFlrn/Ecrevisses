# Bilan des données Ecrevisses en région Nouvelle-Aquitaine

## Objectif
Créer un rapport synthétique descriptif du jeu de données utilisé. 
Le rapport généré se compose de 2 parties :
- Analyse quantitative

- Analyse qualitative


## Arborescence du projet
### Fichier "make.R"
Lancer ce fichier pour exécuter l'ensemble des scripts et générer le rapport Word.

### Assets
Eléments concernant les espèces et les limites administratives géographiques.

### Output
Sortie du rapport Word généré.

### Processed_data
Fichiers de données enregistrés durant les différents traitements et analyses de données.

### Raw-data
Fichiers de données brutes, sous format géopackage ou csv.

### Scripts
Fichiers des scripts nécessaires pour l'exécution du projet R :
- 00_packages : listing des packages utiles
- 01_importation_data_aspe : instructions pour importation et traitement du jeu de données ASPE
- 01_importation_data_fauna : instructions pour importation et traitement du jeu de données FAUNA
- 01_importation_data_oison : instructions pour importation et traitement du jeu de données OISON
- 01_importation_data_naiades : instructions pour importation et traitement du jeu de données NAIADES
- 02_assemblage_bases : instructions pour assemblage des bases et nettoyage de la base de données finale
- 03_analyses_QUANTI : principaux graphiques, tableaux et cartes de la partie analyse quantitative
- 04_analyses_QUALI : principaux graphiques, tableaux et cartes de la partie analyse qualitative
- 05_preparation_qgis : création de geopackages pour la réalisation de cartes améliorées et atlas sous Qgis

### Templates
Fichier du Rmarkdown permettant la génération du rapport word ainsi qu'un document de référence word pour exemple de mise en page. 

