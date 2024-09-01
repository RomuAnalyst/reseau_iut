# CHARGEMENT DES LIBRARIES 
library(geojsonio)
library(ggplot2)
library(dplyr)
library(tidyr)
library(geojson)
library(leaflet)
library(tibble)
library(shiny)
library(shinydashboard)
library(scales)
library(DT)
library(stringr)
library(gpx)
library(sf)
library(geojsonsf)
library(readxl)
library(forcats)
library(jsonlite)
library(httr)
library(tidyverse)
library(plotly)
library(shinydashboardPlus)
library(shinycssloaders)
library(htmltools)
library(geosphere)
library(httr)

#modifier le chemin




# DATA TRAFIC STATIONS :
trafic_2021 <- read.csv("Fichiers/trafic-annuel-entrant-par-station-du-reseau-ferre-2021.csv", sep = ";", header = T)
trafic_2021 <- trafic_2021 %>% 
  mutate(Année = "2021")

trafic_2020 <- read.csv("Fichiers/trafic-annuel-entrant-par-station-du-reseau-ferre-2020.csv", sep = ";", header = T)
trafic_2020 <- trafic_2020 %>% 
  mutate(Année = "2020")

trafic_2019 <- read.csv("Fichiers/trafic-annuel-entrant-par-station-du-reseau-ferre-2019.csv", sep = ";", header = T)
trafic_2019 <- trafic_2019 %>% 
  mutate(Année = "2019")

trafic_2018 <- read.csv("Fichiers/trafic-annuel-entrant-par-station-du-reseau-ferre-2018.csv", sep = ";", header = T)
trafic_2018 <- trafic_2018 %>% 
  mutate(Année = "2018")

trafic_2017 <- read.csv("Fichiers/trafic-annuel-entrant-par-station-du-reseau-ferre-2017.csv", sep = ";", header = T)
trafic_2017 <- trafic_2017 %>% 
  mutate(Année = "2017")

trafic_2016 <- read.csv("Fichiers/trafic-annuel-entrant-par-station-du-reseau-ferre-2016.csv", sep = ";", header = T)
trafic_2016 <- trafic_2016 %>%
  select(-12:-ncol(trafic_2016))
trafic_2016 <- trafic_2016 %>% 
  mutate(Année = "2016")

trafic_2015 <- read.csv("Fichiers/trafic-annuel-entrant-par-station-du-reseau-ferre-2015.csv", sep = ";", header = T)
trafic_2015 <- trafic_2015 %>% 
  mutate(Année = "2015")


trafic_metro <- rbind(trafic_2021, trafic_2020, trafic_2019, trafic_2018, trafic_2017, trafic_2016, trafic_2015)
trafic_metro <- trafic_metro %>%
  filter(Réseau == "Métro") %>%
  filter(Station != "FUNICULAIRE") %>%
  mutate(Correspondance_2 = ifelse(Correspondance_2 %in% c("A", "B", ""), NA, Correspondance_2),
         Correspondance_3 = ifelse(Correspondance_3 %in% c("A", "B", ""), NA, Correspondance_3),
         Correspondance_4 = ifelse(Correspondance_4 %in% c("A", "B", ""), NA, Correspondance_4),
         Correspondance_5 = ifelse(Correspondance_5 %in% c("A", "B", ""), NA, Correspondance_5)) %>%
  mutate(Correspondance_2 = ifelse(Station == "SAINT-LAZARE", NA, Correspondance_2)) %>% 
  arrange(desc(Trafic))


trafic_metro <- trafic_metro %>% 
  mutate(
    Station_et_Ligne = paste0(
      Station,
      " (Ligne ",
      ifelse(!is.na(Correspondance_1), Correspondance_1, ""),
      ifelse(!is.na(Correspondance_2), paste0(",", Correspondance_2), ""),
      ifelse(!is.na(Correspondance_3), paste0(",", Correspondance_3), ""),
      ifelse(!is.na(Correspondance_4), paste0(",", Correspondance_4), ""),
      ifelse(!is.na(Correspondance_5), paste0(",", Correspondance_5), ""),
      ")"
    )
  )



# DATA TRAFIC LIGNES :
trafic_ligne <- read.csv("Fichiers/trafic_metro.csv", sep = ";", header = T)
trafic_ligne <- trafic_ligne %>%
  mutate(across(2:ncol(trafic_ligne), as.numeric))
trafic_ligne[14,1] <- "Ligne 14"


trafic_ligne <- trafic_ligne %>%
  select(-2:-16) %>% 
  slice(-n())


trafic_ligne <- trafic_ligne %>%
  mutate(
    X = case_when(
      X == "Ligne 1" ~ "Ligne 01",
      X == "Ligne 2" ~ "Ligne 02",
      X == "Ligne 3 (+3bis)" ~ "Ligne 03 & 03bis",
      X == "Ligne 4" ~ "Ligne 04",
      X == "Ligne 5" ~ "Ligne 05",
      X == "Ligne 6" ~ "Ligne 06",
      X == "Ligne 7 (+7bis)" ~ "Ligne 07 & 07bis",
      X == "Ligne 8" ~ "Ligne 08",
      X == "Ligne 9" ~ "Ligne 09",
      X == "Ligne 10" ~ "Ligne 10",
      X == "Ligne 11" ~ "Ligne 11",
      X == "Ligne 12" ~ "Ligne 12",
      X == "Ligne 13" ~ "Ligne 13",
      X == "Ligne 14" ~ "Ligne 14",
      TRUE ~ X  # Garder les autres valeurs inchangées
    )
  )



trafic_cor1 <- trafic_metro %>% 
  select(4:5,12) %>% 
  rename(Correspondance = 2)

trafic_cor2 <- trafic_metro %>% 
  select(4,6,12) %>% 
  drop_na() %>% 
  rename(Correspondance = 2)

trafic_cor3 <- trafic_metro %>% 
  select(4,7,12) %>% 
  drop_na() %>% 
  rename(Correspondance = 2)

trafic_cor4 <- trafic_metro %>% 
  select(4,8,12) %>% 
  drop_na() %>% 
  rename(Correspondance = 2)

trafic_cor5 <- trafic_metro %>% 
  select(4,9,12) %>% 
  drop_na() %>% 
  rename(Correspondance = 2)

trafic_boxplot <- rbind(trafic_cor1, trafic_cor2, trafic_cor3, trafic_cor4, trafic_cor5)


trafic_boxplot <- trafic_boxplot %>%
  mutate(Ligne_Bis = case_when(
    Correspondance == "1" ~ "Ligne 01",
    Correspondance == "2" ~ "Ligne 02",
    Correspondance == "3" ~ "Ligne 03",
    Correspondance == "3bis" ~ "Ligne 03bis",
    Correspondance == "4" ~ "Ligne 04",
    Correspondance == "5" ~ "Ligne 05",
    Correspondance == "6" ~ "Ligne 06",
    Correspondance == "7" ~ "Ligne 07",
    Correspondance == "7bis" ~ "Ligne 07bis",
    Correspondance == "8" ~ "Ligne 08",
    Correspondance == "9" ~ "Ligne 09",
    Correspondance == "10" ~ "Ligne 10",
    Correspondance == "11" ~ "Ligne 11",
    Correspondance == "12" ~ "Ligne 12",
    Correspondance == "13" ~ "Ligne 13",
    Correspondance == "14" ~ "Ligne 14",
    TRUE ~ Correspondance
  ))






# ASSEMBLAGE DE CHAQUE VALEUR PAR LIGNE ET PAR ANNEE :
trafic_an_ligne <- gather(trafic_ligne, key = "key", value = "value", -X)
trafic_an_ligne$value <- as.numeric(trafic_an_ligne$value)
trafic_an_ligne <- trafic_an_ligne %>%
  filter(!(key %in% c("Couleur", "X2022", "X2023")))


trafic_an_ligne$key <- substr(trafic_an_ligne$key, 2, nchar(trafic_an_ligne$key))



# DATA POUR LES CARTES :
geojson_data <- st_read("Fichiers/traces-du-reseau-ferre-idf.geojson")
geojson_data <- geojson_data %>%
  select(c(-1:-3, -5, -7:-21))
geojson_data <- geojson_data[geojson_data$mode == "METRO", ]

geojson_data2 <- read.csv("Fichiers/metro-france.csv", sep = ";", header = T)
geojson_data2 <- geojson_data2[geojson_data2$ID.Line %in% 13:31, ]

geojson_data2 <- geojson_data2 %>%
  mutate(plan = case_when(
    Libelle.Line == "1" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-1.1714655768.png",
    Libelle.Line == "2" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-2.1714655842.png",
    Libelle.Line == "3" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-3.1707906095.png",
    Libelle.Line == "3bis" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-3b.1707906037.png",
    Libelle.Line == "4" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-4.1707906152.png",
    Libelle.Line == "5" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-5.1714655892.png",
    Libelle.Line == "6" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-6.1714655990.png",
    Libelle.Line == "7" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-7.1714656045.png",
    Libelle.Line == "7bis" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-7b.1707906369.png",
    Libelle.Line == "8" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-8.1714656091.png",
    Libelle.Line == "9" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-9.1714656151.png",
    Libelle.Line == "10" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-10.1714656195.png",
    Libelle.Line == "11" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-11.1707906569.png",
    Libelle.Line == "12" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-12.1714656339.png",
    Libelle.Line == "13" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-13.1714656389.png",
    Libelle.Line == "14" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-14.1707906674.png",
    TRUE ~ Libelle.Line
  ))



# CORRESPONDANCES DES LIGNES POUR LES FILTRES :
correspondances <- c(
  "Ligne 01" = "1",
  "Ligne 02" = "2",
  "Ligne 03" = "3",
  "Ligne 03bis" = "3bis",
  "Ligne 04" = "4",
  "Ligne 05" = "5",
  "Ligne 06" = "6",
  "Ligne 07" = "7",
  "Ligne 07bis" = "7bis",
  "Ligne 08" = "8",
  "Ligne 09" = "9",
  "Ligne 10" = "10",
  "Ligne 11" = "11",
  "Ligne 12" = "12",
  "Ligne 13" = "13",
  "Ligne 14" = "14"
)


geojson_data <- geojson_data %>%
  mutate(
    res_com = case_when(
      res_com == "METRO 1" ~ "1",
      res_com == "METRO 2" ~ "2",
      res_com == "METRO 3" ~ "3",
      res_com == "METRO 3bis" ~ "3bis",
      res_com == "METRO 4" ~ "4",
      res_com == "METRO 5" ~ "5",
      res_com == "METRO 6" ~ "6",
      res_com == "METRO 7" ~ "7",
      res_com == "METRO 7bis" ~ "7bis",
      res_com == "METRO 8" ~ "8",
      res_com == "METRO 9" ~ "9",
      res_com == "METRO 10" ~ "10",
      res_com == "METRO 11" ~ "11",
      res_com == "METRO 12" ~ "12",
      res_com == "METRO 13" ~ "13",
      res_com == "METRO 14" ~ "14",
      TRUE ~ res_com  # Garder les autres valeurs inchangées
    )
  )


geojson_data <- geojson_data %>%
  mutate(Ligne_Bis = case_when(
    res_com == "1" ~ "Ligne 01",
    res_com == "2" ~ "Ligne 02",
    res_com == "3" ~ "Ligne 03 & 03bis",
    res_com == "3bis" ~ "Ligne 03 & 03bis",
    res_com == "4" ~ "Ligne 04",
    res_com == "5" ~ "Ligne 05",
    res_com == "6" ~ "Ligne 06",
    res_com == "7" ~ "Ligne 07 & 07bis",
    res_com == "7bis" ~ "Ligne 07 & 07bis",
    res_com == "8" ~ "Ligne 08",
    res_com == "9" ~ "Ligne 09",
    res_com == "10" ~ "Ligne 10",
    res_com == "11" ~ "Ligne 11",
    res_com == "12" ~ "Ligne 12",
    res_com == "13" ~ "Ligne 13",
    res_com == "14" ~ "Ligne 14",
    TRUE ~ res_com
  ))



# CORRESPONDANCES DES COULEURS POUR LES FILTRES :
ligne_couleurs <- c(
  "1" = "#FFCE00",
  "2" = "#0064B0",
  "3" = "#9F9825",
  "3bis" = "#98D4E2",
  "4" = "#C04191",
  "5" = "#F28E42",
  "6" = "#83C491",
  "7" = "#F3A4BA",
  "7bis" = "#83C491",
  "8" = "#CEADD2",
  "9" = "#D5C900",
  "10" = "#E3B32A",
  "11" = "#8D5E2A",
  "12" = "#00814F",
  "13" = "#98D4E2",
  "14" = "#662483"
)


geojson_data <- geojson_data %>% 
  mutate(Couleur_Ligne = ligne_couleurs[as.character(res_com)])




# CREATION D'UN CSS POUR L'APPLICATION :
css <- ".content-wrapper {
           background-color: #1F436D !important;
          }
          .center-text {
            text-align: center;
            margin-top: 20px;
          }
          .welcome-box {
            background-color: #84AAB6; /* Adjust the color to match your desired green shade */
            color: white;
            padding: 20px;
            border-radius: 5px;
            margin: 20px;
          }
                /* Style pour les cartes de profil */
          .profile-card {
            background-color: #f9f9f9; /* Couleur de fond des cartes */
            box-shadow: 0 2px 4px rgba(0,0,0,0.1); /* Ombre des cartes */
            border-radius: 10px; /* Bord arrondi des cartes */
            padding: 20px; /* Espace interne des cartes */
            margin: 10px; /* Marge autour des cartes */
            text-align: center; /* Centrage du texte */
            width: 800px; /* Largeur des cartes */
            display: inline-block; /* Affichage en ligne */
          }
          
          /* Style pour les images dans les cartes */
          .profile-card img {
            width: 100px; /* Largeur des images */
            height: 100px; /* Hauteur des images */
            border-radius: 50%; /* Rendre les images rondes */
            margin-bottom: 10px; /* Marge sous les images */
          }
          
          /* Style pour les boutons LinkedIn */
          .btn-linkedin {
            background-color: #0077b5; /* Couleur de fond de LinkedIn */
            color: white; /* Couleur du texte */
            padding: 5px 10px; /* Espace interne des boutons */
            text-decoration: none; /* Pas de soulignement */
            border-radius: 5px; /* Bord arrondi des boutons */
            margin-top: 10px; /* Marge au-dessus des boutons */
            display: inline-block; /* Affichage en ligne */
          }
          .btn-linkedin:hover {
            background-color: #005582; /* Couleur au survol */
          }
          /* Style pour le carré de la section À Propos */
          .about-box {
            background-color: #f8f9fa; /* Couleur de fond du carré */
            border: 1px solid #ddd; /* Bordure du carré */
            border-radius: 5px; /* Bords arrondis du carré */
            padding: 20px; /* Espace interne du carré */
            margin: 20px; /* Marge externe du carré */
          }
          
          /* Style pour les SelectInput */
          .select_input{
            outline: 6px solid white;
            border-radius: 8px;
            margin-bottom: 30px;
          }
  
          h4, h1{
            color: black;
            font-weight: bold;
          }
          
          "

##################PERTURBATION#######################################################
# URL de l'API Perturbations
url = "https://prim.iledefrance-mobilites.fr/marketplace/disruptions_bulk/disruptions/v2"

# Clé API perturb
api_key = "rmSwpNir4rfVGpSbV8Baju1g4SpQ7IZk"

# En-têtes
headers = c(
  'Accept' = 'application/json',
  'apikey' = api_key
)

# Envoi de la requête au serveur
req = GET(url, add_headers(headers))

# Affichage du code réponse - si 200 alors réussite
print(paste("Status:", status_code(req)))

# Affichage du contenu de la réponse
response_content = content(req, "text", encoding = "UTF-8")
date_maj =  format(req$date, "%d-%m-%y %H:%M")
print(response_content)

data = fromJSON(rawToChar(req$content))
names(data)

disruption = data$disruptions

# Mettre temps de coté
disrupt = disruption$applicationPeriods
# Initialisation d'une liste vide 
data_bis = list()

# Boucle
for (i in 1:length(disrupt)) {
  begin_time = disrupt[i]
  end_time = paste(begin_time, "End")  
  data_bis[[i]] = c(begin_time, end_time)
}

# Convertir à un data.frame
df = do.call(rbind, data_bis)
df = as.data.frame(df, stringsAsFactors = FALSE)
colnames(df) = c('Begin', 'End')

# Mettre data dans les bonnes colonnes 
df$Begin = str_extract(df$End, "(?<=begin = \\\").*?(?=\\\")")
df$End = str_extract(df$End, "(?<=end = \\\").*?(?=\\\")")

# Mettre au bon format
df$Begin = as.POSIXct(strptime(df$Begin, format="%Y%m%dT%H%M%S", tz="UTC"))
df$End = as.POSIXct(strptime(df$End, format="%Y%m%dT%H%M%S", tz="UTC"))

perturb = data.frame(
  ID = disruption$id,
  Titre = disruption$title,
  Cause = disruption$cause,
  Message = disruption$message,
  Gravité = disruption$severity,
  Debut = df$Begin,
  Fin = df$End
)

lines = data$lines

ligne = data.frame(
  ID = lines$id,
  Nom = lines$name,
  Type = lines$mode
)

impact = lines$impactedObjects
colnames(impact)
str(impact)

# Créer une liste vide pour stocker les éléments transformés en dataframe
df_list = list()

# Parcourir chaque élément de la liste impact
for (i in seq_along(impact)) {
  # Extraire chaque élément et le transformer en dataframe
  df_list[[i]] = as.data.frame(impact[[i]], stringsAsFactors = FALSE)
}

# Combiner tous les dataframes en un seul dataframe
ref_perturb_by_line = do.call(rbind, df_list)

# Jointure des lignes et du reférentiel des perturbations
line_and_ref = merge(ref_perturb_by_line, ligne, by.x = "id", by.y = "ID", all.y = TRUE)
line_and_ref = select(line_and_ref, -name)

# Transforme les listes de disruptionIds en lignes pour chaque élément de ces listes
line_ref_eclate = unnest_longer(line_and_ref, col = disruptionIds, keep_empty = TRUE)

# Jointure des perturbations et de line_ref_eclate
data_complete = merge(line_ref_eclate, perturb, by.x = "disruptionIds", by.y = "ID", all.x = TRUE)
data_complete = rename(data_complete, "type_transport" = "Type", "type_entite" = "type", "id_entite" = "id")
data_complete$type_transport = gsub("RapidTransit", "RER", data_complete$type_transport)
data_complete$type_transport = gsub("LocalTrain", "Transilien", data_complete$type_transport)
data_complete$type_transport = gsub("Funicular", "Funiculaire", data_complete$type_transport)
data_complete$ligne_nom = paste(data_complete$type_transport, data_complete$Nom)
data_complete = data_complete[!is.na(data_complete$Message), ]

# ----- Début graphiques -----

# Nombre et proportion de perturbations par type de transport rapporté au nombre de lignes de ce type
ratio_perturb = data_complete %>%
  filter(type_entite == "line" | is.na(type_entite)) %>%
  group_by(type_transport) %>%
  summarise(
    nombre_perturbations = n(),
    total_lignes = n_distinct(id_entite),
    .groups = 'drop'
  ) %>%
  mutate(ratio = nombre_perturbations / total_lignes) %>%
  arrange(ratio)

ratio_line = data_complete %>%
  filter(type_entite == "line" | is.na(Nom)) %>%
  group_by(Nom, type_transport) %>%
  summarise(
    nombre_perturbations = n(),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  mutate(
    ratio = nombre_perturbations / n_distinct(Nom),
    nom_complet = paste(type_transport, Nom, sep = " - ")
  ) %>%
  arrange(desc(ratio))

worstLine = ratio_line[which.max(ratio_line$nombre_perturbations), "nom_complet"]

# Table d'information
Table_perturbation = data_complete[, c("ligne_nom","Cause", "Gravité","Debut","Fin", "Message")]


###############Calculateur itinéraire#############
# Fonction pour géocoder une adresse en utilisant l'API Nominatim
geocoder_adresse_nominatim <- function(adresse) {
  url <- paste0("https://nominatim.openstreetmap.org/search?q=", URLencode(adresse), "&format=json&limit=1")
  reponse <- GET(url)
  resultat <- content(reponse, "parsed")[[1]]
  if (is.null(resultat)) {
    stop("Impossible de géocoder l'adresse : ", adresse)
  }
  return(c(as.numeric(resultat$lon), as.numeric(resultat$lat)))
}

# Définir les chemins de fichiers corrects
fichier_gares <- "emplacement-des-gares-idf.csv"
fichier_geojson <- "traces-du-reseau-ferre-idf.geojson"

# Charger les données des gares
gares <- read.csv(fichier_gares, sep = ";", stringsAsFactors = FALSE, row.names = NULL)

# Extraire la latitude et la longitude de la colonne 'Geo Point' si elle existe
if ("Geo.Point" %in% colnames(gares)) {
  gares <- gares %>%
    separate("Geo.Point", into = c("lat", "lon"), sep = ",", fill = "right", convert = TRUE) %>%
    mutate(
      lat = as.numeric(gsub("\\[|\\]", "", lat)),
      lon = as.numeric(gsub("\\[|\\]", "", lon))
    )
}

# Assurer la présence des colonnes requises
colonnes_requises <- c("gares_id", "nom_long", "lon", "lat", "res_com")
colonnes_manquantes <- setdiff(colonnes_requises, names(gares))
if (length(colonnes_manquantes) > 0) {
  stop("Les colonnes suivantes sont manquantes dans le fichier CSV : ", paste(colonnes_manquantes, collapse = ", "))
}

# Renommer les colonnes pour la cohérence
gares <- gares %>%
  rename(
    id = gares_id,
    nom = nom_long,
    res_com = res_com
  )

# Ajouter les plans de lignes de métro
gares <- gares %>%
  mutate(plan = case_when(
    res_com == "METRO 1" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-1.1714655768.png",
    res_com == "METRO 2" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-2.1714655842.png",
    res_com == "METRO 3" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-3.1707906095.png",
    res_com == "METRO 3bis" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-3b.1707906037.png",
    res_com == "METRO 4" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-4.1707906152.png",
    res_com == "METRO 5" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-5.1714655892.png",
    res_com == "METRO 6" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-6.1714655990.png",
    res_com == "METRO 7" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-7.1714656045.png",
    res_com == "METRO 7bis" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-7b.1707906369.png",
    res_com == "METRO 8" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-8.1714656091.png",
    res_com == "METRO 9" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-9.1714656151.png",
    res_com == "METRO 10" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-10.1714656195.png",
    res_com == "METRO 11" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-11.1707906569.png",
    res_com == "METRO 12" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-12.1714656339.png",
    res_com == "METRO 13" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-13.1714656389.png",
    res_com == "METRO 14" ~ "https://www.ratp.fr/sites/default/files/lines-assets/plan-de-ligne/metro/plan-de-ligne_metro_ligne-14.1707906674.png",
    TRUE ~ NA_character_
  ))

# Mappages de couleurs pour les lignes
couleurs_lignes <- c(
  "1" = "#FFCE00",
  "2" = "#0064B0",
  "3" = "#9F9825",
  "3bis" = "#98D4E2",
  "4" = "#C04191",
  "5" = "#F28E42",
  "6" = "#83C491",
  "7" = "#F3A4BA",
  "7bis" = "#83C491",
  "8" = "#CEADD2",
  "9" = "#D5C900",
  "10" = "#E3B32A",
  "11" = "#8D5E2A",
  "12" = "#00814F",
  "13" = "#98D4E2",
  "14" = "#662483"
)

# Charger correctement les données GeoJSON
donnees_geojson <- st_read(fichier_geojson)

# Assurer la présence des colonnes requises
if (!all(c("res_com", "geometry") %in% colnames(donnees_geojson))) {
  stop("Les colonnes 'res_com' et 'geometry' sont manquantes dans le fichier GeoJSON")
}

# Ajouter le mappage de couleurs
donnees_geojson <- donnees_geojson %>%
  mutate(Couleur_Ligne = couleurs_lignes[as.character(res_com)])

# Convertir le data frame en objet sf
donnees_geojson <- st_as_sf(donnees_geojson, sf_column_name = "geometry", crs = 4326)

# Fonction pour trouver la gare la plus proche
trouver_gare_proche <- function(lat, lon, gares) {
  dist <- distm(matrix(c(lon, lat), nrow = 1), gares[, c("lon", "lat")])
  index_proche <- which.min(dist)
  gares[index_proche, ]
}

# Fonction pour estimer le temps de trajet
estimer_temps_trajet <- function(lat_origine, lon_origine, lat_dest, lon_dest) {
  distance <- distVincentyEllipsoid(c(lon_origine, lat_origine), c(lon_dest, lat_dest))
  vitesse_kmh <- 15  # Vitesse moyenne du métro en km/h
  temps_h <- distance / 1000 / vitesse_kmh  # Temps en heures
  temps_min <- temps_h * 60  # Temps en minutes
  return(round(temps_min))
}
########################UI########################

ui <- dashboardPage(
  dashboardHeader(title = tags$span("Transports en Commun", style = "color: #fff; font-size: 80%; width: 100%;")),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Accueil", tabName = "home", icon = icon("home")),
      menuItem("Sources et Nettoyage", tabName = "source", icon = icon("circle-info")),
      menuItem("Histoire", 
               tabName  = "histoire", icon = icon(name = "timeline", lib = "font-awesome")),
      menuItem("Fréquentation du Métro", tabName = "about", icon = icon("train"),
               menuSubItem("Des lignes (Évolution)", tabName = "freq_ligne"),
               menuSubItem("Des lignes (Par année)", tabName = "freq_ligne_annee"),
               menuSubItem("Des stations (Lignes séparées)", tabName = "freq_station_ligne"),
               menuSubItem("Des stations (Lignes rassemblées)", tabName = "freq_station")),
      menuItem("Perturbations", tabName = "Perturb", icon = icon("exclamation-triangle"),
               menuSubItem("Statistiques", tabName = "Stat", icon = icon("dashboard")),
               menuSubItem("Listes perturbations", tabName = "List", icon = icon("table")),
               # Filters as a sub-item of "Perturbations"
               menuItem("Filtres", icon = icon("filter"), startExpanded = TRUE,
                        selectInput("filter_line", "Ligne :", choices = c("Tous", sort(unique(data_complete$ligne_nom))), selected = "Tous"),
                        selectInput("filter_transport", "Transport :", choices = c("Tous", sort(unique(data_complete$type_transport))), selected = "Tous"),
                        selectInput("filter_cause", "Cause :", choices = c("Tous", sort(unique(data_complete$Cause))), selected = "Tous"),
                        selectInput("filter_gravity", "Gravité :", choices = c("Tous", sort(unique(data_complete$Gravité))), selected = "Tous")))
    ),
    menuItem("Calculateur d'itinéraire", tabName = "calculateur", icon = icon("map")),
    img(src="https://img1.picmix.com/output/stamp/normal/2/7/5/5/2205572_5d12c.gif" , width = "100%", height = "auto")
  ), 
  dashboardBody(
    tags$head(
      tags$style(HTML(css))
    ),
    tags$style(HTML("
          .content-wrapper {
           background-color: #1F436D !important;
          }
          .center-text {
            text-align: center;
            margin-top: 20px;
          }
          .welcome-box {
            background-color: #84AAB6; /* Adjust the color to match your desired green shade */
            color: white;
            padding: 20px;
            border-radius: 5px;
            margin: 20px;
          }
         .para {
            font-size: 
            border: 2px solid black;
            background-color: rgb(255,255,255,0.4);
            padding: 10px;
            border-radius: 4px;
            color: white;
            margin: 18px;
         }
         .title {
            background-color: #84AAB6; 
            color: white;
            padding: 20px;
            border-radius: 5px;
            margin: 20px;
         }
         ")
    ),
    
    tabItems(
      tabItem(tabName = "home",
              div(class = "welcome-box center-text",
                  h1("Bienvenue sur cette application Shiny!"),
                  tags$a(id="link", href="https://iutparis-seine.u-paris.fr/", class="btn btn-primary", role="button", "BUT Sciences des Données")),
              div(class = "profile-container center-text",
                  div(class = "row",
                      div(class = "profile-card",
                          img(src = "https://media.licdn.com/dms/image/D4E03AQE-5mtdaKWIWw/profile-displayphoto-shrink_200_200/0/1709820136038?e=1721260800&v=beta&t=4Jrf2GBlY1z52qMXCn1-WLqm3u-XzThajAZpMvUy1YU", alt = "bapt"),
                          h4("Baptiste TIVRIER"),
                          a(href="https://www.linkedin.com/in/baptiste-tivrier/", class="btn-linkedin", "LinkedIn: Baptiste TIVRIER")
                      ),
                      div(class = "profile-card",
                          img(src = "https://media.licdn.com/dms/image/D4E03AQE_x6hTWXtRZQ/profile-displayphoto-shrink_200_200/0/1681851169613?e=2147483647&v=beta&t=8C-KK2T6QHBvGRzTxWCXDN0JbnHpstGKK_bLFKgrcxc", alt = "pierre"),
                          h4("Pierre GAVREL"),
                          a(href="https://www.linkedin.com/in/pierregavrel/", class="btn-linkedin", "LinkedIn: Pierre GAVREL")
                      )
                  ),
                  div(class = "row",
                      div(class = "profile-card",
                          img(src = "https://media.licdn.com/dms/image/D4E03AQFNfq-wKirA6w/profile-displayphoto-shrink_200_200/0/1681987370924?e=1721260800&v=beta&t=NrAgeh_sw4e-k07CjqvZu3nc0mCmj95MlOM6sPrZV_c", alt = "tristan"),
                          h4("Tristan COADOU"),
                          a(href="https://www.linkedin.com/in/tristan-coadou-95484724b/", class="btn-linkedin", "LinkedIn: Tristan COADOU")
                      ),
                      div(class = "profile-card",
                          img(src = "https://media.licdn.com/dms/image/D4E03AQF1yXYxyhAdsA/profile-displayphoto-shrink_200_200/0/1680002100146?e=1721260800&v=beta&t=5lWGdNfsKd0zikIRpk0026IMfccq6XhYlfiHq4TwG8o", alt = "meriam"),
                          h4("Meriam BOUMEDIENE"),
                          a(href="https://www.linkedin.com/in/meriam-boumediene-240b35264/", class="btn-linkedin", "LinkedIn: Meriam BOUMEDIENE")
                      )
                  )
              )
      ),
      
      tabItem(tabName = "source", 
              div(class = "about-box",
                  h1("Sources"),
                  p("Dans notre étude utilisant une application Shiny, les sources jouent un rôle crucial dans la collecte et l'intégration des données. Les sources, qu'elles soient internes ou externes, fournissent les informations nécessaires à l'analyse et à la visualisation des données au sein de l'application Shiny. Elles peuvent prendre différentes formes, telles que des bases de données, des fichiers CSV, des API web, ou encore des données en temps réel provenant de capteurs ou d'appareils connectés."),
                  p("Les sources de données doivent être sélectionnées avec soin pour garantir la qualité et la pertinence des analyses effectuées dans l'application Shiny. Il est essentiel de choisir des sources fiables et bien documentées afin d'éviter les biais et les erreurs dans les résultats de l'étude."),
                  p("Une fois les sources de données identifiées, l'application Shiny peut être configurée pour se connecter et récupérer les données de manière transparente. Les fonctionnalités de l'application peuvent ensuite être développées pour explorer, analyser et visualiser ces données de manière interactive. Les utilisateurs de l'application peuvent ainsi interagir avec les données en temps réel, explorer différents scénarios et obtenir des insights précieux.")
              ),
              
              div(class = "about-box",
                  h1("Nettoyage"),
                  p("Le nettoyage des données est une étape cruciale dans toute étude utilisant une application Shiny. Avant de pouvoir exploiter les données pour des analyses ou des visualisations, il est indispensable de s'assurer que ces données sont propres, cohérentes et sans erreurs. Le processus de nettoyage de données implique plusieurs actions essentielles : la correction des erreurs, la gestion des valeurs manquantes, la suppression des doublons et l'harmonisation des formats."),
                  p("Corriger les erreurs peut signifier rectifier des fautes de frappe, des incohérences dans les valeurs, ou des anomalies dans les enregistrements. La gestion des valeurs manquantes est également critique ; il peut s'agir de les imputer, les supprimer ou les laisser telles quelles selon le contexte et l'impact attendu sur l'analyse. La suppression des doublons aide à éviter les biais et les surreprésentations dans les données. L'harmonisation des formats, comme uniformiser les dates ou les unités de mesure, garantit la comparabilité des données."),
                  p("Dans une application Shiny, un nettoyage de données efficace permet de créer des visualisations interactives précises et pertinentes. Des données propres et bien préparées améliorent la qualité des insights obtenus et la fiabilité des conclusions tirées. Le nettoyage des données est donc non seulement une étape préparatoire, mais aussi un facteur déterminant pour le succès de l'étude."),
                  p("Par exemple, pour l'étude de la fréquentation des métros, bien que les données initiales soient relativement propres, certaines modifications étaient nécessaires. Par exemple, la station Saint-Lazare, reliée par les lignes 3, 12, 13 et 14, offre une correspondance directe avec la ligne 9 à Saint-Augustin. Techniquement, Saint-Lazare n’a pas de station propre pour la ligne 9, mais cette correspondance a été ajoutée par erreur dans le jeu de données. Nous avons donc supprimé cette connexion incorrecte. De plus, une station 'Funiculaire', inexistante sur le réseau de métro, était ajoutée à la ligne 2, probablement à cause de la correspondance avec le Funiculaire de Montmartre à Anvers. Nous avons également supprimé cette entrée erronée. Enfin, certaines gares du RER A et B étaient incluses dans les données, ce qui ne correspondait pas à notre étude centrée sur le métro, et ont donc été retirées.")
              )
              
      ),
      
      ###PAGE HISTOIRE 
      tabItem(tabName  = "histoire","histoire",
              div(class = "welcome-box center-text",
                  h1("L'histoire des transports parisiens")),
              
              valueBox(
                value = "214",
                subtitle = "Kilomètres de voies ferrées",
                icon = icon("road"),
                color = "light-blue",
                width = 4
              ),
              
              valueBox(
                value = "1900",
                subtitle = "Année d'inauguration du métro",
                icon = icon("calendar"),
                color = "olive",
                width = 4
              ),
              
              valueBox(
                value = "4 millions",
                subtitle = "Passagers quotidiens (moyenne)",
                icon = icon("users"),
                color = "light-blue",
                width = 4),
              
              fluidRow(
                column(12, align = "justify",
                       tags$p(class = "para","L'histoire des transports parisiens
                       est fascinante et riche en événements marquants, reflétant
                       l'évolution de la ville à travers les siècles. Depuis ses
                       modestes débuts jusqu'à son réseau sophistiqué d'aujourd'hui, 
                       chaque étape a été influencée par les besoins croissants de 
                       la population, les progrès technologiques et les défis urbains.
                       Au cœur de cette histoire se trouve le métro de Paris, l'un 
                       des systèmes de transport en commun les plus emblématiques 
                       au monde. Son inauguration en 1900, à l'occasion de l'Exposition
                       Universelle, a marqué le début d'une ère nouvelle dans les 
                       déplacements urbains parisiens. Conçu pour désengorger les 
                       rues congestionnées de la ville, le métro a rapidement gagné
                       en popularité, devenant un symbole de l'efficacité et de la 
                       modernité.Cependant, l'histoire des transports parisiens 
                       ne se limite pas au métro. Au fil des ans, d'autres modes 
                       de transport ont été introduits pour répondre aux besoins 
                       changeants de la population. Les tramways, qui ont fait leur
                       apparition au XIXe siècle, ont connu un déclin avant de 
                       renaître sous une forme modernisée au XXIe siècle, contribuant
                       ainsi à la diversification du réseau de transport.Les bus,
                       introduits dans les années 1900, ont également joué un rôle
                       essentiel dans la connectivité de la ville, offrant une 
                       flexibilité accrue par rapport aux lignes de métro prédéfinies.
                       De nos jours, le réseau de bus s'étend sur toute la ville, 
                       desservant même les quartiers les plus éloignés."))
              ),
              
              infoBox(
                title = HTML("<strong>Fait intéressant</strong>"),
                value="Le métro de Paris compte 303 stations, ce qui en fait l'un des plus étendus au monde.",
                icon = icon(name = "eye", lib = "font-awesome", style = "font-size: 25px;"),
                color = "light-blue",
                width = 4),
              
              valueBox(
                value = "300",
                subtitle = "Le nombre de correspondances différentes entre les lignes de métro et de RER à Paris",
                icon = icon(name="train", lib="font-awesome"),
                color = "blue",
                width = 4
              ),
              
              infoBox(
                title =  HTML("<strong>Expansion du réseau</strong>"),
                value = "Le réseau de transport en commun s'est considérablement étendu au fil des ans, passant de quelques lignes de métro à un réseau complexe de métro, RER et bus.",
                icon = icon(name = "map", lib = "font-awesome"),
                color = "light-blue",
                width = 4,
                tags$style(".info-box { height: 130px; }")
              ),
              
              fluidRow(
                column(12, align = "justify",
                       tags$p(class = "para","Parallèlement à ces modes de transport
                       en surface, Paris a également investi dans des infrastructures
                       souterraines. Le RER (Réseau Express Régional), lancé dans 
                       les années 1970, relie la ville à sa banlieue et offre une
                       alternative efficace au métro pour les trajets périphériques
                       . De même, le développement du réseau de tramway moderne 
                       s'est accompagné de la construction de lignes souterraines
                       pour assurer une circulation fluide dans le centre-ville.
                       L'histoire des transports parisiens est également ponctuée
                       d'innovations technologiques et d'événements marquants. 
                       L'introduction de la carte Navigo en 2001, par exemple, 
                       a révolutionné la manière dont les Parisiens paient et 
                       accèdent aux transports en commun. De même, les grèves 
                       périodiques des travailleurs des transports ont souvent
                       été le théâtre de débats animés sur les questions de droits
                       des travailleurs, de services publics et de mobilité urbaine.
                       En résumé, l'histoire des transports parisiens est une chronique
                       de l'évolution de la ville elle-même, reflétant ses défis, ses
                       triomphes et sa capacité à s'adapter aux besoins changeants de
                       ses habitants. De ses modestes débuts avec le métro jusqu'à 
                       son réseau de transport moderne et diversifié, Paris continue
                       d'être une référence en matière de mobilité urbaine."))),
      )
      ,
      
      
      tabItem(
        
        # FREQUENTATION DES LIGNES SUR UNE PERIODE
        tabName = "freq_ligne",  # Assurez-vous que tous les éléments sont dans l'onglet "freq_metro"
        fluidRow(
          box(
            width = 12,
            title = "Filtre par ligne (01 à 14)",
            selectInput(
              "ligne",
              "Sélectionnez la ligne à filtrer :",
              choices = c(
                "Toutes les lignes",
                sort(unique(trafic_an_ligne$X))
              )
            )
          ),
          box(
            width = 12,
            title = "Carte des lignes de métro (rassemblées ou séparées)",
            leafletOutput("ligneMap")
          ),
          box(
            width = 6,
            title = "Courbe de l'évolution de la fréquentation des lignes de métro (rassemblées ou séparées)",
            plotOutput("frequentationPlot")
          ),
          tabBox(
            width = 6,
            title = "Tableau des effectifs de la fréquentation des lignes de métro (rassemblées ou séparées)",
            id = "tabset1",
            tabPanel("Tableau du nombre de voyageurs annuel (en millions)", dataTableOutput("table"))
          )
        )
      ),
      
      
      # FREQUENTATION DES LIGNES TOUTES CONFONDUES PAR ANNEE
      tabItem(
        tabName = "freq_ligne_annee",
        fluidRow(
          box(
            width = 12,
            title = "Filtre par année (2015 à 2021)",
            selectInput(
              "Année_Freq_Ligne",
              "Sélectionnez l'année à filtrer :",
              choices = c(
                sort(unique(trafic_an_ligne$key))
              )
            )
          ),
          box(
            width = 12,
            title = "Diagramme de la fréquentation de chaque ligne de métro",
            plotOutput("frequentationAnneePlot")
          ),
          box(
            width = 6,
            title = "Boxplot de la fréquentation de chaque ligne de métro",
            plotOutput("frequentationBoxplot")
          ),
          tabBox(
            width = 6,
            title = "Tableau de la fréquentation des lignes de métro en fonction du nombre de stations",
            id = "tabset2",
            tabPanel("Tableau des ratios", dataTableOutput("table2"))
          )
        )
      ),
      
      
      # FREQUENTATION DES STATIONS PAR LIGNE PAR ANNEE
      tabItem(
        tabName = "freq_station_ligne",  # Assurez-vous que tous les éléments sont dans l'onglet "freq_metro"
        fluidRow(
          box(
            width = 6,
            title = "Filtre par année (2015 à 2021)",
            selectInput(
              "annee",
              "Sélectionnez l'année à filtrer :",
              choices = c(
                sort(unique(trafic_metro$Année))
              )
            )
          ),
          box(
            width = 6,
            title = "Filtre par ligne (01 à 14)",
            selectInput(
              "correspondance",
              "Sélectionnez la ligne à filter :",
              choices = correspondances
            )
          ),
          box(
            width = 12,
            title = "Plan de la ligne de métro",
            uiOutput("imageBox")
          ),
          box(
            width = 12,
            title = "Carte de la ligne de métro",
            leafletOutput("metroMap3")
          ),
          box(
            width = 6,
            title = "Diagramme des stations de métro les plus fréquentées de la ligne",
            plotOutput("top5Plot")
          ),
          box(
            width = 6,
            title = "Diagramme des stations de métro les moins fréquentées de la ligne",
            plotOutput("bottom5Plot")
          )
        )
      ),
      
      
      # FREQUENCES DES STATIONS TOUTES LIGNES CONFONDUES PAR ANNEE
      tabItem(
        tabName = "freq_station",  # Assurez-vous que tous les éléments sont dans l'onglet "freq_metro"
        fluidRow(
          box(
            width = 12,
            title = "Filtre par année (2015 à 2021)",
            selectInput(
              "Année",
              "Sélectionnez l'année à filtrer :",
              choices = c(
                sort(unique(trafic_metro$Année))
              )
            )
          ),
          box(
            width = 12,
            title = "Carte des stations de métro",
            leafletOutput("metroMap2")
          ),
          box(
            width = 6,
            title = "Diagramme des stations de métro les plus fréquentées",
            plotOutput("traficPlot")
          ),
          box(
            width = 6,
            title = "Diagramme des stations de métro les moins fréquentées",
            plotOutput("traficPlot2")
          )
        )
      ),
      
      ###PERTURBATION
      
      tabItem(
        tabName = "Perturb"
      ),
      
      tabItem(tabName = "Stat",
              fluidRow(
                valueBoxOutput("nbPerturb"),
                valueBoxOutput("worstLine"),
                valueBoxOutput("dataUpdate")
              ),
              fluidRow(
                box(title = "Nombre de Perturbations", status = "primary", solidHeader = TRUE, width = 12, withSpinner(plotOutput(outputId = "nb_perturb"))),
                box(title = "Ratio de Perturbations", status = "primary", solidHeader = TRUE, width = 12, withSpinner(plotOutput(outputId = "ratio_perturb"))),
                box(title = "Perturbations par gravités", status = "primary", solidHeader = TRUE, width = 12, withSpinner(plotOutput(outputId = "disruptions_by_severity"))),
                box(title = "Perturbations par cause", status = "primary", solidHeader = TRUE, width = 12, withSpinner(plotOutput(outputId = "disruptions_by_cause"))))),
      
      tabItem(tabName = "List",
              fluidRow(
                box(title = "Tableau d'information", status = "primary", solidHeader = TRUE, width = 12,
                    div(class = "table-container", withSpinner(htmlOutput(outputId = "info_table")))))),
      
      ###Calculateur        
      #Bookmark1====         
      tabItem(tabName = "calculateur",
              fluidRow(
                box(
                  title = "Entrées", status = "primary", solidHeader = TRUE, width = 4,
                  textInput("origine", "Origine :", value = "48 Rue du Louvre, 75001 Paris, France"),
                  textInput("destination", "Destination :", value = "18 Rue de Dunkerque, 75010 Paris, France"),
                  actionButton("calculer", "Calculer l'itinéraire")
                ),
                box(
                  title = "Carte", status = "primary", solidHeader = TRUE, width = 8,
                  leafletOutput("carte"),
                  uiOutput("details_trajet")))
              
      )
    )
  )
) 
  
  
######################Server######################
server <- function(input, output, session) {
  
  # FILTRE DES COULEURS DE LA SOUS-PAGE 1 :
  observe({
    ligne_selected <- input$ligne
    if (ligne_selected != "Toutes les lignes") {
      
      ligne_selected_color <- ligne_couleurs[ligne_selected]
      updateSelectInput(session, "ligne_color", label = "Couleur de la ligne :", choices = ligne_selected_color, selected = ligne_selected_color)
      
    } else {
      updateSelectInput(session, "ligne_color", label = "Couleur de la ligne :", choices = "#000000", selected = "#000000")
    }
  })
  
  
  # CARTE DE LA SOUS-PAGE 1 :
  output$ligneMap <- renderLeaflet({
    geojson_data <- geojson_data %>%
      filter(input$ligne == "Toutes les lignes" | Ligne_Bis == input$ligne)
    
    couleur <- as.character(geojson_data$Couleur_Ligne)
    
    map1 <- leaflet() %>%
      addTiles() %>%
      addPolylines(data = geojson_data, color = as.character(couleur), opacity = 1)  
  })
  
  
  # COURBE DE LA SOUS-PAGE 1 :
  output$frequentationPlot <- renderPlot({
    trafic_periode <- trafic_an_ligne %>%
      filter(input$ligne == "Toutes les lignes" | X == input$ligne)
    
    ggplot(trafic_periode, aes(x = key, y = value, color = X, group = X)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_color_manual(values = trafic_periode$Couleur) +
      labs(x = "", y = "Nombre de voyageurs annuel (en millions)", title = "Fréquentation des lignes de métro (2015 à 2021)", color = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylim(0, 200)
  })
  
  
  # TABLEAU DE LA SOUS-PAGE 1 :
  output$table <- renderDataTable({
    trafic_ligne <- trafic_ligne %>%
      filter(input$ligne == "Toutes les lignes" | X == input$ligne)
    
    datatable(
      trafic_ligne %>% 
        select(-9:-ncol(trafic_ligne)) %>%
        rename(Ligne = X) %>% 
        rename_with(~substr(., 2, nchar(.)), -1) %>% 
        mutate(Moyenne = round(rowMeans(select(., `2015`:`2021`)))),
      options = list(pageLength = 7, dom = 'tip')
    )
  })
  
  
  
  
  
  # DIAGRAMME DE LA SOUS-PAGE 2 :
  output$frequentationAnneePlot <- renderPlot({
    
    trafic_freq_ligne <- trafic_an_ligne %>%
      filter(key == input$Année_Freq_Ligne) %>%
      arrange(desc(value)) 
    
    ggplot(trafic_freq_ligne, aes(x = reorder(X, -value), y = value, fill = X)) +
      theme_classic() +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_manual(values = correspondance_couleur$Couleur, guide = "none") +
      geom_text(aes(label = scales::number(value, big.mark = " ")), position = position_stack(vjust = 0.5), color = "black", size = 3) +
      labs(x = "", y = "Nombre de voyageurs annuel (en millions)", title = paste0("Fréquentation des lignes de métro (", input$Année_Freq_Ligne, ")")) +
      theme(axis.text.x = element_text(angle = 50, hjust = 1))
  })
  
  
  # BOXPLOT DE LA SOUS-PAGE 2 :
  output$frequentationBoxplot <- renderPlot({
    trafic_boxplot <- trafic_boxplot %>%
      filter(Année == input$Année_Freq_Ligne)
    
    couleur_vector <- setNames(as.character(trafic_boxplot$Couleur), trafic_boxplot$Correspondance)
    
    ggplot(data = trafic_boxplot, aes(x = reorder(Ligne_Bis, -Trafic), y = Trafic, fill = Correspondance)) +
      theme_replace() +
      geom_boxplot() +
      scale_fill_manual(values = couleur_vector, guide = "none") +
      labs(x = "", y = "Nombre de voyageurs annuel", title = paste0("Fréquentation des lignes de métro (", input$Année_Freq_Ligne, ")")) +
      theme(axis.text.x = element_text(angle = 50, hjust = 1))
  })
  
  
  # TABLEAU DE LA SOUS-PAGE 2 :
  output$table2 <- renderDataTable({
    
    ratio_annee <- trafic_boxplot %>%
      group_by(Ligne_Bis, Année) %>%
      summarise(
        Correspondance_count = n(),
        Trafic_count = sum(Trafic, na.rm = TRUE)
      ) %>%
      mutate(
        Ratio = round(Trafic_count / Correspondance_count)
      ) %>% 
      select(-4) %>%
      rename(Ligne = Ligne_Bis) %>% 
      rename(`Nombre de Stations` = Correspondance_count) %>%
      filter(Année == input$Année_Freq_Ligne)
    
    ratio_annee <- ratio_annee %>%
      filter(Année == input$Année_Freq_Ligne)
    
    datatable(
      ratio_annee,
      options = list(pageLength = 7, dom = 'tip')
    )
  })
  
  
  
  
  
  # CARTE DE LA SOUS-PAGE 3 :
  output$metroMap3 <- renderLeaflet({
    correspondance_column <- paste0("Correspondance_", input$correspondance)
    
    filtered_data <- geojson_data %>% 
      filter(res_com == input$correspondance)
    
    filtered_data2 <- geojson_data2 %>% 
      filter(Libelle.Line == input$correspondance)
    
    couleur <- ligne_couleurs[as.character(input$correspondance)]
    
    map3 <- leaflet() %>%
      addTiles() %>%
      addPolylines(data = filtered_data, color = as.character(couleur), opacity = 1) 
    
    map3 <- map3 %>% addMarkers(data = filtered_data2, lng = ~Longitude, lat = ~Latitude, label = ~Libelle.station ,icon = icon_point)
    
  })
  
  
  # PLAN DE LA SOUS-PAGE 3 :
  output$imageBox <- renderUI({
    correspondance_column <- paste0("Correspondance_", input$correspondance)
    
    filtered_data2 <- geojson_data2 %>% 
      filter(Libelle.Line == input$correspondance)
    
    image_url <- filtered_data2$plan[1]  # Utiliser [1] pour extraire l'URL unique
    
    # Créer une balise img avec l'URL de l'image
    tags$img(src = image_url, width = "100%")
  })
  
  
  # DIAGRAMME 1 DE LA SOUS-PAGE 3 :
  output$top5Plot <- renderPlot({
    correspondance_column <- paste0("Correspondance_", input$correspondance)
    
    filtered_data <- trafic_metro %>%
      filter(Année == input$annee & 
               (Correspondance_1 == input$correspondance |
                  Correspondance_2 == input$correspondance |
                  Correspondance_3 == input$correspondance |
                  Correspondance_4 == input$correspondance |
                  Correspondance_5 == input$correspondance)) %>%
      top_n(5, Trafic) %>%
      arrange(desc(Trafic))
    
    couleur <- ligne_couleurs[as.character(input$correspondance)]
    
    ggplot(filtered_data, aes(x = reorder(Station, -Trafic), y = Trafic, label = Trafic)) +
      theme_classic() +
      geom_bar(stat = "identity", fill = couleur, color = "black") +
      geom_text(aes(label = scales::number(Trafic, big.mark = " ")), position = position_stack(vjust = 0.5), color = "black", size = 3) +
      labs(x = "", y = "Nombre de validations annuel", title = paste0("Fréquentation des stations de métro (Ligne ", input$correspondance, ", ", input$annee, ")")) +
      theme(axis.text.x = element_text(angle = 50, hjust = 1))
  })
  
  
  # DIAGRAMME 2 DE LA SOUS-PAGE 3 :
  output$bottom5Plot <- renderPlot({
    correspondance_column <- paste0("Correspondance_", input$correspondance)
    
    filtered_data <- trafic_metro %>%
      filter(Année == input$annee & 
               (Correspondance_1 == input$correspondance |
                  Correspondance_2 == input$correspondance |
                  Correspondance_3 == input$correspondance |
                  Correspondance_4 == input$correspondance |
                  Correspondance_5 == input$correspondance)) %>%
      tail(5) %>%
      arrange(Trafic)
    
    couleur <- ligne_couleurs[as.character(input$correspondance)]
    
    ggplot(filtered_data, aes(x = reorder(Station, Trafic), y = Trafic, label = Trafic)) +
      theme_classic() +
      geom_bar(stat = "identity", fill = couleur, color = "black") +
      geom_text(aes(label = scales::number(Trafic, big.mark = " ")), position = position_stack(vjust = 0.5), color = "black", size = 3) +
      labs(x = "", y = "Nombre de validations annuel", title = paste0("Fréquentation des stations de métro (Ligne ", input$correspondance, ", ", input$annee, ")")) +
      theme(axis.text.x = element_text(angle = 50, hjust = 1))
  })
  
  
  
  
  
  # CARTE DE LA SOUS-PAGE 4 :
  icon_point = makeIcon(iconUrl = "https://cdn-icons-png.flaticon.com/128/7584/7584620.png", iconWidth = 10, iconHeight = 10)
  output$metroMap2 <- renderLeaflet({
    
    couleur <- as.character(geojson_data$Couleur_Ligne)
    
    map2 <- leaflet() %>%
      addTiles() %>%
      addPolylines(data = geojson_data, color = as.character(couleur), opacity = 1) 
    
    map2 <- map2 %>% addMarkers(data = geojson_data2, lng = ~Longitude, lat = ~Latitude, label = ~Libelle.station, icon = icon_point)
    
  })
  
  
  # DIAGRAMME 1 DE LA SOUS-PAGE 4 :
  output$traficPlot <- renderPlot({
    filtered_data_annee <- trafic_metro %>%
      filter(Année == input$Année) %>%
      top_n(10, Trafic) %>%
      arrange(desc(Trafic))
    
    ggplot(filtered_data_annee, aes(x = reorder(Station_et_Ligne, -Trafic), y = Trafic, label = Trafic)) +
      theme_classic() +
      geom_bar(stat = "identity", fill = "lightblue", color = "black") +
      geom_text(aes(label = scales::number(Trafic, big.mark = " ")), position = position_stack(vjust = 0.5), color = "black", size = 3) +
      labs(x = "", y = "Nombre de validations annuel", title = paste0("Fréquentation des stations de métro (", input$Année, ")")) +
      theme(axis.text.x = element_text(angle = 50, hjust = 1))
  })
  
  
  # DIAGRAMME 2 DE LA SOUS-PAGE 4 :
  output$traficPlot2 <- renderPlot({
    filtered_data_annee2 <- trafic_metro %>%
      filter(Année == input$Année) %>%
      tail(10) %>%
      arrange(desc(Trafic))
    
    ggplot(filtered_data_annee2, aes(x = reorder(Station_et_Ligne, Trafic), y = Trafic, label = Trafic)) +
      theme_classic() +
      geom_bar(stat = "identity", fill = "lightsalmon", color = "black") +
      geom_text(aes(label = scales::number(Trafic, big.mark = " ")), position = position_stack(vjust = 0.5), color = "black", size = 3) +
      labs(x = "", y = "Nombre de validations annuel", title = paste0("Fréquentation des stations de métro (", input$Année, ")")) +
      theme(axis.text.x = element_text(angle = 50, hjust = 1))
  })
  
  
  
  ####PERTURBATION
  # Filtrage des données en fonction des filtres sélectionnés
  filtered_data = reactive({
    data_complete %>%
      filter(
        (input$filter_transport == "Tous" | type_transport == input$filter_transport) &
          (input$filter_cause == "Tous" | Cause == input$filter_cause) &
          (input$filter_gravity == "Tous" | Gravité == input$filter_gravity) &
          (input$filter_line == "Tous" | ligne_nom == input$filter_line)
      )
  })
  
  observe({
    selected_transport <- input$filter_transport
    lines_filtered <- if (selected_transport == "Tous") {
      sort(unique(data_complete$ligne_nom))
    } else {
      sort(unique(data_complete[data_complete$type_transport == selected_transport, "ligne_nom"]))
    }
    
    updateSelectInput(session, "filter_line", choices = c("Tous", lines_filtered))
  })
  
  observe({
    causes <- sort(unique(data_complete$Cause))
    updateSelectInput(session, "filter_cause", choices = c("Tous", causes))
  })
  
  observe({
    gravities <- sort(unique(data_complete$Gravité))
    updateSelectInput(session, "filter_gravity", choices = c("Tous", gravities))
  })
  
  observe({
    transports <- sort(unique(data_complete$type_transport))
    updateSelectInput(session, "filter_transport", choices = c("Tous", transports))
  })
  
  ratio_perturb = reactive({
    filtered_data() %>%
      filter(type_entite == "line" || is.na(type_entite)) %>%
      group_by(type_transport) %>%
      summarise(
        nombre_perturbations = n(),
        total_lignes = n_distinct(id_entite),
        .groups = 'drop'
      ) %>%
      mutate(ratio = nombre_perturbations / total_lignes) %>%
      arrange(ratio)
  })
  
  output$nb_perturb = renderPlot({
    
    # Graphique du nombre de perturbations
    ggplot(ratio_perturb(), aes(x = reorder(type_transport, -nombre_perturbations), y = nombre_perturbations, fill = type_transport)) +
      geom_bar(stat = "identity", color = "black") +
      geom_text(aes(label = nombre_perturbations), 
                position = position_stack(vjust = 0.5), 
                color = "black", size = 5) +
      labs(
        x = "",
        y = "Nombre de Perturbations",
        fill = "Type de Transport"
      ) +
      theme_minimal()
  })
  
  output$ratio_perturb = renderPlot({
    # Debugging output
    print(head(ratio_perturb()))
    
    # Graphique du ratio de perturbations
    ggplot(ratio_perturb(), aes(x = reorder(type_transport, -ratio), y = ratio, fill = type_transport)) +
      geom_bar(stat = "identity", color = "black") +
      geom_text(aes(label = round(ratio, 2)), 
                position = position_stack(vjust = 0.5), 
                color = "black", size = 5) +
      labs(
        x = "",
        y = "Ratio de Perturbations",
        fill = "Type de Transport"
      ) +
      theme_minimal()
  })
  
  output$info_table = renderUI({
    Table_perturbation_filtered = filtered_data() %>%
      select(ligne_nom, Cause, Gravité, Debut, Fin, Message) %>%
      mutate(
        Debut = ifelse(is.na(Debut), "NA", format(Debut, "%d-%m-%Y %H:%M")),
        Fin = ifelse(is.na(Fin), "NA", format(Fin, "%d-%m-%Y %H:%M"))
      )
    
    Table_perturbation_filtered$Message = lapply(Table_perturbation_filtered$Message, HTML)
    
    table = do.call(tagList, apply(Table_perturbation_filtered, 1, function(row) {
      tags$tr(
        tags$td(row[["ligne_nom"]], style = "width: 5%;"),
        tags$td(row[["Cause"]], style = "width: 10%;"),
        tags$td(row[["Gravité"]], style = "width: 10%;"),
        tags$td(row[["Debut"]], style = "width: 10%;"),
        tags$td(row[["Fin"]], style = "width: 10%;"),
        tags$td(row[["Message"]], class = "message-column", style = "width: 50%; text-align: left;")
      )
    }))
    
    tags$table(
      tags$thead(
        tags$tr(
          tags$th("Ligne"),
          tags$th("Cause"),
          tags$th("Gravité"),
          tags$th("Début"),
          tags$th("Fin"),
          tags$th("Message")
        )
      ),
      tags$tbody(
        table
      )
    )
  })
  
  output$nbPerturb = renderValueBox({
    valueBox(
      nrow(filtered_data()), "Nb de Perturbations", icon = icon("chart-simple"),
      color = "olive"
    )
  })
  
  output$worstLine = renderValueBox({
    valueBox(
      worstLine, "Ligne la plus impactée", icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$dataUpdate = renderValueBox({
    valueBox(
      date_maj, "Fraicheur des données", icon = icon("sync"),
      color = "yellow"
    )
  })
  
  output$disruptions_by_severity <- renderPlot({
    severity_data <- filtered_data() %>%
      group_by(Gravité) %>%
      summarise(Number_of_Disruptions = n(), .groups = 'drop')
    ggplot(severity_data, aes(x = reorder(Gravité, -Number_of_Disruptions), y = Number_of_Disruptions, fill = Gravité)) +
      geom_bar(stat = "identity", color = "black") +
      geom_text(aes(label = Number_of_Disruptions), 
                position = position_stack(vjust = 0.5), 
                color = "black", size = 5) +
      labs(
        x = "",
        y = "Nombre de perturbations",
        fill = "Severity"
      ) +
      theme_minimal()
  })
  
  output$disruptions_by_cause <- renderPlot({
    cause_data <- filtered_data() %>%
      group_by(Cause) %>%
      summarise(Number_of_Disruptions = n(), .groups = 'drop')
    ggplot(cause_data, aes(x = reorder(Cause, -Number_of_Disruptions), y = Number_of_Disruptions, fill = Cause)) +
      geom_bar(stat = "identity", color = "black") +
      geom_text(aes(label = Number_of_Disruptions), 
                position = position_stack(vjust = 0.5), 
                color = "black", size = 5) +
      labs(
        x = "",
        y = "Nombre de perturbations",
        fill = "Cause"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  ####Calculateur d'itinéraire
  #bookmark2====
  donnees_trajet <- reactiveValues(origine = NULL, destination = NULL, connexion = NULL, temps_trajet = NULL)
  
  observeEvent(input$calculer, {
    # Géocoder les adresses d'origine et de destination
    coords_origine <- geocoder_adresse_nominatim(input$origine)
    coords_destination <- geocoder_adresse_nominatim(input$destination)
    
    # Trouver les gares les plus proches
    donnees_trajet$origine <- trouver_gare_proche(coords_origine[2], coords_origine[1], gares)
    donnees_trajet$destination <- trouver_gare_proche(coords_destination[2], coords_destination[1], gares)
    
    # Estimer le temps de trajet
    donnees_trajet$temps_trajet <- estimer_temps_trajet(donnees_trajet$origine$lat, donnees_trajet$origine$lon, donnees_trajet$destination$lat, donnees_trajet$destination$lon)
    
    # Trouver la connexion entre les lignes
    trouver_connexion <- function(gare_origine, gare_destination) {
      lignes_origine <- unlist(strsplit(gare_origine$res_com, " "))
      lignes_destination <- unlist(strsplit(gare_destination$res_com, " "))
      lignes_communes <- intersect(lignes_origine, lignes_destination)
      if (length(lignes_communes) > 0) {
        return(lignes_communes)
      } else {
        return(NULL)
      }
    }
    
    donnees_trajet$connexion <- trouver_connexion(donnees_trajet$origine, donnees_trajet$destination)
    
    # Afficher les détails du trajet
    lignes_connexion <- ifelse(is.null(donnees_trajet$connexion), 
                               "Aucune correspondance directe trouvée", 
                               paste("METRO", paste(donnees_trajet$connexion, collapse = ", ")))
    
    details_trajet <- paste0(
      "Station d'origine : ", donnees_trajet$origine$nom,
      "<br>Station de destination : ", donnees_trajet$destination$nom,
      "<br>Ligne de correspondance : ", gsub("METRO METRO", "METRO", lignes_connexion),
      "<br>Durée estimée du trajet : ", donnees_trajet$temps_trajet, " minutes",
      "<br><a href='", donnees_trajet$origine$plan, "' target='_blank'>Plan de la ligne d'origine</a>",
      "<br><a href='", donnees_trajet$destination$plan, "' target='_blank'>Plan de la ligne de destination</a>"
    )
    
    output$details_trajet <- renderUI({
      tags$div(
        class = "trajet",
        HTML(details_trajet)
      )
    })
    
    # Coordonner la ligne droite entre les stations d'origine et de destination
    coords_chemin <- data.frame(
      lon = c(donnees_trajet$origine$lon, donnees_trajet$destination$lon),
      lat = c(donnees_trajet$origine$lat, donnees_trajet$destination$lat)
    )
    
    # S'assurer que les coordonnées du chemin sont numériques
    coords_chemin$lon <- as.numeric(coords_chemin$lon)
    coords_chemin$lat <- as.numeric(coords_chemin$lat)
    
    # Afficher la carte avec la ligne droite
    output$carte <- renderLeaflet({
      carte <- leaflet() %>%
        addTiles() %>%
        addMarkers(lng = donnees_trajet$origine$lon, lat = donnees_trajet$origine$lat, popup = donnees_trajet$origine$nom) %>%
        addMarkers(lng = donnees_trajet$destination$lon, lat = donnees_trajet$destination$lat, popup = donnees_trajet$destination$nom) %>%
        addPolylines(
          lng = coords_chemin$lon,
          lat = coords_chemin$lat,
          color = "blue",
          weight = 5
        )
      
      carte
    })
  })
}

# Lancement de l'application Shiny
shinyApp(ui, server)
