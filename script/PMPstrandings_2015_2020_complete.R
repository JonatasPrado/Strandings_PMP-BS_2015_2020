## 
## Intensive and wide-ranging beach surveys uncover 
## temporal and spatial stranding patterns of marine megafauna
##
## ICES Journal of Marine Science - 2022
##
## Code by Jonatas F. H. Prado & Nicholas W Daudt

## Description

### General tidy rules - - - - - 
# variable_names <- write with lower case and underlines
# dataframeNames <- cammelCase style (startLowerCaseAndEveryNewWordStartWithCapital)
# package::function notation, unless from 'base' packages
###- - - - - - - - - - - - - - - 

################################################################################

rm(list=ls())

# Libraries ####
library(plyr)
library(tidyverse)
library(sf)
library(gamlss)
library(ggspatial)
library(cowplot)
library(readxl)
library(rnaturalearth)
library(mapview) # used only for checking spatial features

################################################################################
############################## Dataset manipulation ############################
################################################################################

##----------------------------------------------------------------------------##
##                              Stranding dataset                             ##
##----------------------------------------------------------------------------##

## Open stranding dataset
df <- as.data.frame(
  readxl::read_xlsx("./data/strandingDatasetPMP_BS_2015_2020.xlsx",sheet = 3))

## Filter columns
df <- 
  df %>% 
  dplyr::select(`Identificador do indivíduo`, Estado, Praia, Trecho, 
                `Estratégia do trecho`, `Tipo do monitoramento`,`Monitoramento`,
                `Data/Hora`, `Ponto - Lat`, `Ponto - Long`,`Espécies - Classe`,
                `Espécies - Ordem`,`Espécies - Subordem`,`Espécies - Família`,
                `Espécies - Gênero`,`Espécies - Espécie`,`Condição da carcaça`,
                `Condição`)

## Rename columns and levels
names(df) <- c("id_individual", "state", "beach","stretch", 
               "stretch_scheme", "monitoring_type","monitoring_number",
               "date_hour", "lat", "long","class","order","suborder", "family",
               "genus","species", "cod_decomposition","dead_live")

df$state <- as.factor(df$state)

df$stretch_scheme <- as.factor(df$stretch_scheme)
levels(df$stretch_scheme) <- list(daily = "Diário", 
                                  weekly = "Semanal", 
                                  fortnightly = "Diário 15", 
                                  call = "Acionamento")

df$monitoring_type <- as.factor(df$monitoring_type)
levels(df$monitoring_type) <- list(regular = "Regular", 
                                   call = "Acionamento")

df$dead_live <- as.factor(df$dead_live)
levels(df$dead_live) <- list(dead = "Morto", 
                             live = "Vivo")

## Remove stretch_scheme == "call", 
## monitoring_type == "call" & stretch_scheme == "weekly", 
## monitoring_type == "call" & stretch_scheme == "fortnightly", and 
## "date_hour" column;
## Create columns date, year, month, year_n, season  
## Filter survey effort from date > "2015-08-31" & <= "2020-06-07"
df <- 
  df %>% 
  dplyr::filter(stretch_scheme != "call") %>% 
  dplyr::filter(!((monitoring_type == "call" & 
                     stretch_scheme == "weekly") |
                    (monitoring_type == "call" & 
                       stretch_scheme == "fortnightly"))) %>%
  dplyr::mutate(date = lubridate::as_date(date_hour)) %>% 
  dplyr::select(- date_hour) %>%
  dplyr::filter(date > "2015-08-31" & date <= "2020-07-17") %>% 
  dplyr::mutate(month = lubridate::month(date), 
                year = lubridate::year(date)) %>% 
  dplyr::mutate(year_n = 
                  ifelse(date > "2015-08-31" & date < "2016-09-01","Year1", 
                         ifelse(date > "2016-08-31" & date < "2017-09-01","Year2", 
                                ifelse(date > "2017-08-31" & date < "2018-09-01","Year3", 
                                       ifelse(date > "2018-08-31" & date < "2019-09-01","Year4",
                                              "Year5"))))) %>%
  arrange(date)

## Remove taxa that won't be analyzed
df <- 
  df %>%
  dplyr::filter_at(vars(order, suborder, family, genus), 
                   all_vars(!. %in% c("Carnivora", "Podicipediformes", "Crocodilia", 
                                      "Gruiformes", "Coraciiformes", "Pleurodira", 
                                      "Phalacrocoracidae", "Threskiornithidae", 
                                      "Botaurus", "Bubulcus", "Butorides", "Egretta", 
                                      "Gallinago", "Ixobrychus", "Nyctanassa", 
                                      "Nycticorax", "Syrigma", "Tigrisoma", "Vanellus", 
                                      "Ardea", "Ardeidae")))

## Include the lower taxon level in "species" column
df$species <- ifelse(is.na(df$species), paste(df$genus), paste(df$species))
df$species <- ifelse(df$species == "NA", paste(df$family), paste(df$species))
df$species <- ifelse(df$species == "NA", paste(df$suborder), paste(df$species))
df$species <- ifelse(df$species == "NA", paste(df$order), paste(df$species))
df$species <- ifelse(df$species == "NA", paste(df$class), paste(df$species))

## A few "Pontoporia" and "Sotalia" records had just genus, 
## so include the species name
df$species <- ifelse(df$species == "Pontoporia", "Pontoporia blainvillei", 
                     paste(df$species))
df$species <- ifelse(df$species == "Sotalia", "Sotalia guianensis", 
                     paste(df$species))

## Change the species names follow IUCN redlist
df <-
  df %>%
  dplyr::mutate(species = ifelse(species == "Calonectris diomedea borealis","Calonectris borealis",
                                 ifelse(species == "Calonectris diomedea","Calonectris borealis",
                                        ifelse(species == "Puffinus gravis","Ardenna gravis",
                                               ifelse(species == "Puffinus griseus","Ardenna grisea",
                                                      ifelse(species == "Thalasseus acuflavidus", "Thalasseus sandvicensis",
                                                             ifelse(species == "Chroicocephalus maculipennis", "Larus maculipennis",
                                                                    ifelse(species == "Chroicocephalus cirrocephalus","Larus cirrocephalus",
                                                                           ifelse(species == "Himantopus melanurus","Himantopus himantopus",
                                                                                  ifelse(species == "Stercorarius maccormicki","Catharacta maccormicki",
                                                                                         ifelse(species == "Stercorarius chilensis","Catharacta chilensis",
                                                                                                ifelse(species == "Stercorarius antarticus","Catharacta antarctica",
                                                                                                       ifelse(species == "Balaenoptera brydei","Balaenoptera edeni",
                                                                                                              species)))))))))))))        

## Remove records distant more than 300 m from the coast line

# Open shapefiles (shp) of beach monitoring lines
# Create a list with all subdirectories containing the shp
ff <- as.list(list.files(path = ".", pattern = "linha.shp$", 
                         recursive = TRUE, full.names = TRUE))

# Function to open the shp in subdirectories
open_shp <- function(ff){
  linha <- sf::read_sf(ff[i])
  linha
}

# Loop creating a list with all shp
lines <- list()
for (i in 1:length(ff)) {
  lines[[i]] <- open_shp(ff)
}

# Merge all beach monitoring lines into one shp
originalStretches <- do.call(rbind, lines)

# Save to backup

# sf::st_write(originalStretches, "./data_out/originalStretches.gpkg")

# Remove stretches with strategy == "noneeffort", compriment == "0",  
# and beach_name == "Ponta do Itaguá"; Create an id for stretches
thisWorkStretches <- 
  originalStretches %>% 
  dplyr::filter(stretch_st != "noneffort") %>%  
  dplyr::filter(compriment != 0) %>%
  dplyr::rename(original_length = compriment) %>%
  dplyr::filter(beach_name != "Ponta do Itaguá") %>%
  dplyr::rename(beach = beach_name) %>%
  dplyr::rename(stretch = stretch_na) %>%
  dplyr::mutate(id_original = dplyr::row_number()) %>% 
  sf::st_cast("MULTILINESTRING")

# Create a buffer of 300 m around the beach monitoring lines
thisWorkStretches <- sf::st_transform(thisWorkStretches, crs = 31982)
buffer <- sf::st_buffer(thisWorkStretches, dist = 300)

# Transform 'df' into a geospatial feature
dfSpatial <-
  df %>%
  dplyr::mutate(long1 = long,
                lat1 = lat) %>%
  sf::st_as_sf(coords = c("long1","lat1"), crs = 4326)

# Set same 'crs' for stranding data
dfSpatial <- sf::st_transform(dfSpatial, crs = 31982)

# Remove records outside the buffer
dfSpatial <- 
  dfSpatial %>% 
  dplyr::mutate(on_buffer = lengths(sf::st_within(dfSpatial, buffer))) %>% 
  dplyr::filter(on_buffer > 0) %>% 
  dplyr::select(- on_buffer)

# mapview::mapview(dfSpatial) + buffer

##----------------------------------------------------------------------------##
##                             Survey effort dataset                          ##
##----------------------------------------------------------------------------##

## Open datasets and merge them
ef_SP <- read.csv2("./data/effort_SP_aug2019_jul_2020.csv",
                   header = TRUE, encoding = "UTF-8")
ef_SC_PR <- read.csv2("./data/effort_SC_PR_aug2019_jul_2020.csv",
                      header = TRUE, encoding = "UTF-8")
ef_SP_PR_SC <- read.csv2("./data/effort_SC_PR_SP_aug2015_aug_2019.csv",
                         header = TRUE, encoding = "UTF-8")

eff <- rbind(ef_SP_PR_SC,ef_SP,ef_SC_PR)

rm(list = ls(pattern = "ef_"))

## Split data and hour
eff$date <- lubridate::dmy(
  sapply(strsplit(as.character(eff$Data.Hora.início), " "), "[", 1))

## Removing unused columns
eff[,c(2:3, 11:12, 15:16)] <- list(NULL) 

## Rename columns and levels
colnames(eff) <- c("code","state","city","beach","stretch","type",
                   "stretch_scheme","vehicle","initialLat","initialLong",
                   "complete","date")

eff$stretch_scheme<- as.factor(eff$stretch_scheme)
levels(eff$stretch_scheme) <- list(daily = "Diário", 
                                   weekly = "Semanal", 
                                   fortnightly = "Diário 15", 
                                   call = "Acionamento")

eff$type <- as.factor(eff$type)
levels(eff$type) <- list(boat = "Embarcado", 
                         land = "Terrestre")

eff$complete <- as.factor(eff$complete)
levels(eff$complete) <- list(yes = "Sim", 
                             no = "Não")

eff$vehicle <- as.factor(eff$vehicle)
levels(eff$vehicle) <- list(foot = "A pé",
                            boat = "Barco",
                            bike = "Bicicleta",
                            car = "Carro",
                            car = "Caminhonete",
                            motorcycle = "Moto",
                            motorcycle = "Quadriciculo",
                            indetermined = "")

## Filter survey effort from date > "September2015" and <= "2020-06-07";
## Remove stretch_scheme == "call" and complete == "NA";
## Create columns year, month and year_n.  
eff <- 
  eff %>% 
  dplyr::filter(date > "2015-08-31") %>%
  dplyr::filter(stretch_scheme != "call") %>%
  dplyr::filter(complete != "NA") %>%
  dplyr::mutate(month = lubridate::month(date), 
                year = lubridate::year(date)) %>% 
  dplyr::mutate(year_n = 
                  ifelse(date > "2015-08-31" & date < "2016-09-01", "Year1",
                         ifelse(date > "2016-08-31" & date < "2017-09-01", "Year2",
                                ifelse(date > "2017-08-31" & date < "2018-09-01", "Year3",
                                       ifelse(date > "2018-08-31" & date < "2019-09-01", "Year4",
                                              "Year5"))))) %>% 
  arrange(date)

##----------------------------------------------------------------------------## 
##               Standardize the numbers and names of beach surveyed          ##
##                         in eff and thisWorkStretches                       ##
##----------------------------------------------------------------------------##

## Removing excess of spaces between characters
eff$beach <- gsub("\\s+", " ", eff$beach)
eff$stretch <-gsub("\\s+", " ", eff$stretch)

thisWorkStretches$beach <- gsub("\\s+", " ", thisWorkStretches$beach)
thisWorkStretches$stretch <- gsub("\\s+", " ", thisWorkStretches$stretch)

## Identify beaches and stretches in 'eff' 
## that are not in 'thisWorkStretches' and vice versa
identify1 <- dplyr::setdiff(eff$beach,thisWorkStretches$beach)
identify2 <- dplyr::setdiff(thisWorkStretches$beach,eff$beach)
identify3 <- dplyr::setdiff(eff$stretch,thisWorkStretches$stretch)
identify4 <- dplyr::setdiff(thisWorkStretches$stretch,eff$stretch)

## Select and view the beaches and stretches in the above conditions
## Transform 'eff' into a geospatial feature
effSpatial <- 
  eff %>% 
  sf::st_as_sf(coords = c("initialLong", "initialLat"), crs = 4326)

select1 <-
  effSpatial %>% 
  dplyr::filter(beach == "Bal. Barra do Sul" | beach == "Itararé")

select3 <-
  effSpatial %>% 
  dplyr::filter(stretch == "antigo D15- Ilha Comprida" | 
                  stretch == "Antigo Superagui Sul e trapiche-Rio" | 
                  stretch == "Ipanema" | stretch == "Matinhos 1" | 
                  stretch == "Matinhos 2" | stretch == "Pontal do Sul" | 
                  stretch == "Praia da Barra N" | stretch == "Praia da Barra S" | 
                  stretch == "Praia de Garopaba N" | stretch=="Praia de Garopaba S" | 
                  stretch == "Praia de Leste")

# mapview::mapview(thisWorkStretches) + select1
# mapview::mapview(thisWorkStretches) + select3

## Verify duplicated beach names
eff_BeachNames <-
  effSpatial %>%
  dplyr::group_by(beach, stretch, city, state) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::group_by(beach) %>%
  dplyr::filter(n() > 1)

## Select and view the duplicated beaches
select_duplicated_BeachNames <-
  thisWorkStretches %>%
  dplyr::filter(beach == "...") # insert the beach name in "..." to visualize

## mapview::mapview(select_duplicated_BeachNames)

##---------------
## Conclusions:

# C1: Same beaches with different names in 'eff' and 'thisWorkStretches':
# "Bal. Barra do Sul" == ""Barra do Sul - 2 - 395 - 396 - 67" and
# "Itararé" == "Praia do José Menino, Praia do Gonzaga, Praia do Boqueirão, Praia
# do Embaré, Praia Aparecida, Ponta da Praia".

# C2: Different beaches identified by the same name in 'eff' and 'thisWorkStretches': 
#"Armação", "Brava","Camburizinho","Estaleiro","Pereque","Praia Brava","Praia Grande".

# C3: Beaches with two or more stretches in 'eff' but one in 'thisWorkStretches': 
# "Praia da Barra N" and "Praia da Barra S" == "Praia da Barra";
# "Praia de Garopaba N" and "Praia de Garopaba S" == "Praia de Garopara";
# "antigo D15- Ilha Comprida" and "Ilha Comprida" == "Ilha Comprida";
# "Ipanema","Matinhos,Matinhos 2", "Pontal do Sul","Pontal do Sul/ Flamingo" and
# "Praia de Leste" == "Pontal do Sul/ Flamingo".

# C3.1: The stretch name "antigo D15- Ilha Comprida" was the unique stretch used
# for "Ilha Comprida" beach until 2017/03/23. Since then it was 
# rename to "Ilha Comprida".

# C3.2: The stretch "Pontal do Sul/ Flamingo" was used from september2015 to 2019/08/19.
# Then, it was split into five stretches: "Ipanema","Matinhos", "Matinhos 2",
# "Pontal do Sul" and "Praia de Leste".

# C4: The stretch name "Antigo Superagui Sul e trapiche-Rio" was the unique stretch used
# for "Ilha do Superagui" beach until 2017/10/21. Then, it was split into:
# "Superagui Trapiche-Rio Novo" and "Superagui Sul Novo.
##---------------

##---------------
## Standardize beaches and stretches in 'eff' and 'thisWorkStretches' according 
## to the above conclusions
##---------------

## C1: rename beaches "Bal. Barra do Sul" and "Itararé" as in 'thisWorkStretches'
effStandardize <-
  eff %>%
  dplyr::mutate(beach = recode_factor(beach, 
                                      "Bal. Barra do Sul" = "Barra do Sul - 2 - 395 - 396 - 67", 
                                      "Itararé" = "Praia do José Menino, Praia do Gonzaga, Praia do Boqueirão, Praia do Embaré, Praia Aparecida, Ponta da Praia"))

## C2: rename different beaches identified by the same name in 'eff' and 
## 'thisWorkStretches'
effStandardize$beach <- as.character(effStandardize$beach)
effStandardize$city <- as.character(effStandardize$city)

effStandardize <-
  effStandardize %>%
  dplyr::mutate(beach = 
                  ifelse(beach == "Armação" & city == "Florianópolis", "ArmaçãoFloripa",
                         ifelse(beach == "Armação" & city == "Ilhabela", "ArmaçãoIlhaBela",
                                ifelse(beach == "Armação" & city == "Penha", "ArmaçãoPenha",
                                       ifelse(beach == "Brava" & city == "Florianópolis", "BravaFloripa",
                                              ifelse(beach == "Brava" & city == "Balneário Camboriú", "BravaItajaí",
                                                     ifelse(beach == "Brava" & city == "Balneário Camboriú, Itajaí", "BravaItajaí",
                                                            ifelse(beach == "Camburizinho" & city == "Guarujá", "CamburizinhoGuarujá",
                                                                   ifelse(beach == "Camburizinho" & city == "São Sebastião", "CamburizinhoSebastião",
                                                                          ifelse(beach == "Estaleiro" & city == "Balneário Camboriú", "EstaleiroCamburiú",
                                                                                 ifelse(beach == "Estaleiro" & city == "Ubatuba", "EstaleiroUbatuba",
                                                                                        ifelse(beach == "Pereque" & city == "Guarujá", "PerequeGarujá",
                                                                                               ifelse(beach == "Pereque" & city == "Ilhabela", "PerequeIlhaBela",
                                                                                                      ifelse(beach == "Pereque" & city == "Porto Belo", "PerequePortoBelo",
                                                                                                             ifelse(beach == "Praia Brava" & city == "Ilhabela", "Praia BravaIlhaBela",
                                                                                                                    ifelse(beach == "Praia Brava" & city == "Matinhos","Praia BravaMatinhos",
                                                                                                                           ifelse(beach == "Praia Grande" & city == "Praia Grande","Praia Grande",
                                                                                                                                  ifelse(beach == "Praia Grande" & city == "Penha","Praia GrandePenha", 
                                                                                                                                         beach))))))))))))))))))

thisWorkStretchesStandardize <-
  thisWorkStretches %>%
  dplyr::mutate(beach = 
                  ifelse(beach == "Armação" & id_original == 282, "ArmaçãoFloripa",
                         ifelse(beach == "Armação" & id_original ==  89, "ArmaçãoIlhaBela",
                                ifelse(beach == "Armação" & id_original == 339, "ArmaçãoPenha",
                                       ifelse(beach == "Brava" & id_original == 287, "BravaFloripa",
                                              ifelse(beach == "Brava" & id_original == 334, "BravaItajaí",
                                                     ifelse(beach == "Brava" & id_original == 371, "BravaItajaí",
                                                            ifelse(beach == "Camburizinho" & id_original == 234, "CamburizinhoGuarujá",
                                                                   ifelse(beach == "Camburizinho" & id_original == 75, "CamburizinhoSebastião",
                                                                          ifelse(beach == "Estaleiro" & id_original == 338, "EstaleiroCamburiú",
                                                                                 ifelse(beach == "Estaleiro" & id_original == 146, "EstaleiroUbatuba",
                                                                                        ifelse(beach == "Pereque" & id_original == 254, "PerequeGarujá",
                                                                                               ifelse(beach == "Pereque" & id_original == 77, "PerequeIlhaBela",
                                                                                                      ifelse(beach == "Pereque" & id_original == 359, "PerequePortoBelo",
                                                                                                             ifelse(beach == "Praia Brava" & id_original == 202, "Praia BravaIlhaBela",
                                                                                                                    ifelse(beach == "Praia Brava" & id_original == 312, "Praia BravaMatinhos",
                                                                                                                           ifelse(beach == "Praia Grande" & id_original == 222, "Praia Grande",
                                                                                                                                  ifelse(beach == "Praia Grande" & id_original == 352, "Praia GrandePenha", 
                                                                                                                                         beach))))))))))))))))))

## C3: For those beaches with multiple stretches in the same period, choose one 
## stretches, rename it as in 'thisWorkStretches' and remove the remain stretches.
## For those beaches with multiple stretches, but in different periods 
## (e.g. "Ilha Comprida"), just rename the stretch as in 'thisWorkStretches' 

# Step1: remove the stretches  
effStandardize <-
  effStandardize %>%
  dplyr::filter(stretch != "Praia da Barra N") %>%
  dplyr::filter(stretch !="Praia de Garopaba N") %>%
  dplyr::filter(stretch != "Matinhos 1") %>%
  dplyr::filter(stretch != "Matinhos 2") %>%
  dplyr::filter(stretch != "Ipanema") %>%
  dplyr::filter(stretch != "Praia de Leste")

# Step2: rename the stretches as in thisWorkStretches
effStandardize <-
  effStandardize %>%
  dplyr::mutate(stretch = 
                  ifelse(beach == "Praia da Barra" & stretch == "Praia da Barra S",
                         "Praia da Barra",
                         ifelse(beach == "Praia de Garopaba" & stretch == "Praia de Garopaba S",
                                "Praia de Garopaba",
                                ifelse(beach == "Pontal do Sul/ Flamingo" & stretch == "Pontal do Sul",
                                       "Pontal do Sul/ Flamingo",
                                       ifelse(beach == "Ilha Comprida" & stretch =="antigo D15- Ilha Comprida",
                                              "Ilha Comprida",
                                              stretch)))))

## C4: split "Antigo Superagui Sul e trapiche-Rio" in two stretches: 
## "Superagui Sul Novo" and "Superagui Trapiche-Rio Novo"

# Step1: remove repeated date for stretch == "Antigo Superagui Sul e trapiche-Rio" 
# in 'eff'
Superagui_df <-
  effStandardize %>%
  dplyr::filter(stretch == "Antigo Superagui Sul e trapiche-Rio")  

Superagui_df <- Superagui_df[!duplicated(Superagui_df$date), ]

# Step2: rename "Antigo Superagui Sul e trapiche-Rio" as "Superagui Sul Novo" 
Superagui_df$stretch <- as.factor(Superagui_df$stretch)

Superagui_df_SulNovo <- 
  Superagui_df %>%
  dplyr::mutate(stretch = recode_factor(stretch, 
                                        "Antigo Superagui Sul e trapiche-Rio" = "Superagui Sul Novo")) 

# Step3: rename "Antigo Superagui Sul e trapiche-Rio" as "Superagui Trapiche-Rio Novo"
Superagui_df_TrapicheNovo <- 
  Superagui_df %>% 
  dplyr::mutate(stretch = recode_factor(stretch, 
                                        "Antigo Superagui Sul e trapiche-Rio" = "Superagui Trapiche-Rio Novo")) 

# Step4: merge "Superagui_df_TrapicheNovo" and "Superagui_df_SulNovo"
Superagui_df <- rbind(Superagui_df_SulNovo, Superagui_df_TrapicheNovo)

# Step5: combine "Superagui_df" and "effStandardize"
effStandardize <- 
  effStandardize %>% 
  dplyr::filter(stretch != "Antigo Superagui Sul e trapiche-Rio")

effStandardize <- rbind(effStandardize, Superagui_df)  

## Superscribe 'effStandardize' to 'eff' and 
## 'thisWorkStretchesStandardize' to 'thisWorkStretches' and 
## create a id for both based on "beach" and "stretch" columns 
eff <-
  effStandardize %>%
  dplyr::mutate(beach_id = paste(beach, stretch,sep = "/")) %>% 
  dplyr::relocate(stretch, .before = beach) %>% 
  dplyr::relocate(beach_id, .after = beach)

thisWorkStretches <- 
  thisWorkStretchesStandardize %>% 
  dplyr::select(-c(beach_id, pmp_id, stretch_id, executing_, geometry, executin_1,
                   OBJECTID, stretch__1)) %>%
  dplyr::mutate(beach_id = paste(beach, stretch, sep = "/")) %>% 
  dplyr::select(id_original, executing1, stretch_st, stretch_ty, 
                stretch, beach, beach_id, original_length, geometry)

# sf::st_write(thisWorkStretches, "./data_out/thisWorkStretches.gpkg")

##----------------------------------------------------------------------------##
##            Split monitored beach segments based on polygon sectors         ##
##----------------------------------------------------------------------------##

## Create a 20 latitudinal-band polygon, which will be the basis for 
## final analyses

# Trasform crs
thisWorkStretches <- sf::st_transform(thisWorkStretches, crs = 4326)

# Latitudinal definition
n <- diff(sf::st_bbox(thisWorkStretches)[c(2, 4)])/20

# Longitudinal definition
m <- diff(sf::st_bbox(thisWorkStretches)[c(1, 3)])

# Create the 'sectorsPolygon' based on 'thisWorkStretches' extent
sectorsPolygon <- sf::st_make_grid(thisWorkStretches, cellsize = c(m, n))

# Set the correct spatial attributes for 'sectorsPolygon' layer
# Define 'sectorsPolygon' as single feature
sectorsPolygon <- sf::st_sf(sectorsPolygon)

# Create an identifier for each sector
sectorsPolygon$id_polygon <- 1:nrow(sectorsPolygon)

# Define 'sectorsPolygon' as a multipolygon
sectorsPolygon <- 
  sectorsPolygon %>% 
  sf::st_cast("MULTIPOLYGON")

# sf::st_write(sectorsPolygon, "./data_out/sectorsPolygon.gpkg")

# Create a multiline feature based on 'sectorsPolygon' id limits
sectorsPolygonLines <- sf::st_cast(sectorsPolygon, "MULTILINESTRING", 
                                   group_or_split = FALSE)

# sf::st_write(sectorsPolygonLines, "./data_out/sectorsPolygonLines.gpkg")

#------------
# Look for intersections between the 20 latitudinal-band 'sectorsPolygon' and
# the monitored beaches 'thisWorkStretches';
# Cut features and calculate each segment length to calculate effort later on
#------------

## Calculate intersection between 'thisWorkStretches' and 'sectorsPolygon'
newStretches <- 
  sf::st_intersection(thisWorkStretches, sectorsPolygon)

# Add a new id for each segment line, called "id_newStretches"
newStretches$id_newStretches <- 1:nrow(newStretches)

# mapview::mapview(newStretches) + sectorsPolygonLines

## Calculate individual segment length and set them 'as.numeric'
newStretches$new_length <- as.numeric(sf::st_length(newStretches))

# Set "new_length" and "original_length" in kilometers
newStretches <- 
  newStretches %>%
  dplyr::mutate(new_length = as.numeric(new_length/1000)) %>%
  dplyr::mutate(original_length = as.numeric(original_length/1000)) %>%
  dplyr::mutate_if(is.numeric, round, digits = 2)

## Clean 'newStretches' and rename some columns
newStretches <- 
  newStretches %>% 
  dplyr::select(id_original, 
                id_newStretches,
                id_polygon, 
                executing1, 
                beach,
                stretch, 
                beach_id, 
                stretch_st,
                new_length, 
                geometry) %>%
  dplyr::rename(stretch_scheme = stretch_st) %>%
  dplyr::rename(new_length_km = new_length) %>%
  sf::st_as_sf() %>% 
  sf::st_set_crs(4326)

##----------------------------------------------------------------------------##
##                   Join 'dfSpatial' and eff with 'newStretches'             ##
##----------------------------------------------------------------------------##

# Trasform crs
dfSpatial <- sf::st_transform(dfSpatial, crs = 4326)

# Join attributes from 'newStretches' into 'dfSpatial'
dfSpatialJoin <- sf::st_join(dfSpatial, newStretches, 
                             join = sf::st_nearest_feature)

# Return it into a data.frame format, clean and rename some columns
df <- 
  dfSpatialJoin %>% 
  as.data.frame() %>% 
  dplyr::rename(beach = beach.x) %>% 
  dplyr::rename(stretch = stretch.x) %>% 
  dplyr::rename(stretch_scheme = stretch_scheme.x) %>%
  dplyr::select(id_original, id_newStretches, id_polygon, 
                id_individual,class,family,species, state, beach, stretch, 
                beach_id, new_length_km, stretch_scheme, monitoring_type,
                lat, long,date, year, year_n, month, cod_decomposition,dead_live)

# Join attributes from 'newStretches' into 'eff'
effJoin <- merge(eff, newStretches, by= "beach_id", all = T)

# Clean, rename and reorganize the columns 
eff <- 
  effJoin %>% 
  dplyr::rename(beach = beach.x) %>% 
  dplyr::rename(stretch = stretch.x) %>% 
  dplyr::rename(stretch_scheme = stretch_scheme.x) %>% 
  dplyr::select(code, id_original, id_newStretches, id_polygon, executing1, 
                state, beach, stretch, beach_id, new_length_km, 
                stretch_scheme, type, initialLat, initialLong, complete, 
                date, year, month, year_n,vehicle) %>%
  dplyr::arrange(date)

##----------------------------------------------------------------------------##
##          Stranding records collected in complete-surveyed stretches        ##
##----------------------------------------------------------------------------##

##---------
## Removed records from 'df' (stranding data) based on incomplete surveys in 
## 'eff' (effort data) 
##---------

# Create 'eff_i' and "date_id_newStretches" column
eff_i <- 
  eff %>% 
  dplyr::filter(complete != "yes") %>% 
  dplyr::mutate(date_id_newStretches = paste(date, id_newStretches)) 

# Create "date_id_newStretches" column in 'df'
df <- 
  dplyr::mutate(df, date_id_newStretches = paste(date, id_newStretches)) 

# Remove strandings from 'df' records during incomplete beach surveys
df <- 
  dplyr::anti_join(df, eff_i, by = "date_id_newStretches") 

# Remove "date_id_newStretches" from 'df'
df <-
  dplyr::select(df,- date_id_newStretches)

# Remove records classified as mummified or skeletal remains (COD = 5)
df <-
  dplyr::filter(df, cod_decomposition != 5) # records removed 7,907

# Save the final stranding dataset used to the analysis

# write.csv2(df,"./data_out/dfFinal.csv")

##----------------------------------------------------------------------------##
##                            Complete survey effort                          ##
##----------------------------------------------------------------------------##

eff_c <-
  dplyr::filter(eff,complete != "no")

# Save final effort data

# write.csv2(eff_c,"./data_out/effFinal.csv") 

##----------------------------------------------------------------------------##
##         Transform to wide format and summarize by month and polygon        ##
##----------------------------------------------------------------------------##

##-------
## This section prepares data for GAMLSS models
##-------

# Summarizing and reshaping for a wide-format table
dfSum <- 
  df %>% 
  dplyr::group_by(year_n, month, id_polygon, class, species) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::mutate_at(c(1:3), as.factor) %>%
  tidyr::pivot_wider(names_from = c(species, class), 
                     values_from = n, values_fill = 0)

effSum <-
  eff_c %>%
  dplyr::group_by(year_n, month, id_polygon) %>% 
  dplyr::summarise(effort_km = sum(new_length_km)) %>%
  dplyr::mutate_at(c(1:3), as.factor)

dfSumJoin <-
  dplyr::left_join(effSum, dfSum, 
                   by = c("year_n", "month", "id_polygon")) %>% 
  replace(., is.na(.), 0) %>%
  dplyr::ungroup()

# Create the column total sum for each vertebrate group
dfTotalSum <- 
  dfSumJoin %>% 
  dplyr::mutate(total = rowSums(.[,c(5:129)])) %>% 
  dplyr::mutate(totalTurtles = 
                  rowSums(.[grep("Reptilia", names(.))])) %>% 
  dplyr::mutate(totalBirds = 
                  rowSums(.[grep("Aves", names(.))])) %>% 
  dplyr::mutate(totalMammals = 
                  rowSums(.[grep("Mammalia", names(.))])) %>%
  dplyr::select(year_n,month,id_polygon,total,totalTurtles, 
                totalBirds, totalMammals) 

# Create the column total sum for each vertebrate group considering only lowest
# numeric frequencies (NF) species (NF<1)
dfTurtleRareSum <-
  dfSumJoin %>%
  dplyr::select_at(vars(contains(c("year_n","id_polygon","month","Reptilia")))) %>%
  dplyr::select(-c("Chelonia mydas_Reptilia", "Caretta caretta_Reptilia", 
                   "Lepidochelys olivacea_Reptilia")) %>%
  dplyr::mutate(totalTurtlesRare = rowSums(.[,c(5:6)])) %>%
  dplyr::select(year_n,month,id_polygon,totalTurtlesRare)

dfBirdRareSum <-
  dfSumJoin %>%
  dplyr::select_at(vars(contains(c("year_n","id_polygon","month","Aves")))) %>%
  dplyr::select(-c("Spheniscus magellanicus_Aves", 
                   "Puffinus puffinus_Aves","Larus dominicanus_Aves", 
                   "Sula leucogaster_Aves","Thalassarche chlororhynchos_Aves",
                   "Procellaria aequinoctialis_Aves","Fregata magnificens_Aves", 
                   "Thalassarche melanophris_Aves")) %>%
  dplyr::select(contains(c("year_n","id_polygon","month"," "))) %>% # Select only species
  dplyr::mutate(totalBirdsRare = rowSums(.[,c(4:50)])) %>%
  dplyr::select(year_n,month,id_polygon,totalBirdsRare)

dfMammalRareSum <-
  dfSumJoin %>%
  dplyr::select_at(vars(contains(c("year_n","id_polygon","month","Mammalia")))) %>%
  dplyr::select(-c("Pontoporia blainvillei_Mammalia", 
                   "Tursiops truncatus_Mammalia","Sotalia guianensis_Mammalia",
                   "Stenella frontalis_Mammalia","Megaptera novaeangliae_Mammalia")) %>%
  dplyr::select(contains(c("year_n","id_polygon","month"," "))) %>% # Select only species
  dplyr::mutate(totalMammalsRare = rowSums(.[,c(4:20)])) %>%
  dplyr::select(year_n,month,id_polygon,totalMammalsRare)

# Final dataset for modelling 
dfSumFinal <- 
  dfSumJoin %>%
  dplyr::left_join(dfTotalSum, 
                   by = c( "year_n","month", "id_polygon")) %>%
  dplyr::left_join(dfTurtleRareSum, 
                   by = c( "year_n","month", "id_polygon")) %>%
  dplyr::left_join(dfBirdRareSum, 
                   by = c( "year_n","month", "id_polygon")) %>%
  dplyr::left_join(dfMammalRareSum, 
                   by = c( "year_n","month", "id_polygon")) %>%
  dplyr::rename_all(funs(str_replace_all(., " ", "_"))) %>% 
  dplyr::mutate(monthf = factor(month, levels = as.character(1:12), 
                                labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                                ordered = FALSE)) %>% 
  dplyr::relocate(monthf, .before = "month")

# write.csv2(dfSumFinal,"./data_out/dfSumFinal.csv")

################################################################################
################################# Data sets ####################################
################################################################################

## Open dataset
dfFinal <- read.csv2("./data_out/dfFinal.csv")
dfSumFinal <- read.csv2("./data_out/dfSumFinal.csv")
effFinal <- read.csv2("./data_out/effFinal.csv") 
validation <- read.csv2("./data_out/acuracySpeciesIdentificationDataset.csv")

## Open spatial features
brazil <- 
  sf::read_sf(dsn = "./data/shape_brazil", layer = "brasil") %>% 
  sf::st_set_crs(4326)

thisWorkStretches <-
  sf::read_sf("./data_out/thisWorkStretches.gpkg")

thisWorkStretches <- 
  sf::st_transform(thisWorkStretches, crs = 4326)

sectorsPolygonLines <-
  sf::read_sf("./data_out/sectorsPolygonLines.gpkg")

sectorsPolygon <-
  sf::read_sf("./data_out/sectorsPolygon.gpkg")

################################################################################
########################### Material and Methods ###############################
################################################################################
##----------------------------------------------------------------------------##
##                               Study area                                   ##
##--------------------------------Figure 1------------------------------------##
##----------------------------------------------------------------------------##

## First do the main plot  

# Latitudinal division of the study area 
lat_division <- 
  ggplot() + 
  ggspatial::geom_sf(data = brazil) + 
  geom_sf(data = thisWorkStretches, 
          color = "red", show.legend = F,size=1.3) + 
  geom_sf(data = sectorsPolygonLines, 
          color = "black", show.legend = F) + 
  labs(x = "", y = "") + 
  coord_sf(xlim = c(-51, -44), ylim = c(-28.5, -23)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.title = element_text(colour = "black", size = 18), 
        axis.text = element_text(colour = "black", size = 14), 
        axis.line = element_line(colour = "black")) + 
  ggspatial::annotation_scale(location = "br", width_hint = 0.3,
                              text_cex = 1,height = unit(0.2, "cm")) +
  ggspatial::annotation_north_arrow(location = "br", which_north = "true", 
                                    pad_x = unit(0.08, "in"), pad_y = unit(0.3, "in"),
                                    style = ggspatial::north_arrow_fancy_orienteering,
                                    height = unit(1.6, "cm"),width = unit(1.6, "cm")) + 
  annotate(geom = "text", x = -46, y = -25.77, label = "Atlantic Ocean", 
           color = "grey22", size = 5) + 
  annotate(geom = "text", x = -47.5, y = -23.10, label = "São Paulo (SP)", 
           color = "grey22", size = 5) + 
  annotate(geom = "text", x = -49.8, y = -25.5, label = "Paraná", 
           color = "grey22", size = 5) + 
  annotate(geom = "text", x = -50, y = -27.3, label = "Santa Catarina (SC)", 
           color = "grey22", size = 5) +
  geom_sf_label(data= sectorsPolygon, aes(label = id_polygon),nudge_x = 2,
                nudge_y = 0)

# Second, do the inset plot
brazil_inset <-  
  ggplot() + 
  ggspatial::geom_sf(data = sf::st_union(brazil),fill= "black") +
  coord_sf(xlim = c(-73, -35), ylim = c(-33, 5)) + 
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_rect(fill="gray"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) + 
  theme_void() +
  ggplot2::geom_rect(data = brazil, 
                     aes(xmin = -44,5, xmax = -50, ymin = -28.6, ymax = -23.4), 
                     colour = "dodgerblue", fill = "NA",size = 1.5) +
  annotate(geom = "text", x = -51, y = -12, label = "Brazil", 
           color = "white", size = 5) 

## Final map
figure1 <-
  cowplot::ggdraw() +
  cowplot::draw_plot(lat_division) +
  cowplot::draw_plot(brazil_inset, x = 0.124, y = 0.67, width = 0.25, height = 0.25)

ggplot2::ggsave(figure1, 
                filename = "./results/figure1.tiff", 
                height = 170 , width = 170, units = "mm", dpi = 400,
                compression = "lzw")

################################################################################
############################## General Results #################################
################################################################################
##----------------------------------------------------------------------------##
##                          SIMBA database accuracy                           ##
##----------------------------------------------------------------------------##

## Number of records reviewed (by group) to verify the accuracy of species 
## identification
View(
  validation %>%
    dplyr::group_by(Class) %>%
    dplyr::summarise(Total_validated = sum(Total_validated)))

## Identify only records classified to species levels
spIdent <- dfFinal[sapply(strsplit(as.character(dfFinal$species)," "), 
                          length) >= 2,]

## Calculate total percentage of species identified correctly 

# First get species name by classes
spIdent <-
  spIdent %>%
  dplyr::mutate(records = 1) %>%
  dplyr::group_by(species,class) %>%
  dplyr::summarise (total_records = sum(records))

# Join validation data by species
spIdent <-
  spIdent %>%
  dplyr::left_join(validation,by="species") %>%
  dplyr::select(class,species,`Species.validation..`,total_records) %>%
  # columns "n" and "identification" to calculate percentages later on 
  dplyr::mutate(n = 1,
                identification = ifelse(test = `Species.validation..` >= 90,
                                        yes=1,no=0)) 

# Six species did not have a validation value, so remove them 
spIdent_NA <-         
  spIdent %>%
  tidyr::drop_na()

# Overall SIMBA accuracy 
sum((spIdent_NA$identification)/sum(spIdent_NA$n))*100

## Calculate the percentage of species correctly identified by group

# Reptilia
spIdentReptilia <- 
  spIdent_NA %>% 
  dplyr::filter(class == "Reptilia")
sum(spIdentReptilia$identification)/sum(spIdentReptilia$n)*100

# Aves
spIdentAves <- 
  spIdent_NA %>% 
  dplyr::filter(class == "Aves")
sum(spIdentAves$identification)/sum(spIdentAves$n)*100

# Cetacea
spIdentCetacea <- 
  spIdent_NA %>% 
  dplyr::filter(class == "Mammalia")
sum(spIdentCetacea$identification)/sum(spIdentCetacea$n)*100

# Species identified incorrectly
View(
  spIdentAves %>%
    dplyr::filter( `Species.validation..` == 0)) 

## Number of species in which photos were not available, or did not allow
## species identification

# Reptilia
View( 
  spIdent %>% 
    dplyr::filter(class == "Reptilia") %>%
    dplyr::filter(is.na(identification)))

# Aves
View( 
  spIdent %>% 
    dplyr::filter(class == "Aves") %>%
    dplyr::filter(is.na(identification)))

# Cetacea
View( 
  spIdent %>% 
    dplyr::filter(class == "Mammalia") %>%
    dplyr::filter(is.na(identification)))

##----------------------------------------------------------------------------##
##                       Summary of effort and strandings                     ##
##----------------------------------------------------------------------------##

## Total cumulative survey distance covered
sum(effFinal$new_length_km) 

## Mean and strand deviation of total distance covered by polygon 
mean_distance <-
  effFinal %>%
  dplyr::group_by(id_polygon) %>%
  dplyr::summarise(distance = sum(new_length_km))

mean(mean_distance$distance)
sd(mean_distance$distance)

## Total number of strandings by group
totalSt <-
  dfFinal %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(record = n()) %>%
  dplyr::mutate(percentage = (record/sum(record))*100) %>%
  janitor::adorn_totals("row") %>%
  dplyr::mutate(percentage = round(percentage,digits = 2))

totalStr[4,2]

## Percentage of alive individuals from the total records by group
View( 
  dfFinal %>% 
    dplyr::group_by(class, cod_decomposition) %>%
    dplyr::summarise(record = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(percentage = (record/sum(record)*100)) %>%
    dplyr::filter(cod_decomposition == 1)) 

## Total number of species and family

# Identify only records classified to species levels
sp <- dfFinal[sapply(strsplit(as.character(dfFinal$species)," "), 
                     length) >= 2,] 

# Remove the four Aves species identified incorrectly
sp <-
  sp %>%
  dplyr::filter(species != "Pachyptila vittata" & 
                  species != "Larus atlanticus" & 
                  species != "Larus cirrocephalus" &
                  species != "Tringa solitaria")

View( 
  sp %>%
    dplyr::distinct(species, .keep_all = TRUE) %>%
    dplyr::group_by(class) %>%
    dplyr::summarise(n=n()) %>%
    janitor::adorn_totals("row"))

View(
  sp %>%
    dplyr::distinct(species, .keep_all = TRUE) %>%
    dplyr::group_by(class,family) %>%
    dplyr::summarise(n_sp=n()) %>%
    dplyr::group_by(class) %>%
    dplyr::summarise(n_family=n()) %>%
    janitor::adorn_totals("row"))

## Total number of the 14 selected species (numeric frequency > 1% and n>=100)
View(
  sp %>%
    dplyr::filter(species == "Chelonia mydas" | 
                    species == "Caretta caretta" |
                    species == "Lepidochelys olivacea" | 
                    species == "Spheniscus magellanicus" |
                    species == "Puffinus puffinus" | 
                    species == "Larus dominicanus" | 
                    species == "Sula leucogaster" |  
                    species == "Thalassarche chlororhynchos" | 
                    species == "Procellaria aequinoctialis" | 
                    species == "Fregata magnificens" | 
                    species == "Thalassarche melanophris" | 
                    species == "Pontoporia blainvillei" | 
                    species == "Tursiops truncatus" | 
                    species == "Sotalia guianensis") %>%
    dplyr::group_by(species) %>%
    dplyr::summarise(n=n()) %>%
    dplyr::arrange(dplyr::desc(n)))

## Frequency of occurrence see tableS1

## Stranding number of unidentified records

# Records not identified to species level
str_unidentified <- dfFinal[sapply(strsplit(as.character(dfFinal$species),
                                            " "), length) < 2,] 

str_unidentified <-
  str_unidentified %>%
  dplyr::group_by(species) %>%
  dplyr::summarise(n = n())

sum(str_unidentified$n)
(sum(str_unidentified$n)/totalStr[4,2])*100

##----------------------------------------------------------------------------##
##                   Species richness by month and polygon                    ##
##---------------------------------Figure 2-----------------------------------## 
##----------------------------------------------------------------------------##

## Identify only records classified to species level and remove the four 
## species identified incorrectly
sp <- dfFinal[sapply(strsplit(as.character(dfFinal$species)," "), 
                     length) >= 2,] %>%
  dplyr::filter(species != "Pachyptila vittata" & 
                  species != "Larus atlanticus" & 
                  species != "Larus cirrocephalus" &
                  species != "Tringa solitaria")

##
## Per month
##

sp_month<- 
  sp %>% 
  dplyr::group_by(month,class) %>% 
  dplyr::summarise(n_species = n_distinct(species))%>%
  dplyr::mutate(class=ifelse(class=="Mammalia","Cetacea", class ) )

# Reorder the levels 
sp_month$class<- factor(sp_month$class, 
                        levels=c('Reptilia','Aves','Cetacea'))

sp_month_plot<-
  ggplot2::ggplot(sp_month,aes(x = as.factor(month) , y =n_species)) +
  geom_bar(stat = "identity") + guides(fill = guide_legend(reverse = F)) + 
  xlab("Month") + ylab("Number of species") +
  facet_wrap(facets = "class", nrow = 1,scales="free_y")+ 
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8)) +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  theme(axis.title.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12))+
  theme(strip.text.x = element_text(size=20, face="bold.italic")) +
  scale_y_continuous(n.breaks=9)


##
## Per polygon
##

sp_poly<- 
  sp %>% 
  dplyr::group_by(id_polygon,class) %>% 
  dplyr::summarise(n_species = n_distinct(species)) %>%
  dplyr::mutate(class=ifelse(class=="Mammalia","Cetacea", class ) ) 

## Reorder the levels 
sp_poly$class<- factor(sp_poly$class, 
                       levels=c('Reptilia','Aves','Cetacea'))

sp_poly_plot <-
  ggplot2::ggplot(sp_poly,aes(x = factor(id_polygon), y =n_species)) +
  geom_bar(stat = "identity") + 
  guides(fill = guide_legend(reverse = F)) + 
  xlab("Polygon") + ylab("Number of species") +
  facet_wrap(facets = "class", nrow = 1,scales="free_x")+ 
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  coord_flip() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)) +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))+
  theme(strip.background = element_blank(), strip.text = element_blank())+
  scale_y_continuous(n.breaks=9)

figure2 <- 
  gridExtra::grid.arrange(sp_month_plot,sp_poly_plot,ncol = 1)

ggplot2::ggsave(figure2, 
                filename = "./results/figure2.tiff", 
                height = 170 , width = 170, units = "mm", dpi = 400,
                compression = "lzw")

##----------------------------------------------------------------------------##
##                    Stranding rate by polygon and month                     ##
##--------------------------------Figure 3------------------------------------## 
##----------------------------------------------------------------------------##

## Remove the four species identified incorrectly
dfSumFinal_Remove <-
  dfSumFinal %>%
  dplyr::select(-c(Pachyptila_vittata_Aves, 
                   Larus_atlanticus_Aves, 
                   Larus_cirrocephalus_Aves,
                   Tringa_solitaria_Aves))

##
## Per Month
##

# Considering all records
total_month <- 
  dfSumFinal_Remove %>% 
  dplyr::select(month, effort_km,
                total,Reptilia=totalTurtles,Aves=totalBirds,Cetacea=totalMammals)%>% 
  dplyr::group_by(month) %>% 
  dplyr::summarise_at(vars(contains(c("effort","Reptilia","Aves","Cetacea"))),sum) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate_at(vars(contains(c("Reptilia","Aves","Cetacea"))), 
                   function(i) round(((i*1000)/.$effort_km), digits = 4)) %>% 
  tidyr::pivot_longer(cols = contains(c("Reptilia","Aves","Cetacea")),
                      names_to = "taxon", 
                      values_to = "str_rate")

# Considering only the low numeric infrequent (NF) species (NF<1)
total_rare_month<- 
  dfSumFinal_Remove %>% 
  dplyr::select(month, effort_km,Reptilia=totalTurtlesRare,Aves=totalBirdsRare,
                Cetacea=totalMammalsRare) %>% 
  dplyr::group_by(month) %>% 
  dplyr::summarise_at(vars(contains(c("effort","Reptilia","Aves","Cetacea")))
                      ,sum) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate_at(vars(contains(c("Reptilia","Aves","Cetacea"))), 
                   function(i) round(((i*1000)/.$effort_km), digits = 4)) %>%
  tidyr::pivot_longer(cols = contains(c("Reptilia","Aves","Cetacea")),
                      names_to = "taxon", 
                      values_to = "str_rateRare") %>%
  dplyr::select(-effort_km)

Join_month <- 
  total_month %>%
  dplyr::left_join(total_rare_month,by = c("month","taxon")) 

# Reorder the levels 
Join_month$taxon <- factor(Join_month$taxon, 
                           levels=c('Reptilia','Aves','Cetacea'))

# Value used to scale the data for the second axis when plotting 
coeff <- 20

# Plot
month_plot <-
  ggplot2::ggplot() +
  geom_bar(Join_month,mapping = aes(x = as.factor(month),  y= str_rate),stat = "identity",group=1) +
  geom_line(Join_month,mapping = aes(x = as.factor(month),y= str_rateRare*coeff,group=1),
            size = 0.8,col = "red") +  
  facet_wrap(facets = "taxon", nrow = 1, scales = "free") +
  scale_y_continuous(name = "Stranding rate (ind./1000 km) of total records",
                     sec.axis = sec_axis( trans=~./coeff, 
                                          name="Stranding rate (ind./1000 km) of low NF species")) +
  guides(fill = guide_legend(reverse = F)) +
  xlab("Month") +
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8)) +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10))+
  theme(strip.text.x = element_text(size=20, face="bold.italic"))

##
## Per polygon
##

# Considering all records
total_poly<- 
  dfSumFinal_Remove %>% 
  dplyr::select(id_polygon, effort_km,
                total,Reptilia=totalTurtles,Aves=totalBirds,Cetacea=totalMammals) %>% 
  dplyr::group_by(id_polygon) %>% 
  dplyr::summarise_at(vars(contains(c("effort","Reptilia","Aves","Cetacea"))),sum) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate_at(vars(contains(c("Reptilia","Aves","Cetacea"))), 
                   function(i) round(((i*1000)/.$effort_km), digits = 4)) %>% 
  tidyr::pivot_longer(cols = contains(c("Reptilia","Aves","Cetacea")),
                      names_to = "taxon", 
                      values_to = "str_rate") 

# Considering only the lowest numeric frequencies (NF) species (NF<1)
total_rare_poly<- 
  dfSumFinal_Remove %>% 
  dplyr::select(id_polygon,effort_km,Reptilia=totalTurtlesRare,Aves=totalBirdsRare,
                Cetacea=totalMammalsRare) %>% 
  dplyr::group_by(id_polygon) %>% 
  dplyr::summarise_at(vars(contains(c("effort","Reptilia","Aves","Cetacea")))
                      ,sum) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate_at(vars(contains(c("Reptilia","Aves","Cetacea"))), 
                   function(i) round(((i*1000)/.$effort_km), digits = 4)) %>%
  tidyr::pivot_longer(cols = contains(c("Reptilia","Aves","Cetacea")),
                      names_to = "taxon", 
                      values_to = "str_rateRare") %>%
  dplyr::select(-effort_km)

Join_poly <- 
  total_poly %>%
  dplyr::left_join(total_rare_poly,by = c("id_polygon","taxon")) %>%
  dplyr::mutate(id_polygon = as.numeric(id_polygon))

# Reorder the levels  
Join_poly$taxon <- factor(Join_poly$taxon, 
                          levels=c('Reptilia','Aves','Cetacea')) 


# Plot
poly_plot <-
  ggplot2::ggplot() +
  geom_bar(Join_poly,mapping = aes(x = id_polygon,  y= str_rate),stat = "identity") +
  geom_line(Join_poly,mapping = aes(x = id_polygon,y= str_rateRare*coeff),
            size = 0.8,col = "red") +  
  facet_wrap(facets = "taxon", nrow = 1, scales = "free_x") +
  scale_y_continuous(name = "Stranding rate (ind./1000 km) of total records",
                     sec.axis = sec_axis( trans=~./coeff, 
                                          name="Stranding rate (ind./1000 km) of low NF species")) +
  scale_x_continuous("Polygon", breaks = 1:20) +
  coord_flip() +
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8)) +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10)) +
  theme(strip.background = element_blank(), strip.text = element_blank())

figure3 <- 
  gridExtra::grid.arrange(month_plot,poly_plot, ncol = 1)

ggplot2::ggsave(figure3, 
                filename = "./results/figure3.tiff", 
                height = 170 , width = 170, units = "mm", dpi = 400,
                compression = "lzw")


##----------------------------------------------------------------------------##
##                  Stranding proportion by month and polygon                 ##
##-------------------------------Figure S1 and S2-----------------------------## 
##----------------------------------------------------------------------------##

## Filter only the 14 selected species
str_proportion <- 
  dfSumFinal %>%
  dplyr::select(month,id_polygon,effort_km, 
                Chelonia_mydas_Reptilia, Caretta_caretta_Reptilia, 
                Lepidochelys_olivacea_Reptilia, Spheniscus_magellanicus_Aves, 
                Puffinus_puffinus_Aves, Larus_dominicanus_Aves, 
                Sula_leucogaster_Aves, Thalassarche_chlororhynchos_Aves, 
                Procellaria_aequinoctialis_Aves, Fregata_magnificens_Aves, 
                Thalassarche_melanophris_Aves, Pontoporia_blainvillei_Mammalia, 
                Tursiops_truncatus_Mammalia, Sotalia_guianensis_Mammalia) 

## Identify migratory Aves species
names(str_proportion) <- c("month","id_polygon", "effort_km", 
                           "Chelonia_mydas_Reptilia", "Caretta_caretta_Reptilia", 
                           "Lepidochelys_olivacea_Reptilia", 
                           "Spheniscus_magellanicus_Aves_Migratory", 
                           "Puffinus_puffinus_Aves_Migratory", "Larus_dominicanus_Aves", 
                           "Sula_leucogaster_Aves", 
                           "Thalassarche_chlororhynchos_Aves_Migratory", 
                           "Procellaria_aequinoctialis_Aves_Migratory",
                           "Fregata_magnificens_Aves", 
                           "Thalassarche_melanophris_Aves_Migratory", 
                           "Pontoporia_blainvillei_Mammalia", 
                           "Tursiops_truncatus_Mammalia", "Sotalia_guianensis_Mammalia") 

##
## Per month
##

# Calculating the proportion of stranding rates by month
str_proportion_month <- 
  str_proportion %>%
  dplyr::group_by(month) %>% 
  dplyr::summarise_at(vars(contains(c("effort", "Mammalia", "Aves", "Reptilia"))), 
                      sum) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate_at(vars(contains(c("Mammalia", "Aves", "Reptilia"))), 
                   function(i) round(((i*1000)/.$effort_km), digits = 4)) %>%
  dplyr::mutate_at(vars(contains(c("Mammalia", "Aves", "Reptilia"))),
                   function(i) round(((i)/sum(i)), digits = 6)) %>% 
  tidyr::pivot_longer(cols = contains(c("Mammalia", "Aves", "Reptilia")),
                      names_to = "species", 
                      values_to = "str_proportion") %>%
  dplyr::mutate(class = 
                  dplyr::case_when(
                    endsWith(species,"Mammalia") ~ "Cetacea",
                    endsWith(species,"Reptilia") ~ "Reptilia",
                    endsWith(species,"Aves") ~ "Aves: non-migratory",
                    endsWith(species,"Migratory") ~ "Aves: migratory")) %>%
  dplyr::mutate(species = stringr::str_remove_all (species,"_Mammalia")) %>%
  dplyr::mutate(species = stringr::str_remove_all(species,"_Aves")) %>%
  dplyr::mutate(species = stringr::str_remove_all(species,"_Reptilia")) %>%
  dplyr::mutate(species = stringr::str_replace(species, "_", " "))

# Split data for each "class"
aves_migratory_prop_month <-
  str_proportion_month %>%
  dplyr::filter(class == "Aves: migratory") %>%
  dplyr::mutate(species = stringr::str_remove_all(species,"_Migratory")) %>%
  ggplot(aes(x=as.factor(month), y=str_proportion, group=species)) +
  geom_line(aes(colour = species)) + 
  ggtitle("Aves: migratory") +
  theme(plot.title=element_text(size=rel(1.5), lineheight=.9, family="Times",
                                face="bold.italic", colour="black")) +
  xlab("") + ylab("") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(legend.position="bottom") +
  guides(colour = guide_legend(nrow=3)) +
  theme(legend.background=element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(1, "mm"),
        legend.spacing.y = unit(1, "mm")) +
  theme(legend.text = element_text(face="italic",size = 6)) +
  theme(legend.text = element_text(face="italic")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA)) + 
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10))+
  theme(strip.text.x = element_text(size=18, face="italic"))

aves_prop_month <-
  str_proportion_month %>%
  dplyr::filter(class == "Aves: non-migratory") %>%
  ggplot(aes(x=as.factor(month), y=str_proportion, group=species)) +
  geom_line(aes(colour = species)) + 
  xlab("") + ylab("Proportion of stranding rate") +
  ggtitle("Aves: resident") +
  theme(plot.title=element_text(size=rel(1.5), lineheight=.9, family="Times",
                                face="bold.italic", colour="black")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(legend.position="bottom") +
  guides(colour = guide_legend(nrow=2)) +
  theme(legend.background=element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(1, "mm"),
        legend.spacing.y = unit(1, "mm")) +
  theme(legend.text = element_text(face="italic",size = 6)) +
  theme(legend.text = element_text(face="italic")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA)) + 
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10))+
  theme(strip.text.x = element_text(size=18, face="italic"))

turtle_prop_month <-
  str_proportion_month %>%
  dplyr::filter(class == "Reptilia") %>%
  ggplot(aes(x=as.factor(month), y=str_proportion, group=species)) +
  geom_line(aes(colour = species)) + 
  ggtitle("Reptilia") +
  theme(plot.title=element_text(size=rel(1.5), lineheight=.9, family="Times",
                                face="bold.italic", colour="black")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(legend.position="bottom") +
  guides(colour = guide_legend(nrow=2)) +
  theme(legend.background=element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(1, "mm"),
        legend.spacing.y = unit(1, "mm")) +
  theme(legend.text = element_text(face="italic",size = 6)) +
  theme(legend.text = element_text(face="italic")) +
  theme(legend.title = element_blank()) +
  xlab("Month") + ylab("Proportion of stranding rate") +
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA)) + 
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10))+
  theme(strip.text.x = element_text(size=18, face="italic"))

cetacea_prop_month <-
  str_proportion_month %>%
  dplyr::filter(class == "Cetacea") %>%
  ggplot(aes(x=as.factor(month), y=str_proportion, group=species)) +
  geom_line(aes(colour = species)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  ggtitle("Cetacea") +
  theme(plot.title=element_text(size=rel(1.5), lineheight=.9, family="Times",
                                face="bold.italic", colour="black")) +
  xlab("Month") + ylab("") +
  theme(legend.position="bottom") +
  guides(colour = guide_legend(nrow=2)) +
  theme(legend.background=element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(1, "mm"),
        legend.spacing.y = unit(1, "mm")) +
  theme(legend.text = element_text(face="italic",size = 6)) +
  theme(legend.text = element_text(face="italic")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA)) + 
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10))+
  theme(strip.text.x = element_text(size=18, face="italic"))

##
## Per polygon
##

# Calculating the proportion of stranding rates by polygon
str_proportion_poly <- 
  str_proportion %>%
  dplyr::group_by(id_polygon) %>% 
  dplyr::summarise_at(vars(contains(c("effort", "Mammalia", "Aves", "Reptilia"))), 
                      sum) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate_at(vars(contains(c("Mammalia", "Aves", "Reptilia"))), 
                   function(i) round(((i*1000)/.$effort_km), digits = 4)) %>%
  dplyr::mutate_at(vars(contains(c("Mammalia", "Aves", "Reptilia"))),
                   function(i) round(((i)/sum(i)), digits = 4)) %>% 
  tidyr::pivot_longer(cols = contains(c("Mammalia", "Aves", "Reptilia")),
                      names_to = "species", 
                      values_to = "str_proportion") %>%
  dplyr::mutate(class = 
                  dplyr::case_when(
                    endsWith(species,"Mammalia") ~ "Cetacea",
                    endsWith(species,"Reptilia") ~ "Reptilia",
                    endsWith(species,"Aves") ~ "Aves: non-migratory",
                    endsWith(species,"Migratory") ~ "Aves: migratory")) %>%
  dplyr::mutate(species = stringr::str_remove_all(species,"_Mammalia")) %>%
  dplyr::mutate(species = stringr::str_remove_all(species,"_Aves")) %>%
  dplyr::mutate(species = stringr::str_remove_all(species,"_Reptilia")) %>%
  dplyr::mutate(species = stringr::str_replace(species, "_", " "))

# Split data for each "class"
aves_migratory_prop_poly <-
  str_proportion_poly %>%
  dplyr::filter(class == "Aves: migratory") %>%
  dplyr::mutate(species = stringr::str_remove_all(species,"_Migratory")) %>%
  ggplot(aes(x=id_polygon, y=str_proportion, group=species)) +
  geom_line(aes(colour = species)) + 
  ggtitle("Aves: migratory") +
  theme(plot.title=element_text(size=rel(1.), lineheight=.9, family="Times",
                                face="bold.italic", colour="black")) +
  xlab("") + ylab("") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(legend.position="bottom") +
  guides(colour = guide_legend(nrow=3)) +
  theme(legend.background=element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(1, "mm"),
        legend.spacing.y = unit(1, "mm")) +
  theme(legend.text = element_text(face="italic",size = 6)) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA)) + 
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10))+
  theme(strip.text.x = element_text(size=18, face="italic")) +
  scale_x_continuous(n.breaks = 20) 

aves_prop_poly <-
  str_proportion_poly %>%
  dplyr::filter(class == "Aves: non-migratory") %>%
  ggplot(aes(x=id_polygon, y=str_proportion, group=species)) +
  geom_line(aes(colour = species)) + 
  xlab("") + ylab("Proportion of stranding rate") +
  ggtitle("Aves: resident") +
  theme(plot.title=element_text(size=rel(1), lineheight=.9, family="Times",
                                face="bold.italic", colour="black")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(legend.position="bottom") +
  guides(colour = guide_legend(nrow=2)) +
  theme(legend.background=element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(1, "mm"),
        legend.spacing.y = unit(1, "mm")) +
  theme(legend.text = element_text(face="italic",size = 6)) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA)) + 
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10))+
  theme(strip.text.x = element_text(size=18, face="italic")) +
  scale_x_continuous(n.breaks = 20)

turtle_prop_poly <-
  str_proportion_poly %>%
  dplyr::filter(class == "Reptilia") %>%
  ggplot(aes(x=id_polygon, y=str_proportion, group=species)) +
  geom_line(aes(colour = species)) + 
  ggtitle("Reptilia") +
  theme(plot.title=element_text(size=rel(1.), lineheight=.9, family="Times",
                                face="bold.italic", colour="black")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(legend.position="bottom") +
  guides(colour = guide_legend(nrow=2)) +
  theme(legend.background=element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(1, "mm"),
        legend.spacing.y = unit(1, "mm")) +
  theme(legend.text = element_text(face="italic",size = 6)) +
  theme(legend.title = element_blank()) +
  xlab("Polygon") + ylab("Proportion of stranding rate") +
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA)) + 
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10))+
  theme(strip.text.x = element_text(size=18, face="italic")) +
  scale_x_continuous(n.breaks = 20)

cetacea_prop_poly <-
  str_proportion_poly %>%
  dplyr::filter(class == "Cetacea") %>%
  ggplot(aes(x=id_polygon, y=str_proportion, group=species)) +
  geom_line(aes(colour = species)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  ggtitle("Cetacea") +
  theme(plot.title=element_text(size=rel(1.), lineheight=.9, family="Times",
                                face="bold.italic", colour="black")) +
  xlab("Polygon") + ylab("") +
  theme(legend.position="bottom") +
  guides(colour = guide_legend(nrow=2)) +
  theme(legend.background=element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(1, "mm"),
        legend.spacing.y = unit(1, "mm")) +
  theme(legend.text = element_text(face="italic",size = 6)) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA)) + 
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10))+
  theme(strip.text.x = element_text(size=18, face="italic")) +
  scale_x_continuous(n.breaks = 20)

figureS1 <- 
  gridExtra::grid.arrange(aves_prop_month,aves_migratory_prop_month,
                          turtle_prop_month,cetacea_prop_month,ncol = 2)

figureS2 <- 
  gridExtra::grid.arrange(aves_prop_poly,aves_migratory_prop_poly,
                          turtle_prop_poly,cetacea_prop_poly,ncol = 2)

ggplot2::ggsave(figureS1, 
                filename = "./results/figureS1.tiff", 
                height = 170 , width = 170, units = "mm", dpi = 400,
                compression = "lzw")

ggplot2::ggsave(figureS2, 
                filename = "./results/figureS2.tiff", 
                height = 170 , width = 170, units = "mm", dpi = 400,
                compression = "lzw")

##----------------------------------------------------------------------------##
##         Annual stranding rate of frequent and occasional species           ##
##---------------------------------Figure S3----------------------------------## 
##----------------------------------------------------------------------------##

## Filter the 14 selected species and obtain the annual stranding rate
str_rate_annual <- 
  dfSumFinal %>%
  dplyr::select(year_n, effort_km, 
                Chelonia_mydas_Reptilia, Caretta_caretta_Reptilia, 
                Lepidochelys_olivacea_Reptilia, Spheniscus_magellanicus_Aves, 
                Puffinus_puffinus_Aves, Larus_dominicanus_Aves, 
                Sula_leucogaster_Aves, Thalassarche_chlororhynchos_Aves, 
                Procellaria_aequinoctialis_Aves, Fregata_magnificens_Aves, 
                Thalassarche_melanophris_Aves, Pontoporia_blainvillei_Mammalia, 
                Tursiops_truncatus_Mammalia, Sotalia_guianensis_Mammalia,
                Stenella_frontalis_Mammalia,Megaptera_novaeangliae_Mammalia) %>% 
  dplyr::group_by(year_n) %>% 
  dplyr::summarise_at(vars(contains(c("effort", "Mammalia", "Aves", "Reptilia"))), 
                      sum) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate_at(vars(contains(c("Mammalia", "Aves", "Reptilia"))), 
                   function(i) round(((i*1000)/.$effort_km), digits = 4)) %>% 
  tidyr::pivot_longer(cols = contains(c("Mammalia", "Aves", "Reptilia")),
                      names_to = "species", 
                      values_to = "str_rate")%>%
  dplyr::mutate(species = stringr::str_remove_all(species,"_Mammalia")) %>%
  dplyr::mutate(species = stringr::str_remove_all(species,"_Aves")) %>%
  dplyr::mutate(species = stringr::str_remove_all(species,"_Reptilia")) %>%
  dplyr::mutate(species = stringr::str_replace(species, "_", " ")) 

# Reorder the levels
str_rate_annual$species <- factor(str_rate_annual$species, 
                                  levels=c("Chelonia mydas", "Caretta caretta", 
                                           "Lepidochelys olivacea", "Spheniscus magellanicus", 
                                           "Puffinus puffinus", "Larus dominicanus", 
                                           "Sula leucogaster", "Thalassarche chlororhynchos", 
                                           "Procellaria aequinoctialis", "Fregata magnificens", 
                                           "Thalassarche melanophris", "Pontoporia blainvillei", 
                                           "Tursiops truncatus", "Sotalia guianensis",
                                           "Stenella frontalis", "Megaptera novaeangliae"))

# Rename "year_n" levels
str_rate_annual$year_n <- as.factor(str_rate_annual$year_n)
levels(str_rate_annual$year_n) <- list("1" = "Year1", 
                                       "2" = "Year2", 
                                       "3" = "Year3", 
                                       "4" = "Year4",
                                       "5" = "Year5")

figureS3 <-
  ggplot2::ggplot(str_rate_annual, 
                  aes(x = year_n, y = str_rate)) + 
  geom_line(group = 1)+
  xlab("Year") + ylab("Stranding rate (ind./1000 km)") + 
  theme(legend.title = element_blank()) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) + 
  facet_wrap(facets = "species", nrow = 4, scales = "free_y") + 
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 8)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA)) + 
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10))+
  theme(strip.text.x = element_text(size=5.8, face="italic"))

ggplot2::ggsave(figureS3, 
                filename = "./results/figureS3.tiff", 
                height = 170 , width = 170, units = "mm", dpi = 400,
                compression = "lzw")

##----------------------------------------------------------------------------##
##          Stranding rate of high frequent species by month and polygon      ##
##------------------------------Figure S4 and S5 -----------------------------## 
##----------------------------------------------------------------------------##

##
## Per month
##

# Filter 14 selected species and obtain the stranding rates by 
# month for each year
str_rate_month <- 
  dfSumFinal %>%
  dplyr::select(month, year_n, effort_km, 
                Chelonia_mydas_Reptilia, Caretta_caretta_Reptilia, 
                Lepidochelys_olivacea_Reptilia, Spheniscus_magellanicus_Aves, 
                Puffinus_puffinus_Aves, Larus_dominicanus_Aves, 
                Sula_leucogaster_Aves, Thalassarche_chlororhynchos_Aves, 
                Procellaria_aequinoctialis_Aves, Fregata_magnificens_Aves, 
                Thalassarche_melanophris_Aves, Pontoporia_blainvillei_Mammalia, 
                Tursiops_truncatus_Mammalia, Sotalia_guianensis_Mammalia,
                Stenella_frontalis_Mammalia,Megaptera_novaeangliae_Mammalia) %>% 
  dplyr::group_by(month, year_n) %>% 
  dplyr::summarise_at(vars(contains(c("effort", "Mammalia", "Aves", "Reptilia"))), 
                      sum) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate_at(vars(contains(c("Mammalia", "Aves", "Reptilia"))), 
                   function(i) round(((i*1000)/.$effort_km), digits = 4)) %>% 
  tidyr::pivot_longer(cols = contains(c("Mammalia", "Aves", "Reptilia")),
                      names_to = "species", 
                      values_to = "str_rate")%>%
  dplyr::mutate(species = stringr::str_remove_all(species,"_Mammalia")) %>%
  dplyr::mutate(species = stringr::str_remove_all(species,"_Aves")) %>%
  dplyr::mutate(species = stringr::str_remove_all(species,"_Reptilia")) %>%
  dplyr::mutate(species = stringr::str_replace(species, "_", " ")) 

# Reorder the levels
str_rate_month$species <- factor(str_rate_month$species, 
                                 levels=c("Chelonia mydas", "Caretta caretta", 
                                          "Lepidochelys olivacea", "Spheniscus magellanicus", 
                                          "Puffinus puffinus", "Larus dominicanus", 
                                          "Sula leucogaster", "Thalassarche chlororhynchos", 
                                          "Procellaria aequinoctialis", "Fregata magnificens", 
                                          "Thalassarche melanophris", "Pontoporia blainvillei", 
                                          "Tursiops truncatus", "Sotalia guianensis",
                                          "Stenella frontalis", "Megaptera novaeangliae"))

##
## Per polygon
##

# Filter 14 selected species and obtain the stranding rates by 
# polygon for each year
str_rate_poly <- 
  dfSumFinal %>% 
  dplyr::select(id_polygon, year_n, effort_km, 
                Chelonia_mydas_Reptilia, Caretta_caretta_Reptilia, 
                Lepidochelys_olivacea_Reptilia, Spheniscus_magellanicus_Aves, 
                Puffinus_puffinus_Aves, Larus_dominicanus_Aves, 
                Sula_leucogaster_Aves, Thalassarche_chlororhynchos_Aves, 
                Procellaria_aequinoctialis_Aves, Fregata_magnificens_Aves, 
                Thalassarche_melanophris_Aves, Pontoporia_blainvillei_Mammalia, 
                Tursiops_truncatus_Mammalia, Sotalia_guianensis_Mammalia,
                Stenella_frontalis_Mammalia,Megaptera_novaeangliae_Mammalia) %>% 
  dplyr::group_by(id_polygon, year_n) %>% 
  dplyr::summarise_at(vars(contains(c("effort", "Mammalia", "Aves", "Reptilia"))), 
                      sum) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate_at(vars(contains(c("Mammalia", "Aves", "Reptilia"))), 
                   function(i) round(((i*1000)/.$effort_km), digits = 4)) %>% 
  tidyr::pivot_longer(cols = contains(c("Mammalia", "Aves", "Reptilia")),
                      names_to = "species", 
                      values_to = "str_rate") %>%
  dplyr::mutate(species = stringr::str_remove_all(species,"_Mammalia"))%>%
  dplyr::mutate(species = stringr::str_remove_all(species,"_Aves"))%>%
  dplyr::mutate(species = stringr::str_remove_all(species,"_Reptilia"))%>%
  dplyr::mutate(species = stringr::str_replace(species, "_", " "))

# Reorder the levels
str_rate_poly$species <- factor(str_rate_poly$species, 
                                levels=c("Chelonia mydas", "Caretta caretta", 
                                         "Lepidochelys olivacea", "Spheniscus magellanicus", 
                                         "Puffinus puffinus", "Larus dominicanus", 
                                         "Sula leucogaster", "Thalassarche chlororhynchos", 
                                         "Procellaria aequinoctialis", "Fregata magnificens", 
                                         "Thalassarche melanophris", "Pontoporia blainvillei", 
                                         "Tursiops truncatus", "Sotalia guianensis",
                                         "Stenella frontalis", "Megaptera novaeangliae"))


figureS4 <-
  ggplot2::ggplot(str_rate_month, 
                  aes(x = as.factor(month), y = str_rate, group = year_n)) + 
  geom_line(aes(color = year_n))+
  xlab("Month") + ylab("Stranding rate (ind./1000 km)") + 
  theme(legend.position = "bottom",legend.key.size = unit(1, "cm"),
        legend.key.height = unit(1,"cm"),
        legend.text=element_text(size=10)) +
  theme(legend.title = element_blank()) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) + 
  facet_wrap(facets = "species", nrow = 4, scales = "free_y") + 
  theme(axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 8)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA)) + 
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10))+
  theme(strip.text.x = element_text(size=6, face="italic"))


figureS5 <-
  ggplot2::ggplot(str_rate_poly,
                  aes(x = id_polygon, y = str_rate, group = year_n)) + 
  geom_line(aes(color = year_n))+ 
  xlab("Polygon") + ylab("Stranding rate (ind./1000 km)") + 
  theme(legend.position = "bottom",legend.key.size = unit(1, "cm"),
        legend.key.height = unit(1,"cm"),
        legend.text=element_text(size=10)) +
  theme(legend.title = element_blank()) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) + 
  facet_wrap(facets = "species", nrow = 4, scales = "free_y") + 
  theme(axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 8)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA)) + 
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10))+
  theme(strip.text.x = element_text(size=6, face="italic"))+
  scale_x_continuous(n.breaks = 10)


ggplot2::ggsave(figureS4, 
                filename = "./results/figureS4.tiff", 
                height = 170 , width = 170, units = "mm", dpi = 400,
                compression = "lzw")

ggplot2::ggsave(figureS5, 
                filename = "./results/figureS5.tiff", 
                height = 170 , width = 170, units = "mm", dpi = 400,
                compression = "lzw")

##----------------------------------------------------------------------------##
##                   Number of strandings by vertebrate group                 ##
##------------------------------------Table S1--------------------------------##
##----------------------------------------------------------------------------##

# Stranding number by year: alive and dead
step1 <- 
  dfFinal %>% 
  dplyr::mutate(year_condition = paste(year_n, dead_live)) %>% 
  dplyr::group_by(species, class, year_condition) %>% 
  dplyr::summarise(record = n()) %>%
  tidyr::pivot_wider(names_from = year_condition, values_from = record) %>%
  replace(is.na(.), 0) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Total = rowSums(.[,c(3:12)]))

# Annual stranding rate for each taxa (ind./1000 km)
step2 <- 
  dfFinal %>% 
  dplyr::group_by(species, year_n) %>% 
  dplyr::summarise(record = n()) %>% 
  tidyr::pivot_wider(names_from = year_n, values_from = record) %>% 
  dplyr::mutate(rate_Year1 = Year1*1000/sum(subset(effFinal, 
                                                   effFinal$year_n=="Year1")$new_length_km)) %>% 
  dplyr::mutate(rate_Year2 = Year2*1000/sum(subset(effFinal, 
                                                   effFinal$year_n=="Year2")$new_length_km)) %>% 
  dplyr::mutate(rate_Year3 = Year3*1000/sum(subset(effFinal, 
                                                   effFinal$year_n=="Year3")$new_length_km)) %>% 
  dplyr::mutate(rate_Year4 = Year4*1000/sum(subset(effFinal, 
                                                   effFinal$year_n=="Year4")$new_length_km)) %>% 
  dplyr::mutate(rate_Year5 = Year5*1000/sum(subset(effFinal, 
                                                   effFinal$year_n=="Year5")$new_length_km)) %>% 
  replace(is.na(.), 0)

# Mean annual stranding rate
step2$mean_rate <- round(rowMeans(step2[,7:11]),digits = 4)
step2 <- 
  step2 %>% 
  dplyr::select(species, mean_rate) 

# Numeric frequency (NF)
taxon<-c("Mammalia","Aves","Reptilia")

fn<-list()
for(i in 1:3){
  fn[[i]]<-
    dfFinal %>%
    dplyr::filter(class == taxon[i]) %>% 
    dplyr::group_by(species) %>% 
    dplyr::summarise(n = n())%>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(fn = round((n/sum(n))*100, digits = 2)) %>% 
    dplyr::arrange(desc(fn))
}

step3 <- plyr::ldply (fn, data.frame) %>%
  dplyr::select(-c(n)) %>%
  dplyr::rename(`NF %`=fn)

# Frequency of occurrence (OF)
dallySurvey <-
  effFinal %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(n = 1)

dallySurvey <- sum(dallySurvey$n) # dally monitoring survey across the study area

step4 <-
  dfFinal %>%
  dplyr::group_by(species) %>%
  dplyr::summarise(n = unique(date)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(species) %>%
  dplyr::summarise(ocurrence = n()) %>%
  dplyr::mutate(`FO %` = round((ocurrence/dallySurvey*100),digits = 2)) %>%
  dplyr::select(species,`FO %`)

# Species validation and IUCN redlist
step5 <- 
  validation %>%
  as.data.frame() %>%
  dplyr::rename(species=Species) %>%
  dplyr::mutate("Species validation %"= round(as.numeric(Revalidation),digits = 1),
                Revalidation=NULL) %>%
  dplyr::mutate(`No photography available %`= round(as.numeric(NA_percentage),digits = 1)) %>%
  dplyr::select(-c(NA_percentage,NA_records,OBS))

# Change the species names follow IUCN redlist
step5 <-
  step5 %>%
  dplyr::mutate(species = ifelse(species == "Calonectris diomedea","Calonectris borealis",
                                 ifelse(species == "Puffinus gravis","Ardenna gravis",
                                        ifelse(species == "Puffinus griseus","Ardenna grisea",
                                               ifelse(species == "Thalasseus acuflavidus", "Thalasseus sandvicensis",
                                                      ifelse(species == "Chroicocephalus maculipennis", "Larus maculipennis",
                                                             ifelse(species == "Chroicocephalus cirrocephalus","Larus cirrocephalus",
                                                                    ifelse(species == "Himantopus melanurus","Himantopus himantopus",
                                                                           ifelse(species == "Stercorarius maccormicki","Catharacta maccormicki",
                                                                                  ifelse(species == "Stercorarius chilensis","Catharacta chilensis",
                                                                                         ifelse(species == "Stercorarius antarticus","Catharacta antarctica",
                                                                                                species)))))))))))

# write.csv2(step5, file = './data_out/acuracySpeciesIdentificationDataset_IUCN_Names.csv', row.names = FALSE)

step5 <- dplyr::select(step5,-c(Class,Total_validated)) 

tableS1 <-
  step1 %>% 
  dplyr::left_join(step2, by = "species") %>%
  dplyr::left_join(step3,by="species") %>%
  dplyr::left_join(step4,by="species") %>%
  dplyr::left_join(step5,by="species") %>%
  dplyr::mutate(class = factor(class, 
                               levels = c("Reptilia", "Aves", "Mammalia"))) %>% 
  dplyr::arrange(class, desc(mean_rate)) %>%
  dplyr::rename("Annual stranding rate"=mean_rate) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(class)) %>%
  dplyr::rename(Taxon=species)

tableS1 <- tableS1[, c(1,16,2,7,8,4,9,10,11,5,3,6,12,13,14,15,17,18)] #Reorder column position    

tableS1$IUCN[is.na(tableS1$IUCN)] <- "-"

tableS1$`Species validation %`[is.na(tableS1$`Species validation %`)] <- "-"

tableS1$`No photography available %`[is.na(tableS1$`No photography available %`)] <- "-"

# write.csv2(tableS1, file = './results/tableS1.csv', row.names = FALSE)

##----------------------------------------------------------------------------##
##         Summary of survey effort and stranded records by polygons          ##
##---------------------------------Table S2-----------------------------------##
##----------------------------------------------------------------------------##

## Summary of survey effort by polygon
step1 <- 
  effFinal %>% 
  dplyr::group_by(id_polygon, id_newStretches, new_length_km) %>% 
  dplyr::summarise(Monitoring = n()) %>% 
  dplyr::group_by(id_polygon) %>% 
  dplyr::mutate(Beach_surveyed = new_length_km*Monitoring) %>% 
  dplyr::summarise(id_polygon_km = sum(new_length_km),
                   Total_beach_surveyed = sum(Beach_surveyed))

## Summary of strandings by polygon

# Considering only records classified as species level 
sp <- dfFinal[sapply(strsplit(as.character(dfFinal$species)," "), 
                     length) >= 2,] 

# Remove four species identified incorrectly
sp <-
  sp %>%
  dplyr::filter(species != "Pachyptila vittata" & 
                  species != "Larus atlanticus" & 
                  species != "Larus cirrocephalus" &
                  species != "Tringa solitaria")

# Species richness by polygon
step2 <- 
  sp %>% 
  dplyr::group_by(id_polygon, class) %>% 
  dplyr::summarise(Species_richness = n_distinct(species)) %>% 
  dplyr::group_by(id_polygon, class) %>% 
  tidyr::pivot_wider(names_from = c(class), 
                     names_prefix = "spRichness_", 
                     values_from = Species_richness, values_fill = 0)

# Total number of strandings considering all records by polygon 
# (including the four species incorrectly identified)
step3 <- 
  dfFinal %>% 
  dplyr::group_by(id_polygon, class) %>% 
  dplyr::summarise(Strandings = n()) %>%
  tidyr::pivot_wider(names_from = c(class), 
                     names_prefix = "n_", 
                     values_from = Strandings, values_fill = 0)

# Merge steps
tableS2 <- 
  step1 %>% 
  dplyr::left_join(step2, by = "id_polygon") %>% 
  dplyr::left_join(step3, by = "id_polygon") %>% 
  dplyr::select(id_polygon, id_polygon_km, 
                Total_beach_surveyed, 
                n_Reptilia, spRichness_Reptilia, 
                n_Aves, spRichness_Aves, 
                n_Mammalia, spRichness_Mammalia) %>%
  dplyr::mutate_at(c(5,7,9),as.factor) %>%
  janitor::adorn_totals("row")

# Round the values and rename columns
tableS2$id_polygon_km <- round(tableS2$id_polygon_km, digits = 2)
tableS2$Total_beach_surveyed <- round(tableS2$Total_beach_surveyed, digits = 2)

names(tableS2) <- c("Polygon", "Daily effort (km)", 
                    "Total beach surveyed (km)", 
                    "n_Reptilia", "spRichness_Reptilia", 
                    "n_Aves", "spRichness_Aves", 
                    "n_Cetacea", "spRichness_Cetacea")

# Save tableS2
# write.csv2(tableS2, file = './results/tableS2.csv', row.names = FALSE)

##----------------------------------------------------------------------------##
##                  Number of stranding by decomposition state                ##
##----------------------------------Table S3----------------------------------##
##----------------------------------------------------------------------------##

## Number of records by decomposition level and groups
tableS3 <- 
  dfFinal %>% 
  dplyr::group_by(cod_decomposition,class) %>% 
  dplyr::summarise(Strandings = n()) %>%
  tidyr::pivot_wider(names_from = c(class), 
                     names_prefix = "n_", 
                     values_from = Strandings, values_fill = 0) %>%
  dplyr::select(cod_decomposition, n_Reptilia,n_Aves, n_Mammalia)

names(tableS3) <- c("COD", "Reptilia", "Aves", "Cetacea")

# Save tableS3
# write.csv2(tableS3, file = './results/tableS3.csv', row.names = FALSE)

################################################################################
############################  GAMLSS by species ################################
################################################################################

#-------------
# Modeling procedure applied only to frequent and occasional species, 
# with records higher than 100 individuals. 
# For all species "month" and "id_polygon" were treated as 
# fixed effects and "year_n" as random effect. 
# The km of beach surveyed ("effort") was included as an offset term.
# Two best distributions (lesser AIC) were considered based on the distFit function.
# The traditional distributions ("PO","NBI","NBII", "ZINBI", "ZANBI") 
# were also included in the modelling procedure. 
#-------------

## Open dataset
dfModel <- read.csv2("./data_out/dfSumFinal.csv")

dfModel <-
  dfModel %>%
  dplyr::rename(Month = month, Polygon = id_polygon, Year = year_n)

dfModel$Month <- as.numeric(dfModel$Month)
dfModel$Polygon <- as.numeric(dfModel$Polygon)
dfModel$Year <- as.factor(dfModel$Year)

#----------------------------Reptilia

########### Caretta caretta

## Exploratory data analyses
par(mfrow = c(2, 2))
hist(dfModel$Caretta_caretta_Reptilia)
boxplot(dfModel$Caretta_caretta_Reptilia ~ dfModel$Polygon)
boxplot(dfModel$Caretta_caretta_Reptilia ~ dfModel$Month)
boxplot(dfModel$Caretta_caretta_Reptilia ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model 
distFit <- gamlss::fitDist(y = Caretta_caretta_Reptilia, 
                           data = dfModel, type = "counts")

distFit$fits # Selected distributions with lowest AIC and 
# the ones with less than two units of difference: WARING BNB

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("WARING","BNB","PO","NBI","NBII","ZINBI","ZANBI")

dist.test.Cc <- list()

for(fam in 1:length(fams)) {
  dist.test.Cc[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Caretta_caretta_Reptilia ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) +
                     re(random = ~1|Year) +
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel))}

unlist(dist.test.Cc) #lowest AIC BNB 

## Modeling "mu" (Model M) using the selected distribution BNB
CcM1 <- gamlss(Caretta_caretta_Reptilia ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) + 
                 offset(log(effort_km)), 
               family = BNB, data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution BNB
CcM2 <- update(CcM1,sigma.fo = ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7))

AIC (CcM1,CcM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow=c(1,1))
gamlss::wp(CcM2)
gamlss::wp(CcM2, ylim.all = 4)
plot(CcM2)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function 

# "mu"
dropCcM2_mu <- drop1(CcM2) %>%
  as.data.frame() 

# "sigma"
dropCcM2_si <- drop1(CcM2, what = "sigma") %>%
  as.data.frame()

# Combine both objects 'dropCcM2_mu' and 'dropCcM2_si' for building table1
Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropCcM2_mu <-
  dropCcM2_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Caretta caretta", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("BNB", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(CcM2)*100) %>%
  dplyr::mutate(Model1 = AIC(CcM1)) %>%
  dplyr::mutate(Model2 = AIC(CcM2))

Parameter <- c("Si_none","Si_month","Si_polygon")

dropCcM2_si <-
  dropCcM2_si %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::add_row() %>%
  dplyr::mutate(Species = rep("Caretta caretta", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("BNB", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(CcM2)*100) %>%
  dplyr::mutate(Model1 = AIC(CcM1)) %>%
  dplyr::mutate(Model2 = AIC(CcM2)) 

dropCcM2 <-rbind(dropCcM2_mu, dropCcM2_si)

## Generalized R-squared
gamlss::Rsq(CcM2)

########### Chelonia mydas

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Chelonia_mydas_Reptilia)
boxplot(dfModel$Chelonia_mydas_Reptilia ~ dfModel$Polygon)
boxplot(dfModel$Chelonia_mydas_Reptilia ~ dfModel$Month)
boxplot(dfModel$Chelonia_mydas_Reptilia ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Chelonia_mydas_Reptilia, 
                           data = dfModel, type = "counts")

distFit$fits  # Selected distributions with lowest AIC and 
# the ones with less than two units of difference: GEOM 
# GEOMo ZISICHEL ZASICHEL WARING NBI NBII

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("GEOM","GEOMo","ZISICHEL","ZASICHEL","WARING",
          "PO","NBI","NBII","ZINBI","ZANBI")

dist.test.Cm <- list()

for(fam in 1:length(fams)) {
  dist.test.Cm[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Chelonia_mydas_Reptilia ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) +
                     re(random = ~1|Year) + 
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel))}

unlist(dist.test.Cm) #lowest AIC NBII

## Modeling "mu" (Model M) using the selected distribution NBII
CmM1 <- gamlss(Chelonia_mydas_Reptilia ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) + 
                 offset(log(effort_km)), 
               family = NBII, data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution NBII
CmM2 <- update(CmM1,sigma.fo= ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7))

AIC(CmM1,CmM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(2,2))
gamlss::wp(CmM2)
gamlss::wp(CmM2, ylim.all = 1)
plot(CmM2)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropCmM2_mu <- drop1(CmM2) %>%
  as.data.frame() 

# "sigma"
dropCmM2_si <- drop1(CmM2, what = "sigma") %>%
  as.data.frame() 

# Combine both objects 'dropCcM2_mu' and 'dropCcM2_si' for building table1
Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropCmM2_mu <-
  dropCmM2_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Chelonia mydas", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("NBII", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(CmM2)*100) %>%
  dplyr::mutate(Model1 = AIC(CmM1)) %>%
  dplyr::mutate(Model2 = AIC(CmM2))

Parameter <- c("Si_none","Si_month","Si_polygon")

dropCmM2_si <-
  dropCmM2_si %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::add_row() %>%
  dplyr::mutate(Species = rep("Chelonia mydas", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("NBII", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(CmM2)*100) %>%
  dplyr::mutate(Model1 = AIC(CmM1)) %>%
  dplyr::mutate(Model2 = AIC(CmM2)) 

dropCmM2 <-rbind(dropCmM2_mu, dropCmM2_si)

## Generalized R-squared
gamlss::Rsq(CmM2) 

########### Lepidochelys olivacea

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Lepidochelys_olivacea_Reptilia)
boxplot(dfModel$Lepidochelys_olivacea_Reptilia ~ dfModel$Polygon)
boxplot(dfModel$Lepidochelys_olivacea_Reptilia ~ dfModel$Month)
boxplot(dfModel$Lepidochelys_olivacea_Reptilia ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Lepidochelys_olivacea_Reptilia, 
                           data = dfModel, type = "counts")

distFit$fits  # Selected distributions with lowest AIC and 
# the ones with less than two units of difference: 
# NBII NBI GPO ZANBI ZINBI ZALG ZIPIG ZAPIG SICHEL NBF SI DEL BNB

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("GPO","ZALG","SICHEL","NBF","SI","DEL","BNB",
          "NBII","NBI","PO","ZINBI","ZANBI","ZIPIG","ZAPIG")

dist.test.Lo <- list()

for(fam in 1:length(fams)) {
  dist.test.Lo[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Lepidochelys_olivacea_Reptilia ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) + 
                     re(random = ~1|Year) +
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel, n.cyc = 200))}

unlist(dist.test.Lo) #lowest AIC GPO 

## Modeling "mu" (Model M) using the selected distribution GPO
LoM1 <- gamlss(Lepidochelys_olivacea_Reptilia ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) +
                 offset(log(effort_km)), 
               family = GPO , data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution GPO
LoM2 <- update(LoM1, sigma.fo = ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7))

AIC(LoM1,LoM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(LoM1)
gamlss::wp(LoM1, ylim.all = 4)
plot(LoM1)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropLoM1_mu <- drop1(LoM1) %>%
  as.data.frame() 

Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropLoM1_mu <-
  dropLoM1_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Lepidochelys olivacea", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("GPO", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(LoM1)*100) %>%
  dplyr::mutate(Model1 = AIC(LoM1)) %>%
  dplyr::mutate(Model2 = AIC(LoM2))

dropLoM1 <- dropLoM1_mu

## Generalized R-squared
gamlss::Rsq(LoM1)

#---------------------------------------Aves

########### Spheniscus magellanicus

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Spheniscus_magellanicus_Aves)
boxplot(dfModel$Spheniscus_magellanicus_Aves ~ dfModel$Polygon)
boxplot(dfModel$Spheniscus_magellanicus_Aves ~ dfModel$Month)
boxplot(dfModel$Spheniscus_magellanicus_Aves ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Spheniscus_magellanicus_Aves, 
                           data = dfModel, type = "counts")

distFit$fits # Selected distributions with lowest AIC and 
# the ones with less than two units of difference:  
# DEL SI SICHEL ZALG 

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("DEL","SI","SICHEL","ZALG","PO","NBI","NBII","ZINBI","ZANBI")

dist.test.Sm <- list()

for(fam in 1:length(fams)) {
  dist.test.Sm[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Spheniscus_magellanicus_Aves ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) +  
                     re(random = ~1|Year) +
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel, n.cycy = 200))}

unlist(dist.test.Sm) #lowest AIC SI

## Modeling "mu" (Model M) using the selected distribution SI
SmM1 <- gamlss(Spheniscus_magellanicus_Aves ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) + 
                 offset(log(effort_km)), 
               family = SI , data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution SI
SmM2 <- update(SmM1, sigma.fo = ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7))

AIC(SmM1,SmM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(SmM2)
gamlss::wp(SmM2, ylim.all = 4)
plot(SmM2)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropSmM2_mu <- drop1(SmM2) %>%
  as.data.frame() 

# "sigma"
dropSmM2_si <- drop1(SmM2, what = "sigma") %>%
  as.data.frame() 

# Combine both objects 'dropCcM2_mu' and 'dropCcM2_si' for building table1
Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropSmM2_mu <-
  dropSmM2_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Spheniscus magellanicus", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("SI", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(SmM2)*100) %>%
  dplyr::mutate(Model1 = AIC(SmM1)) %>%
  dplyr::mutate(Model2 = AIC(SmM2))

Parameter <- c("Si_none","Si_month","Si_polygon")

dropSmM2_si <-
  dropSmM2_si %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::add_row() %>%
  dplyr::mutate(Species = rep("Spheniscus magellanicus", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("SI", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(SmM2)*100) %>%
  dplyr::mutate(Model1 = AIC(SmM1)) %>%
  dplyr::mutate(Model2 = AIC(SmM2)) 

dropSmM2 <-rbind(dropSmM2_mu, dropSmM2_si)

## Generalized R-squared
gamlss::Rsq(SmM2)

########### Larus dominicanus

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Larus_dominicanus_Aves)
boxplot(dfModel$Larus_dominicanus_Aves ~ dfModel$Polygon)
boxplot(dfModel$Larus_dominicanus_Aves ~ dfModel$Month)
boxplot(dfModel$Larus_dominicanus_Aves ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Larus_dominicanus_Aves, 
                           data = dfModel, type = "counts")

distFit$fits  # Selected distributions with lowest AIC and 
# the ones with less than two units of difference: 
# GPO WARING ZANBI SI SICHEL

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("GPO","SICHEL","SI","WARING","PO","NBI","NBII","ZINBI","ZANBI")

dist.test.Ld <- list()

for(fam in 1:length(fams)) {
  dist.test.Ld[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Larus_dominicanus_Aves ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) +
                     re(random = ~1|Year) + 
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel, n.cyc = 200))}

unlist(dist.test.Ld) #lowest AIC GPO 

## Modeling "mu" (Model M) using the selected distribution GPO
LdM1 <- gamlss(Larus_dominicanus_Aves ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) + 
                 offset(log(effort_km)), 
               family = GPO, data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution GPO
LdM2 <- update(LdM1, sigma.fo = ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7))

AIC(LdM1,LdM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(LdM2)
gamlss::wp(LdM2, ylim.all = 6)
plot(LdM2)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropLdM2_mu <- drop1(LdM2) %>%
  as.data.frame() 

# "sigma"
dropLdM2_si <- drop1(LdM2, what = "sigma") %>%
  as.data.frame() 

# Combine both objects 'dropCcM2_mu' and 'dropCcM2_si' for building table1
Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropLdM2_mu <-
  dropLdM2_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Larus dominicanus", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("GPO", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(LdM2)*100) %>%
  dplyr::mutate(Model1 = AIC(LdM1)) %>%
  dplyr::mutate(Model2 = AIC(LdM2))

Parameter <- c("Si_none","Si_month","Si_polygon")

dropLdM2_si <-
  dropLdM2_si %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::add_row() %>%
  dplyr::mutate(Species = rep("Larus dominicanus", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("GPO", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(LdM2)*100) %>%
  dplyr::mutate(Model1 = AIC(LdM1)) %>%
  dplyr::mutate(Model2 = AIC(LdM2)) 

dropLdM2 <- rbind(dropLdM2_mu, dropLdM2_si)

## Generalized R-squared
gamlss::Rsq(LdM2)

########### Puffinus puffinus 

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Puffinus_puffinus_Aves)
boxplot(dfModel$Puffinus_puffinus_Aves ~ dfModel$Polygon)
boxplot(dfModel$Puffinus_puffinus_Aves ~ dfModel$Month)
boxplot(dfModel$Puffinus_puffinus_Aves ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Puffinus_puffinus_Aves, 
                           data = dfModel, type = "counts")

distFit$fits # Selected distributions with lowest AIC and 
# the ones with less than two units of difference: GPO SI SICHEL  

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("SI","GPO","SICHEL","PO","NBI","NBII","ZINBI","ZANBI")

dist.test.Pp <- list()

for(fam in 1:length(fams)) {
  dist.test.Pp[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Puffinus_puffinus_Aves ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) +
                     re(random = ~1|Year) + 
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel, n.cyc = 200))}

unlist(dist.test.Pp) #lowest AIC SI

## Modeling "mu" (Model M) using the selected distribution SICHEL
PpM1 <- gamlss(Puffinus_puffinus_Aves ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) + 
                 offset(log(effort_km)),
               family = SI, data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution SICHEL
PpM2 <- update(PpM1, sigma.fo = ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon,df = 7))

AIC(PpM1,PpM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(PpM2)
gamlss::wp(PpM2, ylim.all = 6)
plot(PpM2)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropPpM2_mu <- drop1(PpM2) %>%
  as.data.frame() 

# "sigma"
dropPpM2_si <- drop1(PpM2, what = "sigma") %>%
  as.data.frame() 

# Combine both objects 'dropCcM2_mu' and 'dropCcM2_si' for building table1
Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropPpM2_mu <-
  dropPpM2_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Puffinus puffinus", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("SI", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(PpM2)*100) %>%
  dplyr::mutate(Model1 = AIC(PpM1)) %>%
  dplyr::mutate(Model2 = AIC(PpM2))

Parameter <- c("Si_none","Si_month","Si_polygon")

dropPpM2_si <-
  dropPpM2_si %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::add_row() %>%
  dplyr::mutate(Species = rep("Puffinus puffinus", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("SI", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(PpM2)*100) %>%
  dplyr::mutate(Model1 = AIC(PpM1)) %>%
  dplyr::mutate(Model2 = AIC(PpM2)) 

dropPpM2 <- rbind(dropPpM2_mu, dropPpM2_si)

## Generalized R-squared  
gamlss::Rsq(PpM2)

########### Thalassarche chlororhynchos

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Thalassarche_chlororhynchos_Aves)
boxplot(dfModel$Thalassarche_chlororhynchos_Aves~ dfModel$Polygon)
boxplot(dfModel$Thalassarche_chlororhynchos_Aves ~ dfModel$Month)
boxplot(dfModel$Thalassarche_chlororhynchos_Aves ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Thalassarche_chlororhynchos_Aves, 
                           data = dfModel, type = "counts")

distFit$fits # Selected distributions with lowest AIC and 
# the ones with less than two units of difference: 
# GPO ZALG SI SICHEL  

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("GPO","SI","SICHEL","PO","NBI","NBII","ZINBI","ZANBI","ZALG")

dist.test.Tc <- list()

for(fam in 1:length(fams)) {
  dist.test.Tc[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Thalassarche_chlororhynchos_Aves ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) + 
                     re(random = ~1|Year) +
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel,n.cyc = 200))}

unlist(dist.test.Tc) #lowest AIC NBII

## Modeling "mu" (Model M) using the selected distribution NBII
TcM1 <- gamlss(Thalassarche_chlororhynchos_Aves ~ 
                 cs(Month,df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) + 
                 offset(log(effort_km)), 
               family = NBII, data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution NBII
TcM2 <- update(TcM1, sigma.fo = ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7))

AIC(TcM1,TcM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(TcM2)
gamlss::wp(TcM2, ylim.all = 6)
plot(TcM2)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropTcM2_mu <- drop1(TcM2) %>%
  as.data.frame() 

# "sigma"
dropTcM2_si <- drop1(TcM2, what = "sigma") %>%
  as.data.frame()

# Combine both objects 'dropCcM2_mu' and 'dropCcM2_si' for building table1
Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropTcM2_mu <-
  dropTcM2_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Thalassarche chlororhynchos", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("NBII", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(TcM2)*100) %>%
  dplyr::mutate(Model1 = AIC(TcM1)) %>%
  dplyr::mutate(Model2 = AIC(TcM2))

Parameter <- c("Si_none", "Si_month", "Si_polygon")

dropTcM2_si <-
  dropTcM2_si %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::add_row() %>%
  dplyr::mutate(Species = rep("Thalassarche chlororhynchos", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("NBII", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(TcM2)*100) %>%
  dplyr::mutate(Model1 = AIC(TcM1)) %>%
  dplyr::mutate(Model2 = AIC(TcM2)) 

dropTcM2 <- rbind(dropTcM2_mu, dropTcM2_si)

## Generalized R-squared
gamlss::Rsq(TcM2)

########### Thalassarche melanophris

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Thalassarche_melanophris_Aves)
boxplot(dfModel$Thalassarche_melanophris_Aves~ dfModel$Polygon)
boxplot(dfModel$Thalassarche_melanophris_Aves~ dfModel$Month)
boxplot(dfModel$Thalassarche_melanophris_Aves~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Thalassarche_melanophris_Aves, 
                           data = dfModel, type = "counts")

distFit$fits # Selected distributions with lowest AIC and 
# the ones with less than two units of difference: 
# ZALG GPO NBII NBI DEL SICHEL SI ZANBI

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("ZALG","GPO","DEL","SICHEL","SI","PO","NBI","NBII","ZINBI","ZANBI")

dist.test.Tm <- list()

for(fam in 1:length(fams)) {
  dist.test.Tm[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Thalassarche_melanophris_Aves ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) + 
                     random(as.factor(Year)) +
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel, n.cyc = 200))}

unlist(dist.test.Tm) #lowest AIC DEL 

## Modeling "mu" (Model M) using the selected distribution DEL
TmM1 <- gamlss(Thalassarche_melanophris_Aves ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) + 
                 offset(log(effort_km)), 
               family = DEL, data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution DEL
TmM2 <- update(TmM1, sigma.fo= ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7))

AIC(TmM1,TmM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(TmM1)
gamlss::wp(TmM1, ylim.all = 6)
plot(TmM1)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropTmM1_mu <- drop1(TmM1) %>%
  as.data.frame() 

Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropTmM1_mu <-
  dropTmM1_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Thalassarche melanophris", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("DEL", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(TmM1)*100) %>%
  dplyr::mutate(Model1 = AIC(TmM1)) %>%
  dplyr::mutate(Model2 = AIC(TmM2))

dropTmM1 <- dropTmM1_mu

## Generalized R-squared
gamlss::Rsq(TmM1)

########### Procellaria aequinoctialis

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Procellaria_aequinoctialis_Aves)
boxplot(dfModel$Procellaria_aequinoctialis_Aves ~ dfModel$Polygon)
boxplot(dfModel$Procellaria_aequinoctialis_Aves ~ dfModel$Month)
boxplot(dfModel$Procellaria_aequinoctialis_Aves ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Procellaria_aequinoctialis_Aves, 
                           data = dfModel, type = "counts")

distFit$fits # Selected distributions with lowest AIC and 
# the ones with less than two units of difference: 
# GPO BNB ZALG ZIPIG ZAPIG WARING

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("GPO","BNB","ZALG","WARING","PO",
          "NBI","NBII","ZINBI","ZANBI","ZIPIG","ZAPIG")

dist.test.Pa <- list()

for(fam in 1:length(fams)) {
  dist.test.Pa[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Procellaria_aequinoctialis_Aves ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) + 
                     re(random = ~1|Year) +
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel, n.cyc = 200))}

unlist(dist.test.Pa) #lowest AIC GPO

## Modeling "mu" (Model M) using the selected distribution GPO
PaM1 <- gamlss(Procellaria_aequinoctialis_Aves ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) +
                 offset(log(effort_km)), 
               family = GPO, data = dfModel, n.cyc = 200)


## Modeling "mu" and "sigma" (Model MS) using the selected GPO
PaM2 <- update(PaM1, sigma.fo = ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7))

AIC(PaM1,PaM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(PaM1)
gamlss::wp(PaM1, ylim.all = 6)
plot(PaM1)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropPaM1_mu <- drop1(PaM1) %>%
  as.data.frame() 

Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropPaM1_mu <-
  dropPaM1_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Procellaria aequinoctialis", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("GPO", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(PaM1)*100) %>%
  dplyr::mutate(Model1 = AIC(PaM1)) %>%
  dplyr::mutate(Model2 = AIC(PaM2))

dropPaM1 <- dropPaM1_mu

## Generalized R-squared
gamlss::Rsq(PaM1)

########### Fregata magnificens

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Fregata_magnificens_Aves)
boxplot(dfModel$Fregata_magnificens_Aves ~ dfModel$Polygon)
boxplot(dfModel$Fregata_magnificens_Aves ~ dfModel$Month)
boxplot(dfModel$Fregata_magnificens_Aves ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Fregata_magnificens_Aves, 
                           data = dfModel, type = "counts")

distFit$fits # Selected distributions with lowest AIC and 
# the ones with less than two units of difference: 
# ZALG WARING PIG GPO ZAPIG ZIPIG SICHEL SI BNB ZANBI

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("ZALG","WARING","PIG","GPO","ZAPIG","ZIPIG","SICHEL", 
          "SI","BNB","PO","NBI","NBII","ZINBI","ZANBI")

dist.test.Fm <- list()

for(fam in 1:length(fams)) {
  dist.test.Fm[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Fregata_magnificens_Aves ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) +
                     re(random = ~1|Year) + 
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel, n.cyc = 200))}

unlist(dist.test.Fm) #lowest AIC SICHEL

## Modeling "mu" (Model M) using the selected distribution SICHEL
FmM1 <- gamlss(Fregata_magnificens_Aves ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) + 
                 offset(log(effort_km)), 
               family = SICHEL, data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution SICHEL
FmM2 <- update(FmM1, sigma.fo = ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7))

AIC(FmM1,FmM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(FmM2)
gamlss::wp(FmM2, ylim.all = 6)
plot(FmM2)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropFmM2_mu <- drop1(FmM2) %>%  
  as.data.frame() 

# "sigma"
dropFmM2_si <- drop1(FmM2, what = "sigma") %>%
  as.data.frame() 

# Combine both objects 'dropCcM2_mu' and 'dropCcM2_si' for building table1
Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropFmM2_mu <-
  dropFmM2_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Fregata magnificens", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("SICHEL", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(FmM2)*100) %>%
  dplyr::mutate(Model1 = AIC(FmM1)) %>%
  dplyr::mutate(Model2 = AIC(FmM2))

Parameter <- c("Si_none", "Si_month", "Si_polygon")

dropFmM2_si <-
  dropFmM2_si %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::add_row() %>%
  dplyr::mutate(Species = rep("Fregata magnificens", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("SICHEL", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(FmM2)*100) %>%
  dplyr::mutate(Model1 = AIC(FmM1)) %>%
  dplyr::mutate(Model2 = AIC(FmM2)) 

dropFmM2 <- rbind(dropFmM2_mu, dropFmM2_si)

## Generalized R-squared
gamlss::Rsq(FmM2)

########### Sula leucogaster

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Sula_leucogaster_Aves)
boxplot(dfModel$Sula_leucogaster_Aves ~ dfModel$Polygon)
boxplot(dfModel$Sula_leucogaster_Aves ~ dfModel$Month)
boxplot(dfModel$Sula_leucogaster_Aves ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Sula_leucogaster_Aves, 
                           data = dfModel, type = "counts")

distFit$fits # Selected distributions with lowest AIC and 
# the ones with less than two units of difference: 
# PIG SICHEL SI ZISICHEL ZASICHEL ZIPIG ZAPIG

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("PIG","SI","SICHEL","ZISICHEL","ZIPIG","ZAPIG","PO","NBI",
          "NBII","ZINBI","ZANBI","ZASICHEL")

dist.test.Sl <- list()

for(fam in 1:length(fams)) {
  dist.test.Sl[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Sula_leucogaster_Aves ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) +
                     re(random = ~1|Year) + 
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel, n.cyc = 200))}

unlist(dist.test.Sl) #lowest AIC SICHEL and ZINBI

#---------
# However, when running Model MS, SICHEL did not converged. 
# Thus we used the second lowest GAIC ZINBI, which was less than two 
# units 
#---------

## Modeling "mu" (Model M) using the selected distribution ZINBI
SlM1 <- gamlss(Sula_leucogaster_Aves ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) +
                 offset(log(effort_km)), 
               family = ZINBI, data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution ZINBI             
SlM2 <- update(SlM1, sigma.fo = ~ 
                 cs(Month, df = 3) +  
                 cs(Polygon, df = 7))

AIC(SlM1,SlM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(SlM2)
gamlss::wp(SlM2, ylim.all = 6)
plot(SlM2)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropSlM2_mu <- drop1(SlM2) %>%
  as.data.frame() 

# "sigma"
dropSlM2_si <- drop1(SlM2, what = "sigma") %>%
  as.data.frame() 

# Combine both objects 'dropCcM2_mu' and 'dropCcM2_si' for building table1
Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropSlM2_mu <-
  dropSlM2_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Sula leucogaster", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("ZINBI", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(SlM2)*100) %>%
  dplyr::mutate(Model1 = AIC(SlM1)) %>%
  dplyr::mutate(Model2 = AIC(SlM2))

Parameter <- c("Si_none","Si_month","Si_polygon")

dropSlM2_si <-
  dropSlM2_si %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::add_row() %>%
  dplyr::mutate(Species = rep("Sula leucogaster", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("ZINBI", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(SlM2)*100) %>%
  dplyr::mutate(Model1 = AIC(SlM1)) %>%
  dplyr::mutate(Model2 = AIC(SlM2)) 

dropSlM2 <- rbind(dropSlM2_mu, dropSlM2_si)

## Generalized R-squared 
gamlss::Rsq(SlM2)

#---------------------------------Mammalia

########### Pontoporia blainvillei

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Pontoporia_blainvillei_Mammalia)
boxplot(dfModel$Pontoporia_blainvillei_Mammalia ~ dfModel$Polygon)
boxplot(dfModel$Pontoporia_blainvillei_Mammalia ~ dfModel$Month)
boxplot(dfModel$Pontoporia_blainvillei_Mammalia ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Pontoporia_blainvillei_Mammalia, 
                           data = dfModel, type = "counts")

distFit$fits # Selected distributions with lowest AIC and 
# the ones with less than two units of difference: 
# GPO ZANBI DEL SICHEL SI WARING

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("GPO","DEL","SICHEL","SI","WARING","PO","NBI","NBII","ZINBI","ZANBI")

dist.test.Pb <- list()

for(fam in 1:length(fams)) {
  dist.test.Pb[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Pontoporia_blainvillei_Mammalia ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) +  
                     re(random = ~1|Year) +
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel, n.cyc = 200))}

unlist(dist.test.Pb) #lowest AIC SICHEL

## Modeling "mu" (Model M) using the selected distribution SICHEL
PbM1 <- gamlss(Pontoporia_blainvillei_Mammalia ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) + 
                 re(random = ~1|Year) +
                 offset(log(effort_km)), 
               family = SICHEL, data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected SICHEL
PbM2<-update(PbM1, sigma.fo = ~ 
               cs(Month, df = 3) + 
               cs(Polygon, df = 7))

AIC(PbM1,PbM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(PbM1)
gamlss::wp(PbM1, ylim.all = 6)
plot(PbM1)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropPbM1_mu <- drop1(PbM1) %>%
  as.data.frame() 

Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropPbM1_mu <-
  dropPbM1_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Pontoporia blainvillei", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("SICHEL",4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(PbM1)*100) %>%
  dplyr::mutate(Model1 = AIC(PbM1)) %>%
  dplyr::mutate(Model2 = AIC(PbM2))

dropPbM1 <- dropPbM1_mu

## Generalized R-squared
gamlss::Rsq(PbM1)

########### Sotalia guianensis

## Exploratory analyses
par(mfrow = c(1, 1))
hist(dfModel$Sotalia_guianensis_Mammalia)
boxplot(dfModel$Sotalia_guianensis_Mammalia~ dfModel$Polygon)
boxplot(dfModel$Sotalia_guianensis_Mammalia~ dfModel$Month)
boxplot(dfModel$Sotalia_guianensis_Mammalia~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Sotalia_guianensis_Mammalia, 
                           data = dfModel, type = "counts")

distFit$fits # Selected distributions with lowest AIC and 
# the ones with less than two units of difference: 
# GPO ZALG ZIPIG ZAPIG BNB NBI NBII

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("GPO","ZALG","ZIPIG","BNB","PO","NBI","NBII", "ZINBI","ZANBI","ZAPIG")

dist.test.Sg <- list()

for(fam in 1:length(fams)) {
  dist.test.Sg[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Sotalia_guianensis_Mammalia ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) + 
                     re(random = ~1|Year) +
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel, n.cyc = 200))}

unlist(dist.test.Sg) #lowest AIC GPO

## Modeling "mu" (Model M) using the selected distribution GPO
SgM1 <- gamlss(Sotalia_guianensis_Mammalia ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) + 
                 offset(log(effort_km)), 
               family = GPO, data = dfModel, n.cyc = 100)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution GPO
SgM2 <- update(SgM1, sigma.fo = ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7))

AIC(SgM1,SgM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(SgM2)
gamlss::wp(SgM2, ylim.all = 6)
plot(SgM2)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropSgM2_mu <- drop1(SgM2) %>%  #Model with term  cs(Month, df = 3) has failed
  as.data.frame()  

# "sigma"
dropSgM2_si <- drop1(SgM2, what = "sigma") %>%
  as.data.frame() 

# Combine both objects 'dropCcM2_mu' and 'dropCcM2_si' for building table1
Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropSgM2_mu <-
  dropSgM2_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Sotalia guianensis", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("GPO", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(SgM2)*100) %>%
  dplyr::mutate(Model1 = AIC(SgM1)) %>%
  dplyr::mutate(Model2 = AIC(SgM2))

Parameter <- c("Si_none","Si_month","Si_polygon")

dropSgM2_si <-
  dropSgM2_si %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::add_row() %>%
  dplyr::mutate(Species = rep("Sotalia guianensis", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("GPO", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(SgM2)*100) %>%
  dplyr::mutate(Model1 = AIC(SgM1)) %>%
  dplyr::mutate(Model2 = AIC(SgM2)) 

dropSgM2 <- rbind(dropSgM2_mu, dropSgM2_si)

## Generalized R-squared
gamlss::Rsq(SgM2)

########### Tursiops trucatus

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Tursiops_truncatus_Mammalia)
boxplot(dfModel$Tursiops_truncatus_Mammalia ~ dfModel$Polygon)
boxplot(dfModel$Tursiops_truncatus_Mammalia ~ dfModel$Month)
boxplot(dfModel$Tursiops_truncatus_Mammalia ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Tursiops_truncatus_Mammalia, 
                           data = dfModel, type = "counts")

distFit$fits # Selected distributions with lowest AIC and 
# the ones with less than two units of difference: 
# YULE WARING PIG ZALG GPO

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("PIG","YULE","WARING","ZALG","GPO","PO","NBI","NBII","ZINBI","ZANBI")

dist.test.Tt <- list()

for(fam in 1:length(fams)) {
  dist.test.Tt[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Tursiops_truncatus_Mammalia ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) +
                     re(random = ~1|Year) +
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel, n.cyc = 200))}

unlist(dist.test.Tt) #lowest AIC PIG

#---------
# When running Model MS, PIG did not converged. 
# Thus we used the second lowest GAIC GPO, which was less than two 
# units. However, when compared model M and MS using GPO with model M using PIG,
# the later had the lowest AIC
#---------

## Modeling "mu" (Model M) using the selected distribution PIG
TtM1 <- gamlss(Tursiops_truncatus_Mammalia ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) + 
                 offset(log(effort_km)), 
               family = PIG, data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution PIG 
TtM2 <- update(TtM1, sigma.fo = ~      # not converged
                 cs(Month, df = 3) +  
                 cs(Polygon, df = 7), 
               n.cyc = 100)

AIC(TtM1)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(TtM1)
gamlss::wp(TtM1, ylim.all = 6)
plot(TtM1)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropTtM1_mu <- drop1(TtM1) %>%
  as.data.frame() 

Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropTtM1_mu <-
  dropTtM1_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Tursiops truncatus",4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("PIG",4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(TtM1)*100) %>%
  dplyr::mutate(Model1 = AIC(TtM1)) %>%
  dplyr::mutate(Model2 = "not_converged")

dropTtM1 <- dropTtM1_mu

## Generalized R-squared
gamlss::Rsq(TtM1) 

##----------------------------------------------------------------------------##
##                             Smothers GAMLSS: Mu                            ##
##-------------------------------Figures 4 and 5------------------------------##
##----------------------------------------------------------------------------##

##
## Per Month
##

tiff("./results/figure4.tiff", height = 170 , width = 170, 
     units = 'mm', res = 400, compression ='lzw')

par(mfrow = c(4, 4),cex.axis = 0.8,cex.lab = 0.8,cex.main = 1)

gamlss::term.plot(CcM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Caretta caretta", col.term = "black", font.main = 3)
gamlss::term.plot(CmM2, what = "mu",ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Chelonia mydas", col.term = "black", font.main = 3)
gamlss::term.plot(LoM1, what = "mu", ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Lepidochelys olivacea", col.term = "black", 
                  font.main = 3)
gamlss::term.plot(SmM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Spheniscus magellanicus", col.term = "black",
                  font.main = 3)
gamlss::term.plot(LdM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Larus dominicanus", col.term = "black", font.main = 3)
gamlss::term.plot(SlM2, what = "mu",ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Sula leucogaster", col.term = "black", font.main = 3)
gamlss::term.plot(TcM2, what = "mu",ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Thalassarche chlororhynchos", col.term = "black",
                  font.mai = 3)  
gamlss::term.plot(TmM1, what = "mu",ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Thalassarche melanophris", col.term = "black",
                  font.main = 3)
gamlss::term.plot(PaM1, what = "mu",ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Procellaria aequinoctialis", col.term = "black",
                  font.main = 3)
gamlss::term.plot(PpM2, what = "mu",ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Puffinus puffinus", col.term = "black", font.main = 3)
gamlss::term.plot(FmM2, what = "mu",ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Fregata magnificens", col.term = "black", 
                  font.main = 3)
gamlss::term.plot(PbM1, what ="mu",ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Pontoporia blainvillei", col.term = "black",
                  font.main = 3)
gamlss::term.plot(SgM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Sotalia guianensis", col.term = "black", 
                  font.main = 3)
gamlss::term.plot(TtM1, what = "mu", ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Tursiops truncatus", col.term = "black", 
                  font.main = 3, sub = "Month", cex.sub = 1.5)

dev.off()

##
##Per polygon
##

tiff("./results/figure5.tiff", height = 170 , width = 170, units = 'mm',
     res = 400, compression ='lzw')

par(mfrow = c(4, 4),cex.axis = 0.8,cex.lab = 0.8,cex.main = 1)

gamlss::term.plot(CcM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Caretta caretta", col.term = "black", font.main = 3)
gamlss::term.plot(CmM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Chelonia mydas", col.term = "black", font.main = 3)
gamlss::term.plot(LoM1, what ="mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Lepidochelys olivacea", col.term = "black",
                  font.main = 3)
gamlss::term.plot(SmM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Spheniscus magellanicus", col.term = "black",
                  font.main = 3)
gamlss::term.plot(LdM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Larus dominicanus", col.term = "black", font.main = 3)
gamlss::term.plot(SlM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Sula leucogaster", col.term = "black", font.main = 3)
gamlss::term.plot(TcM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Thalassarche chlororhynchos", col.term = "black",
                  font.main = 3)
gamlss::term.plot(TmM1, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Thalassarche melanophris", col.term = "black",
                  font.main = 3)
gamlss::term.plot(PaM1, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Procellaria aequinoctialis", col.term = "black",
                  font.main = 3)
gamlss::term.plot(PpM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Puffinus puffinus", col.term = "black", font.main = 3)
gamlss::term.plot(FmM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Fregata magnificens", col.term = "black", 
                  font.main = 3)
gamlss::term.plot(PbM1, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Pontoporia blainvillei", col.term = "black",
                  font.main = 3)
gamlss::term.plot(SgM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Sotalia guianensis", col.term = "black", 
                  font.main = 3)
gamlss::term.plot(TtM1, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Tursiops truncatus", col.term = "black", 
                  font.main = 3, sub = "Polygon", cex.sub = 1.5)

dev.off()

##----------------------------------------------------------------------------##
##                           Smothers GAMLSS: Sigma                           ##
##----------------------------------S6 and S7---------------------------------##
##----------------------------------------------------------------------------##

##
## Per Month
##

tiff("./results/figureS6.tiff", height = 170 , width = 170, units = 'mm',
     res = 400, compression ='lzw')

par(mfrow = c(3, 3),cex.axis = 0.8,cex.lab = 0.8,cex.main = 1)

gamlss::term.plot(CcM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 1, main = "Caretta caretta", col.term = "black",
                  font.main = 3)
gamlss::term.plot(CmM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 1, main = "Chelonia mydas", col.term = "black",
                  font.main = 3)
gamlss::term.plot(SmM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 1, main = "Spheniscus magellanicus", 
                  col.term = "black", font.main = 3)
gamlss::term.plot(LdM2, what = "sigma", ylim = 'free', scheme = 'shaded', 
                  term = 1, main = "Larus dominicanus", col.term = "black", 
                  font.main = 3)
gamlss::term.plot(SlM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 1, main = "Sula leucogaster", col.term = "black", 
                  font.main=3)
gamlss::term.plot(TcM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 1, main = "Thalassarche chlororhynchos",
                  col.term = "black", font.main=3)
gamlss::term.plot(PpM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 1, main = "Puffinus puffinus", col.term = "black",
                  font.main = 3)
gamlss::term.plot(FmM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 1, main = "Fregata magnificens", col.term = "black",
                  font.main = 3, sub = "Month", cex.sub = 1.5)
gamlss::term.plot(SgM2, what = "sigma", ylim = 'free', scheme = 'shaded', 
                  term = 1, main = "Sotalia guianensis", col.term = "black",
                  font.main = 3)

dev.off()

##
##Per polygon
##

tiff("./results/figureS7.tiff", height = 170 , width = 170, units = 'mm',
     res = 400, compression ='lzw')

par(mfrow = c(3, 3),cex.axis = 0.8,cex.lab = 0.8,cex.main = 1)

gamlss::term.plot(CcM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 2, main = "Caretta caretta", col.term = "black",
                  font.main = 3)
gamlss::term.plot(CmM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 2, main = "Chelonia mydas", col.term = "black", 
                  font.main = 3)
gamlss::term.plot(SmM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 2, main = "Spheniscus magellanicus", 
                  col.term = "black", font.main = 3)
gamlss::term.plot(LdM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 2, main = "Larus dominicanus", col.term = "black",
                  font.main = 3)
gamlss::term.plot(SlM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 2, main = "Sula leucogaster", col.term = "black",
                  font.main = 3)
gamlss::term.plot(TcM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 2, main = "Thalassarche chlororhynchos",
                  col.term = "black", font.main = 3)
gamlss::term.plot(PpM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 2, main = "Puffinus puffinus", col.term = "black",
                  font.main = 3)
gamlss::term.plot(FmM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 2, main = "Fregata magnificens", col.term = "black",
                  cex.sub = 1.5, sub = "Polygon", font.main = 3)
gamlss::term.plot(SgM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 2, main = "Sotalia guianensis", col.term = "black",
                  font.main = 3)

dev.off()

##----------------------------------------------------------------------------##
##                             Summaries GAMLSS                               ##
##----------------------------------Table 1-----------------------------------##
##----------------------------------------------------------------------------##

## Combine the drop1 objects
options(digits = 2)
dropModels <- rbind.fill(dropCcM2,dropCmM2,dropLoM1,dropSmM2,dropLdM2,dropPpM2,
                         dropTcM2,dropTmM1,dropPaM1,dropFmM2,dropSlM2,dropPbM1,
                         dropSgM2,dropTtM1)
dropModels  <-
  dropModels  %>%
  tidyr::pivot_wider(names_from = c(Parameter), 
                     values_from = pvalue) %>%
  dplyr::relocate(Model1,.after = Distribution) %>%
  dplyr::relocate(Model2,.after = Model1) %>%
  dplyr::select(c(1:5,7,8,11,12)) %>%
  dplyr::mutate_at(c(3:4), as.numeric) %>%
  dplyr::mutate_at(c(6:9), round, digits = 3)

# write.csv2(dropModels,"./results/Table1.1.csv", row.names = FALSE)

## End
