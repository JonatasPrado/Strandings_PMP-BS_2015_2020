 
## Intensive and wide-ranging beach surveys uncover 
## temporal and spatial stranding patterns of marine megafauna
##
## ICES Journal of Marine Science - 2022
##
## Code by Jonatas F. H. Prado & Nicholas W. Daudt

## This script does the heavy-lifting processes to end up on our final data 
## sets for analysis. It goes through stranding and effort data (which initially 
## were different data sets) and does all the necessary tidying and 
## standardization needed. See the main text for rationale and detailed 
## explanations

################################################################################

# Libraries ####
library(plyr)
library(tidyverse)
library(sf)
library(readxl)
library(lubridate)
library(mapview) # used only for checking spatial features

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

# End