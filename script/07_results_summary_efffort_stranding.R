
## Intensive and wide-ranging beach surveys uncover 
## temporal and spatial stranding patterns of marine megafauna
##
## ICES Journal of Marine Science - 2022
##
## Code by Jonatas F. H. Prado & Nicholas W. Daudt

## This script summarize the general results of survey effort and 
## strandings

################################################################################

# Libraries ####
library(dplyr)
library(tidyr)
library(janitor)

################################################################################

## Open effort and stranding datasets
effFinal <- read.csv2("./data_out/effFinal.csv")
dfFinal <- read.csv2("./data_out/dfFinal.csv")

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

## End