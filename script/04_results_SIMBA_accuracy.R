## Intensive and wide-ranging beach surveys uncover 
## temporal and spatial stranding patterns of marine megafauna
##
## ICES Journal of Marine Science - 2022
##
## Code by Jonatas F. H. Prado & Nicholas W. Daudt

## This script summarize the accuracy of species identification from 
## the SIMBA data base. See the main text for rationale and detailed explanations

################################################################################

# Libraries ####
library(dplyr)
library(tidyr)

################################################################################

## Open stranding and validation datasets
dfFinal <- read.csv2("./data_out/dfFinal.csv")
validation <- read.csv2("./data_out/acuracySpeciesIdentificationDataset_IUCN_Names.csv") 

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

## End