
## Intensive and wide-ranging beach surveys uncover 
## temporal and spatial stranding patterns of marine megafauna
##
## ICES Journal of Marine Science - 2022
##
## Code by Jonatas F. H. Prado & Nicholas W. Daudt

## This script builds table S1 that summarize the results of strandings for each
## taxa. See the main text for rationale and detailed explanations

################################################################################

# Libraries ####
library(dplyr)
library(tidyr)

################################################################################

## Open strandings, survey effort and carcasses validation datasets
dfFinal <- read.csv2("./data_out/dfFinal.csv")
effFinal <- read.csv2("./data_out/effFinal.csv")
validation <- readxl::read_xlsx("./data/acuracySpeciesIdentificationDataset.xlsx",
                                sheet = 1)

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

## End