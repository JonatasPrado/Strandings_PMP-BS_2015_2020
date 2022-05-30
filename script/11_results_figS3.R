
## Intensive and wide-ranging beach surveys uncover 
## temporal and spatial stranding patterns of marine megafauna
##
## ICES Journal of Marine Science - 2022
##
## Code by Jonatas F. H. Prado & Nicholas W. Daudt

## This script builds figureS3 showing annual stranding rate for 
## the selected Reptilia, Aves and Cetacea species (i.e. species with
## numeric frequency>1% and n>100)

################################################################################

# Libraries ####
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

################################################################################

## Open stranding dataset
dfSumFinal <- read.csv2("./data_out/dfSumFinal.csv")

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

## End