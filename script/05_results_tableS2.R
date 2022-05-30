
## Intensive and wide-ranging beach surveys uncover 
## temporal and spatial stranding patterns of marine megafauna
##
## ICES Journal of Marine Science - 2022
##
## Code by Jonatas F. H. Prado & Nicholas W. Daudt

## This script builds tableS2 summarizing results of strandings and survey
## effort by polygon for each tetrapod group (Reptilia, Aves and Cetacea). 
## See the main text for rationale and detailed explanations

################################################################################

# Libraries ####
library(dplyr)
library(tidyr)

################################################################################

## Open effort and stranding datasets
effFinal <- read.csv2("./data_out/effFinal.csv")
dfFinal <- read.csv2("./data_out/dfFinal.csv")

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

## End