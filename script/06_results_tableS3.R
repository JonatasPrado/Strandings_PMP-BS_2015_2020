
## Intensive and wide-ranging beach surveys uncover 
## temporal and spatial stranding patterns of marine megafauna
##
## ICES Journal of Marine Science - 2022
##
## Code by Jonatas F. H. Prado & Nicholas W. Daudt

## This script builds tableS3 summarizing the number of stranded individuals 
## by decomposition level for each tetrapod group (Reptilia, Aves and Cetacea)

################################################################################

# Libraries ####
library(dplyr)
library(tidyr)

################################################################################

## Open stranding dataset
dfFinal <- read.csv2("./data_out/dfFinal.csv")

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

## End