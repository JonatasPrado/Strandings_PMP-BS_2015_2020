
## Intensive and wide-ranging beach surveys uncover 
## temporal and spatial stranding patterns of marine megafauna
##
## ICES Journal of Marine Science - 2022
##
## Code by Jonatas F. H. Prado & Nicholas W. Daudt

## This script builds figure3 showing stranding rates 
## (individuals/1000 km of coastline) by month and polygon 
## for each tetrapod group (Reptilia, Aves and Cetacea)      

################################################################################

# Libraries ####
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

################################################################################

## Open stranding dataset 
dfSumFinal <- read.csv2("./data_out/dfSumFinal.csv")

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

## End