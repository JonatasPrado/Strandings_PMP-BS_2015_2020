
## Intensive and wide-ranging beach surveys uncover 
## temporal and spatial stranding patterns of marine megafauna
##
## ICES Journal of Marine Science - 2022
##
## Code by Jonatas F. H. Prado & Nicholas W. Daudt

## This script builds figure2 showing species richness by 
## month and polygon for each tetrapod group (Reptilia, Aves and Cetacea)     

################################################################################

# Libraries ####
library(dplyr)
library(ggplot2)
library(gridExtra)

################################################################################

## Open stranding dataset
dfFinal <- read.csv2("./data_out/dfFinal.csv")

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

## End
