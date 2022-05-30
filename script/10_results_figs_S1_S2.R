
## Intensive and wide-ranging beach surveys uncover 
## temporal and spatial stranding patterns of marine megafauna
##
## ICES Journal of Marine Science - 2022
##
## Code by Jonatas F. H. Prado & Nicholas W. Daudt

# This script builds figure S1 and S2 showing the proportion of 
# stranding rates, by month and polygon, respectively, for the selected Reptilia, 
# Aves and Cetacea species (i.e. species with numeric frequency>1% and n>100)

################################################################################

# Libraries ####
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)

################################################################################

## Open stranding dataset
dfSumFinal <- read.csv2("./data_out/dfSumFinal.csv")

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

## End