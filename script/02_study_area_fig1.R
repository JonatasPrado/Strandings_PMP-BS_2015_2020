
## Intensive and wide-ranging beach surveys uncover 
## temporal and spatial stranding patterns of marine megafauna
##
## ICES Journal of Marine Science - 2022
##
## Code by Jonatas F. H. Prado & Nicholas W. Daudt

## This script builds figure 1 that represents the study area. See the main text 
## for rationale and detailed explanations

################################################################################

# Libraries ####
library(dplyr)
library(ggplot)
library(sf)
library(ggspatial)
library(cowplot)

################################################################################

## Open spatial features
brazil <- 
  sf::read_sf(dsn = "./data/shape_brazil", layer = "brasil") %>% 
  sf::st_set_crs(4326)

thisWorkStretches <-
  sf::read_sf("./data_out/thisWorkStretches.gpkg")

thisWorkStretches <- 
  sf::st_transform(thisWorkStretches, crs = 4326)

sectorsPolygonLines <-
  sf::read_sf("./data_out/sectorsPolygonLines.gpkg")

sectorsPolygon <-
  sf::read_sf("./data_out/sectorsPolygon.gpkg")

## First do the main plot  

# Latitudinal division of the study area 
lat_division <- 
  ggplot() + 
  ggspatial::geom_sf(data = brazil) + 
  geom_sf(data = thisWorkStretches, 
          color = "red", show.legend = F,size=1.3) + 
  geom_sf(data = sectorsPolygonLines, 
          color = "black", show.legend = F) + 
  labs(x = "", y = "") + 
  coord_sf(xlim = c(-51, -44), ylim = c(-28.5, -23)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.title = element_text(colour = "black", size = 18), 
        axis.text = element_text(colour = "black", size = 14), 
        axis.line = element_line(colour = "black")) + 
  ggspatial::annotation_scale(location = "br", width_hint = 0.3,
                   text_cex = 1,height = unit(0.2, "cm")) +
  ggspatial::annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.08, "in"), pad_y = unit(0.3, "in"),
                         style = ggspatial::north_arrow_fancy_orienteering,
                         height = unit(1.6, "cm"),width = unit(1.6, "cm")) + 
  annotate(geom = "text", x = -46, y = -25.77, label = "Atlantic Ocean", 
           color = "grey22", size = 5) + 
  annotate(geom = "text", x = -47.5, y = -23.10, label = "São Paulo (SP)", 
           color = "grey22", size = 5) + 
  annotate(geom = "text", x = -49.8, y = -25.5, label = "Paraná", 
           color = "grey22", size = 5) + 
  annotate(geom = "text", x = -50, y = -27.3, label = "Santa Catarina (SC)", 
           color = "grey22", size = 5) +
  geom_sf_label(data= sectorsPolygon, aes(label = id_polygon),nudge_x = 2,
                nudge_y = 0)

# Second, do the inset plot
brazil_inset <-  
  ggplot() + 
  ggspatial::geom_sf(data = sf::st_union(brazil),fill= "black") +
  coord_sf(xlim = c(-73, -35), ylim = c(-33, 5)) + 
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_rect(fill="gray"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) + 
  theme_void() +
  ggplot2::geom_rect(data = brazil, 
                     aes(xmin = -44,5, xmax = -50, ymin = -28.6, ymax = -23.4), 
                     colour = "dodgerblue", fill = "NA",size = 1.5) +
  annotate(geom = "text", x = -51, y = -12, label = "Brazil", 
           color = "white", size = 5) 

## Final map
figure1 <-
  cowplot::ggdraw() +
  cowplot::draw_plot(lat_division) +
  cowplot::draw_plot(brazil_inset, x = 0.124, y = 0.67, width = 0.25, height = 0.25)

ggplot2::ggsave(figure1, 
                filename = "./results/figure1.tiff", 
                height = 170 , width = 170, units = "mm", dpi = 400,
                compression = "lzw")

## End