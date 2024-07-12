#map of 2023 enclosure study

library(viridis)
library(sf)
library(dplyr)
library(ggplot2)
library(spData)
library(here)
library(readr)
library(janitor)
library(deltamapr)
library(cowplot)
library(ggspatial)

library(here)
library(readxl)

latlon <- read_csv("biofouling/cagelocations2023.csv")



stations_sf23 <- st_as_sf(latlon, coords = c("Longitude", "Latitude"), crs = 4326)
data("us_states", package = "spData")
California = dplyr::filter(us_states, NAME == "California")
stations_sf_4269 <- st_transform(stations_sf23, crs = st_crs(California))
WW_Watershed_4269 <- st_transform(WW_Watershed, crs = st_crs(California))

(smelt_map <- ggplot() +
    geom_sf(data = WW_Watershed_4269, fill = "slategray1", colour = "gray80", alpha = 0.8, inherit.aes = FALSE) +
    geom_sf(data = stations_sf_4269, inherit.aes = FALSE, size = 4, color = "black", shape = c(21, 21,15,  15),
            fill = "orangered2")+
    geom_sf_label(data = stations_sf_4269, aes(label = factor(Location)), inherit.aes = FALSE, size = 4, color = "black",
                  nudge_x = c(-0.06, -0.05),
                  nudge_y = c(0.01, 0.02))+
    annotate(geom = "text", x = -121.75, y = 38.10, size = 3.2, label = "Sacramento River", fontface = "italic",angle = 36) +
    annotate(geom = "text", x = -121.62, y = 38.09, size = 3.2, label = "San Joaquin River", fontface = "italic", angle = 10) +
    annotate(geom = "text", x = -122.02, y = 38.12, size = 3.2, label = "Grizzly Bay", fontface = "italic") +
   # annotate(geom = "text", x = -121.68, y = 38.26, size =3.2 , label = "Liberty Island", fontface = "italic") +
    scale_x_continuous(limits = c(-122.1, -121.55)) +
    scale_y_continuous(limits = c(38, 38.3))+
    annotation_north_arrow(location = "tl", which_north = "true",
                           pad_x = unit(.005, "in"), pad_y = unit(0.15, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("slateblue2", "white","slateblue2", "white"), text_cex = 1.1)+
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_text(size = 13) ,
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust =1, vjust = 1)))
smelt_map

ggsave("plots/smeltcagemap2023.tiff", device = "tiff", width =8, height = 7)

insetbbox = st_bbox(c(xmin = -122.2, xmax = -121.45, ymax = 37.95, ymin = 38.4), crs = st_crs(4326))
box_sf <- st_as_sfc(insetbbox)

(inset <- ggplot() +
    geom_sf(data = California, fill = "white") +
    geom_sf(data = WW_Delta, colour = "steelblue4", size = 0.3) +
    geom_sf(data = box_sf, fill = NA, color = "red", size = 0.5) +
    
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background =  element_blank(),
          panel.background = element_rect(color = "black", fill = "white")))

(gg_inset_map = ggdraw() +
    draw_plot(smelt_map) +
    draw_plot(inset, x = 0.38, y = 0.6,  height = 0.3))


tiff(filename = here::here("biofouling", "map_sites.tiff"), width = 8, height = 7, units = "in", pointsize = 12, family = "sans", res = 300, compress = "lzw")
gg_inset_map

dev.off()

