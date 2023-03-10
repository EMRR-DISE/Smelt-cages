
#animated recapture map


#data manipulation libraries
library(tidyverse)
library(readxl)
library(lubridate)

#GIS libraries
library(ggmap)
library(deltamapr)
library(sf)

#these ones for animated plots
library(gganimate)
library(gifski)
library(transformr)

recapssf2 = st_as_sf(filter(recaps3, !is.na(Latitude)), coords = c("Longitude", "Latitude"), crs = 4326)

timePlot = ggplot(data = recapssf2, aes(group = fishID))+
  geom_sf(data = WW_Delta, color = "grey", fill = "lightblue", inherit.aes = FALSE)+ 
  geom_sf( aes(color = Type, shape = ReleaseEvent), size = 4)+
  scale_color_brewer(palette = "Dark2")+
  scale_shape_manual(values = c(15, 16, 17, 18, 8), 
labels = c("11/29 Rio Vista Hard", "1/18 - Rio Vista Hard", 
           "1/18 - Rio Vista Trailer", "1/25 SDWSC hard", "1/26 - SDWSC soft"))+
  theme_bw()+
  labs(title = 'Date: {frame_time}') +
  scale_x_continuous(limits = c(-122.2, -121.4)) +
  scale_y_continuous( limits = c(37.7, 38.4))+

  shadow_wake(wake_length = 3)+
  transition_time(Date)

animate(timePlot, height = 500, width = 800, fps = 3, duration = 30,
        end_pause = 10, res = 100)
anim_save("smelt.gif")

ggplot(data = filter(HABssf, Date < as.Date("2017-02-01")))+
  geom_sf(data = WW_Delta, color = "grey", fill = "lightblue")+ 
  geom_sf( aes(color = Microcystis))+
  scale_color_viridis_b(option = "B")+
  theme_bw()+
  labs(title = 'Date: {frame_time}') +
  scale_x_continuous(limits = c(-122.4, -121.2)) +
  scale_y_continuous( limits = c(37.65, 38.4))


anim <- ggplot(mtcars, aes(factor(gear), mpg)) +
  geom_boxplot() +
  transition_states(gear, 2, 1)

# Fade-in, fly-out
anim1 <- anim +
  enter_fade() +
  exit_fly(x_loc = 7, y_loc = 40)
anim1
