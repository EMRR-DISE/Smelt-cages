
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

recapssf2 = st_as_sf(filter(recaps3, !is.na(Latitude)), coords = c("Longitude", "Latitude"), crs = 4326) %>%
  arrange(Date)

# timePlot = ggplot(data = recapssf2, aes(group = fishID))+
#   geom_sf(data = WW_Delta, color = "grey", fill = "lightblue", inherit.aes = FALSE)+ 
#   geom_sf( aes(color = Type, shape = ReleaseEvent), size = 4)+
#   scale_color_brewer(palette = "Dark2")+
#   scale_shape_manual(values = c(15, 16, 17, 18, 8), 
# labels = c("11/29 Rio Vista Hard", "1/18 - Rio Vista Hard", 
#            "1/18 - Rio Vista Trailer", "1/25 SDWSC hard", "1/26 - SDWSC soft"))+
#   theme_bw()+
#   labs(title = 'Date: {frame_time}') +
#   scale_x_continuous(limits = c(-122.2, -121.4)) +
#   scale_y_continuous( limits = c(37.7, 38.4))+
# 
#   shadow_wake(wake_length = 3)+
#   transition_time(Date)

recapssf2 = mutate(recapssf2, ReleaseEvent = case_when(is.na(ReleaseEvent) ~ "Wild",
                                                       TRUE ~ ReleaseEvent))

timePlot = ggplot(data = recapssf2, aes(group = fishID))+
  geom_sf(data = WW_Delta, color = "grey", fill = "lightblue", inherit.aes = FALSE)+
  geom_sf( aes(color = Type, shape = ReleaseEvent), size = 4)+
  scale_color_brewer(palette = "Dark2")+
  scale_shape_manual(values = c(15, 16, 17, 18, 8,9),
labels = c("11/29 Rio Vista Hard", "1/18 - Rio Vista Hard",
           "1/18 - Rio Vista Trailer", "1/25 SDWSC hard", "1/26 - SDWSC soft", "Wild"))+
  theme_bw()+
  labs(title = 'Date: {frame_time}') +
  scale_x_continuous(limits = c(-122.2, -121.4)) +
  scale_y_continuous( limits = c(37.7, 38.4))+
  shadow_mark(alpha = 0.5, size = 2)+
  transition_time(Date)

animate(timePlot, height = 500, width = 800, fps = 3, duration = 30,
        end_pause = 10, res = 100)
anim_save("smelt3.gif")




#Do second version with just DWSC releases
dwsc = filter(recapssf2, ReleaseEvent %in% c("3a", "3b"))

timePlot2 = ggplot(data = dwsc, aes(group = fishID))+
  geom_sf(data = WW_Delta, color = "grey", fill = "lightblue", inherit.aes = FALSE)+
  geom_sf( aes(color = Type, shape = ReleaseEvent), size = 4)+
  scale_color_brewer(palette = "Dark2")+
  scale_shape_manual(values = c(15, 16),
                     labels = c( "1/25 SDWSC hard", "1/26 - SDWSC soft"))+
  theme_bw()+
  labs(title = 'Date: {frame_time}') +
  scale_x_continuous(limits = c(-122.2, -121.4)) +
  scale_y_continuous( limits = c(37.7, 38.4))+
  shadow_mark(alpha = 0.5, size = 2)+
  transition_time(Date)

animate(timePlot2, height = 500, width = 800, fps = 3, duration = 30,
        end_pause = 10, res = 100)
anim_save("smelt4.gif")
