library(tidyverse)
library(here) 
library(sf) #simple features is the standard package for working with vector data
library(ggspatial) #for scale bars and north arrows etc. 

wa.us <- here("WA_state_outline", "WA_state_outline.shp") %>% 
  read_sf() 

bc.ca <- here("Canada_outline", "Canada_outline.shp") %>% 
  read_sf() %>% 
  filter(PRUID == 59)

seattle <- data.frame(
  long = -122.317917,
  lat = 47.610568, 
  city = "Seattle")

ggplot() +
  geom_sf(data = bc.ca,
          linewidth = 0.5,
          fill = "grey70",
          color = "grey40") +
  geom_sf(data = wa.us, 
          linewidth = 0.5,
          fill = "grey70",
          color = "grey40") +
  geom_text(data = seattle, 
            aes(x = long, y = lat, label = city), 
            color = "grey40",
            hjust = -.1,
            size = 3) +
  coord_sf(xlim = c(-123.70, -121.75),
           ylim = c(46.9, 49.1),
           crs = 4326, #can change coordinate reference system depending on which projection you prefer (but will need to adjust x and y lims to match the units of the crs e.g. decimal degrees, ft, meters, etc.)
           expand = FALSE) +
  theme(panel.background = element_rect(fill = "lightblue2"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "") +
  annotation_scale(location = "br", style = "ticks") + 
  annotation_north_arrow(location = "br", which_north = "true",
                         height = unit(1, "cm"), 
                         pad_y = unit(0.75, "cm"),
                         style = north_arrow_minimal)

#if something is taking forever to render in the plotting window, sometimes it's faster to save and then view, for example:
#ggsave(plot = mymap, here("outputs", "Puget_Sound_map.pdf")) 


