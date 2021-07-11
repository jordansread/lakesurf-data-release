

library(rmapshaper);library(sf);library(maps)
library(viridis)
library(tidyverse);library(lubridate)

proj<-"+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=999999.9999898402 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"

# prep data ------------------------------------------------------------

## temperature observation data
sites_sf <- read.csv("out_data/lake_metadata.csv") %>%
  filter(num_obs > 0) %>%
  st_as_sf(coords = c("lake_lon_deg", "lake_lat_deg"), crs = 4326) %>%
  st_transform(proj)
#str(counts)

# make hex grid -----------------------------------------------------------

## spatially summarize state polygons to make map outline of usa
usa_sf <- maps::map('usa', fill=TRUE) %>%
  st_as_sf() %>%
  st_transform(proj)

## make hex tesselation of CONUS

site_grid <- usa_sf %>%
  st_make_grid(cellsize = 500000, what="polygons", square = FALSE) %>%
  st_as_sf() %>%
  mutate(geometry=x) %>%
  mutate(hex  =  as.character(seq.int(nrow(.))))
glimpse(site_grid)

## intersect the hex grid with observation data
site_hex <- site_grid %>%
  select(-x) %>%
  st_intersection(sites_sf) %>%
  st_drop_geometry() %>%
  group_by(hex) %>%
  summarize(n_obs_hex = sum(num_obs),
            med_rmse = ifelse(sum(!is.na(RMSE_EALSTM)) >= 3, median(RMSE_EALSTM, na.rm = TRUE), NA),
            n_sites = length(unique(site_id)))
glimpse(site_hex)

# join counts back with spatial grid
sitey <- site_grid %>% left_join(site_hex, by='hex')

# plot map ----------------------------------------------------------

scale_breaks <- c(1, 1000)

sitey%>%
  ungroup()%>%
  ggplot()+
  geom_sf(aes(fill=as.numeric(med_rmse)), color=NA, size=.2) +
  scale_fill_viridis_c(option='viridis',
                       limits = c(0.5, 4),
                       direction=1, na.value=NA)+
  theme_void()+
  theme(legend.position="bottom",
        plot.background = element_rect(fill=NA),
        legend.text = element_text(color="white"),
        legend.title=element_text(color="white"))+
  guides(fill=guide_legend(title="Observations",
                           title.position="top",
                           barwidth = 10,
                           barhieght= 5,
                           show.limits = TRUE,
                           ticks=FALSE,
                           direction = "horizontal"))

ggsave("temp_hex_map.png", width=32, height = 20)
