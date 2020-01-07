# Install and load the required packages ---------------------------------------
if (!require("pacman")) install.packages("pacman")
pkgs = c("dplyr")
pacman::p_load(pkgs, character.only = T)

# Load raw data and functions --------------------------------------------------
sth <- readr::read_csv("data/raw_sth_data.csv")

source("R/functions.R")

# Data cleaning ----------------------------------------------------------------

# Check number of unique villages and unique coordinates
# They are not the same

lunique(sth$Village)
lunique(cbind(sth$GPS_DEVICE_LONG, sth$GPS_DEVICE_LATI))

# More coordinates than villages 
# Let's see wath's going on

cases <- sth %>% 
  group_by(village = Village, long = GPS_DEVICE_LONG, lat = GPS_DEVICE_LATI) %>% 
  summarise(nobs = n()) 


case1 <- cases %>% 
  group_by(village) %>% 
  summarise(n = n()) %>% 
  filter(n > 1) %>% 
  left_join(cases, by = c("village")) %>% 
  select(-n)
case1$case <- 1 # same village name but different coordinates

case2 <- cases %>% 
  group_by(long, lat) %>% 
  summarise(n = n()) %>% 
  filter(n > 1 & n < 9) %>% 
  left_join(cases, by = c("long", "lat")) %>% 
  select(-n)
case2$case <- 2 # same coordinates but differnt village name

case2 <- dplyr::bind_cols(case2[, 3], case2[, -3])

cases <- rbind(case1, case2)

readr::write_csv(cases, "output/village_coords.csv")

# Map the cases 
cases$id <- as.factor(as.numeric(as.factor(cases$village)))
library(mapview)
library(sf)
cases_sf <- cases %>% 
  na.omit() %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

plot(cases_sf)

map <- mapview(cases_sf, zcol = "id", legend = F, labels = NULL, cex = 12) %>% 
  addStaticLabels(label = cases_sf$id)

withr::with_dir("output", mapshot(map, "map.html"))

# To tidy ----------------------------------------------------------------------
library(geoR)
library(dplyr)

sth$POS_ASCARIS <- ifelse(sth$POS_ASCARIS == "YES", 1, 0)
sth$POS_TRICHURIS <- ifelse(sth$POS_TRICHURIS == "YES", 1, 0)
sth$POS_HOOKWORM <- ifelse(sth$POS_ASCARIS == "YES", 1, 0)

sth$anypos <- apply(sth[, c("POS_ASCARIS", "POS_TRICHURIS", "POS_HOOKWORM")], 1, max)
sth_grouped <- sth %>% 
  group_by(long = GPS_DEVICE_LONG, lat = GPS_DEVICE_LATI) %>% 
  summarise(anypos = sum(anypos), examined = n()) %>% 
  mutate(anyprev = anypos / examined) %>% 
  filter(!is.na(long))

library(sf)
sth_sf <- st_as_sf(sth_grouped, coords = c("long", "lat"), crs = 4326)

library(tmap)


map <- tm_shape(sth_sf) +
  tm_symbols(col = "anyprev", style = "cont", pal = "-viridis")

tmap_mode("plot")
map
as.geodata(cbind(sth_grouped$long, sth_grouped$lat, sth_grouped$anyprev))


sthpos <- filter(sth, anypos > 0)

sth_list <- split(sthpos, f = as.factor(sthpos$GPS_DEVICE_LONG + 1000 * sthpos$GPS_DEVICE_LATI))

plot()
sthpos$eggs <- as.numeric(ifelse(sthpos$EPG_ASCARIS_FINAL == ".", 0, sthpos$EPG_ASCARIS_FINAL))
ll <- lapply(sth_list, function(x) cumsum(x$eggs))

max(sapply(ll, max))

plot(ll[[8]], 1:length(ll[[8]]) / length(ll[[8]]), type = "l", ylim = c(0, 1), 
     xlim = c(0, 160000))

i <- 1
lines(ll[[i]], 1:length(ll[[i]]) / length(ll[[i]]), type = "l")
i <- i + 1
lapply(ll, function(x) plot())

