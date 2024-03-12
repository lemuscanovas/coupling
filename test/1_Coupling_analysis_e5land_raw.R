
# -------------------------------------------------------------------------
## Title: Soil moisture - temperature coupling using E5-Land
## Based on Miralles et al. 2012
## Date: 10/05/2023
# -------------------------------------------------------------------------
# Common libraries

library(terra)
library(tidyverse)
library(lubridate)
library(sf)
library(tidyterra)
library(cptcity)
library(mpsTemplates)
source("funs/coupling_funs.R")
## Test 1: Using Evaporation (E) and Potential E (Ep) to compute Pi and pi. 
## Temperature data from E5 Land, resampled to 0.25.
## Checking algorithm just for 2022.


# Loading T2m from E5 Land, computing daily mean and resampling to 0.2
tas <- list.files("data/reanalysis/era5_land/", pattern = "tas",
                  full.names = T) %>% rast()
yrs5022 <- which(year(time(tas)) <2023)
tas <- tas[[yrs5022]]

tas <- tas_03[[-c(which(noms_tas_03 %in% str_subset(noms_tas_03, "t2m_expver")))]]


## Process of correcting tas of 2023/03
tas_03_2 <- tas_03[[c(which(noms_tas_03 %in% str_subset(noms_tas_03, "t2m_expver=1")))]][[1]]
tas_03_3 <- tas_03[[c(which(noms_tas_03 %in% str_subset(noms_tas_03, "t2m_expver=5")))]][[-1]]
tas_03 <- c(tas_03_1, tas_03_2, tas_03_3)

# 
# tas_03_2023cor <- c(tas_03_2,tas_03_3) 
# datetimes_032023 <- time(tas_03_2023cor)
# 
# tas_03_2023cor <- tas_03_2023cor %>% as.data.frame(xy = T)
# names(tas_03_2023cor)[3:ncol(tas_03_2023cor)] <- datetimes_032023
# 
# date_table <- tibble(time_id = names(tas_03_2023cor)[-c(1:2)], time = datetimes_032023)
# 
# tas_03_2023cor <- tas_03_2023cor %>% 
#   pivot_longer(3:ncol(.), names_to = "time_id", values_to = "t2m") %>%
#   inner_join(date_table, by = "time_id") %>%
#   select(-time_id) %>%
#   pivot_wider(names_from = time, values_from = t2m) %>%
#   rast(type = "xyz")
# crs(tas_03_2023cor) <- crs(tas_03_1)
# time(tas_03_2023cor) <- datetimes_032023
# tas_03_2023cor <- tas_03_2023cor %>% project(tas_03_1)
## end of correction
# tas_03 <- c(tas_03_1, tas_03_2023cor)

tas_04 <- list.files("data/reanalysis/era5_land/", pattern = "tas_04",
                     full.names = T) %>% rast()
tas_05 <- list.files("data/reanalysis/era5_land/", pattern = "tas_05",
                     full.names = T) %>% rast()
tas <- c(tas_03, tas_04,tas_05)

# tibble(time(tas)) %>% setNames("time") %>% 
#   mutate(hour = hour(time), day = day(time),year = year(time)) %>% 
#   distinct(year) %>% view


dom <- c(-20,15,25,50)
# Sorting dates
tas <-  tas[[order(time(tas))]]
h2mean <- which(hour(time(tas)) %in% c(0,6,12,18))
tas2mean <- tas[[h2mean]]
dates <- time(tas2mean)

plot(tas2mean[[1]])
tas2mean <- tas2mean %>% crop(dom)



# Computing daily mean
system.time(tasmean <- tapp(tas2mean,as.factor(as_date(dates)),"mean") -273.15)
time(tasmean) <- as_date(str_remove(names(tasmean),"X"))
dom <- ext(tasmean)

# Loading boundaries
world_admin <- rnaturalearth::ne_states(
  country = c("spain","france"),
  returnclass = "sf") %>%
  st_crop(dom) %>%
  vect()
world <- rnaturalearth::ne_countries(
  country = c("spain","france"),scale = 10,
  returnclass = "sf") %>%
  st_crop(dom) %>%
  vect()
huesca_encamp <- tibble(observation = c("Huesca","Encamp","Tarbes"),
                        x = c(-0.4,1.55, 0.0),
                        y = c(42.14,42.52,43.2)) %>%
  st_as_sf(coords = c("x","y"),crs = "EPSG:4326")

# Loading Evaporation (E) (actual or total E)
E <- list.files("data/reanalysis/era5_land/", pattern = "tev",
                full.names = T)  %>% rast()
time(E) <- as_date(time(E))
yrs5022_E <- which(year(time(E)) <2023)
E <- E[[yrs5022]]
# Sorting dates
E <-  E[[order(time(E))]]
dates <- time(E) - days(1)
time(E) <- dates
E_NA <- E[[1]]
E_NA[E_NA<10000000] <-NA
time(E_NA) <- as_date("2023-05-05")
E <- c(E[[-1]],E_NA) * 1000
E <- -E
E[[time(E) %in% time(tasmean)]]

E <- project(E, tasmean)
# Loading Potential Evaporation (Ep)
Ep <- list.files("data/reanalysis/era5_land//", pattern = "pev",
                 full.names = T)  %>% rast()

# Sorting dates
# Sorting dates
Ep <-  Ep[[order(time(Ep))]]
dates <- time(Ep) - days(1)
time(Ep) <- dates
Ep_NA <- Ep[[1]]
Ep_NA[Ep_NA<10000000] <-NA
time(Ep_NA) <- as_date("2023-05-05")
Ep <- c(Ep[[-1]],Ep_NA) * 1000
Ep <- -Ep
Ep <- project(Ep, tasmean)


# Loading Solar Net Surface Radiation
Rn <- list.files("data/reanalysis/era5_land//", pattern = "snsr",
                 full.names = T)  %>% rast()

# Sorting dates
Rn <-  Rn[[order(time(Rn))]]
dates <- time(Rn) - days(1)
time(Rn) <- dates
Rn_NA <- Rn[[1]]
Rn_NA[Rn_NA<10000000] <-NA
time(Rn_NA) <- as_date("2023-05-05")
Rn <- c(Rn[[-1]],Rn_NA)
Rn <- -Rn
Rn <- project(Rn, tasmean)


# SUBSTRACTING 28 OF FEB TO ALL T YEARS -----------------------------------
library(operators)
tasmean[[-c(time(E)  %!in% time(tasmean))]]

# Computing long term coupling for summer 1950-2022 ----------------------------
# Computing latent heat of vaporisation (Lv)
Lv <- lambda(tasmean)

# Compute H in w/m2
H <- calculate_H(Rn, Lv, E)
H_ <- app(H,scale)
time(H_) <- time(H)
# Hp calculation in W m-2
Hp <- calculate_H(Rn, Lv, Ep)
Hp_ <- app(Hp,scale)
time(Hp_) <- time(Hp)

T_ = app(tasmean,scale)
time(T_) <- time(tasmean)
plot(T_)

dates_june <- seq(from = as_date("2023-04-18"),to= as_date("2023-04-30"), by = "day")
plot((H_[[which(as_date(time(E)) %in% dates_june)]] - Hp_[[which(as_date(time(E)) %in% dates_june)]])*T_[[which(as_date(time(E)) %in% dates_june)]])
plot((H_ - Hp_)*T_ )
# Calculate P for SpatRaster time series (long-term value) (equation1-2)

PI <- calculate_P(H,Hp,tasmean)
plot(PI); plot(world_admin, add =T)

ggplot() +
  # geom_spatraster(data = bs_tx[[1:2]]) +
  geom_spatraster(data = PI) +
  geom_spatvector(data = world, fill = NA, size = 0.4, color = "black")+
  geom_spatvector(data = world_admin, fill = NA, size = 0.4, color = "black")+
  geom_spatvector(data = huesca_encamp, size = 0.8, color = "magenta")+
  geom_spatvector(data = huesca_encamp, size = 0.8,shape = 21, 
                  color = "black", linewidth = 0.4)+
  scale_fill_gradientn(colours = cpt(pal = "ncl_WhiteBlueGreenYellowRed",rev = F), 
                       limits = c(-0,0.62),
                       breaks = seq(-1,0.9,0.1),
                       guide = guide_colorbar(barheight = 0.5,
                                              barwidth = 10,
                                              title.position = "top"),
                       name = "[Π]",
                       na.value = "white")+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(40.5,44.5))+
  theme_mps_noto()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom",
        strip.text  = element_text(face = "plain", size = 10),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9))
ggsave("png/preliminars//long_term_coupling_1950_2022_JJA.png",width = 9.5,height = 10,
       units = "cm",dpi = 600)

# Calculate p (equation 3-4)
dates_june <- seq(from = as_date("2022-06-12"),to= as_date("2022-06-19"), by = "day")
dates_july <- seq(from = as_date("2022-07-15"),to= as_date("2022-07-19"), by = "day")
pi <- calculate_p(tasmean,H,Hp)
writeCDF(pi,filename = "data/reanalysis/data_coupling/metric/pi_miralles_1950_2022_JJA.nc",
         varname = "pi",compression = 7)
pi_event_june <- app(pi[[which(as_date(time(pi)) %in% dates_june)]],"mean")
names(pi_event_june) <- "June HW"
pi_event_july <- app(pi[[which(as_date(time(pi)) %in% dates_july)]],"mean")
names(pi_event_july) <- "July HW"

pi_event <- c(pi_event_june,pi_event_july)
plot(pi_event)
plot(world_admin, add =T)

ggplot() +
  geom_spatraster(data = pi_event) +
  geom_spatvector(data = world, fill = NA, size = 0.4, color = "black")+
  geom_spatvector(data = world_admin, fill = NA, size = 0.4, color = "black")+
  geom_spatvector(data = huesca_encamp, size = 0.8, color = "magenta")+
  geom_spatvector(data = huesca_encamp, size = 0.8,shape = 21, 
                  color = "black", linewidth = 0.4)+
  scale_fill_stepsn(colours = cpt(pal = "ncl_WhiteBlueGreenYellowRed",rev = F), 
                       limits = c(-0,8),
                       breaks = seq(-0,9,1),
                       guide = guide_colorbar(barheight = 0.5,
                                              barwidth = 10,
                                              title.position = "top"),
                       name = "[π]",
                       na.value = "white")+
  facet_wrap(~lyr)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(40.5,44.5))+
  theme_mps_noto()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom",
        strip.text  = element_text(face = "plain", size = 10),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9))
ggsave("png/preliminars//event_coupling_JuneHW_JulyHW2022.png",width = 17,height = 10,
       units = "cm",dpi = 600)
