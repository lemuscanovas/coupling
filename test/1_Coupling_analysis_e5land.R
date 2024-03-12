
# -------------------------------------------------------------------------
## Title: Soil moisture - temperature coupling using E5-Land
## Based on Miralles et al. 2012
## Date: 10/05/2023
## Info: E5 Land produces some errors in the last updated data
## In this sense, we extracted only April data and first days of may 2023
# -------------------------------------------------------------------------
# Common libraries

library(terra)
library(tidyverse)
library(lubridate)
library(sf)
library(tidyterra)
library(cptcity)
library(mpsTemplates)
library(colorplaner)
library(pracma)
source("funs/coupling_funs.R")
source("funs/paleta_bivariada.R")
source("funs/detrend_fun.R")
## Test 1: Using Evaporation (E) and Potential E (Ep) to compute Pi and pi. 
## Temperature data from E5 Land, resampled to 0.25.
## Checking algorithm just for 2022.

world <- rnaturalearth::ne_countries(scale = 50,returnclass = "sf")
# Loading T2m from E5 Land, computing daily mean and resampling to 0.2
tas <- list.files("data/reanalysis/era5_land/", pattern = "tas",
                  full.names = T) %>% rast()
yr <- which(year(time(tas)) %in% 1950:2022)
tas_1950_2022 <- tas[[yr]]

yr23 <- which(year(time(tas)) %in% 2023)

tas2023 <-tas[[yr23]]
tas2023 <- tas2023[[-c(which(names(tas2023) %in% str_subset(names(tas2023), "t2m_expver")))]]

tas <- c(tas_1950_2022, tas2023)
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
tasmean <- tasmean[[-nlyr(tasmean)]] # eliminem ultim dia q esta a mtiges (06/05)
dom <- ext(tasmean)
# tasmean <- app(tasmean, detrend_pracma, k = 2)

print(paste("tasmean layers",":",nlyr(tasmean)))


# Loading Evaporation (E) (actual or total E)
E <- list.files("data/reanalysis/era5_land/", pattern = "tev",
                full.names = T)  %>% rast()
time(E) <- as_date(time(E))
yr <- which(year(time(E)) %in% 1950:2022)
E_1950_2022 <- E[[yr]]

yr23 <- which(year(time(E)) %in% 2023)
E2023 <-E[[yr23]]
E2023 <- E2023[[-c(which(names(E2023) %in% str_subset(names(E2023), "e_expver")))]]

E <- c(E_1950_2022, E2023)
# Sorting dates
E <-  E[[order(time(E))]]
dates <- time(E) - days(1)
time(E) <- dates
E_NA <- E[[1]]
E_NA[E_NA<10000000] <-NA
time(E_NA) <- as_date("2023-05-05")
E <- c(E[[-1]],E_NA) * 1000
E <- -E
E <- project(E, tasmean)
# E <- app(E, detrend_pracma, k = 2)
print(paste("E layers:",nlyr(E)))

# Loading Potential Evaporation (Ep)
Ep <- list.files("data/reanalysis/era5_land/", pattern = "pev",
                full.names = T)  %>% rast()
time(Ep) <- as_date(time(Ep))
yr <- which(year(time(Ep)) %in% 1950:2022)
Ep_1950_2022 <- Ep[[yr]]

yr23 <- which(year(time(Ep)) %in% 2023)
Ep2023 <-Ep[[yr23]]
Ep2023 <- Ep2023[[-c(which(names(Ep2023) %in% str_subset(names(Ep2023), "pev_expver")))]]

Ep <- c(Ep_1950_2022, Ep2023)


Ep <-  Ep[[order(time(Ep))]]
dates <- time(Ep) - days(1)
time(Ep) <- dates
Ep_NA <- Ep[[1]]
Ep_NA[Ep_NA<10000000] <-NA
time(Ep_NA) <- as_date("2023-05-05")
Ep <- c(Ep[[-1]],Ep_NA) * 1000
Ep <- -Ep
Ep <- project(Ep, tasmean)
# Ep <- app(Ep, detrend_pracma, k = 2)
print(paste("Ep",nlyr(Ep)))


# Loading Solar Net Surface Radiation
Rn <- list.files("data/reanalysis/era5_land//", pattern = "snsr",
                 full.names = T)  %>% rast()
Rn <- Rn[[order(time(Rn ))]]
time(Rn) <- as_date(time(Rn))
yr <- which(year(time(Rn)) %in% 1950:2022)
Rn_1950_2022 <- Rn[[yr]]

yr23 <- which(year(time(Rn)) %in% 2023)
Rn2023 <-Rn[[yr23]]
Rn2023 <- Rn2023[[-c(which(names(Rn2023) %in% str_subset(names(Rn2023), "ssr_expver")))]]

Rn <- c(Rn_1950_2022, Rn2023[[-nlyr(Rn2023)]])

# Sorting dates
Rn <-  Rn[[order(time(Rn))]]
dates <- time(Rn) - days(1)
time(Rn) <- dates
Rn_NA <- Rn[[1]]
Rn_NA[Rn_NA < 10000000000] <-NA
time(Rn_NA) <- as_date("2023-05-05")
Rn <- c(Rn[[-1]],Rn_NA)
Rn <- -Rn
Rn <- project(Rn, tasmean)
# Rn <- app(Rn, detrend_pracma, k = 2)
print(paste("Rn any",nlyr(Rn)))



# Loading Thermal Net Surface Radiation
Rt <- list.files("data/reanalysis/era5_land//", pattern = "sntr",
                 full.names = T)  %>% rast()
Rt <-  Rt[[order(time(Rt))]]
Rt <- Rt[[1:6788]]

time(Rt) <- as_date(time(Rt))
yr <- which(year(time(Rt)) %in% 1950:2022)
Rt_1950_2022 <- Rt[[yr]]

yr23 <- which(year(time(Rt)) %in% 2023)
Rt2023 <-Rt[[yr23]]
Rt2023_04 <- Rt2023[[-c(which(names(Rt2023) %in% str_subset(names(Rt2023), "str_expver")))]][[32:61]] # april
Rt2023_05 <- Rt2023[[c(62,65,67,69,71,73)]] # may first 6 days

Rt2023 <- c(Rt2023_04,Rt2023_05)

Rt <- c(Rt_1950_2022,Rt2023)

# Sorting dates
Rt <-  Rt[[order(time(Rt))]]
dates <- time(Rt) - days(1)
time(Rt) <- dates
Rt_NA <- Rt[[1]]
Rt_NA[Rt_NA<10000000000] <-NA
time(Rt_NA) <- as_date("2023-05-05")
Rt <- c(Rt[[-1]],Rt_NA)
Rt <- -Rt
Rt <- project(Rt, tasmean)
# Rn <- app(Rn, detrend_pracma, k = 2)
print(paste("Rt any",nlyr(Rt)))

Rn = Rn + Rt
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

time(T_) <- as_date(str_remove(names(tasmean),"X"))
time(H_) <- dates 
time(Hp_) <- dates 

## APRIL HW COUPLING
April_HW <- seq(from = as_date("2023-04-22"),to= as_date("2023-04-30"), by = "day")
plot((H_[[which(dates %in% April_HW)]] - Hp_[[which(dates %in% April_HW)]])*T_[[which(as_date(time(T_)) %in% April_HW)]])
plot((H_[[which(dates %in% April_HW)]] - Hp_[[which(dates %in% April_HW)]]))

april_HW_rast <- (H_[[which(dates %in% April_HW)]] - Hp_[[which(dates %in% April_HW)]])*T_[[which(as_date(time(T_)) %in% April_HW)]]
names(april_HW_rast) <- April_HW

# Full pi ts Aprils 
mo <- time(H_) %>% month()
April_dates <- which(mo == 4)

pi = (H_ - Hp_)*T_
energy_terms <- (H_ - Hp_)
energy_terms[energy_terms< 0] <-0
pi[pi <0] <- 0
writeCDF(pi,"outputs/pi_climatology_1950_2023_sw_EU.nc",varname = "pi")

pi_april <- pi[[April_dates]]
names(pi_april) <- time(pi_april)
writeCDF(pi_april,"outputs/pi_climatology_1950_2023_April_sw_EU.nc",varname = "pi",overwrite =T)

energy_april <- energy_terms[[April_dates]]
names(energy_april) <- time(energy_april)
writeCDF(energy_april,"outputs/H_Hp_climatology_1950_2023_April_sw_EU.nc",varname = "Hdif",overwrite =T)



# PLOT
domini <- st_bbox(c(xmin = -7.5, xmax = -2.5, ymax = 38.6, ymin = 37.3), crs = st_crs(4326)) %>% st_as_sfc()
col_temp <- c("white","#7DC971","#E1F166","#FDB34E","#FA4B26","#830024")

sites <- tibble(observation = c("Córdoba","Marrakech"),
                        x = c(-4.8,-8),
                        y = c(37.9,31.6)) %>%
  st_as_sf(coords = c("x","y"),crs = "EPSG:4326")

april_HW_rast_2_plot <- april_HW_rast
april_HW_rast_2_plot[april_HW_rast_2_plot <0] <- 0
april_HW_rast_2_plot[april_HW_rast_2_plot >7] <- 7

first <- april_HW_rast_2_plot[[1:3]] %>% app("mean") %>%
  setNames(c("2023-04-22/24"))
second <- april_HW_rast_2_plot[[5:7]] %>% app("mean") %>%
  setNames(c("2023-04-26/28"))
third <- april_HW_rast_2_plot[[8:9]] %>% app("mean") %>%
  setNames(c("2023-04-29/30"))
compo_HW_rast_2_plot <- c(first, second,third)

ggplot() +
  geom_spatraster(data = april_HW_rast_2_plot) +
  geom_spatvector(data = world, fill = NA, size = 0.4, color = "black")+
  geom_spatvector(data = domini, fill= "transparent",color = "darkred", linewidth = .5)+
  geom_spatvector(data = sites, size = 0.8, color = "magenta")+
  geom_spatvector(data = sites, size = 0.8,shape = 21, 
                  color = "black", linewidth = 0.4)+
  facet_wrap(~lyr,ncol = 3) +
  scale_fill_gradientn(colours = col_temp, 
                       limits = c(0,7),
                       breaks = seq(0,8,1),
                       guide = guide_colorbar(barheight = 0.5,
                                              barwidth = 10,
                                              title.position = "top"),
                       name = "[π]",
                       na.value = "white")+
  scale_x_continuous(expand = c(0,0),limits = c(-20,15))+
  scale_y_continuous(expand = c(0,0), limits = c(30,50))+
  theme_mps_noto(base_family = "Dejavu_Sans")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom",
        strip.text  = element_text(face = "plain", size = 11),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11))
ggsave("png/pi_coupling_april_HW_dt.png",dpi = 600,
       units = "cm",width = 15,height = 15)


ggplot() +
  geom_spatraster(data = compo_HW_rast_2_plot) +
  geom_spatvector(data = world, fill = NA, size = 0.4, color = "black")+
  geom_spatvector(data = domini, fill= "transparent",color = "darkred", linewidth = .5)+
  geom_spatvector(data = sites, size = 0.8, color = "magenta")+
  geom_spatvector(data = sites, size = 0.8,shape = 21, 
                  color = "black", linewidth = 0.4)+
  facet_wrap(~lyr,ncol = 9) +
  scale_fill_gradientn(colours = col_temp, 
                       limits = c(0,7),
                       breaks = seq(0,10,1),
                       guide = guide_colorbar(barheight = 5.3,frame.colour = "black",
                                              barwidth = 0.5,
                                              title.position = "top"),
                       name = "π",
                       na.value = "white")+
  scale_x_continuous(expand = c(0,0),limits = c(-20,15))+
  scale_y_continuous(expand = c(0,0), limits = c(30,50))+
  theme_mps_noto(base_family = "Dejavu_Sans")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        # legend.position = "top",
        strip.text  = element_text(face = "plain", size = 11),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.justification = c(0, 1), legend.position = c(0.02, 0.99),legend.direction = "vertical",
        legend.box = "vertical")
ggsave("png/def/pi_coupling_april_HW_dt_divided.png",dpi = 600,
       units = "cm",width = 17,height = 5)


April_HW_T_rast <- T_[[which(as_date(time(T_)) %in% April_HW)]]
April_HW_tas_rast <- tasmean[[which(as_date(time(T_)) %in% April_HW)]]
names(April_HW_T_rast) <- April_HW

first <- April_HW_T_rast[[1:3]] %>% app("mean") %>%
  setNames(c("2023-04-22/24"))
second <- April_HW_T_rast[[5:7]] %>% app("mean") %>%
  setNames(c("2023-04-26/28"))
third <- April_HW_T_rast[[8:9]] %>% app("mean") %>%
  setNames(c("2023-04-29/30"))

compo_April_HW_tas_rast <- c(first, second,third)
ggplot() +
  geom_spatraster(data = compo_April_HW_tas_rast) +
  geom_spatvector(data = world, fill = NA, size = 0.4, color = "black")+
  geom_spatvector(data = domini, fill= "transparent",color = "white", linewidth = .5)+
  geom_spatvector(data = sites, size = 0.8, color = "magenta")+
  geom_spatvector(data = sites, size = 0.8,shape = 21, 
                  color = "black", linewidth = 0.4)+
  facet_wrap(~lyr,ncol = 4) +
  scale_fill_gradientn(colours = cpt(pal = "ncl_WhiteBlueGreenYellowRed",n = 100), 
                       limits = c(-1,3),
                       breaks = seq(-4,4,1),
                       guide = guide_colorbar(barheight = 5,frame.colour = "black",
                                              barwidth = 0.5,
                                              title.position = "top"),
                       name = "[T']",
                       na.value = "white")+
  scale_x_continuous(expand = c(0,0),limits = c(-20,15))+
  scale_y_continuous(expand = c(0,0), limits = c(30,50))+
  theme_mps_noto(base_family = "Dejavu_Sans")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        # legend.position = "top",
        strip.text  = element_text(face = "plain", size = 11),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.justification = c(0, 1), legend.position = c(0.02, 0.99),legend.direction = "vertical",
        legend.box = "vertical")
ggsave("png/T_deviations_coupling_april_HW_dt_divided.png",dpi = 600,
       units = "cm",width = 17,height = 5)

# BIVARIATE MAP  ----------------------------------------------------------
# computing H' - Hp'
dif_H_Hp_ <- H_ - Hp_

# selecting only H'-Hp' and T' for june hw dates 
dif_H_Hp_HW <- dif_H_Hp_[[which(dates %in% April_HW)]]
names(dif_H_Hp_HW) <- April_HW

first_dif <- dif_H_Hp_HW[[1:3]] %>% app("mean") %>%
  setNames(c("2023-04-22/24"))
second_dif <- dif_H_Hp_HW[[5:7]] %>% app("mean") %>%
  setNames(c("2023-04-26/28"))
third_dif <- dif_H_Hp_HW[[8:9]] %>% app("mean") %>%
  setNames(c("2023-04-29/30"))
dif_H_Hp_HW <- c(first_dif, second_dif, third_dif)

T_HW <-  T_[[which(as_date(time(T_)) %in% April_HW)]]
names(T_HW) <- April_HW

first <- T_HW[[1:3]] %>% app("mean") %>%
  setNames(c("2023-04-22/24"))
second <- T_HW[[5:7]] %>% app("mean") %>%
  setNames(c("2023-04-26/28"))
third <- T_HW[[8:9]] %>% app("mean") %>%
  setNames(c("2023-04-29/30"))

T_HW <- c(first, second, third)

# test case for April HW
dif_H_Hp_HW <- dif_H_Hp_HW %>% as.data.frame(xy = T) %>%
  pivot_longer(3:ncol(.),names_to = "time") %>%
  as_tibble() %>% rename("H_Hp_anom" = 4)
T_HW <- T_HW %>% as.data.frame(xy = T) %>%
  pivot_longer(3:ncol(.),names_to = "time") %>%
  as_tibble() %>% rename("T_anom" = 4)

data_bi <- bind_cols(dif_H_Hp_HW, select(T_HW,T_anom)) %>%
  mutate(H_Hp_anom = ifelse(H_Hp_anom >3,3,H_Hp_anom),
         H_Hp_anom = ifelse(H_Hp_anom <0,0,H_Hp_anom),
         T_anom = ifelse(T_anom >3,3,T_anom),
         T_anom = ifelse(T_anom <0,0,T_anom))

ggplot()+
  geom_raster(data=data_bi,aes(x,y,fill=H_Hp_anom,fill2=T_anom),interpolate = F)+
  scale_fill_colourplane(name = "",axis_title_y = "T'",axis_title = "H'-Hp'",
                       na.value = "transaparent",breaks = seq(0,4,1),
                       breaks_y = seq(0,4,1),
                       color_projection = YUV_projection,
                       Y = 1)+
                       
                       # zero_color = "white",
                       # horizontal_color = "brown1",
                       # vertical_color = "dodgerblue4",)+
  geom_sf(data = world, fill = NA, size = 0.4, color = "black")+
  geom_sf(data = domini, fill= "transparent",color = "darkred", linewidth = .5)+
  geom_sf(data = sites, size = 0.8, color = "magenta")+
  geom_sf(data = sites, size = 0.8,shape = 21, 
                  color = "black", linewidth = 0.4)+
  scale_x_continuous(expand = c(0,0),limits = c(-20,15),breaks = c(-30,60))+
  scale_y_continuous(expand = c(0,0), limits = c(30,50),breaks = c(20,60))+
  facet_wrap(~time, ncol = 4)+
  coord_sf()+
  theme_mps_noto(base_family = "Dejavu_Sans")+
  labs(x="",y="")+
  theme_mps_noto(base_family = "Dejavu_Sans")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        # legend.position = "top",
        strip.text  = element_text(face = "plain", size = 11),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.justification = c(0, 1), legend.position = "none",legend.direction = "none",
        legend.box = "none")
ggsave("png/bivariate_map_energyterm_vs_tanom_april_HW_dt_divided.png",dpi = 600,
       units = "cm",width = 17,height = 5)


ggplot()+
  geom_raster(data=data_bi,aes(x,y,fill=H_Hp_anom,fill2=T_anom),interpolate = F)+
  scale_fill_colourplane(name = "",axis_title_y = "T'",axis_title = "H'-Hp'",
                         na.value = "transaparent",breaks = seq(0,4,1),
                         breaks_y = seq(0,4,1),
                         color_projection = YUV_projection,
                         Y = 1)+
  
  # zero_color = "white",
  # horizontal_color = "brown1",
  # vertical_color = "dodgerblue4",)+
  geom_sf(data = world, fill = NA, size = 0.4, color = "black")+
  geom_sf(data = domini, fill= "transparent",color = "darkred", linewidth = .5)+
  geom_sf(data = sites, size = 0.8, color = "magenta")+
  geom_sf(data = sites, size = 0.8,shape = 21, 
          color = "black", linewidth = 0.4)+
  scale_x_continuous(expand = c(0,0),limits = c(-20,15),breaks = c(-30,60))+
  scale_y_continuous(expand = c(0,0), limits = c(30,50),breaks = c(20,60))+
  facet_wrap(~time, ncol = 4)+
  coord_sf()+
  theme_mps_noto(base_family = "Dejavu_Sans")+
  labs(x="",y="")+
  theme(
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.position = "top",
        strip.text  = element_text(face = "plain", size = 11),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.justification = c(0, 1))
ggsave("png/bivariate_legend.png",dpi = 600,
       units = "cm",width = 17,height = 15)

# TIME-SERIES H, Hp y T'-------------------------------------------------------------
names(T_) <- time(T_)
names(H_) <- dates
names(Hp_) <- dates
names(dif_H_Hp_) <- dates
April_HW2 <- seq(from = as_date("2023-04-01"),to= as_date("2023-05-03"), by = "day")

T_HW_dom <-  T_[[which(as_date(time(T_)) %in% April_HW2)]]
H_HW_dom <-  H_[[which(dates %in% April_HW2)]]
Hp_HW_dom <-  Hp_[[which(dates %in% April_HW2)]]
diff_H_Hp_HW_dom <-  dif_H_Hp_[[which(dates %in% April_HW2)]]

T_dom <- terra::extract(T_HW_dom,vect(domini), "mean", na.rm =T) %>% 
  pivot_longer(2:ncol(.), names_to = "time",values_to = "T_") %>% select(-ID)
H_dom <- terra::extract(H_HW_dom,vect(domini), "mean", na.rm =T) %>% 
  pivot_longer(2:ncol(.), names_to = "time",values_to = "H_") %>% select(-ID)
Hp_dom <- terra::extract(Hp_HW_dom,vect(domini), "mean", na.rm =T) %>% 
  pivot_longer(2:ncol(.), names_to = "time",values_to = "Hp_") %>% select(-ID)
dif_H_Hp_dom <- terra::extract(diff_H_Hp_HW_dom,vect(domini), "mean", na.rm =T) %>% 
  pivot_longer(2:ncol(.), names_to = "time",values_to = "H_Hp") %>% select(-ID)

data_binding_event <- bind_cols(T_dom, 
                                select(H_dom,H_), 
                                select(Hp_dom, Hp_), 
                                select(dif_H_Hp_dom, H_Hp)) %>%
  mutate(time = April_HW2)

library(heatwaveR)
ggplot(data = data_binding_event, aes(x = time, group = 1 ))+
  geom_line(aes(y = T_, color = "T'"))+
  geom_line(aes(y = H_, color = "H'"))+
  geom_line(aes(y = Hp_, color = "Hp'",linetype = "Hp'"))+
  scale_color_manual(values = c("darkblue","darkblue","darkorange"))+
  scale_linetype_manual(values = "dashed")+
  geom_ribbon(ymin = 0, aes(ymax = pmax(0,T_)), fill = "goldenrod1", alpha = 0.4)+
  geom_ribbon(aes(ymin =Hp_, ymax = H_), fill = "cyan3", alpha = 0.4)+
  geom_hline(yintercept = 0)+  
  # geom_vline(xintercept = as.numeric(c(as.Date("2023-04-22"),
  #                                      as.Date("2023-04-30"))), linetype=1, lwd = 0.4)+
  geom_rect(aes(xmin = as.Date("2023-04-22"),
                xmax = as.Date("2023-04-24"),
                ymin = -1.95, ymax = 2.95), color = "black",
            fill = "white", alpha = 0, linewidth = 0.2)+
  geom_rect(aes(xmin = as.Date("2023-04-26"),
                xmax = as.Date("2023-04-28"),
                ymin = -1.95, ymax = 2.95), color = "black",
            fill = "white", alpha = 0,linewidth = 0.2)+
  geom_rect(aes(xmin = as.Date("2023-04-29"),
                xmax = as.Date("2023-04-30"),
                ymin = -1.95, ymax = 2.95), 
            color = "black",fill = "white", alpha = 0,linewidth = 0.2)+
  scale_y_continuous(limits = c(-2,3),breaks = seq(-2,4,1),expand = c(0,0))+
  scale_x_date(date_breaks = "5 days",expand = c(0.02,0.02),date_labels = "%m/%d")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")
ggsave("png/coupling_ts_hw_event_V2.png",width = 17, height = 5,units = "cm",dpi = 600)
