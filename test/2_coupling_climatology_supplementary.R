
# COUPLING CLIMATOLOGY ----------------------------------------------------


library(metR)
library(tidyverse)
library(terra)
library(scales)
library(data.table)
library(mpsTemplates)
library(patchwork)
library(sf)
domini <- st_bbox(c(xmin = -7.5, xmax = -2.5, ymax = 38.6, ymin = 37.3), crs = st_crs(4326)) %>% st_as_sfc()

pi_SW_IB <- rast("outputs/pi_climatology_1950_2023_April_sw_EU.nc") %>% crop(domini)
names(pi_SW_IB) <- time(pi_SW_IB)

energy_SW_IB <- rast("outputs/H_Hp_climatology_1950_2023_April_sw_EU.nc") %>% crop(domini)
names(energy_SW_IB) <- time(energy_SW_IB)

ts_pi_sw_IB <- global(pi_SW_IB,"max") %>% 
  rownames_to_column(var = "time") %>% 
  mutate(year= year(time)) %>%
  as_tibble() %>% 
    group_by(year) %>%
    summarise(max_pi = mean(max))

ts_energy_SW_IB <- global(energy_SW_IB,"max") %>% 
  rownames_to_column(var = "time") %>% 
  mutate(year= year(time)) %>%
  as_tibble() %>% 
  group_by(year) %>%
  summarise(max_H_Hp = mean(max))

data_coupling_sw <- bind_cols(ts_pi_sw_IB, select(ts_energy_SW_IB, max_H_Hp))

col_temp <- c("#F4FBF1","#7DC971","#E1F166","#FDB34E","#FA4B26","#830024")

pi_trend <- ggplot() +
  geom_line(data = data_coupling_sw, aes(x = year, y = max_pi)) +
  geom_point(data = data_coupling_sw, aes(x = year, y = max_pi,color = max_pi))+
  scale_color_gradientn(colours = col_temp,name = "[π]")+
  scale_y_continuous(breaks = seq(0,3,1))+
  labs(y = "[π]") +
  scale_x_continuous(breaks = c(seq(1950,2019,5),2023))+
  theme_mps_noto(base_family = "Dejavu_Sans")+
  theme(
    axis.title.x = element_blank(),
    panel.margin = unit(-0.05, "lines"),
    axis.text = element_text(size = 9),
    strip.text=element_text(size=10),
    strip.background=element_rect(fill=NA),
    legend.position = "right",legend.direction = "vertical",
    legend.text = element_text(size = 9))

h_hp <- ggplot() +
  geom_line(data = data_coupling_sw, aes(x = year, y = max_H_Hp), color = "#99EBEB") +
  geom_point(data = data_coupling_sw, aes(x = year, y = max_H_Hp), color = "black", size = 0.75)+
  scale_color_gradientn(colours = col_temp)+
  scale_y_continuous(breaks = seq(0,3,1))+
  labs(y ="H'-Hp'") +
  scale_x_continuous(breaks = c(seq(1950,2020,15),2023))+
  theme_mps_noto(base_family = "Dejavu_Sans")+
  theme(plot.background = element_rect(fill = "white", color = NA),
    axis.title.x = element_blank(),
    axis.title = element_text(size = 8),
    panel.margin = unit(-0.05, "lines"),
    axis.text = element_text(size = 8),
    strip.text=element_text(size=10),
    strip.background=element_rect(fill=NA),
    legend.position = "right",legend.direction = "vertical",
    legend.text = element_text(size = 8))

pi_trend + inset_element(h_hp, 0.01, 0.51, 0.7, 0.95)
ggsave("png/def/SUPLEMENT/pi_coupling_sw_ib_april_TS_1950_2023.png",
       width = 17,height = 10, units = "cm",dpi = 600)

pi_cross <- as.data.frame(pi_SW_IB, xy =T) %>%
  pivot_longer(names_to = "time",values_to = "pi", 3:ncol(.)) %>%
  group_by(y, time) %>%
  summarise(pi = mean(pi)) %>%
  ungroup() %>%
  mutate(id = data.table::rleid(time)) %>%
  mutate(year = as.factor(year(time))) %>%
  mutate(time = as_date(time),
         pi = ifelse(pi <0,0,pi),
         pi = ifelse(pi >8,8,pi))


pi_cross <- ReadNetCDF("outputs/pi_climatology_1950_2023_April_sw_EU.nc",
                       subset = list(longitude = 0.0)) %>%
  mutate(id = data.table::rleid(time)) %>%
  mutate(year = as.factor(year(time))) %>%
  mutate(time = as_date(time),
         pi = ifelse(pi <0,0,pi),
         pi = ifelse(pi >8,8,pi))

ts_utils <- pi_cross %>% mutate(yday = yday(time)) %>%
  filter(yday == 100 & latitude== 45) %>%
  mutate(sino = c(rep(c(1,0,0,0,0), 74/5),0,0,0,0)) %>%
  filter(sino ==1)
ids = ts_utils$id
yr = ts_utils$year

col_temp <- c("#F4FBF1","#7DC971","#E1F166","#FDB34E","#FA4B26","#830024")

ggplot(data = pi_cross, aes(x= as.factor(id), y = latitude, fill = pi))+
  geom_raster(interpolate = T)+
  scale_fill_gradientn(colours = col_temp, name = "[π]",
                       guide = guide_colorbar(barheight = 7, barwidth = 0.4,
                                              title.position = "top"))+
  ylab("Latitude [°]")+
  scale_x_discrete(breaks=ids,
                   labels=yr,)+
  scale_y_continuous(expand = c(0,0))+
  theme_mps_noto()+
  theme(
    axis.title.x = element_blank(),
    panel.margin = unit(-0.05, "lines"),
    axis.text = element_text(size = 9),
    strip.text=element_text(size=10),
    strip.background=element_rect(fill=NA),
    legend.position = "right",legend.direction = "vertical",
    legend.text = element_text(size = 9))

ggsave("png/preliminars/pi_climatology_cross_v2.png",dpi = 600,units = "cm",
       width = 20, height = 5)