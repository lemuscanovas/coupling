# Coupling

## Simple example

Reading all variables needed for computing (lower pi)
```r
# loading needed variables for computing lower pi

# Actual/total evaporation (cummulated value until 00h)
e <- rast("data/e_04_00h_2010_2023.nc")
e <- e * 1000 # to mm
e <- -e # reverting the ECMWF convention for vertical fluxes which is positive downwards 

# Potential evaporation (as above)
pev <- rast("data/pev_04_00h_2010_2023.nc")
pev <- pev * 1000
pev <- -pev


# Surface Solar net radiation and surface thermal radiation
ssr <- rast("data/ssr_04_00h_2010_2023.nc")
ssr <- -ssr
str <- rast("data/str_04_00h_2010_2023.nc")
str <- -str
# daily mean temperature at 2m
tasmean <- rast("data/tasmean_04_2010_2023.nc") -273.15
```

Computing the latent heat of vaporisation following Priestley & Taylor (1972)

```r
l <- latent_heat_vapor(tas = tasmean)
```

Computing the energy balances for actual and potential evaporation to estimate
actual and potential sensible heat fluxes.

```r
# First we sum both radiation components
sr <- ssr+str
H <- energy_balance(Rn = sr,l = l,E = e)

Hp <- energy_balance(Rn = sr,l = l,E =  pev)
```

Calculating lower pi
```r
pi <- lower_pi(tas = tasmean, H = H, Hp = Hp)
```

```r
world <- giscoR::gisco_get_countries()

col_temp <- c("white","white","#7DC971","#E1F166","#FDB34E","#FA4B26","#830024","purple")


event <- pi[[416:418]] %>% app("mean") %>%
  setNames(c("2023-04-26/28"))

ggplot() +
  geom_spatraster(data = event) +
  geom_spatvector(data = world, fill = NA, size = 0.4, color = "black")+
  scale_fill_stepsn(colours = col_temp, 
                       limits = c(0,8),
                       breaks = seq(0,12,1),
                       guide = guide_colorbar(barheight = 0.5,
                                              barwidth = 10,
                                              title.position = "top"),
                       name = "[π]",
                       na.value = "white")+
  scale_x_continuous(expand = c(0,0),limits = c(-20,15))+
  scale_y_continuous(expand = c(0,0), limits = c(30,50))+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom",
        strip.text  = element_text(face = "plain", size = 11),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11))
```

