## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning= FALSE, echo = TRUE, message=FALSE------------------------
# Load require libraries
library(UPtooltest)
library(raster)
library(sf)
library(dplyr)

## -----------------------------------------------------------------------------
# Load spatial data
data(aoi.cun)
data(roads.cun)
data(transport.cun)
data(cycle.cun)
data(amenities.cun)

## Raster data
population.base <- system.file("extdata", "POP_2025.tif", package = "UPtooltest")
population.base <- raster::raster(population.base)

population.horizon <- system.file("extdata", "POP_2030.tif", package = "UPtooltest")
population.horizon <- raster::raster(population.horizon)

buildup.base <- system.file("extdata", "Build_up_2025.tif", package = "UPtooltest")
buildup.base <- raster::raster(buildup.base)

buildup.horizon <- system.file("extdata", "Build_up_2030.tif", package = "UPtooltest")
buildup.horizon <- raster::raster(buildup.horizon)

jobs.distribution <- system.file("extdata", "Jobs.tif", package = "UPtooltest")
jobs.distribution <- raster::raster(jobs.distribution)

hazard.zones <- system.file("extdata", "Hazard.tif", package = "UPtooltest")
hazard.zones <- raster::raster(hazard.zones)

land.cover.18 <- system.file("extdata", "Land_cover_2018.tif", package = "UPtooltest")
land.cover.18 <- raster::raster(land.cover.18)

land.cover <- system.file("extdata", "Land_cover.tif", package = "UPtooltest")
land.cover <- raster::raster(land.cover)


## ----fig.width= 7, fig.height= 8----------------------------------------------
## Verify urban extents 
hazard.zones <- rasterchecker(hazard.zones, base = population.base)
buildup.base <- rasterchecker(buildup.base,  base = population.base)
buildup.horizon <- rasterchecker(buildup.horizon,  base = population.base)
jobs.distribution <- rasterchecker(jobs.distribution,  base = population.base)
land.cover <- rasterchecker(land.cover,  base = population.base)


## ----fig.width= 4, fig.height= 5, fig.pos="H", fig.align = "center"-----------
## Library for plotting rasters
library(rasterVis)

levelplot(population.base, margin=FALSE, colorkey=list(space="bottom"), 
          main = "Population distribution" )

## ----fig.width= 5, fig.height= 6, fig.pos="H"---------------------------------
#library for plotting tables
library(kableExtra)

total_popultion <- tot.pop(population.base)

kable(total_popultion, "latex", booktabs = T) %>%
  kable_styling(latex_options = "HOLD_position")


## ----fig.width= 5, fig.height= 6, fig.pos="H"---------------------------------
urban_footprint <- urban.footprint(buildup.base)

kable(urban_footprint, "latex", booktabs = T) %>%
  kable_styling(latex_options = "HOLD_position")


## ----fig.width= 4, fig.height= 5, fig.pos="H", fig.align = "center"-----------
levelplot(buildup.base, margin=FALSE, colorkey=list(space="bottom"), 
          main = "Urban area" )

## ----fig.width= 5, fig.height= 6, fig.pos="H"---------------------------------
pop.dens <- population.density(population.base,buildup.base)

kable(pop.dens, "latex", booktabs = T) %>%
  kable_styling(latex_options = "HOLD_position")


## ----fig.width= 4, fig.height= 5, fig.pos="H", fig.align = "center"-----------
levelplot(land.cover.18, margin=FALSE, colorkey=list(space="bottom"), 
          main = "Land cover 2018")
levelplot(land.cover, margin=FALSE, colorkey=list(space="bottom"), 
          main = "Land cover 2025")

## ----fig.height=6, fig.pos="H", fig.width=5, message=FALSE, warning=FALSE-----
land.l <- stack(land.cover.18, land.cover)
lc.areas <- land.cover.areas(land.l, land.cover.classes)

kable(lc.areas, "latex", booktabs = T) %>%
  kable_styling(latex_options = "HOLD_position")


## ----fig.width= 5, fig.height= 6, fig.pos="H"---------------------------------
land.loss <- land.cover.loss(land.l,areas = lc.areas)

kable(land.loss, "latex", booktabs = T) %>%
  kable_styling(latex_options = "HOLD_position")

## ----fig.width= 5, fig.height= 6, fig.pos="H"---------------------------------
green.land <- land.cover[[1]]
green.land[green.land !=  8]<- NA

green.a.capita <- green.area.pcapita(green.land, pop =population.base)

kable(green.a.capita, "latex", booktabs = T) %>%
  kable_styling(latex_options = "HOLD_position")

## ----fig.width= 5, fig.height= 6, fig.pos="H"---------------------------------
roads.l <- roads.length(roads.cun)

kable(roads.l, "latex", booktabs = T) %>%
  kable_styling(latex_options = "HOLD_position")

## ----fig.width= 5, fig.height= 6, fig.pos="H"---------------------------------
roads.dens <- roads.density(roads.cun, buildup.base)

kable(roads.dens, "latex", booktabs = T) %>%
  kable_styling(latex_options = "HOLD_position")


## ----fig.width= 5, fig.height= 6, fig.pos="H"---------------------------------
intersection.dens <- intersection.density(roads.cun, buildup.base)

kable(intersection.dens, "latex", booktabs = T) %>%
  kable_styling(latex_options = "HOLD_position")


## ----fig.width= 5, fig.height= 6, fig.pos="H"---------------------------------
cycle.dens <- cycle_track_density(cycle.cun, buildup.base)

kable(cycle.dens, "latex", booktabs = T) %>%
  kable_styling(latex_options = "HOLD_position")


## ----fig.width= 5, fig.height= 6, fig.pos="H"---------------------------------
cycle.prox <- cycle_proximity(cycle.cun, population.base)

kable(cycle.prox, "latex", booktabs = T) %>%
  kable_styling(latex_options = "HOLD_position")

## ----fig.width= 5, fig.height= 6, fig.pos="H"---------------------------------
ptransport.prox <- public.transport.proximity(transport.cun, pop = population.base)

kable(ptransport.prox, "latex", booktabs = T) %>%
  kable_styling(latex_options = "HOLD_position")


## ----fig.width= 5, fig.height= 6, fig.pos="H"---------------------------------
amenities.prox <- amenities.proximity(amenities.cun, pop = population.base, save = FALSE)

kable(amenities.prox, "latex", booktabs = T) %>%
  kable_styling(latex_options = "HOLD_position")


## ----fig.width= 5, fig.height= 6, fig.pos="H"---------------------------------
jobs.prox <- jobs.proximity(jobs.distribution[[1]], pop = population.base)

kable(jobs.prox, "latex", booktabs = T) %>%
  kable_styling(latex_options = "HOLD_position")


## ----fig.width= 4, fig.height= 5, fig.pos="H", fig.align = "center"-----------
levelplot(jobs.distribution, margin=FALSE, colorkey=list(space="bottom"), 
          main = "Jobs concentration")

## ----fig.width= 5, fig.height= 6, fig.pos="H"---------------------------------
pop.in.hazard <- hazard.exposure(hazard.zones, pop = population.base)

kable(pop.in.hazard, "latex", booktabs = T) %>%
  kable_styling(latex_options = "HOLD_position")


## ----fig.width= 4, fig.height= 5, fig.pos="H", fig.align = "center"-----------
levelplot(hazard.zones, margin=FALSE, colorkey=list(space="bottom"))


## ----fig.width= 5, fig.height= 6, fig.pos="H"---------------------------------
l.consumption <- land.consumption(buildup.base, buildup.horizon)

kable(l.consumption, "latex", booktabs = T) %>%
  kable_styling(latex_options = "HOLD_position")


## ----fig.width= 4, fig.height= 5, fig.pos="H", fig.align = "center"-----------
levelplot(`urban.expansion_ Build_up_2030`, margin=FALSE, colorkey=list(space="bottom"))


## ----fig.width= 5, fig.height= 6, fig.pos="H"---------------------------------
infill.area <- infill(population.base, population.horizon)

kable(infill.area, "latex", booktabs = T) %>%
  kable_styling(latex_options = "HOLD_position")


## ----fig.width= 5, fig.height= 6, fig.pos="H"---------------------------------

## Join all proximities in a data frame
proximities <- rbind(cycle.prox, ptransport.prox, amenities.prox, jobs.prox)

## Select only percentages
proximities.pct <- subset(proximities, units == "%")


## ----fig.width= 5, fig.height= 4, fig.pos="H", fig.align = "center"-----------
##library for plotting proximity results
library(ggplot2)
p <- ggplot(proximities.pct, aes(x = fclass, y = value, fill = fclass)) +
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "Set2")+
  theme_bw()+
  ylab("Percentage of inhabitants with proximity")+
  xlab("Urban amenities and services")+
  theme(legend.position="none")+
  ylim(0,100)
  
p

