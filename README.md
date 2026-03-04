# urbanperformance

<!-- badges: start -->
[![R-CMD-check](https://github.com/ricardoochoa/urbanperformance/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ricardoochoa/urbanperformance/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

The urbanperformance R package is a core component of the **Urban Performance (UP) Tool** framework. It is designed to help urban planners, researchers, and policymakers assess and project the conditions of a city across various spatial scenarios.

By calculating a set of key indicators aligned with the Sustainable Development Goals (SDGs), the package enables the evaluation of investment projects and policy interventions. It allows users to rigorously test the effectiveness of different spatial solutions in advancing sustainable urban development.

## Technical Details

Under the hood, urbanperformance relies heavily on spatial data processing packages such as `raster` and `sf`, along with `dplyr` for data manipulation. It operationalizes the calculation methodologies for nearly 20 spatial and demographic indicators.

### Inputs

To calculate these indicators, the package requires two types of inputs:

1. **Spatial Data:** Georeferenced information layers in standard formats (Rasters like `.tif` or Vectors like `.shp`, `.geojson`). These represent features such as population distribution, land cover zones, transport systems, hazard-prone areas, and the locations of urban amenities.  
2. **Numerical Data:** Proximity parameters, land use classifications, and scalar values representing specific facts or assumptions about the urban environment.

### Key Indicators Calculated

The package provides functions to calculate the following categories of indicators:

* **Demographics & Land Use:** Total population, Urban footprint, Population density, Infill.  
* **Land Consumption & Ecology:** General land consumption, Biodiversity/Agricultural/Green land consumption, Green area per capita, Land cover loss.  
* **Mobility & Infrastructure:** Roads length, Roads density, Intersection density, Cycle track density.  
* **Accessibility & Exposure:** Proximity to cycle tracks, public transport, amenities, and jobs, as well as Population exposed to hazards.

## Installation

You can install the development version of urbanperformance from GitHub using the devtools or remotes package:

```r
# Install devtools if you haven't already  
# install.packages("devtools")

devtools::install_github("ricardoochoa/urbanperformance")
```

## Basic Usage

Here is a basic example of how to use urbanperformance to calculate the total population, urban footprint, and population density of a study area using the dummy dataset provided within the package.

```r
library(urbanperformance)  
library(raster)

# 1. Load the spatial raster data included in the package  
pop_2025_path <- system.file("extdata", "POP_2025.tif", package = "urbanperformance")  
population_base <- raster::raster(pop_2025_path)

buildup_2025_path <- system.file("extdata", "Build_up_2025.tif", package = "urbanperformance")  
buildup_base <- raster::raster(buildup_2025_path)

# 2. Standardize rasters (Ensures resolution, projection, and bounding boxes match)  
buildup_base <- rasterchecker(buildup_base, base = population_base)

# 3. Calculate Indicators

# Calculate Total Population  
total_pop <- tot_pop(population_base)  
print(total_pop)

# Calculate Urban Footprint (Built-up area)  
footprint_area <- urban_footprint(buildup_base)  
print(footprint_area)

# Calculate Population Density (Inhabitants per built-up area)  
pop_density <- population_density(pop = population_base, fp = buildup_base)  
print(pop_density)
```

### Advanced Usage (Proximity Analysis)

The package also allows you to calculate the percentage of the population with access to specific urban amenities using vector point/polygon data and population rasters:

```r
library(sf)

# Load amenities and public transport vector data  
data(amenities.cun)  
data(transport.cun)

# Calculate percentage of population near public transport  
transport_prox <- public_transport_proximity(transport.cun, pop = population_base)  
print(transport_prox)

# Calculate percentage of population near basic amenities (schools, health, grocery)  
# Note: save = FALSE prevents saving the intermediate distance raster to the global environment  
amenities_prox <- amenities_proximity(amenities.cun, pop = population_base, save = FALSE)  
print(amenities_prox)
```

## License

MIT License. See `LICENSE` for more information.