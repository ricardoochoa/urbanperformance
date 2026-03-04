## **title: "urbanperformance" output: rmarkdown::pdf\_document: extra\_dependencies: \["float"\] toc: true**

number\_sections: true vignette: \> %\\VignetteIndexEntry{urbanperformance} %\\VignetteEngine{knitr::rmarkdown} %\\VignetteEncoding{UTF-8}

knitr::opts\_chunk$set(  
  collapse \= TRUE,  
  comment \= "\#\>"  
)

# **Introduction**

This vignette introduces the urbanperformance package, developed **within** the Urban Performance (UP) Tool framework. The UP Tool is designed to generate scenarios to project possible futures for specific study areas. The tool includes two main components: the Urban Performance Calculator and the Urban Performance Visualizer.

The urbanperformance package integrates a list of indicators **aligned with** the Sustainable Development Goals (SDGs). Included in the Urban Performance Calculator, these indicators assess a city's present and future performance by modeling the impacts of investment projects and policy interventions.

Through this package, users can access functions to evaluate multiple scenarios and spatial solutions, enabling rigorous testing of their effectiveness in advancing sustainable urban development.

## **Indicators**

The urbanperformance package includes a list of indicators designated to assess city conditions across various scenarios. These indicators are numeric values that characterize the urban environment and identify critical issues, facilitating the evaluation, monitoring, and communication of a city's status. They are key for integrated urban planning, enabling users to model how an area currently addresses specific urban challenges.

The indicators included in the package are listed in the table below. Note that several of these metrics comprise sub-indicators to enhance the granularity of the analysis.

| List of indicators |  |
| :---- | :---- |
| 1\. Total population | 11\. Roads length |
| 2\. Urban footprint | 12\. Roads density |
| 3\. Population density | 13\. Intersection density |
| 4\. Land consumption | 14\. Cycle track density |
| 5\. Infill | 15\. Cycle proximity |
| 6\. Biodiversity land consumption | 16\. Public transport proximity |
| 7\. Agricultural land consumption | 17\. Amenities proximity |
| 8\. Green land consumption | 18\. Jobs proximity |
| 9\. Green land per capita | 19\. Hazard exposure |
| 10\. Land cover loss |  |

Similar to the Urban Performance Calculator, the urbanperformance package operationalizes the calculation methodology for these indicators. These methodologies are demonstrated in this document through a practical exercise. In essence, urbanperformance executes a series of geoprocessing tasks and calculations.

## **Basic Data**

To calculate the indicators, the urbanperformance package requires two types of inputs: spatial data and numerical data.

**Spatial Data**

This comprises a set of georeferenced information layers, including:

| Data | Description |
| :---- | :---- |
| Population | Spatial distribution of inhabitants |
| Urban Amenities location | Location of schools, hospitals, and basic services |
| Employment distribution | Spatial distribution of employment |
| Roads network | Location of roads, including categories |
| Land cover zones | Land cover areas and classification |
| Cycle tracks | Location of cycle tracks |
| Transport systems | Location of bus stops or transport routes |
| Hazard-prone areas | Location of hazard-prone areas in the city |
| Biodiversity areas | Delimitation of biodiversity zones |
| Green areas | Location of green areas in the city |

The package supports various standard formats, including rasters (.tif) or vectors (.shp, .kml, .geojson).

*Note: All spatial datasets must be cleaned and standardized prior to use within the package.*

**Numerical Data**

In addition to spatial layers, numerical inputs are required. These include proximity parameters, land use classifications, and scalar values representing specific facts or assumptions about the urban environment.

To demonstrate the functionality of urbanperformance, this vignette utilizes pre-loaded dummy data based on a localized area in Cancún, Mexico. Users can use this dataset to explore the indicators and assess conditions for the example city.

# **urbanperformance Application**

The exercise is integrated by four main steps to use the urbanperformance package:

1. Installation  
2. Input data  
3. Indicators calculation  
4. Results analysis

## **Installation**

The first step is to load the necessary packages to perform the complete example. Additionally, as can be consulted in the urbanperformance package documentation, the calculations included use functions from other packages to simplify processes.

\# Load require libraries  
library(urbanperformance)  
library(raster)  
library(sf)  
library(dplyr)

## **Input data**

The second step is to load the city data. This data could be gathered from open data sources or local sources, depending on availability. For this exercise, a dummy dataset is included in the package to perform an example of application of the calculations.

The example dataset is already cleaned and standardized following a specific structure, described in the urbanperformance documentation. This dataset includes information about population, roads, land cover, cycle infrastructure, amenities location, hazard areas, and jobs proximity.

Dataset can be loaded as follows:

\# Load spatial data  
data(aoi.cun)  
data(roads.cun)  
data(transport.cun)  
data(cycle.cun)  
data(amenities.cun)  
data(land.cover.classes)

\#\# Raster data  
population\_base \<- system.file("extdata", "POP\_2025.tif",  
  package \= "urbanperformance"  
)  
population\_base \<- raster::raster(population\_base)

population\_horizon \<- system.file("extdata", "POP\_2030.tif",  
  package \= "urbanperformance"  
)  
population\_horizon \<- raster::raster(population\_horizon)

buildup\_base \<- system.file("extdata", "Build\_up\_2025.tif",  
  package \= "urbanperformance"  
)  
buildup\_base \<- raster::raster(buildup\_base)

buildup\_horizon \<- system.file("extdata", "Build\_up\_2030.tif",  
  package \= "urbanperformance"  
)  
buildup\_horizon \<- raster::raster(buildup\_horizon)

jobs\_distribution \<- system.file("extdata", "Jobs.tif",  
  package \= "urbanperformance"  
)  
jobs\_distribution \<- raster::raster(jobs\_distribution)

hazard\_zones \<- system.file("extdata", "Hazard.tif",  
  package \= "urbanperformance"  
)  
hazard\_zones \<- raster::raster(hazard\_zones)

land\_cover\_18 \<- system.file("extdata", "Land\_cover\_2018.tif",  
  package \= "urbanperformance"  
)  
land\_cover\_18 \<- raster::raster(land\_cover\_18)

land\_cover \<- system.file("extdata", "Land\_cover.tif",  
  package \= "urbanperformance"  
)  
land\_cover \<- raster::raster(land\_cover)

The rasterchecker() function ensures that all spatial rasters share the same resolution, projection, and bounding box as the base population raster.

\#\# Verify urban extents  
hazard\_zones \<- rasterchecker(hazard\_zones, base \= population\_base)  
buildup\_base \<- rasterchecker(buildup\_base, base \= population\_base)  
buildup\_horizon \<- rasterchecker(buildup\_horizon, base \= population\_base)  
jobs\_distribution \<- rasterchecker(jobs\_distribution, base \= population\_base)  
land\_cover \<- rasterchecker(land\_cover, base \= population\_base)

## **Indicators calculation**

### **Total population**

This function calculates the total population in the urban area by adding the population located in each pixel.

This metric serves as the fundamental baseline for the entire analysis. Accurate population counts are essential for normalizing other indicators (creating per-capita metrics) and for dimensioning the scale of urban needs, such as utility demand and service coverage.

\#\# Library for plotting rasters  
library(rasterVis)

levelplot(population\_base,  
  margin \= FALSE, colorkey \= list(space \= "bottom"),  
  main \= "Population distribution"  
)

Calculate total population:

\# library for plotting tables  
library(kableExtra)

total\_population \<- tot\_pop(population\_base)

kable(total\_population, "latex", booktabs \= TRUE) |\>  
  kable\_styling(latex\_options \= "HOLD\_position")

### **Urban Footprint**

This function quantifies the total physical area occupied by artificial surfaces, built-up areas, and urban structures.

urban\_footprint \<- urban\_footprint(buildup\_base)

kable(urban\_footprint, "latex", booktabs \= TRUE) |\>  
  kable\_styling(latex\_options \= "HOLD\_position")

levelplot(buildup\_base,  
  margin \= FALSE, colorkey \= list(space \= "bottom"),  
  main \= "Urban area"  
)

### **Population density**

Population density is the amount of inhabitants per built-up area. This indicator measures the intensity of land use by dividing the total population by the urban footprint area.

Density is a key driver of urban vitality and sustainability. Higher densities can support better public transport viability and local businesses, while very low densities may indicate inefficient land use. This indicator helps assess whether the city is meeting density targets required to support sustainable infrastructure.

pop\_dens \<- population\_density(population\_base, buildup\_base)

kable(pop\_dens, "latex", booktabs \= TRUE) |\>  
  kable\_styling(latex\_options \= "HOLD\_position")

### **Land cover areas**

This function calculates areas for each land cover in a specific scenario.

levelplot(land\_cover\_18,  
  margin \= FALSE, colorkey \= list(space \= "bottom"),  
  main \= "Land cover 2018"  
)  
levelplot(land\_cover,  
  margin \= FALSE, colorkey \= list(space \= "bottom"),  
  main \= "Land cover 2025"  
)

land\_l \<- stack(land\_cover\_18, land\_cover)  
lc\_areas \<- land\_cover\_areas(land\_l, land.cover.classes)

kable(lc\_areas, "latex", booktabs \= TRUE) |\>  
  kable\_styling(latex\_options \= "HOLD\_position")

### **Land cover loss**

This function calculates the amount of a specific land coverage that is become to a different land cover, between different scenarios. This indicator can be calculate for a specific land coverage loss or different coverages including in the same raster.

land\_loss \<- land\_cover\_loss(land\_l, areas \= lc\_areas)

kable(land\_loss, "latex", booktabs \= TRUE) |\>  
  kable\_styling(latex\_options \= "HOLD\_position")

### **Green land per capita**

This function calculates the total green area within the urban area (forests, parks, gardens, etc.) per inhabitant.

green\_land \<- land\_cover\[\[1\]\]  
green\_land\[green\_land \!= 8\] \<- NA

green\_a\_capita \<- green\_area\_pcapita(green\_land, pop \= population\_base)

kable(green\_a\_capita, "latex", booktabs \= TRUE) |\>  
  kable\_styling(latex\_options \= "HOLD\_position")

### **Roads length**

This function calculates the roads kilometers per each fclass in the study area.

roads\_l \<- roads\_length(roads.cun)

kable(roads\_l, "latex", booktabs \= TRUE) |\>  
  kable\_styling(latex\_options \= "HOLD\_position")

### **Roads density**

This function calculates the roads kilometers per square kilometer in the study area. Road density indicates the level of infrastructure penetration. While high density implies good connectivity, an excessively high density may suggest an over-dominance of car-oriented infrastructure and high impervious surface coverage, which can contribute to heat island effects and runoff issues.

roads\_dens \<- roads\_density(roads.cun, buildup\_base)

kable(roads\_dens, "latex", booktabs \= TRUE) |\>  
  kable\_styling(latex\_options \= "HOLD\_position")

### **Intersection density**

This function calculates the average number of urban crossroads per pixel. Intersection density is a widely recognized proxy for walkability. A higher density typically indicates a finer street grain (smaller blocks) and greater connectivity, which offers pedestrians more route choices and shorter distances between destinations. Conversely, low intersection density often characterizes car-dependent suburban designs.

intersection\_dens \<- intersection\_density(roads.cun, buildup\_base)

kable(intersection\_dens, "latex", booktabs \= TRUE) |\>  
  kable\_styling(latex\_options \= "HOLD\_position")

### **Cycle track density**

This function calculates the cycle track kilometers per square kilometer of urban area.

This measures the supply of active mobility infrastructure relative to the city size. It helps identify if the investment in cycling networks matches the scale of the urban area.

cycle\_dens \<- cycle\_track\_density(cycle.cun, buildup\_base)

kable(cycle\_dens, "latex", booktabs \= TRUE) |\>  
  kable\_styling(latex\_options \= "HOLD\_position")

### **Cycle proximity**

This function calculates the total and the percentage of the population that lives within a maximum recommended distance from a cycle track. This indicator reveals who actually has access to the network, highlighting gaps where neighborhoods are disconnected from safe cycling routes.

cycle\_prox \<- cycle\_proximity(cycle.cun, population\_base)

kable(cycle\_prox, "latex", booktabs \= TRUE) |\>  
  kable\_styling(latex\_options \= "HOLD\_position")

### **Public transport proximity**

This function calculates the total and the percentage of the population that lives within a maximum recommended distance from a public transport station, according to the transport classification.

High proximity ensures that mass transit is a viable option for residents, reducing dependency on private vehicles and lowering carbon emissions.

ptransport\_prox \<- public\_transport\_proximity(transport.cun,  
  pop \= population\_base  
)

kable(ptransport\_prox, "latex", booktabs \= TRUE) |\>  
  kable\_styling(latex\_options \= "HOLD\_position")

### **Amenities proximity**

This function calculates the total and the percentage of the population that lives within the maximum recommended distance to each of the following urban public services and amenities. Setting save \= FALSE prevents the function from writing intermediate shapefiles to your local working directory.

amenities\_prox \<- amenities\_proximity(amenities.cun,  
  pop \= population\_base,  
  save \= FALSE  
)

kable(amenities\_prox, "latex", booktabs \= TRUE) |\>  
  kable\_styling(latex\_options \= "HOLD\_position")

### **Jobs proximity**

This function calculates the percentage of the population that lives within the areas with high job density in the urban area. This indicator helps in evaluation of the proportion of residents living near major employment clusters or high job-density zones.

jobs\_prox \<- jobs\_proximity(jobs\_distribution\[\[1\]\], pop \= population\_base)

kable(jobs\_prox, "latex", booktabs \= TRUE) |\>  
  kable\_styling(latex\_options \= "HOLD\_position")

levelplot(jobs\_distribution,  
  margin \= FALSE, colorkey \= list(space \= "bottom"),  
  main \= "Jobs concentration"  
)

### **Population in risk**

This function calculates the number and percentage of the population that is exposed to hazards because they live within the zone of influence of natural and anthropogenic sources.

It quantifies the human risk associated with current settlement patterns and helps prioritize areas for mitigation strategies or relocation policies.

pop\_in\_hazard \<- hazard\_exposure(hazard\_zones, pop \= population\_base)

kable(pop\_in\_hazard, "latex", booktabs \= TRUE) |\>  
  kable\_styling(latex\_options \= "HOLD\_position")

levelplot(hazard\_zones, margin \= FALSE, colorkey \= list(space \= "bottom"))

### **Land consumption**

This function calculates land consumption between two urban growth scenarios. Land consumption is the amount of land predicted to change from natural habitats or agricultural uses into urban human settlements between the base year and the horizon year.

l\_consumption \<- land\_consumption(buildup\_base, buildup\_horizon)

kable(l\_consumption, "latex", booktabs \= TRUE) |\>  
  kable\_styling(latex\_options \= "HOLD\_position")

*Note: The land\_consumption function automatically generates a raster object in your global environment named \`urban.expansion\_ Build\_up\_2030\` representing the expanded areas. You can plot this object directly:*

levelplot(\`urban.expansion\_ Build\_up\_2030\`,  
  margin \= FALSE,  
  colorkey \= list(space \= "bottom")  
)

### **Infill**

This function calculates the amount of land that was located within the boundaries of the urban area in the base scenario and that at least doubled its population between the base year and the horizon year. It is also known as intensification area.

infill\_area \<- infill(population\_base, population\_horizon)

kable(infill\_area, "latex", booktabs \= TRUE) |\>  
  kable\_styling(latex\_options \= "HOLD\_position")

### **Results analysis**

Select proximity data for visualization

\#\# Join all proximities in a data frame  
proximities \<- rbind(cycle\_prox, ptransport\_prox, amenities\_prox, jobs\_prox)

\#\# Select only percentages  
proximities\_pct \<- subset(proximities, units \== "%")

Visualize results

\#\# library for plotting proximity results  
library(ggplot2)  
p \<- ggplot(proximities\_pct, aes(x \= fclass, y \= value, fill \= fclass)) \+  
  geom\_bar(stat \= "identity") \+  
  scale\_fill\_brewer(palette \= "Set2") \+  
  theme\_bw() \+  
  ylab("Percentage of inhabitants with proximity") \+  
  xlab("Urban amenities and services") \+  
  theme(legend.position \= "none") \+  
  ylim(0, 100\)

p  
