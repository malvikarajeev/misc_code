#####experimenting with crosstalk

#devtools::install_github("rstudio/crosstalk")
#devtools::install_github("jcheng5/d3scatter")
#devtools::install_github("rstudio/leaflet")

library(d3scatter)
library(crosstalk)
library(leaflet)
library(sf)
library(sp)
library(rgdal)
library(rgeos)
library(ggplot2)
library(ggthemes)
library(plotly)
shared_dat <- merge(q85, q65, by = c('city', 'state', 'lat','long','state','zip'))

shared_dat <- SharedData$new(shared_dat)


#####linked brushing using bootstrap



shared_quakes <- SharedData$new(sample_n(q85_long, 100))
bscols(
  leaflet(shared_quakes, width = "100%", height = 300) %>%
   addTiles() %>%
    addMarkers(),
  d3scatter(shared_quakes, ~option, ~no_of_ppl, ~state, width = "100%", height = 300)
)


##ADDING FILTERS: what to filter by? 


library(dplyr)



  

#########
library(d3scatter)
library(crosstalk)
library(leaflet)
library(sf)
library(sp)
library(rgdal)
library(rgeos)
library(ggplot2)
library(ggthemes)
library(plotly)










