library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(RSQLite)
library(readr)


#importing shpaefile data
shape <- st_read("C:\\Users\\Lenovo\\Documents\\CASA\\modules\\GIS\\Data\\statistical-gis-boundaries-london\\statistical-gis-boundaries-london\\ESRI\\London_Borough_Excluding_MHW.shp")

#plotting shapefile for each attribut
plot(shape)

#acquiring geographical data (outline) only using st_geometry command
shape %>%
  st_geometry() %>%
  plot()

#importing csv data
mycsv <- read_csv2("fly-tipping-borough.csv")
View(mycsv)
mycsv
cols(mycsv)

#merging data csv with shape file using merge command
shape <- shape %>%
  merge(.,
        mycsv,
        by.x = "GSS_CODE",
        by.y = "Row Labels")

#checking if the join is successful or not
shape %>% 
  head(., n=10)
#if it succesed the shape wil contain data from csv

#plotting tematic map using tmap_mode command
tmap_mode("plot")
shape %>%
  qtm(., fill = "Grand Total.x")

#Export data as Geopackage type datafile using st_write command
shape %>%
st_write(.,"C:\\Users\\Lenovo\\Documents\\CASA\\modules\\GIS\\Practice\\Rwk1.gpkg",
         "Rlondon_boroughs_fly_tipping", 
         delete_layer = TRUE)

#adding CSV using SQLite database package
con <- dbConnect(RSQLite::SQLite(),dbname = "C:\\Users\\Lenovo\\Documents\\CASA\\modules\\GIS\\Practice\\Rwk1.gpkg")
con %>%
  dbListTables()

#adding .csv and disconected from gpkg
con %>%
  dbWriteTable(.,
               "original.csv",
               mycsv,
               overwrite = TRUE)

#Disconnecting it
con %>%
  dbDisconnect()
