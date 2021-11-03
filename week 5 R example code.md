# GIS-R-repository
---
title: "week5Rlive"
author: "AMacLachlan"
date: "03/11/2021"
output: html_document
---
# Notes

bedroom is at ward - convert to borough
could also probably just use Local Area District Data
Hotel is poly - summarise over borough
Airbnb is points - summarise over borough

```{r library}
library(sf)
library(tidyverse)
library(janitor)
library(here)
```

```{r wards etc}
Londonboroughs <- st_read(here::here("statistical-gis-boundaries-london",
                                     "statistical-gis-boundaries-london",
                                     "ESRI",
                                     "London_Borough_Excluding_MHW.shp"))%>%
  clean_names()%>%
  st_transform(., 27700)

Londonwards <- st_read(here::here("statistical-gis-boundaries-london",
                                     "statistical-gis-boundaries-london",
                                     "ESRI",
                                     "London_Ward.shp"))%>%
  clean_names()%>%
  st_transform(., 27700)

```


```{r accomodation data}
OSM <- st_read(here::here("greater-london-latest-free.shp",
                          "gis_osm_pois_a_free_1.shp"))%>%
  st_transform(., 27700)%>%
  clean_names()

Airbnb <- read_csv(here::here("listings.csv"))%>%
  st_as_sf(., coords = c("longitude", "latitude"),
           crs = 4326)%>%
  st_transform(., 27700)%>%
  clean_names()

Worldcities <- st_read(here::here("World_Cities",
                          "World_Cities.shp"))

ward_beds <- read_csv(here::here("ward_bedrooms.csv"))%>%
  clean_names()
```

```{r bed to borough}

beds_borough <- Londonwards %>%
  left_join(.,
            ward_beds,
            by = c("gss_code" = "geography_code"))%>%
  group_by(lb_gss_cd, borough)%>%
  summarise(total_beds_per_borough=sum(bedrooms_all_categories_number_of_bedrooms_measures_value, na.rm=TRUE))%>%
  st_drop_geometry()

```

```{r airbnb_borough}

Airbnb_borough <- Airbnb %>%
    filter(room_type == 'Entire home/apt')%>%
     filter(availability_365 =='365')%>%
    st_join(Londonboroughs, .)%>%
  group_by(gss_code)%>%
  summarise(airbnbcount=n())%>%
  st_drop_geometry()

```

```{r osm_hotels}
OSM_hotels <- OSM %>%
  filter(fclass == "hotel")%>%
  st_join(Londonboroughs, .)%>%
  group_by(gss_code)%>%
  summarise(hotelcount=n())%>%
  st_drop_geometry()

```

```{r all together}

all_data_one <- Londonboroughs %>%
  left_join(.,
             beds_borough,
            by = c("gss_code" = "lb_gss_cd"))

all_data_two <- all_data_one %>%
  left_join(.,
             Airbnb_borough,
            by = c("gss_code" = "gss_code"))

all_data_three <- all_data_two %>%
  left_join(.,
             OSM_hotels,
            by = c("gss_code" = "gss_code"))

mapnorm <- all_data_three %>%
  mutate(airbnb_norm=airbnbcount/total_beds_per_borough)%>%
  mutate(hotelnorm=hotelcount/total_beds_per_borough)

```


```{r extra_info}

Worldcities_extract <- Worldcities %>%
  clean_names()%>%
  fliter(c)

```
