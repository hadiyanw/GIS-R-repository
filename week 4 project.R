library(tidyverse)
library(ggplot2)
library(tmap)
library(sf)

#importing csv and shape file data
GII <- read.csv('Gender Inequality Index (GII)_edited.csv', sep = ';', header = TRUE)
View(GII)
head(GII)
shapewolrd <- st_read('World_Countries__Generalized_.shp')
plot(shapewolrd)
head(shapewolrd)


# change data type of 2011 and 2019 index from character to numeric
GII <- GII %>%
  mutate( X2019 = case_when(X2019 == ".." ~ "0",
                            TRUE ~ X2019))
GII <- GII %>%
  mutate( X2011 = case_when(X2011 == ".." ~ "0",
                            TRUE ~ X2011))
GII <- GII %>%
  mutate(X2019 = as.numeric(X2019))
GII <- GII %>%
  mutate(X2011 = as.numeric(X2011))

#Creating coloumn index difference between 2011 and 2019 gender inequalitiy index
GII <- GII %>%
  mutate(indexgap_19_11 = X2019 - X2011) # this equation means the smaller the value (include negative) the better

#Merging data
shapewolrd <- shapewolrd %>% 
    merge(., 
          GII,
          by.x = 'ISO',
          by.y = 'code')
shapewolrd %>%
  head(., n=10 )
view(shapewolrd)
tmap_mode('plot')
shapewolrd %>%
  qtm(., fill = 'indexgap_19_11', midpoint = NA, palette = "seq")
