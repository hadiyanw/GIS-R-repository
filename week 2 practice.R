library(RSQLite)
library(dplyr)
library(tidyverse)
library(janitor)
##Basic plotting for displaying data as a graphical output

  #Creating datasets
  Data1 <- c(1:100)
  Data2 <- c(101:200)
  
  #creating normally distributed (rnorm) datasets with some prorerties (mean, std deviation)
  Data3 <- rnorm(100, mean = 53, sd = 34)
  Data4 <- rnorm(100, mean = 64, sd = 14)
  
  #Plotting the Data
  plot(Data1,Data2, col= "red")
  plot(Data3,Data4, col = "magenta")

##Creating dataframe using data.frame command and plotting it in
df <- data.frame(Data1,Data2)
plot(df)
  #looking the partial data, usually a head or/and tail of it using head/tail command
  head(df, n = 3)
  tail(df, n = 4)
  
  #Calling/indexing spesific row and coloumn using square bracket with nameofdataframe[row,coloumn]
  df[70,2]
  #another examples
  df[1:10,1]
  df[5:15,]
  df[c(2,3,6),2]
  df[,1]  
  
  #renaming the coloumn
  df <- df %>%
    dplyr::rename(coloumn1 = Data1, coloumn2 = Data2)
    df

  #Selecting coloumn by it's name
    df <- df %>% 
    dplyr::select('coloumn2') #insert by the name of coloumn
    df
    
  #another method to select column is by using $, because not all of data is compatible with dplyr
  df$coloumn1
  df$coloumn2
  #or using double square bracket[[]] and insert it with the coloumn's name
  df[['coloumn1']]
  
  #Importing LondonData.csv file, both methods are acceptable
  LondonDataOSK <- read.csv2('C:/Users/Lenovo/Documents/CASA/modules/GIS/Project/week 2 data/LondonData.csv')
  View(LondonDataOSK)  
  LondonDataOSK2 <- read.csv('C:/Users/Lenovo/Documents/CASA/modules/GIS/Project/week 2 data/LondonData.csv',
                             header = TRUE,
                             sep = ';')
  #another way to read the data with 'here' library
  install.packages('here')
  library('here')
  here::here()  
  LondonDataOSK <- read.csv(here::here('week 2 data','LondonData.csv'), #this method significantly reduce the use of / or\\
                            head = TRUE,
                            sep = ';',
                            encoding = 'latin1')
  
# New school cleaning by using readr in tidyverse library
  LondonData <- read_csv('https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv',
                                locale = locale(encoding = 'latin1'), #this code specifies the way we store the data, the guide say R is using latin1 meanwhile the package using UTF-8, we have to specify it
                                na = "n/a") # <- this code automatically define "n/a" data as na data type, so we dont have to manually change it in excel file firt as old school method
  view(LondonDataOSK_web) # i will use this data for a whole practical section
  UKData <- read_csv('https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv',
                         locale = locale(encoding = 'latin1'), #this code specifies the way we store the data, the guide say R is using latin1 meanwhile the package using UTF-8, we have to specify it
                         na = "n/a")
# checking data type  
  class(LondonData)
  datatypelist <- LondonData %>%
    summarise_all(class) %>% #summarizing the name of the coloumn and the class type of it in a list form (liting in a row)
    pivot_longer(everything(), # <- pivoting (change the row into coloumn for all of the data frame variables)
                 names_to = 'variables name', # <- giving the name of names coloumn
                 values_to = 'variables value') # <- giving the name of values coloumn
  view(datatypelist)
  
  #we can edit the data by typing this code
  LondonData <- edit(LondonData)

  
# Data Manipulation in R
  # Selecting Row (Selecting row 626 untul 658 the place where most of us live)
  LondonBorough <- LondonData[626:658,] #method 1
  LondonBorough <- LondonData %>% # method 2, Using dplyr library
      slice(626:658)
  #filtering value of data frame using certain criteria
 view(LondonData)
   femalelifeexp <- LondonData %>%
     filter(`Female life expectancy -2009-13`>90) # Dont forget to use `` sign suround column name to call it as a coloumn name
  view(femalelifeexp)
  #filtering values to detect some string as indication to certain value, in this case we want all E09... in new code to be selected as Londonborough
  LondonBorough <- LondonData %>%
    filter(str_detect(`New code`,'E09'))
  view(LondonBorough)
  LondonBorough$`Ward name` #Then we can call the data that has been filtered
  dplyr::select(LondonBorough,`Ward name`) %>% #or we can use this method
      print()
  #Selecting coloumn 
  LondonBorough_manualcols <- LondonBorough[,c(1,19,20,21)] #we can use this method if qwe already know the index of the coloumn
  LondonBorough_manualcols <- dplyr::select(LondonBorough,c(1,19,20,21)) #or we can use this method also                                        
  LondonBorough_contain <- LondonBorough %>% #selecting coloumn using contains word from coloumn names
    dplyr::select(contains('expectancy'), 
                  contains("obese"),
                  contains("Ward name"))
  view(LondonBorough_contain)  
  #Renaming the coloumn
  install.packages('janitor') #use janitor packages, by default this package removes all capitals and gives underscore wherever there is a space
  library(janitor)
  LondonBorough <- LondonBorough %>%
    dplyr::rename(Borough = `Ward name`) %>%
    clean_names() #this function is inside janitor package
  head(LondonBorough) #overall what changes in the coloumns name are 1.there is no capital word, 2. all space is changed with underscore, 3. there is no `` sign surrounding coloumn name
  #Also if you want to make all of coloumn name is capital use this code
  #LondonBorugh <- LondonBorough %>%
  #clean_names(.,case ="Big_camel") ., sign is for all data
  
  #Creating a new coloumn using mutate command
  Life_expectancy <- LondonBorough %>%
      #New coloumn with average of male and female life expectancy
      mutate(averagelifeexpectancy = (female_life_expectancy_2009_13 + male_life_expectancy_2009_13)/2) %>%
      #New coloumn with normalised life expectancy
      mutate(normalisedlifeexpectancy = averagelifeexpectancy/mean(averagelifeexpectancy)) %>%
      #selecting the coloumn we need
      dplyr::select(new_code, 
                    borough,
                    averagelifeexpectancy,
                    normalisedlifeexpectancy) %>%
      #Trying to presenting it descendingly
      arrange(desc(normalisedlifeexpectancy))
      view(Life_expectancy)        
  
    slice_head(Life_expectancy, n = 5)      
    head(Life_expectancy, n = 5) #These two are the same function but we will use slice because it has bigger family such slice_max/min/head/tail
    
    #geting the UK average life expectancy
    UKData <- UKData %>%
      clean_names()
    view(UKData)
    head(UKData)
    UKData <- UKData %>% 
      mutate(male_female_averagelifeexpectancy = (male_life_expectancy_2009_13 + female_life_expectancy_2009_13)/2)
    UKData   
        UK_life_expectancyaverage <- UKData %>%
          dplyr::select(male_female_averagelifeexpectancy) %>%
          colMeans(., na.rm = TRUE)
    UK_life_expectancyaverage    
    
    #giving indicator of live expectancy relative to UK average
    Life_expectancy2 <- Life_expectancy %>%
      mutate(UKCompare = case_when(averagelifeexpectancy > UK_life_expectancyaverage ~ "above UK average",
                                   TRUE ~ "below UK average"))
    view(Life_expectancy2)
    
    #Grouping London boroughs which have life expectancy greater than UK average
    #Also summarising the range, count, and average of London life expectancy between above and below average group
    Life_expectancy2_group <- Life_expectancy2 %>%
      mutate(UKdiff = averagelifeexpectancy - UK_life_expectancyaverage) %>%
      group_by(UKCompare) %>%
      summarise(range = max(UKdiff)-min(UKdiff), count = n(), Average = mean(UKdiff))
    view(Life_expectancy2_group)
    
    #More and more andvance in summarising data about UK to make it more meaningful
    #The fuck is this I have to look at it again and again
    Life_expectancy3 <- Life_expectancy %>%
      mutate(UKdiff = averagelifeexpectancy-UK_life_expectancyaverage) %>%
      mutate(across(where(is.numeric),round,3)) %>%
      mutate(across(UKdiff, round, 0)) %>%
      mutate(UKcompare = case_when(averagelifeexpectancy >= 82 ~
                                     str_c("equal or above UK average by", UKdiff, "years", sep = " "),
                                   TRUE ~ str_c("below UK average by", UKdiff, "years", sep = " "))) %>%
      group_by(UKcompare) %>%
      summarise(count=n())
    Life_expectancy3
    
#Plotting London Borough life expectancy and children obesity
    plot(LondonBorough$male_life_expectancy_2009_13,
         LondonBorough$percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14)
    #decorate the plot
    install.packages("plotly")
    library(ggplot2)
    library(plotly)    
    plot_ly(LondonBorough,
            x = ~male_life_expectancy_2009_13,
            y = ~percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14,
            text = ~borough,
            type = "scatter",
            mode = "markers")    
#Spatial Data in R    
    install.packages("maptools")    
    install.packages(c("classInt","tmap"))
    install.packages(c("RColorBrewer", "sp", "rgeos", 
                       "tmaptools", "sf", "downloader", "rgdal", 
                       "geojsonio"))   
    #Load Packages (ignore any error messages about being built under a 
    #different R version):
    library(maptools)
    library(RColorBrewer)
    library(classInt)
    library(sp)
    library(rgeos)
    library(tmap)
    library(tmaptools)
    library(sf)
    library(rgdal)
    library(geojsonio)   