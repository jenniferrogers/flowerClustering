library(plyr)
library(sp)
library(spdep) # For the spatial weights matrix

##### Data Cleaning #####
# Primary function: cleanData(file.choose())
source("./dataCleaning.R")

############ Transect Building #############
# Primary function: makeTransects(flowerType, flowerObservations)
source("./transects.R")

############ Data Filtering & Transect Building #############

filterData <- function(flowerSpecies, transect, date, flowerData) 
{
  # takes in a data frame of cleaned data and filters it according to the transect,
  # species, and date given
  # run this function by typing filterData(flowerSpecies,transect) and give
  # the species name wanted to analyze, and the transect (note two spaces between
  # the name and letter) and then select the file (when prompted) that contains
  # the bee data
  
  # find all rows we want, dependent on whether we give a date or not
  if (missing(date)){
    flowers_data = which(flowerData$Species.Name == flowerSpecies & flowerData$Transect == transect)
  } else{
    flowers_data = which(flowerData$Species.Name == flowerSpecies & flowerData$Transect == transect &
                           flowerData$Date == date)
  }
  needed_data = c("Location..m.","X.Flowers")
  neededFlowerData <- flowerData[flowers_data,needed_data]
  cleanedFlowerData <- data.frame(Location = c(1:50),flowerNum = c(rep(0, 50)))
  # Add all of the locations we've seen to our cleanedFlowerData
  numRecords <- length(neededFlowerData$Location..m.)
  if (numRecords == 0){
    return(makeTransects(flowerType = flowerSpecies , cleanedFlowerData))
  } else {
    for (i in 1:numRecords)
      {
      # Look through i entries of the flower locations
      # Counting flowers in patch patchNumber
      patchNumber <- neededFlowerData$Location..m.[i]
      countedFlowers <- neededFlowerData$X.Flowers[i]
      
      if (is.na(countedFlowers)) {
        # Don't count the NA flowers.
        countedFlowers <- 0
      }
      oldCount <- cleanedFlowerData$flowerNum[patchNumber]
      cleanedFlowerData$flowerNum[patchNumber] = (oldCount + countedFlowers)
    }
  }
  return(makeTransects(flowerType = flowerSpecies, cleanedFlowerData))
}


# Input: Species, transect, set of dates, returns Moran's I for each date (and error bars)

getOneMoransI <- function(species, transect, date, flowerData)
{
  # Given a flower species, a transect, and a date on which that transect was visited,
  # as well as the cleaned flower data,
  # return the Moran's I measure of spatial autocorrelation, as well as the 95% confidence
  # intervals on that statistic
  
  # A spatial polygons data frame, including the subsection of the
  # transects, along with the number of flowers found in each subsection
  spatialData <- filterData(flowerSpecies = species,
                             transect = transect,
                             date = date, 
                             flowerData)
  
  neighbors <- poly2nb(spatialData)
  weights <- nb2listw(neighbors, style = "B", zero.policy = FALSE)
  moran_result <- moran.test(x = spatialData$value.flowerNum, 
                             listw = weights, 
                             zero.policy = FALSE)
  return(moran_result)
}

############ SEASON PLOTS :) #############
seasonData <- function(speciesName, transectName, season, file = file.choose())
{
  # this function will plot the species given in the transect given
  # during the four seasons (dates explained in paper)
  seasons <- seasonDates(transectName) # get the dates for the seasons
  # Spring Spatial Data Frame
  springDate <- seasons[1]
  summerDate <- seasons[2]
  fallDate <- seasons[3]
  winterDate <- seasons[4]
  
  cleanedData <- cleanData(file)
  
  if (season == "spring"){
    return(filterData(flowerSpecies=speciesName,transect=transectName,
                          date=springDate, cleanedData))
  } else if (season == "summer"){
    return(filterData(flowerSpecies=speciesName,transect=transectName,
                          date=summerDate, cleanedData))
  } else if (season == "fall"){
    if (transectName == "USS a" | transectName == "USS b"){
      print("This transect does not have fall data")
      return
    }
    else { 
      return(filterData(flowerSpecies=speciesName,transect=transectName,
                        date=fallDate, cleanedData))
    }
  }
  else if (season == "winter"){
    if (transectName == "USS a" | transectName == "USS b"){
      print("This transect does not have winter data")
      return()
    }
    else {
      return(filterData(flowerSpecies=speciesName,transect=transectName,
                        date=winterDate, cleanedData))
    }
  }
}

# Put all of the scripts in a function, so we can source this file
# without it running code. :)
runScripts <- function()
{
  # We can run the above for instance with the following code:
  #springData <- seasonData(speciesName="erodium cicutarium",transectName="BRS a",season="spring")
  #spplot(springData["value.flowerNum"])
  # We can plot all seasons for one species as follows:
  springData <- seasonData(speciesName="erodium cicutarium",transectName="BRS a",season="spring")
  summerData <- seasonData(speciesName="erodium cicutarium",transectName="BRS a",season="summer")
  fallData <- seasonData(speciesName="erodium cicutarium",transectName="BRS a",season="fall")
  winterData <- seasonData(speciesName="erodium cicutarium",transectName="BRS a",season="winter")
  p1 = spplot(springData["value.flowerNum"])
  p2 = spplot(summerData["value.flowerNum"])
  p3 = spplot(fallData["value.flowerNum"])
  p4 = spplot(winterData["value.flowerNum"])
  # Make a composite plot:
  print(p1, position = c(0,.5,.5,1),more=T)
  print(p2, position = c(.5,.5,1,1),more = T)
  print(p3, position = c(0,0,.5,.5),more=T)
  print(p4, position = c(.5,0,1,.5))
  # get four plots with spring summer winter and fall plotted
  # help from : https://stat.ethz.ch/pipermail/r-sig-geo/2008-August/004026.html
  
  
  springData <- seasonData(speciesName="eriogonum fasciculatum",transectName="NNG b",season="spring")
  summerData <- seasonData(speciesName="eriogonum fasciculatum",transectName="NNG b",season="summer")
  fallData <- seasonData(speciesName="eriogonum fasciculatum",transectName="NNG b",season="fall")
  winterData <- seasonData(speciesName="eriogonum fasciculatum",transectName="NNG b",season="winter")
  p1 = spplot(springData["value.flowerNum"])
  p2 = spplot(summerData["value.flowerNum"])
  p3 = spplot(fallData["value.flowerNum"])
  p4 = spplot(winterData["value.flowerNum"])
  # Make a composite plot:
  print(p1, position = c(0,.5,.5,1),more=T)
  print(p2, position = c(.5,.5,1,1),more = T)
  print(p3, position = c(0,0,.5,.5),more=T)
  print(p4, position = c(.5,0,1,.5))
}
