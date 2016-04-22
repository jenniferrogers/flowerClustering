library(plyr)
library(sp)
library(spdep) # For the spatial weights matrix
library(ggplot2)
library(ggthemes)

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

plotAllDatesMoransI <- function (species, transect, flowerData)
{
  # For a given species name and transect, report the Moran's I
  # for every date that flowers were found.
  # Input: species, a string
  #        transect, a string
  #        flowerdata, the cleaned data (from cleanData())
  # Output: A line plot showing the moran's I statistic over time
  
  moransIData <- allDatesMoransI(species, transect, flowerData)
  
  moransIData$Date <- as.Date(moransIData$Date, format = "%m/%d/%Y")
  
  ciPlot <- ggplot(moransIData, 
                   aes(Date, moransI)) + geom_pointrange(aes(ymin = lowerCI, ymax = upperCI))
  ciPlot <- ciPlot + labs(title = paste("Moran's I for", species,"in Transect", transect, "with 95% Confidence Intervals"),
                          x = "Date",
                          y = "Moran's I")
  ciPlot <- ciPlot + theme_hc()
  print(ciPlot)
}

allDatesMoransI <- function(species, transect, flowerData)
{
  # For a given species name and transect, report the Moran's I
  # for every date that flowers were found.
  # Input: species, a string
  #        transect, a string
  #        flowerdata, the cleaned data (from cleanData())
  # Output: A data frame containing the Date, moransI, variance of moran's I,
  #         the associated p-value, and its 95% confidence interval bounds
  
  dates <- unique(flowerData[which(flowerData$Species.Name == species
                                   & flowerData$Transect == transect),]$Date)
  
  allIs <- adply(.data = dates,
                 .margins = 1,
                 .fun = function(obsDate) 
                   {
                   oneMoran <- getOneMoransI(species, 
                                             transect, 
                                             obsDate, 
                                             flowerData);
                   data.frame(Date = c(obsDate),
                              moransI = oneMoran$statistic,
                              variance = oneMoran$estimate[3],
                              p.value = oneMoran$p.value)
                 }
                 )
  
  # Find the distance between the mean and the upper and lower 95% confidence interval bounds
  # using the t-test with 50 - 1 degrees of freedom
  halfInterval <- qt(0.95, df = 50 - 1)/2 * (sqrt(allIs$variance) / sqrt(50))
  
  allIs$lowerCI <- allIs$moransI - halfInterval
  allIs$upperCI <- allIs$moransI + halfInterval
  
  return(allIs)
}

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

plotOne <- function(species, transect, date, flowerData)
{
  # Given a flower species, a transect, a date, and the cleaned flower data,
  # plot the number of flowers observed in each subsection and return the
  # plot as well
  
  # A spatial polygons data frame, including the subsection of the
  # transects, along with the number of flowers found in each subsection
  spatialData <- filterData(flowerSpecies = species,
                            transect = transect,
                            date = date, 
                            flowerData)
  
  spatialPlot <- spplot(spatialData["value.flowerNum"], main=list(label=date))
  
  print(spatialPlot)
  
  return(spatialPlot)
}

plotFour <- function(plot1, plot2, plot3, plot4,title)
{
  # GGplot help:
  # http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/#working-with-multi-panel-plots
  grid.arrange(plot1, plot2, plot3,plot4, ncol=2,nrow=2,top=title)
}

fourPlots <- function(species,transect,date1,date2,date3,date4,flowerData)
{
  # one plot containg 4 distributions on the given days for the given species
  # in the given transect
  plot1 = plotOne(species, transect, date1, flowerData)
  plot2 = plotOne(species, transect, date2, flowerData)
  plot3 = plotOne(species, transect, date3, flowerData)
  plot4 = plotOne(species, transect, date4, flowerData)
  # make the title!
  title <- paste("The Distribution of",species,"in",transect,sep=" ")
  #Plot it!
  plotFour(plot1, plot2, plot3, plot4,title)
}
