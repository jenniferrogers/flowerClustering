# Data cleaning

# Given the collected observations (downloaded from the Google sheet as csv),
# do basic data cleaning, espcially to remove typos
# Import the data into R, remove typos and inconsistencies

library(plyr)

# Takes a CSV file, and returns a data frame with the flower data, 
# cleaned of typos
cleanData <- function(filename = "./BFS Master Survey - Sheet1.csv")
{
  flowerData <- read.csv2(file = filename, 
                          header = TRUE, 
                          sep = ",", 
                          strip.white = TRUE,
                          colClasses = c("Species.Name" = "character",
                                         "Date" = "character"))
  
  # Some of the rows are blank
  filledInRows <- which(flowerData$Species.Name != "")
  
  # We only want the transects labeled with "a" or "b"
  correctTransectRows <- grep(" a| b", flowerData$Transect)
  
  # Sometimes, there's an extra space in the names. Remove it.
  flowerData$Transect <- cleanTransect(flowerData$Transect)
  
  # Sometimes the data is recorded weirdly, fix it 
  flowerData$Transect <- aaply(flowerData$Transect, 1, renameTransects)
  
  # If the location is not just a number, we throw it out
  rowsOutsideTransect <- which(is.na(as.numeric(as.character(flowerData$Location..m.))))
  
  correctRows <- setdiff(intersect(filledInRows, correctTransectRows), rowsOutsideTransect)
  flowerObservations <- flowerData[correctRows,]
  
  # Correct for spelling mistakes in the species name
  flowerObservations$Species.Name <- aaply(flowerObservations$Species.Name, 1, cleanSpecies)
  
  # Change location to number
  flowerObservations$Location..m. <- as.numeric(as.character(flowerObservations$Location..m.))
  
  # Change flowers to number
  flowerObservations$X.Flowers <- as.numeric(as.character(flowerObservations$X.Flowers))
  
  return(flowerObservations)
}

cleanSpecies <- function(speciesName)
{
  ### Consolidate species names
  newName = speciesName
  
  if (speciesName == "Amsinckia menzeisii" | speciesName == "Amsinckia intermedia")
  {
    newName = "Amsinckia menzeissi"
  }
  
  if (speciesName == "Ericamaria pinifolia")
  {
    newName = "Ericameria pinifolia"
  }
  
  if (speciesName == "Erodium cicutarum" |
      speciesName == "Eriodium cicutarium")
  {
    newName = "Erodium cicutarium"
  }
  
  if (speciesName == "Hirschfeldiana incana" | 
      speciesName == "Hirshfeldia incana" |
      speciesName == "Hirshfeldiana incana")
  {
    newName = "Hirschfeldia incana"
  }
  
  if (speciesName == "Mirablilis laevis")
  {
    newName = "Mirabilis laevis"
  }
  
  if (speciesName == "Phacelis distans")
  {
    newName = "Phacelia distans"
  }
  
  if (speciesName == "Sambuccus nigra")
  {
    newName = "Sambucus nigra"
  }
  
  
  # Return all names as lowercase, so we don't have to deal with issues of case
  return(tolower(newName))
}

cleanTransect <- function(transectName)
{
  newName = sub("  ", " ", transectName)
  return(newName)
}

renameTransects <- function(transectName)
{
  if (transectName == "NNG a (South to North)"){
    newName = "NNG a"
  } else if (transectName == "BRS a (West to East)"){
    newName = "BRS a"
  } else {
    newName = transectName
  }
  return(newName)
}
