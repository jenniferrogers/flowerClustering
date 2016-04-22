# Transect Building

# Create the spatial framework for analyzing this data.
# In R, you need to outline the polygons that your data
# belongs to. You need to establish relationships between
# these polygons in order to do spatial analysis.

makeTransects <- function(flowerType = "", flowerObservations)
{
  ## The transect starts at (0, 0) and has 2x1 boxes along the 50 unit transect
  # Returns a SPDF
  
  if (length(flowerObservations) != 50)
  {
    simpleError("Need 50 transects worth of data")
  }
  
  ID <- paste0(flowerType, c(1:50))
  
  onePatch <- matrix(c(0, 0, 2, 0, 2, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)
  onePolygon <- Polygon(onePatch)
  
  # A list of matrices representing the coordinates of patches
  allPatches <- alply(c(0:49), 1, translatePatchUp, patch = onePatch)
  
  allPolygons <- llply(allPatches, Polygon)
  
  listOfPolygons <- list()
  # Need to do this for all polygons
  for(i in 1:50)
  {
    listOfPolygons[[i]] <- Polygons(list(allPolygons[[i]]), ID[i])
  }
  
  spatialFlowerPolygons <- SpatialPolygons(listOfPolygons)
  
  # Add values to data frame
  flowerDataFrame <- data.frame(value = flowerObservations, row.names = ID)
  
  flowerSpatialDataFrame <- SpatialPolygonsDataFrame(spatialFlowerPolygons, flowerDataFrame)
  
}

translatePatchUp <- function(patch, units)
{
  ## Move the transect patch in the y-direction, "units" units
  patch[,2] <- patch[,2] + units
  return(patch)
}

makeSinglePolygon <- function(polygon, id)
{
  # For ease of applying to many polygons and ids
  return(Polygons(list(polygon), id))
}
