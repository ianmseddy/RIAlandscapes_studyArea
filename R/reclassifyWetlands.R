#' @param lcc the landcover raster
#' @param destinationPath the path to download the Wulter et al. wetland product
#' @param origWetland the original wetland classes
#' @param origForest the original forest classes
#' @return a reclassified landcover map, with forested wetlands as a new class
#'
#' @export
#' @importFrom reproducible prepInputs
#' @importFrom data.table data.table

reclassifyWetlands <- function(lcc, destinationPath, origWetland = 14, origForest = 1:6) {
  wetlands <- prepInputs(url = "https://opendata.nfis.org/downloads/forest_change/CA_wetlands_post2000.zip",
                         rasterToMatch = lcc,
                         useCache = TRUE,
                         userTags = c("wetlands", "RIAlandscapes_studyArea"),
                         destinationPath = destinationPath)
  classes <- data.table(pixelID = 1:ncell(lcc),
                        lcc = getValues(lcc),
                        wetland = getValues(wetlands))
  newForest <- max(classes$lcc, na.rm = TRUE) + 1
  message("adjusting ",  nrow(classes[!lcc %in% origForest & !lcc %in% origWetland & wetland == 1]),
          " nonforested pixels with wetland")
  classes[!lcc %in% origForest & wetland == 1, lcc := origWetland]
  message('adjusting ', nrow(classes[lcc %in% origForest & wetland == 1]),
          " forested pixels to class ", newForest, " with wetlands")
  classes[lcc %in% origForest & wetland == 1, lcc := newForest]

  lcc <- setValues(lcc, classes$lcc)
  return(lcc)
}
