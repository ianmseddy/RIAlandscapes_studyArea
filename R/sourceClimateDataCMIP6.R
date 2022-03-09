sourceClimateDataCMIP6 <- function(Type, gcm, ssp, studyAreaNameLong, dt,
                                   years, studyArea, rasterToMatch, dPath) {

  projectedClimateUrl <- dt[studyArea == "RIA" &
                              GCM == gcm &
                              SSP == ssp &
                              type == Type, GID]
  demUrl <- "https://drive.google.com/file/d/13sGg1X9DEOSkedg1m0PxcdJiuBESk072/"

  ## FUTURE CLIMATE DATA
  projectedClimatePath <- checkPath(file.path(dPath, "climate", "future",
                                              paste0(gcm, "_ssp", ssp)), create = TRUE)
  projectedClimateArchive <- file.path(dirname(projectedClimatePath),
                                       paste0(studyAreaNameLong, "_",
                                              gcm, "_ssp",
                                              ssp, ".zip"))

  if (Type == "proj_monthly") {

    projectedMDCfile <- file.path(dirname(projectedClimatePath),
                                  paste0("MDC_future_", gcm,
                                         "_ssp", ssp, "_", studyAreaName, ".tif"))

    ## need to download and extract w/o prepInputs to preserve folder structure!
    if (!file.exists(projectedMDCfile)) {
      if (!file.exists(projectedClimateArchive)){
        googledrive::drive_download(file = as_id(projectedClimateUrl), path = projectedClimateArchive)
        archive::archive_extract(projectedClimateArchive, projectedClimatePath)
        patterns <- "01.asc$|02.asc$|12.asc$|11.asc$|10.asc$|DD5_|DD18_|Rad|^PAS|^RH"
        notNeeded <- list.files(projectedClimatePath, pattern = patterns, full.names = TRUE, recursive = TRUE)
        invisible(lapply(notNeeded, unlink))
      }
      projectedMDC <- climateData::makeMDC(
        inputPath = file.path(projectedClimatePath),
        years = years)

      projectedMDC <- postProcessTerra(
        from = terra::rast(projectedMDC),
        to = rasterToMatch,
        maskTo = studyArea,
        writeTo = projectedMDCfile,
        quick = "writeTo",
        datatype = "INT2U")

      projectedMDC <- raster::stack(projectedMDC) # fast
    } else {
      projectedMDC <- raster::stack(projectedMDCfile)
    }
    return(projectedMDC)
  } else {

    ## CLIMATE DATA FOR gmcsDataPrep
    ## 1) get and unzip normals and projected annual
    ## 2) run makeLandRCS_1950_2010normals, it returns a raster stack with two layers, normal MAT, and normal CMI
    ## 3) assign normal CMI to sim
    ## 4) run makeLandRCS_projectedCMIandATA, with normal MAT as input. returns a list of stacks (projected ATA and CMI).
    historicalClimatePath <- checkPath(file.path(dPath, "climate", "historic"), create = TRUE)
    normalsClimateUrl <- dt[studyArea == "RIA" & type == "hist_normals", GID]
    normalsClimatePath <- checkPath(file.path(historicalClimatePath, "normals"), create = TRUE)
    normalsClimateArchive <- file.path(normalsClimatePath, paste0(studyAreaNameLong, "_normals.zip"))

    if (!file.exists(normalsClimateArchive)) {
      ## need to download and extract w/o prepInputs to preserve folder structure!
      googledrive::drive_download(file = as_id(normalsClimateUrl), path = normalsClimateArchive)
      archive::archive_extract(normalsClimateArchive, normalsClimatePath)
    }

    normals <- makeLandRCS_1950_2010_normals(
      pathToNormalRasters = file.path(normalsClimatePath, studyAreaNameLong),
      rasterToMatch = rasterToMatch)

    #projAnnual
    projAnnualClimateUrl <- dt[studyArea == "RIA" &
                                 GCM == gcm &
                                 SSP == ssp &
                                 type == "proj_annual", GID]
    projAnnualClimatePath <- checkPath(file.path(projectedClimatePath, "annual"), create = TRUE)
    projAnnualClimateArchive <- file.path(dirname(projAnnualClimatePath),
                                          paste0(studyAreaNameLong, "_",
                                                 gcm, "_ssp",
                                                 ssp, "_annual.zip"))


    CMIfile <- file.path(dPath, "climate/future",
                         paste0("CMI_future_", gcm, "_ssp", sspNoDots, "_", studyAreaName, ".tif"))
    ATAfile <- file.path(dPath, "climate/future",
                         paste0("ATA_future_", gcm, "_ssp", sspNoDots, "_", studyAreaName, ".tif"))

    if (!file.exists(CMIfile) | !file.exists(ATAfile)) {
      if (!file.exists(projectedClimateArchive)){
        googledrive::drive_download(file = as_id(projAnnualClimateUrl), path = projAnnualClimateArchive)
        archive::archive_extract(projAnnualClimateArchive, projAnnualClimatePath)
        patterns <- "DD18.asc$|NFFD.asc$|bFFP.asc$|eFFP.asc$|DD_0.asc$|DD1040.asc$|TD.asc$|DD5.asc$|EXT.asc$"
        notNeeded <- list.files(projAnnualClimatePath, pattern = patterns, full.names = TRUE, recursive = TRUE)
        invisible(lapply(notNeeded, unlink))
      }
      projCMIATA <- makeLandRCS_projectedCMIandATA(
        normalMAT = normals$MATnormal,
        pathToFutureRasters = file.path(projAnnualClimatePath, studyAreaNameLong),
        years = years)

      if (!file.exists(ATAfile)){
        raster::writeRaster(projCMIATA$projectedATA, ATAfile)
      }
      if (!file.exists(CMIfile)){
        raster::writeRaster(projCMIATA$projectedCMI, CMIfile)
      }
    }
    projATA <- raster::stack(ATAfile)
    projCMI <- raster::stack(CMIfile)

    return(list("CMInormal" = normals[["CMInormal"]],
                "projATA" = projATA,
                "projCMI" = projCMI))
  }
}
