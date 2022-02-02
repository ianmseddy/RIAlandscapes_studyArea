sourceClimateDataCMIP6 <- function(type, GCM, SSP, studyAreaNameLong, dt,
                                   years, studyArea, rasterToMatch, dPath) {
  projectedClimateUrl <- dt[studyArea == "RIA" &
                             GCM == GCM &
                             SSP == SSP &
                             type == type, GID]
  demUrl <- "https://drive.google.com/file/d/13sGg1X9DEOSkedg1m0PxcdJiuBESk072/"

  ## FUTURE CLIMATE DATA
  projectedClimatePath <- checkPath(file.path(dPath, "climate", "future",
                                              paste0(GCM, "_ssp", SSP)), create = TRUE)
  projectedClimateArchive <- file.path(dirname(projectedClimatePath),
                                       paste0(studyAreaNameLong, "_",
                                              GCM, "_ssp",
                                              SSP, ".zip"))

  if (type == "proj_monthly"){


    projectedMDCfile <- file.path(dirname(projectedClimatePath),
                                  paste0("MDC_future_", GCM,
                                         "_ssp", SSP, "_", studyAreaName, ".tif"))

    ## need to download and extract w/o prepInputs to preserve folder structure!
    if (!file.exists(projectedClimateArchive)) {
      googledrive::drive_download(file = as_id(projectedClimateUrl), path = projectedClimateArchive)
      archive::archive_extract(projectedClimateArchive, projectedClimatePath)
      patterns <- "01.asc$|02.asc$|12.asc$|11.asc$|10.asc$|DD_|Rad"
      notNeeded <- list.files(projectedClimatePath, pattern = patterns, full.names = TRUE, recursive = TRUE)
      invisible(lapply(notNeeded, unlink))
    }

    projectedMDC <- Cache(
      climateData::makeMDC,
      inputPath = file.path(projectedClimatePath, studyAreaNameLong),
      years = years,
      # quick = "inputPath",
      omitArgs = c("years", "inputPath")
    )

    projectedMDC <- postProcessTerra(
      from = terra::rast(projectedMDC),
      to = rasterToMatch,
      maskTo = studyArea,
      writeTo = projectedMDCfile,
      quick = "writeTo",
      datatype = "INT2U")
    projectedMDc <- terra::rast(projectedMDC)
    projectedMDC <- raster::stack(projectedMDC) # fast
    return(projectedMDC)
  } else {


    ## CLIMATE DATA FOR gmcsDataPrep
    ## 1) get and unzip normals and projected annual
    ## 2) run makeLandRCS_1950_2010normals, it returns a raster stack with two layers, normal MAT, and normal CMI
    ## 3) assign normal CMI to sim
    ## 4) run makeLandRCS_projectedCMIandATA, with normal MAT as input. returns a list of stacks (projected ATA and CMI). Assign both to sim
    ## 5) Profit
    historicalClimatePath <- checkPath(file.path(dPath, "climate", "historic"), create = TRUE)
    normalsClimateUrl <- dt[studyArea == "RIA" & type == "hist_normals", GID]
    normalsClimatePath <- checkPath(file.path(historicalClimatePath, "normals"), create = TRUE)
    normalsClimateArchive <- file.path(normalsClimatePath, paste0(studyAreaNameLong, "_normals.zip"))

    if (!file.exists(normalsClimateArchive)) {
      ## need to download and extract w/o prepInputs to preserve folder structure!
      googledrive::drive_download(file = as_id(normalsClimateUrl), path = normalsClimateArchive)
      archive::archive_extract(normalsClimateArchive, normalsClimatePath)
    }

    normals <- Cache(makeLandRCS_1950_2010_normals,
                     pathToNormalRasters = file.path(normalsClimatePath, studyAreaNameLong),
                     rasterToMatch = rasterToMatch)
    ughGCM <- GCM
    ughSSP <- SSP
    #projAnnual
    projAnnualClimateUrl <- dt[studyArea == "RIA" &
                                 GCM == ughGCM &
                                 SSP == ughSSP &
                                 type == "proj_annual", GID]
    projAnnualClimatePath <- checkPath(file.path(projectedClimatePath, "annual"), create = TRUE)
    projAnnualClimateArchive <- file.path(dirname(projAnnualClimatePath),
                                          paste0(studyAreaNameLong, "_",
                                                 GCM, "_ssp",
                                                 SSP, "_annual.zip"))

    if (!file.exists(projAnnualClimateArchive)) {
      googledrive::drive_download(file = as_id(projAnnualClimateUrl), path = projAnnualClimateArchive)
      archive::archive_extract(projAnnualClimateArchive, projAnnualClimatePath)
      patterns <- "DD18.asc$|NFFD.asc$|bFFP.asc$|eFFP.asc$|DD_0.asc$|DD_18.asc|TD.asc$"
      notNeeded <- list.files(projAnnualClimatePath, pattern = patterns, full.names = TRUE, recursive = TRUE)
      invisible(lapply(notNeeded, unlink))
    }

    projCMIATA <- Cache(makeLandRCS_projectedCMIandATA,
                                normalMAT = normals$MATnormal,
                                pathToFutureRasters = file.path(projAnnualClimatePath, studyAreaNameLong),
                                years = years,
                                useCache =  'overwrite')

    return(list("CMInormal" = normals[["CMInormal"]],
                "projATA" = projCMIATA[["projectedATA"]],
                "projCMI" = projCMIATA[["projectedCMI"]]))

  }
}
