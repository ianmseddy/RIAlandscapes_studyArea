defineModule(sim, list(
  name = "RIAlandscapes_studyArea",
  description = "",
  keywords = "",
  authors = structure(list(list(given = c("Ian"), family = "Eddy", role = c("aut", "cre"), email = "ian.eddy@canada.ca", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.6.9026", RIAlandscapes_studyArea = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "RIAlandscapes_studyArea.Rmd")),
  reqdPkgs = list("ggplot2", "raster", "data.table", "sf", "rgeos", "LandR",
                  "RColorBrewer", "terra", "climateData"),
  parameters = rbind(
    defineParameter("GCM", "character", "CanESM2", NA, NA,
                    desc = "the GCM to use - will be passed to prepInputs call"),
    defineParameter("historicalFireYears", "numeric", default = 1991:2019, NA, NA,
                    desc = "range of years captured by the historical climate data in prepInputs call"),
    defineParameter("SSP", "numeric", 370, NA, NA,
                    desc = "the SSP  - 235, 370, etc"),
    defineParameter("studyAreaName", "character", "BC", NA, NA,
                    desc = "currently one of BC, Yukon, or 5TSA"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should caching of events or module be activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant"))
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput("ATAstack", objectClass = "RasterStack",
                  desc = "raster stack of annual temperature anomaly"),
    createsOutput("CMInormal", objectClass = "RasterLayer",
                  desc = "raster of 1950-2010 climate moisture index normal"),
    createsOutput("CMIstack", objectClass = "RasterStack",
                  desc = "stack of annual climate moisture index"),
    createsOutput("ecoregionRst", objectClass = "RasterLayer", desc = "Ecoregions - BEC Zones"),
    createsOutput("historicalClimateRasters", objectClass = "list",
                  desc = "list of a single raster stack - historical MDC calculated from ClimateNA data"),
    createsOutput("projectedClimateLayers", objectClass = "list",
                  desc = "list of raster stacks - in this case just MDC"),
    createsOutput("rasterToMatch", objectClass = "RasterLayer",
                  desc = "template raster"),
    createsOutput("rasterToMatchLarge", objectClass = "RasterLayer",
                  desc = "template raster for larger area"),
    createsOutput("rasterToMatchReporting", objectClass = "RasterLayer",
                  desc = "template raster for reporting area"),
    createsOutput("rstLCC2010", objectClass = "RasterLayer",
                  desc = "2010 landcover classes"),
    createsOutput("sppColorVect", objectClass = "character",
                  desc = "species colours for plotting"),
    createsOutput("sppEquiv", objectClass = "character",
                  desc = "table of LandR species names equivalencies"),
    createsOutput("sppEquivCol", objectClass = "character",
                  desc = "name of column to use in sppEquiv"),
    createsOutput("standAgeMap2011", objectClass = "RasterLayer",
                  desc = "time since disturbance raster for year 2011"),
    createsOutput("studyArea", objectClass = "SpatialPolygonsDataFrame",
                  desc = "study area used for simulation (buffered to mitigate edge effects)"),
    createsOutput("studyAreaLarge", objectClass = "SpatialPolygonsDataFrame",
                  desc = "study area used for module parameterization (buffered)"),
    createsOutput("studyAreaPSP", objectClass = "SpatialPolygonsDataFrame",
                  desc = "study area used to parameterize LandR.CS PSPs"),
    createsOutput("studyAreaReporting", objectClass = "SpatialPolygonsDataFrame",
                  desc = "study area used for reporting/post-processing")
))
)

## event types
#   - type `init` is required for initialization

doEvent.RIAlandscapes_studyArea = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {

      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "RIAlandscapes_studyArea", "save")
    },
    save = {
      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {

  studyAreaNameLong <- "wholeRIA_20kmbuff_1ArcMinDEM"

  dPath <- file.path('modules', currentModule(sim), 'data')
  cacheTags <- c(P(sim)$studyAreaName, currentModule(sim))

  allowedStudyAreas <- c("BC", "Yukon", "RIA", "WCB", "WB", "SB")
  if (!P(sim)$studyAreaName %in% allowedStudyAreas) {
    stop("incorrectly specified studyAreaName")
  }

  #soem default URLS
  ecozoneUrl <-"https://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip"
  #ria Ecoregions
  ecoregionRstUrl <- "https://drive.google.com/file/d/1ZwkMt6ux6RY-SpWvUsGST009EnB0U541/view?usp=sharing"
  #the combined BEC sub-zones and Yukon subzones - Yukon zones are preceeded by 1000

  switch(P(sim)$studyAreaName,
         "BC" = {
           studyAreaUrl <- "https://drive.google.com/file/d/1LAXjmuaCt0xOWP-Nmll3xfRqCq-NbJP-/view?usp=sharing"
           #6 TSAs inside RIA, merged

           #rasterized BEC zone variants
           ecoregionRstUrl <- 'https://drive.google.com/file/d/1R38CXviHP72pbMq7hqV5CfT-jdJFZuWL/view?usp=sharing'

           #the projected BECzones

         },
         "Yukon" = {
           studyAreaUrl <- "https://drive.google.com/file/d/14f2Hb0UDL6sn49gXAFY9LxPQ6NTgUflM/view?usp=sharing"
           #Yukon BEC zones, from
           # https://map-data.service.yukon.ca/GeoYukon/Biophysical/Bioclimate_Zones_and_Subzones/Bioclimate_zones_and_subzones.zip
           ecoregionRstUrl <- "https://drive.google.com/file/d/1Dce0_rSBkxKjNM9q7-Zsg0JFidYu6cKP/view?usp=sharing"
         },
         "RIA" = {
           studyAreaUrl <- "https://drive.google.com/file/d/1FlC5YdjNF8wXLcA4hQxLvrRjVC6ShqND/view?usp=sharing"
           #the whole RIA area with geometry issues solved
           ecoregionRstUrl <- "https://drive.google.com/file/d/1ZwkMt6ux6RY-SpWvUsGST009EnB0U541/view?usp=sharing"
           #the combined BEC sub-zones and Yukon subzones - Yukon zones are preceeded by 1000
         }
  )


  #These objects were created from the original RIA boundaries, cropped by province.
  #Some geoprocessing was done in advance because the RIA polygons were full of topological errors,
  # and the BEC zone shapefiles were too slow and unreliable with regards to downloading/caching
  # the fire regime polygons are not needed with fireSense but kept in case of scfm runs
  # this is not ideal as the study area cannot be easily changed, also for reproducible reasons.
  if (P(sim)$studyAreaName %in% c("Yukon", "BC", "RIA")) {
    ####studyArea####
    sim$studyArea <- Cache(prepInputs,
                           url = studyAreaUrl,
                           destinationPath = dPath,
                           useCache = P(sim)$.useCache,

                           overwrite = TRUE,
                           userTags = c(P(sim)$studyAreaName, "studyArea"))
    sim$studyAreaReporting <- sim$studyArea

  } else {
    #get RIA - crop ecodistricts to RIA. sort out ecodistricts. merge polygons.
    sim$studyAreaReporting <- Cache(getStudyArea,
                                    studyArea = P(sim)$studyAreaName,
                                    dPath = dPath,
                                    userTags = c(P(sim)$studyAreaName, "studyArea"))
    kNNCRS <- crs("+proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
    #convert to sf
    sim$studyAreaReporting <- st_transform(sim$studyAreaReporting,crs = kNNCRS)
    sim$studyArea <- sf::st_buffer(sim$studyAreaReporting, 2000)
    sim$studyArea$studyAreaName <- P(sim)$studyAreaName
  }

  sim$standAgeMap2011 <-  prepInputsStandAgeMap(
    ageURL = paste0("https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
                    "canada-forests-attributes_attributs-forests-canada/",
                    "2011-attributes_attributs-2011/",
                    "NFI_MODIS250m_2011_kNN_Structure_Stand_Age_v1.tif"),
    studyArea = sim$studyArea,
    destinationPath = dPath,
    startTime = 2011,
    # filename2 = .suffix("standAgeMap_2011.tif", paste0("_", P(sim)$studyAreaName)),
    userTags = c("prepInputsStandAgeMap", P(sim)$studyAreaname)
  )

  sim$rasterToMatch <- sim$standAgeMap2011
  sim$rasterToMatchLarge <- sim$rasterToMatch
  sim$studyAreaLarge <- sim$studyArea


  ####ecoregionRst####

  #use projected BEC zones instead, for AM
  if (studyAreaName %in% c("BC", "SB", "WCB", "WB")) {
    sim$ecoregionRst <- prepInputs(url = "https://drive.google.com/file/d/19o037fMpMiRSE7MNbcFseDQ09Ot38ahr/view?usp=sharing",
                                   destinatinoPath = dPath,
                                   studyArea = sim$studyArea,
                                   rasterToMatch = sim$rasterToMatch)
  } else {
    sim$ecoregionRst <- Cache(prepInputs,
                              url = ecoregionRstUrl,
                              studyArea = sim$studyArea,
                              rasterToMatch = sim$rasterToMatch,
                              destinationPath = dPath,
                              filename2 = paste0(P(sim)$studyAreaName, "_ecoregionRst.tif"),
                              userTags = c("ecoregionRst", P(sim)$studyAreaName))
  }
  ####LCC2010####
  sim$rstLCC2010 <- Cache(prepInputs,
                          url = 'https://drive.google.com/file/d/1WcCEkwjnDq74fx3ZBizlIKzLkjW6Nfdf/view?usp=sharing',
                          targetFile = 'CAN_LC_2010_CAL.tif',
                          method = 'ngb',
                          destinationPath = dPath,
                          filename2 = paste0(P(sim)$studyAreaName, "_LCC2010.tif"),
                          rasterToMatch = sim$rasterToMatch,
                          studyArea = sim$studyArea)

  #create new wetland class
  #this will reclassify open wetlands using Wulder, and reclassify forest wetlands as new class 20
  sim$rstLCC2010 <- Cache(reclassifyWetlands, lcc = sim$rstLCC2010, destinationPath = dPath,
                          userTags = c("reclassifyWetlansd"))

  #account for edge effects
  lccDat <- getValues(sim$rasterToMatch)
  ecoregionRstDat <- getValues(sim$ecoregionRst)
  sim$ecoregionRst[is.na(ecoregionRstDat) & !is.na(lccDat)] <- 9999

  ####studyAreaPSP###
  #this is the contiguous ecoregions of the RIA area - it may change eventually as Yukon PSP become available
  sim$studyAreaPSP <- prepInputs(url = 'https://drive.google.com/open?id=10yhleaumhwa3hAv_8o7TE15lyesgkDV_',
                                 destinationPath = 'inputs',
                                 overwrite = TRUE) %>%
    st_transform(., crs = crs(sim$studyArea))


  ####fireRegimePolys####
  fireRegimePolys <- prepInputs(url = 'http://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip',
                                destinationPath = dPath,
                                studyArea = sim$studyArea,
                                # rasterToMatch = rasterToMatch, #DO NOT USE RTM DUE TO BUG
                                userTags = c("fireRegimePolys"))
  fireRegimePolys <- as_Spatial(fireRegimePolys)
  sim$fireRegimePolys <- spTransform(fireRegimePolys, CRSobj = crs(sim$rasterToMatch))

  if (P(sim)$studyAreaName == "Yukon") {
    #Yukon will use a custom fireRegimePolys due to large areas with no fire
    #ecoregions 9 and 10 are combined, along with 5, 6, and 11. The latter have
    #almost no fires, the former have a combined 150 (with few large ones)
    #after accounting for slivers < 100 km2, we are left with 8 areas
    sim$fireRegimePolys$newID <- sim$fireRegimePolys$REGION_ID
    sim$fireRegimePolys <- st_as_sf(sim$fireRegimePolys)
    sim$fireRegimePolys[sim$fireRegimePolys$REGION_ID %in% c(54, 60),]$newID <- 20
    sim$fireRegimePolys[sim$fireRegimePolys$REGION_ID %in% c(46, 47, 63),]$newID <- 19
    sim$fireRegimePolys <- as_Spatial(sim$fireRegimePolys)
    sim$fireRegimePolys <- gUnaryUnion(spgeom = sim$fireRegimePolys, id = sim$fireRegimePolys$newID)
    sim$fireRegimePolys$fireRegime <- row.names(sim$fireRegimePolys)
  } else {
    sim$fireRegimePolys <- gUnaryUnion(spgeom = sim$fireRegimePolys, id = sim$fireRegimePolys$REGION_ID)
    sim$fireRegimePolys$fireRegime <- as.numeric(row.names(sim$fireRegimePolys)) #dropping the factor, seems simple
  }

  #do another switch for the projected MDC, once you have multiple files uplaoded
  historicalMDC <- Cache(prepInputs,
                         url = 'https://drive.google.com/file/d/1Sw_MsrekKbFH_L_IpJ_FdcnxgFh2M7f_/view?usp=sharing',
                         destinationPath = dPath,
                         rasterToMatch = sim$rasterToMatchLarge,
                         studyArea = sim$studyAreaLarge,
                         fun = raster::stack,
                         userTags = c(P(sim)$studyAreaName, "historicalMDC"))
  names(historicalMDC) <- paste0("year", 2001:2019)
  sim$historicalClimateRasters <- list("MDC" = historicalMDC)

  #projected climate layers

  if (P(sim)$GCM != "historical") {
    ## lookup table to get climate urls  based on studyArea, GCM, and SSP
    dt <- data.table::fread(file = file.path(dataPath(sim), "climateDataURLs.csv"))

    projectedMDC <- Cache(sourceClimateDataCMIP6,
                          Type = "proj_monthly", gcm = P(sim)$GCM,
                          ssp = P(sim)$SSP, dPath = dPath,
                          dt = dt, studyAreaNameLong = studyAreaNameLong,
                          studyArea = sim$studyArea, rasterToMatch = sim$rasterToMatch,
                          years = P(sim)$projectedFireYears,
                          userTags = c(P(sim)$SSP, "proj_monthly"))

    projectedAnnuals <- Cache(sourceClimateDataCMIP6,
                              Type = "proj_annual", gcm = P(sim)$GCM,
                              ssp = P(sim)$SSP, dt = dt,dPath = dPath,
                              studyAreaNameLong = studyAreaNameLong,
                              studyArea = sim$studyArea, rasterToMatch = sim$rasterToMatch,
                              years = P(sim)$projectedFireYears,
                              userTags = c(P(sim)$SSP, P(sim)$GCM, "proj_annual"))

    sim$projectedATAstack <- projectedAnnuals$projectedATA
    sim$projectedCMIstack <- projectedAnnuals$projectedCMI
    sim$CMInormal <- projectedAnnuals$CMInormal



  } else {
    sim$projectedATAstack <- NULL
    sim$projectedCMIstack <- NULL
    sim$CMInormal <- NULL
    #randomly sample historical fire layers
    projectedMDCyears <- sample(names(sim$historicalClimateRasters$MDC),
                                size = 90, replace = TRUE)
    projectedMDC <- lapply(projectedMDCyears, FUN = function(x){
      sim$historicalClimateRasters$MDC[[x]]
    })
    names(projectedMDC) <- paste0("year", 2011:2100)
    sim$projectedClimateLayers <- list("MDC" = stack(projectedMDC))

  }

  sppEquiv <- LandR::sppEquivalencies_CA
  sim$sppEquiv <- generateSppEquivRIA(sppEquiv)
  sim$sppEquivCol <- 'RIA'

  #spread fuel classes TBD for WCB and WB
  sim$sppEquiv[, ignitionFuelClass := FuelClass]
  sim$sppEquiv[, spreadFuelClass := FuelClass]
  #fix ignition classes
  sim$sppEquiv <- switch(studyAreaName,
         "Yukon" = {sim$sppEquiv[!RIA %in% c("Pice_eng", "Betu_pap")]},
         "SB" = {sim$sppEquiv[RIA %in% c("Pinu_con"), ignitionFuelClass := "class4"]},
         "WB" = {sim$sppEquiv},
         "BC" = {sim$sppEquiv},
         "WCB" = {sim$sppEquiv}
         )

  #Assign colour
  sppColors <- brewer.pal(name = 'Paired', n = length(unique(sim$sppEquiv$RIA)) + 1)
  setkey(sppEquivalencies_CA, RIA)
  sppNames <- unique(sim$sppEquiv$RIA)
  names(sppColors) <- c(sppNames, "mixed")
  sim$sppColorVect <- sppColors

  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}


.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
