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
  reqdPkgs = list("ggplot2", "raster", "data.table", "sf", "rgeos", "LandR", "RColorBrewer"),
  parameters = rbind(
    defineParameter("GCM", "character", "CanESM2", NA, NA,
                    desc = "the GCM to use - will be passed to prepInputs call"),
    defineParameter("historicalFireYears", "numeric", default = 1991:2019, NA, NA,
                    desc = "range of years captured by the historical climate data in prepInputs call"),
    defineParameter("RCP", "character", "RCP4.5", NA, NA,
                    desc = "either RPC4.5 or RCP8.5 - will be passed to prepInputs call"),
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
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "RIAlandscapes_studyArea", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "RIAlandscapes_studyArea", "save")
    },
    plot = {

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
  dPath <- file.path('modules', currentModule(sim), 'data')
  cacheTags <- c(P(sim)$studyAreaName, currentModule(sim))

  allowedStudyAreas <- c("BC", "Yukon", "RIA")
  if (!P(sim)$studyAreaName %in% allowedStudyAreas) {
    stop("incorrectly specified studyAreaName")
  }

  switch(P(sim)$studyAreaName,
         "BC" = {
           studyAreaUrl <- "https://drive.google.com/file/d/1LAXjmuaCt0xOWP-Nmll3xfRqCq-NbJP-/view?usp=sharing"
           #6 TSAs inside RIA, merged
           ecoregionRstUrl <- 'https://drive.google.com/file/d/1R38CXviHP72pbMq7hqV5CfT-jdJFZuWL/view?usp=sharing'

           #rasterized BEC zone variants
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

  ####studyArea####
  sim$studyArea <- Cache(prepInputs,
                         url = studyAreaUrl,
                         destinationPath = dPath,
                         useCache = P(sim)$.useCache,
                         overwrite = TRUE,
                         userTags = c(P(sim)$studyAreaName, "studyArea"))

  ####ecoregionRst####
  sim$ecoregionRst <- Cache(prepInputs,
                            url = ecoregionRstUrl,
                            studyArea = sim$studyArea,
                            destinationPath = dPath,
                            filename2 = paste0(P(sim)$studyAreaName, "_ecoregionRst.tif"),
                            userTags = c("ecoregionRst", P(sim)$studyAreaName))

  ####LCC2010####
  sim$rstLCC2010 <- Cache(prepInputs,
                          url = 'https://drive.google.com/file/d/1WcCEkwjnDq74fx3ZBizlIKzLkjW6Nfdf/view?usp=sharing',
                          targetFile = 'CAN_LC_2010_CAL.tif',
                          method = 'ngb',
                          destinationPath = dPath,
                          filename2 = paste0(P(sim)$studyAreaName, "_LCC2010.tif"),
                          rasterToMatch = sim$ecoregionRst,
                          studyArea = sim$studyArea)

  sim$rasterToMatch <- sim$rstLCC2010
  sim$rasterToMatchLarge <- sim$rstLCC2010
  sim$studyAreaLarge <- sim$studyArea
  #there is an issue with sub-pixel mismatches of extent in the Yukon study area
  sim$ecoregionRst <- postProcess(sim$ecoregionRst, sim$rasterToMatch)

  ####studyAreaPSP###
  #this is the contiguous ecoregions of the RIA area - it may change eventually as Yukon PSP become available
  sim$studyAreaPSP <- prepInputs(url = 'https://drive.google.com/open?id=10yhleaumhwa3hAv_8o7TE15lyesgkDV_',
                                 destinationPath = 'inputs',
                                 overwrite = TRUE) %>%
    spTransform(., CRSobj = crs(sim$studyArea))


  ####fireRegimePolys####
  fireRegimePolys <- prepInputs(url = 'http://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip',
                                destinationPath = dPath,
                                studyArea = sim$studyArea,
                                # rasterToMatch = rasterToMatch, #DO NOT USE RTM DUE TO BUG
                                userTags = c("fireRegimePolys"))
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
                         overwrite = TRUE,
                         userTags = c(P(sim)$studyAreaName, "historicalMDC"))
  names(historicalMDC) <- paste0("year", 2001:2019)
  sim$historicalClimateRasters <- list("MDC" = historicalMDC)

  #projected climate layers
  projectedLandRCS <- sourceClimDataWholeRIA(model = P(sim)$GCM,
                                             scenario = P(sim)$RCP,
                                             forFireSense = FALSE)

  RCPnoDots <- stringr::str_remove(P(sim)$RCP, "\\.")
  sim$CMInormal <- Cache(
    prepInputs,
    url = projectedLandRCS$CMInormal$url,
    destinationPath = dPath,
    studyArea = sim$studyArea,
    rasterToMatch = sim$rasterToMatch,
    userTags = c("CMInormal", P(sim)$GCM, P(sim)$RCP))


  sim$CMIstack <- Cache(
    prepInputs, url = projectedLandRCS$CMIstack$url,
    targetFile = paste0(projectedLandRCS$CMIstack$filename, ".grd"),
    alsoExtract = paste0(projectedLandRCS$CMIstack$filename, ".gri"),
    # studyArea = sim$studyArea,
    useCache = TRUE,
    # filename2 = paste0("CMIstack_", P(sim)$GCM, "_", RCPnoDots, ".grd"),
    # rasterToMatch = sim$rasterToMatch,
    fun = raster::stack,
    userTags = c("CMIstack", P(sim)$GCM, P(sim)$RCP))

  sim$ATAstack <- Cache(
    prepInputs,
    url = projectedLandRCS$ATAstack$url,
    targetFile = paste0(projectedLandRCS$ATAstack$filename, ".grd"),
    alsoExtract = paste0(projectedLandRCS$ATAstack$filename, ".gri"),
    # studyArea = sim$studyArea,
    # filename2 = paste0("ATAstack_", P(sim)$GCM, "_", RCPnoDots, ".grd"),
    # rasterToMatch = sim$rasterToMatch,
    fun = raster::stack,
    userTags = c("ATAstack", P(sim)$GCM, P(sim)$RCP))

  projectedFireSense <- sourceClimDataWholeRIA(model = P(sim)$GCM,
                                               scenario = P(sim)$RCP,
                                               forFireSense = TRUE)

  projectedMDC <- Cache(
    prepInputs,
    url = projectedFireSense$url,
    destinationPath = dPath,
    targetFile = paste0(projectedFireSense$filename, ".grd"),
    alsoExtract = paste0(projectedFireSense$filename, ".gri"),
    # rasterToMatch = sim$rasterToMatch,
    # studyArea = sim$studyArea,
    # filename2 = paste0("MDC_", P(sim)$GCM, "_", RCPnoDots, ".grd"),
    fun = raster::stack,
    userTags = c("projectedMDC", P(sim)$GCM, P(sim)$RCP))

  names(projectedMDC) <- paste0("year", 2011:2100)
  sim$projectedClimateLayers <- list("MDC" = projectedMDC)


  ####standAgeMap####
  sim$standAgeMap2011 <- Cache(
    prepInputsStandAgeMap,
    ageURL = paste0("https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
                    "canada-forests-attributes_attributs-forests-canada/",
                    "2011-attributes_attributs-2011/",
                    "NFI_MODIS250m_2011_kNN_Structure_Stand_Age_v1.tif"),
    rasterToMatch = sim$rasterToMatchLarge,
    studyArea = sim$studyAreaLarge,
    destinationPath = dPath,
    startTime = 2011,
    filename2 = .suffix("standAgeMap_2011.tif", paste0("_", P(sim)$studyAreaName)),
    userTags = c("prepInputsStandAgeMap", P(sim)$studyAreaname)
  )

  sppEquiv <- LandR::sppEquivalencies_CA
  sim$sppEquiv <- generateSppEquivRIA(sppEquiv)
  sim$sppEquivCol <- 'RIA'

  #there's no Betu_pap or Pice_eng in Yukon
  if (P(sim)$studyAreaName == "Yukon") {
    sim$sppEquiv <- sim$sppEquiv[!RIA %in% c("Pice_eng", "Betu_pap")]
  }


  #Assign colour
  sppColors <- brewer.pal(name = 'Paired', n = length(unique(sim$sppEquiv$RIA)) + 1)
  setkey(sppEquivalencies_CA, RIA)
  sppNames <- unique(sim$sppEquiv$RIA)
  names(sppColors) <- c(sppNames, "mixed")
  sim$sppColors <- sppColors

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
