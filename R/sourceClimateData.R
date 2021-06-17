sourceClimDataWholeRIA <- function(scenario, model, forFireSense = FALSE) {

  #after some deliberation, I decided this should only bring the filename and url back,
  #and let the module deal withlist, so I don't have to pass everything
  #the RCP 4.5 climate data were made by mosaicking two rasters together.
  #this has a problematic edge effect, but it shouldn't affect much of the landscape
  #as only north-west BC is affected, and there isn't much forest there (mostly ice)

  if (!forFireSense) {

    CMInormal <- list(url = "https://drive.google.com/file/d/1S61psu0DXcnuAsUX3DM3geSealgZ65hQ/view?usp=sharing")

    if (model == "CCSM4") {
      if (scenario == "RCP8.5") {
        # ATAstack <-list(url = "https://drive.google.com/file/d/14ZtIzThyinEX-DayV_NQV3Q4TlVKtXU8/view?usp=sharing",
        #                        filename = 'Yukon_1ArcMinute_CCSM4_RCP85_ATA2011-2100.grd',
        #                        alsoExtract = 'Yukon_1ArcMinute_CCSM4_RCP85_ATA2011-2100.gri',
        #                        destinationPath = path,
        #                        filename2 = 'inputs/Yukon/Yukon_1ArcMinute_CCSM4_RCP85_ATA2011-2100.grd',
        # #                        fun = 'raster::brick')
        # CMIstack <-list(url = 'https://drive.google.com/file/d/14ZtIzThyinEX-DayV_NQV3Q4TlVKtXU8/view?usp=sharing',
        #                        filename = 'Yukon_1ArcMinute_CCSM4_RCP85_CMI2011-2100.grd',
        #                        alsoExtract = 'Yukon_1ArcMinute_CCSM4_RCP85_CMI2011-2100.gri',
        #                        destinationPath = path,
        #                        filename2 = 'inputs/Yukon/Yukon_1ArcMinute_CCSM4_RCP85_CMI2011-2100.grd',
        #                        fun = 'raster::brick') #get the high quality stuff
      } else if (scenario == "RCP4.5") {
        ATAstack <- list(url = "https://drive.google.com/file/d/1dgQYAdJUdP0ANJ7qR7_cqzEV0y0HFYYv/view?usp=sharing",
                         filename = 'wholeRIA_1ArcMin_CCSM4_RCP45_ATA2011-2100')
        CMIstack <- list(url = 'https://drive.google.com/file/d/1dgQYAdJUdP0ANJ7qR7_cqzEV0y0HFYYv/view?usp=sharing',
                               filename = 'wholeRIA_1ArcMin_CCSM4_RCP45_CMI2011-2100')
      }

    } else if (model == "INM-CM4") {

      if (scenario == "RCP4.5") {
        ATAstack <- list(url = "https://drive.google.com/file/d/1bsevS1FHLXeLE6UrLDH5X1PqVY8S5Skz/view?usp=sharing",
                         filename = 'wholeRIA_1ArcMin_INM-CM4_RCP45_ATA2011-2100')
        CMIstack <- list(url = 'https://drive.google.com/file/d/1bsevS1FHLXeLE6UrLDH5X1PqVY8S5Skz/view?usp=sharing',
                         filename = 'wholeRIA_1ArcMin_INM-CM4_RCP45_CMI2011-2100')
      } else {
        ATAstack <- list(url = "https://drive.google.com/file/d/14XcPQ6B0em-yKRbRkac15shKwKIGtkQU/view?usp=sharing",
                         filename = 'wholeRIA_INM_CM4_RCP85_ATA2011-2100')
        CMIstack <- list(url = 'https://drive.google.com/file/d/14XcPQ6B0em-yKRbRkac15shKwKIGtkQU/view?usp=sharing',
                         filename = 'wholeRIA_INM_CM4_RCP85_CMI2011-2100')
      }
    } else if (model == 'CanESM2') {

      if (scenario == "RCP4.5") {
        ATAstack <- list(url = "https://drive.google.com/file/d/1AAR2oHX7yzAEr3UXxbgFZdL4ddDDkmCB/view?usp=sharing",
                         filename = 'wholeRIA_1ArcMin_CanESM2_RCP45_ATA2011-2100')
        CMIstack <- list(url = 'https://drive.google.com/file/d/1AAR2oHX7yzAEr3UXxbgFZdL4ddDDkmCB/view?usp=sharing',
                         filename = 'wholeRIA_1ArcMin_CanESM2_RCP45_CMI2011-2100')

      } else {
        # ATAstack <-list(url = "https://drive.google.com/file/d/17zBna_wegLmQs_m4FQd_JBhUcA0Da6lR/view?usp=sharing",
        #                        filename = 'RIA_1ArcMinute_CanESM2_RCP85_ATA2011-2100.grd',
        #                        alsoExtract = 'RIA_1ArcMinute_CanESM2_RCP85_ATA2011-2100.gri',
        #                        destinationPath = path,
        #                        filename2 = 'inputs/RIA_1ArcMinute_CanESM2_85_ATA2011-2100.grd',
        #                        fun = 'raster::stack')
        # CMIstack <-list(url = 'https://drive.google.com/file/d/17zBna_wegLmQs_m4FQd_JBhUcA0Da6lR/view?usp=sharing',
        #                        filename = 'RIA_1ArcMinute_CanESM2_RCP85_CMI2011-2100.grd',
        #                        alsoExtract = 'RIA_1ArcMinute_CanESM2_RCP85_CMI2011-2100.gri',
        #                        destinationPath = path,
        #                        filename2 = 'inputs/RIA_1ArcMinute_CanESM2_85_CMI2011-2100.grd',
        #                        fun = 'raster::stack') #get the high quality stuff
      }
    } else if (model == 'CNRM-CM5') {

      if (scenario == 'RCP4.5') {
        ATAstack <- list(url = "https://drive.google.com/file/d/1IC3hQw0ms555KsSeVuCeE5wwtToc9mUh/view?usp=sharing",
                         filename = 'wholeRIA_1ArcMin_CNRM-CM5_RCP45_ATA2011-2100')
        CMIstack <- list(url = 'https://drive.google.com/file/d/1IC3hQw0ms555KsSeVuCeE5wwtToc9mUh/view?usp=sharing',
                         filename = 'wholeRIA_1ArcMin_CNRM-CM5_RCP45_CMI2011-2100')
      } else {
        # ATAstack <-list(url = "https://drive.google.com/file/d/15idufxxqwAVPU2RR_3S_txKoTnQKFGJw/view?usp=sharing",
        #                        filename = 'RIA_1ArcMinute_CNRM_CM5_RCP85_ATA2011-2100.grd',
        #                        alsoExtract = 'RIA_1ArcMinute_CNRM_CM5_RCP85_ATA2011-2100.gri',
        #                        destinationPath = path,
        #                        filename2 = 'inputs/RIA_1ArcMinute_CanESM2_45_ATA2011-2100.grd',
        #                        fun = 'raster::stack')
        # CMIstack <-list(url = 'https://drive.google.com/file/d/15idufxxqwAVPU2RR_3S_txKoTnQKFGJw/view?usp=sharing',
        #                        filename = 'RIA_1ArcMinute_CNRM_CM5_RCP85_CMI2011-2100.grd',
        #                        alsoExtract = 'RIA_1ArcMinute_CNRM_CM5_RCP85_CMI2011-2100.gri',
        #                        destinationPath = path,
        #                        filename2 = 'inputs/RIA_1ArcMinute_CNRM_CM5_85_CMI2011-2100.grd',
        #                        fun = 'raster::stack') #get the high quality stuff
      }
    } else if (model == 'CSIRO-Mk3') {

      if (scenario == 'RCP4.5') {
        ATAstack <-list(url = "https://drive.google.com/file/d/1iKty7F12M17gzj-koa8hcnTU1nZm0MZ_/view?usp=sharing",
                        filename = 'wholeRIA_1ArcMin_CSIRO-Mk3_RCP45_ATA2011-2100')
        CMIstack <-list(url = 'https://drive.google.com/file/d/1iKty7F12M17gzj-koa8hcnTU1nZm0MZ_/view?usp=sharing',
                        filename = 'wholeRIA_1ArcMin_CSIRO-Mk3_RCP45_CMI2011-2100')
      } else {
        ATAstack <-list(url = "https://drive.google.com/file/d/11kuhp-CZ4kTYhm4a0sSSPHPZPejW87Vk/view?usp=sharing",
                        filename = 'wholeRIA_20kmbuffer_CSIRO_mk3_ATA2011-2100')
        CMIstack <-list(url = 'https://drive.google.com/file/d/11kuhp-CZ4kTYhm4a0sSSPHPZPejW87Vk/view?usp=sharing',
                        filename = 'wholeRIA_20kmbuffer_CSIRO_mk3_CMI2011-2100')
      }
    } else if (model == 'Access1') {

      if (scenario == 'RCP4.5') {
        ATAstack <-list(url = "https://drive.google.com/file/d/1NjQL3828TB3aXfULQJ2xT5Jdph1SasAr/view?usp=sharing",
                        filename = 'wholeRIA_1ArcMin_Access1_RCP45_ATA2011-2100')
        CMIstack <-list(url = 'https://drive.google.com/file/d/1NjQL3828TB3aXfULQJ2xT5Jdph1SasAr/view?usp=sharing',
                        filename = 'wholeRIA_1ArcMin_Access1_RCP45_CMI2011-2100')
      } else {
        # ATAstack <-list(url = "https://drive.google.com/file/d/1p-tCr_N4wsspGsrbN0ukJ8P8OHEWkty5/view?usp=sharing",
        #                        filename = 'RIA_1ArcMinute_Access1_RCP85_ATA2011-2100.grd',
        #                        alsoExtract = 'RIA_1ArcMinute_Access1_RCP85_ATA2011-2100.gri',
        #                        destinationPath = path,
        #                        filename2 = 'inputs/RIA_1ArcMinute_Access1_RCP85_ATA2011-2100.grd',
        #                        fun = 'raster::stack')
        # CMIstack <-list(url = 'https://drive.google.com/file/d/1p-tCr_N4wsspGsrbN0ukJ8P8OHEWkty5/view?usp=sharing',
        #                        filename = 'RIA_1ArcMinute_Access1_RCP85_CMI2011-2100.grd',
        #                        alsoExtract = 'RIA_1ArcMinute_Access1_RCP85_CMI2011-2100.gri',
        #                        destinationPath = path,
        #                        filename2 = 'inputs/RIA_1ArcMinute_Access1_RCP85_CMI2011-2100.grd',
        #                        fun = 'raster::stack') #get the high quality stuff
      }
    } else {
      stop("don't recognize Model")
    }

    climData = list("ATAstack" = ATAstack,
                    "CMIstack" = CMIstack,
                    "CMInormal" = CMInormal)
  } else {
    #put fireSense links
    urls = list(
      "CSIRO-Mk3" = list("RCP8.5" = list(url = 'https://drive.google.com/file/d/1qZnHSHzdeBk_CvG67XNYQNLU-7BMEAMx/view?usp=sharing',
                                         filename = "wholeRIA_CSIRO-Mk3_RCP85_projMDC2011_2100"),
                         "RCP4.5" = list(url = 'https://drive.google.com/file/d/1wA-bpR4DSE_O0oYvXfLa63wLx-RAazYE/view?usp=sharing',
                                         filename = "wholeRIA_CSIRO-Mk3_RCP45_projMDC2011_2100")),
      "Access1" = list("RCP8.5" = list(url = 'https://drive.google.com/file/d/14MgMibS7RQUOiq3wD34NaXq-m1E-wL0u/view?usp=sharing',
                                       filename = "wholeRIA_Access1_RCP85_projMDC2011_2100"),
                       "RCP4.5" = list(url = 'https://drive.google.com/file/d/14MgMibS7RQUOiq3wD34NaXq-m1E-wL0u/view?usp=sharing',
                                       filename = "wholeRIA_Access1_RCP45_projMDC2011_2100")),
      "INM-CM4" = list("RCP8.5" = list(url = "https://drive.google.com/file/d/1oDYpesRUHp8v1QzCwMT5op9kYxdEMiKC/view?usp=sharing",
                                       filename = "wholeRIA_INM_CM4_RCP85_projMDC2011_2100"),
                       "RCP4.5" = list(url = "https://drive.google.com/file/d/1C_iYUHV7IsfC4nLSHf6jhHFZN0ylFPhK/view?usp=sharing",
                                       filename = "wholeRIA_INM-CM4_RCP45_projMDC2011_2100")),
      "CanESM2" = list("RCP8.5" = list(NA),
                       "RCP4.5" = list(url = "https://drive.google.com/file/d/1NvXFe6yoNxDsnVnvVYk3xoG6vqoQCgQD/view?usp=sharing",
                                       filename = "wholeRIA_CanESM2_RCP45_projMDC2011_2100"))
    )
    theUrl <- urls[[model]][[scenario]][["url"]]
    theFile <- urls[[model]][[scenario]][["filename"]]
    climData <- list("url" = theUrl,
                     "filename" = theFile)
  }
  return(climData)
}
