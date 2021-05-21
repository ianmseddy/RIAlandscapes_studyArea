generateSppEquivRIA <- function(sppEquiv) {
  data("sppEquivalencies_CA", package = "LandR")
  sppEquivalencies_CA[grep("Pin", LandR), `:=`(EN_generic_short = "Pine",
                                               EN_generic_full = "Pine",
                                               Leading = "Pine leading")]

  # Make LandWeb spp equivalencies
  sppEquivalencies_CA[, RIA := c(Pice_mar = "Pice_mar", Pice_gla = "Pice_gla",
                                 Pinu_con = "Pinu_con", Popu_tre = "Popu_tre",
                                 Betu_pap = "Betu_pap",
                                 Pice_eng = "Pice_eng")[LandR]]
  sppEquivalencies_CA[LANDIS_traits == "ABIE.LAS"]$RIA <- "Abie_las"

  sppEquivalencies_CA <- sppEquivalencies_CA[!LANDIS_traits == "PINU.CON.CON"]

  sppEquivalencies_CA[RIA == "Abie_las", EN_generic_full := "Subalpine Fir"]
  sppEquivalencies_CA[RIA == "Abie_las", EN_generic_short := "Fir"]
  sppEquivalencies_CA[RIA == "Abie_las", Leading := "Fir leading"]
  sppEquivalencies_CA[RIA == "Popu_tre", Leading := "Pop leading"]
  sppEquivalencies_CA[RIA == "Betu_pap", EN_generic_short := "Betula"]
  sppEquivalencies_CA[RIA == "Betu_pap",  Leading := "Betula leading"]
  sppEquivalencies_CA[RIA == "Betu_pap",  EN_generic_full := "Paper birch"]
  sppEquivalencies_CA[RIA == "Pice_eng", EN_generic_full := 'Engelmann Spruce']
  sppEquivalencies_CA[RIA == 'Pice_eng', EN_generic_short  := "En Spruce"]

  sppEquivalencies_CA <- sppEquivalencies_CA[!is.na(RIA)]
  sppEquivalencies_CA[RIA == "Pinu_con", KNN := "Pinu_Con"]
  return(sppEquivalencies_CA)
}
