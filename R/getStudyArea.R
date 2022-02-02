getStudyArea <- function(studyAreaName, dPath) {

  RIA <- prepInputs(url = "https://drive.google.com/file/d/1FlC5YdjNF8wXLcA4hQxLvrRjVC6ShqND/view?usp=sharing",
                    destinationPath = dPath,
                    fun = "terra::vect",
                    # targetFile = "RIA_StudyArea_Valid.shp",
                    # alsoExtract = c("RIA_StudyArea_Valid.dbf", "RIA_StudyArea_Valid.prj", "RIA_StudyArea_Valid.shx"),
                    useCache = FALSE)
  ecodistricts <- prepInputs(url =  "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip",
                             fun = "terra::vect",
                             useCache = FALSE,
                             destinationPath = dPath)

  eco <- terra::project(ecodistricts, crs(RIA))
  eco2 <- terra::crop(eco, RIA)

  switch(studyAreaName,
         "WB" = {
           ed <- c(881L, 882L, 898L, 899L, 900L, 901L, 896L, 883L, 902L, 897L, 884L,
                   903L, 911L, 885L, 890L, 886L, 891L, 892L, 893L, 924L, 912L, 887L,
                   894L, 888L, 904L, 889L, 905L, 906L, 907L, 908L, 913L, 895L, 936L,
                   909L, 914L, 910L, 915L, 918L, 916L, 919L, 937L, 920L, 921L, 933L,
                   934L, 917L, 922L, 923L, 935L)
         },
         "WCB" = {
           ed <- c(930L, 925L, 931L, 247L, 926L, 932L, 248L, 927L, 928L, 929L,
                  249L, 252L, 244L, 245L, 581L, 582L, 583L, 585L, 591L, 618L, 610L)
         },
         "SB" = {
           ed  <- c(961L, 963L, 964L, 965L, 966L, 967L, 968L,  962L, 969L, 970L, 972L,
                    974L, 980L, 981L, 982L, 996L, 985L, 986L,  993L, 996, 997L, 1013L)
         }
  )

  eco2a <- eco2[eco2$ECODISTRIC %in% ed,]
  eco2a <- terra::disagg(eco2a)
  eco2a$area <- terra::expanse(eco2a)
  eco2ab <- eco2a[eco2a$area > 1]
  studyArea <- terra::aggregate(eco2ab)
  studyArea$studyAreaName <- studyAreaName
  studyArea <- sf::st_as_sf(studyArea)
  return(studyArea)
}
