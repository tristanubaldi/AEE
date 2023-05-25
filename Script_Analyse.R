#################################################################################
#
#       Climate matching approach of exotic tree species in France
#                       UBALDI MASTER THESIS
#
#################################################################################
# January - June 2023
# Tristan Ubaldi
# Clean R space
graphics.off()
rm(list=ls())

#### 1. Define your Library/Data/Directories
### 1.1. LIBRARY
# library(usethis) 
# usethis::edit_r_environ()
# devtools:::install_github("gearslaboratory/gdalUtils", force = TRUE)
# install.packages("https://gitlab.rrz.uni-hamburg.de/helgejentsch/climdatdownloadr/-/archive/0.1.6_alpha/climdatdownloadr-0.1.6_alpha.tar.gz", repos = NULL, type = "source")
# Sys.setenv(LIBCURL_BUILD="winssl")
# install.packages('curl', type = "source")
# 
{
  library(ade4)
  library(berryFunctions)
  library(bigmemory)
  #library(ClimDatDownloadR)
  library(countrycode)
  library(curl)
  library(data.table)
  library(devtools)
  library(dplyr)
  library("factoextra")
  library(Factoshiny)
  library(fuzzyjoin)
  library(geodata)
  library(gdalUtils)
  library(ggplot2)
  library(ggpubr)
  library(ggsci)
  library(grid)
  library(gridExtra)
  library(httr)
  set_config(config(ssl_verifypeer = 0L))
  library(ijtiff)
  library(knitr)
  library(leaflet)
  library(maps)
  library(mapproj)
  library(maptools)
  library(ncdf4)
  library(paletteer)
  library(patchwork)
  library(plyr)
  library(qpdf)
  library(randomForest)
  library(raster)
  library(RColorBrewer)
  library(RCurl)
  library(RefManageR)
  library(rgdal)
  #library(rWCVP)
  library(sf)
  library(sp)
  library(stringr)
  library(svMisc)
  library(terra)
  library(tibble)
  library(tidyr)
  library(tidyverse)
  library(usethis)
  library(utils)
  library(vegan)
  library(viridis)
  #library(woodivsdm)
}
#### 1.2. YOUR DATA
{
## Country or Countries where you want to project the niches? 
count <- "France"
if (length(count)>1) {
## You have define more than one country, 
## you need to give a name to your countries list.
  count_name <- "British_Islands"
} else {
  count_name <- count }
  
## List of species studied
species <- 
  #c("Abies lasiocarpa", "Pinus rigida", "Quercus afares")
  #"Abies firma" # 0 points
  #"Rhododendron ponticum baeticum"
  # "Tetraclinis articulata"
  "Carya illinoensis"
  #"Betula papyrifera"
  #"Robinia pseudoacacia"
  #"Styphnolobium japonicum"
  #"Rhododendron maximum"
   # c("Rhododendron maximum", "Rhododendron catawbiense", 
   #           "Rhododendron ponticum ponticum", "Rhododendron ponticum baeticum")
  #list.files("~/Documents/NicheEssenceExo/GIS/Dataset_trees/Set")
#species <- species[c(36:41)]
#species
## Each periods? (folder names)
periods <- c("1951-1980", "1981-2010","2011-2040", "2041-2070", "2071-2100")
## Each Scenarios? (folder names)
ssp <- c("ssp126", "ssp370", "ssp585")
## Each Bioclimatic var are select?
varbio <- NULL
  #c(1,4,10,11,12,15,16,17)
  #c(1,4,5,6,10,11,12,13,14,15,16,17)
   #c(1,4,5,6,12,13,14,15)
  #c(1:19)
## Each Soil GRIDS variables? 
# (a characters "bdod", "cfvo", "clay", "nitrogen", "ocd", "ocs", "phh2o", "sand", "silt", "soc", "wrb".)
# Meaning: ?soil_world
soil <- c("bdod", "phh2o", "nitrogen", "clay", "soc") 
## Each soil depth? (in cm / 0-5, 5-15, 15-30, 30-60, 60-100, 100-200)
dep <- c("0-5", "5-15", "15-30", "30-60")
## Define the Maximum Distance
# (This is the maximum distance in intersection between the points projected 
# from the variables (bioclimatic, soils or both) in the country to study and 
# the points in the native range of the species to study).
# One max distance or a gradient of max distances can be defined.
md <- 0.2
}
### 1.3. DIRECTORIES 
{
## Change the path to locate your files
## Main directory
dir <- ("/Volumes/DDTristan/NicheEssenceExo")
dir_gis <- (paste0(dir, "/GIS"))
## Folder with dir_chelsa files
dir_chelsa <- paste0(dir_gis, "/Dataset_CHELSA_V.2.1_gfdl-esm4")
## Where save the variables Soil Grids?
dir_soil <- paste0(dir_gis, "/Dataset_SoilGrids")
## Folder destination : Species Native range rasters
dir_nrr <- paste0(dir_gis,"/Bioclim_native_range/R")
## Create Country Folders
if (!dir.exists(paste(dir_gis,"Countries", count_name, sep="/"))) { 
  dir.create(paste(dir_gis,"Countries", count_name, sep="/")) }
if (!dir.exists(paste(dir_gis,"Countries", count_name, "Rasters", sep="/"))) { 
  dir.create(paste(dir_gis,"Countries", count_name, "Rasters", sep="/")) }
if (!dir.exists(paste(dir_gis,"Countries", count_name, "Shapefile", sep="/"))) { 
  dir.create(paste(dir_gis,"Countries", count_name, "Shapefile", sep="/")) }
## Folder destination : Country Rasters
dir_cras <- paste(dir_gis,"Countries", count_name, "Rasters", sep="/")
## Folder destination : Country Shapefile 
dir_cshp <- paste(dir_gis,"Countries", count_name, "Shapefile", sep="/") 
## Folder origin : Set of Species Native range polygon
dir_set <- paste0(dir_gis,"/Dataset_trees/Set")
## Where save the maps?
dir_pc <- paste0("~/Desktop")
                 #/Nextcloud/PC")
}

## Data Frame with number of sel. points by periods, ssp and variables used
dfnp <- data.frame() %>%
  add_column(nb = NA,
             species = NA,
             period = NA, 
             ssp = NA,
             nvars = NA,
             vars = NA)
###### VARBIO LOOP ####
## Which set of varbio?
# varbios <- list(c(1,4,5,6,12,13,14,15))
# # varbios <- list(c(1,12), c(1,4,12), c(1,4,12,15), c(1,4,6,12,15), c(1,4,5,6,12,15), 
# #                 c(1,4,5,6,12,13,15), c(1,4,5,6,12,13,14,15), c(1,4,5,6,7,12,13,14,15),
# #                 c(1,4,5,6,7,8,12,13,14,15), c(1,4,5,6,7,8,10,12,13,14,15), 
# #                 c(1,2,4,5,6,7,8,10,12,13,14,15), c(1:10,12,13,14,15), c(1:15), 
# #                 c(1:16),c(1:17),c(1:18), c(1:19))
# for (varbio in varbios) {
#   print(paste("For the variables", paste(sort(c(varbio)), collapse = ","), sep=" "))
##### SPECIES LOOP ####

  for (i in species) {
    print(paste("For species", i, sep=" "))
    #### 2. Country of study ####
    ### 2.1. Download Vector of Country ####
    ### 2.1.1. Download Countries Shp to convert in SpatVector (polygon) from gadm.org/index ####
    # (diva-gis.org/gdata)
    ## Country name to ISO
    ISO <- countrycode(count, origin = 'country.name', destination = 'iso3c')
    ## Download country (or countries) in function if one or more countries defined
    if(length(count)>1){
      #vcount_list <- list()
      for (h in ISO) {
        if(!length(list.files(path=dir_cshp,
                              pattern = h, 
                              full.names = F))>0) {download.file(paste("https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41", 
                                                                       h, "shp.zip", sep="_"), 
                                                                 paste(dir_cshp, paste("gadm41", h, "shp", sep="_"), sep="/"))
          unzip(paste(dir_cshp, paste("gadm41", h ,"shp", sep="_"), sep="/"), exdir=dir_cshp) }
          r <- st_read(list.files(path=dir_cshp,
                                  pattern = paste("gadm41", h, "0.shp", sep="_"), 
                                  full.names = T))
          assign(paste("r", h, sep = "_"), r)
          rm(r)
      }
      ## Merge all 
      ## Find all data frames starting with "r_" in global environment
      vcount <- vect(do.call(rbind, r_list <- mget(grep("^r_", ls(), value = TRUE))))
      rm(list = ls(pattern = "^r_"))
      # writeOGR(obj = vcount, dsn = paste(dir_cshp, paste0(count_name, ".shp"), sep="/"), 
      #          driver = "ESRI Shapefile")
      # 
      # st_write(vcount, paste(dir_cshp, paste0(count_name, ".shp"), sep="/"))
      # 
    } else {
    if(length(list.files(path=dir_cshp, pattern = "_0.shp", full.names = T))==0) {
      download.file(paste("https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41", 
                          ISO, "shp.zip", sep="_"), paste(dir_cshp, 
                             paste("gadm41", ISO, "shp", sep="_"), sep="/"))
    unzip(paste(dir_cshp, paste("gadm41", ISO ,"shp", sep="_"), sep="/"), exdir=dir_cshp) }
    vcount <- vect(list.files(path=dir_cshp, pattern = "_0.shp", full.names = T))
    #plot(vcount) 
    }
    #### 3. GIS - crops Rasters of the variables to study ####
    ## Error if both NULL
    if (is.null(c(varbio, soil))) { 
      stop("\nArgument <vars>: Please provide at least one bioclimatic or soil variables.")
    } 
    ### 3.1. CHELSA Bioclimatic variables ####
    ## Future update for CHELSA download (waiting Chelsa.CMIP_6.download function update)
    ## Rewrite, if any files or folders are missing in the CHELSA folder, they are created.
    ## ClimDatDownloadR 
    #install.packages("https://gitlab.rrz.uni-hamburg.de/helgejentsch/climdatdownloadr/-/archive/master/climdatdownloadr-master.tar.gz", repos = NULL, type = "source")
    #library(ClimDatDownloadR)
    ## Download CHELSA bioclimatics vars for all periods with Chelsa.CMIP_6.download
    # Chelsa.CMIP_6.download(save.location = paste(dir_chelsa, "ClimDatDownloadR", sep="/"), 
    #                        parameter = "bio",
    #                        bio.var = c(1:19),
    #                        emission.scenario.var = c("ssp126", "ssp370", "ssp585"),
    #                        time.interval.var = c("2011-2040", "2041-2070", "2071-2100"),
    #                        model.var = "gfdl-esm4",
    #                        clip.extent = c(-180, 180, -90, 90))
    
    ## Define the extent of Bioclimatic Rasters to match with Soil Rasters
      j <- "1951-1980"
      x <- paste0(paste(dir_chelsa, j, "Bio", sep="/"), paste(1, j, sep="_"),".tif") %>%
        rast(.)
      ## Crop all 
      c <- crop(x, vcount, mask=TRUE)
      ## Define extent 
      ## The spatial extent of a raster, represents the "X, Y" coordinates of 
      ## the corners of the raster in geographic space
      ext_count <- ext(rast(c))
      rm(x, c, j)
      
    ## First know if no bioclimatic variable defined.
    if (is.null(varbio)) {
        #stop("\nArgument <vars>: Please provide at least one bioclimatic variable between 1 and 19.")
        print("No bioclimatic variable defined. Please provide at least one (in varbio) or continue only with soil variable(s)")
      } else {
          if (file.exists(paste(dir_cras, "CHELSA", sep="/"))) {  # If CHELSA exists already?
            print("CHELSA folder found")
            for (j in periods) {
              #setwd(paste(dir_cras, "CHELSA", j, sep="/"))
            if (file.exists(paste(dir_cras, "CHELSA", j, sep="/"))) {       # If in CHELSA, period folder exists? 
              print(paste("Folder", j, "exists", sep = " "))
              #setwd(file.path(j)) 
            if (length(list.files(path = paste(dir_cras, "CHELSA", j, sep="/"), 
                                  # If period folder exists, has it SSP?
                                  pattern = "*ssp", full.names = F) ) > 0) {
              if (isFALSE(length(list.files(paste(dir_cras, "CHELSA", j, sep="/"))) == length(ssp))) {   
                # If yes, has it the all SSP folders?
                for (k in ssp) {                                                                        
                  # No, so create SSP folder and raster files.
                  if(isFALSE(file.exists(paste(dir_cras, "CHELSA", j, k, sep="/")))) {
                    print(paste("In folder", j, "SSP", k, "missing", sep=" "))
                ## Loop for SSP folder
                x <- paste0(paste(dir_chelsa, j, k,"Bio", sep="/"), paste(1:19, j, k, sep="_"),".tif") %>%
                  rast(.)
                ## Crop all 
                compareCRS(x, vcount)
                c <- crop(x, vcount, mask=TRUE)
                rm(x)
                ## Save all 
                #setwd(paste(dir_cras, "CHELSA", j, k, sep="/"))
                ## SSP
                if (!dir.exists(paste(dir_cras, "CHELSA", j, k, sep="/"))) { 
                  dir.create(paste(dir_cras, "CHELSA", j, k, sep="/")) }
                #setwd(file.path(k))
                ## Save rasters
                writeRaster(c, filename = paste(dir_cras, "CHELSA", j, k, 
                                                paste0(paste(count_name, paste0("Bio", 1:19), 
                                                             j, k, sep="_"),".tif"), sep="/"), 
                            overwrite=TRUE, filetype="GTiff") 
                rm(c)
                print("Files created")
                  } else { print(paste("In folder", j, "SSP", k, "is presents", sep=" ")) }
                }
              }
              # If all SSP folders present, continue.
              for (k in ssp) {
                if(isFALSE(length(list.files(paste(dir_cras, "CHELSA", j, k, sep="/"))) == 19 )) { 
                  ## If varbio =/= ones in CHELSA folder
                  print(paste("In folder", j, "SSP", k, "doesn't have all variables"))        
                  x <- paste0(paste(dir_chelsa, j, k,"Bio", sep="/"), 1:19, "_", j,"_", k,".tif") %>%
                            rast(.)
                  ## Crop all 
                  compareCRS(x, vcount)
                  c <- crop(x, vcount, mask=TRUE)
                  rm(x)
                  ## Save all 
                  #setwd(paste(dir_cras, "CHELSA", j, sep="/"))
                  ## SSP
                  if (!file.exists(paste(dir_cras, "CHELSA", j, k, sep="/"))) { 
                    dir.create(paste(dir_cras, "CHELSA", j, k, sep="/")) }
                  #setwd(file.path(k))
                  # Save rasters
                  writeRaster(c, filename = paste(dir_cras, "CHELSA", j, k, 
                                                  paste0(paste(count_name, paste0("Bio", 1:19),
                                                               j, k, sep="_"),".tif"), sep="/"),
                              overwrite=TRUE, filetype="GTiff")
                  rm(c)
                  print("Files created")
                  } else {print(paste("In folder", j, "SSP", k, "have all variables"))   }
              }
              # If all rasters presents in SSP folder, continue.  
                 } else { 
                   # If in period folder (with no SSP), nb rasters < nb varbio, create them.
                   if(isFALSE(length(list.files(paste(dir_cras, "CHELSA", j, sep="/"))) == 19 )) { 
                     print(paste("In folder", j, "doesn't have all variables", sep=" "))   
                     x  <- paste0(paste(dir_chelsa, j, "Bio", sep="/"), 1:19, "_", j, ".tif") %>%
                      rast(.)
                     ## Crop all 
                     compareCRS(x, vcount)
                     c <- crop(x, vcount, mask=TRUE)
                     rm(x)
                     ## Save all
                     #setwd(paste(dir_cras, "CHELSA", j, sep="/"))
                     writeRaster(c, filename = paste(dir_cras, "CHELSA", j, 
                                                     paste0(paste(count_name, paste0("Bio", 1:19), 
                                                                  j, sep="_"),".tif"), sep="/"), 
                                 overwrite=TRUE, filetype="GTiff")
                     rm(c)
                     print("Files created") } else {
                       print(paste("In folder", j, " have all variables", sep= " "))  }
              } 
              # Otherwise, continue.
            } else {
              # If one period folder doesn't exists,
              print(paste("Folder", j, "doesn't exist yet", sep = " "))
              #setwd(paste(dir_chelsa, j, sep="/"))
              ## If there are SSP folders (future periods)
              if (length(list.files(path = paste(dir_chelsa, j, sep="/"),
                                    pattern = "*ssp", full.names = F) ) > 0) { 
                #print(paste("Folder", j, "have SSP", sep = " "))
                ## Loop 
                for (k in ssp) {
                  print(paste0("Scenario ", k, " ..."))
                  x <- paste0(paste(dir_chelsa, j, k,"Bio", sep="/"), 
                              paste(1:19, j, k, sep="_"),".tif") %>%
                    rast(.)
                  ## Crop all 
                  compareCRS(x, vcount)
                  c <- crop(x, vcount, mask=TRUE)
                  rm(x)
                  ## Save all 
                  #setwd(paste(dir_cras, "CHELSA", sep="/"))
                  ## Folder 
                  if (!file.exists(paste(dir_cras, "CHELSA", j, sep="/"))) { 
                    dir.create(paste(dir_cras, "CHELSA", j, sep="/")) }
                  #setwd(file.path(j))
                  ## SSP
                  if (!file.exists(paste(dir_cras, "CHELSA", j, k, sep="/"))) { 
                    dir.create(paste(dir_cras, "CHELSA", j, k, sep="/")) }
                  #setwd(file.path(k))
                  ## Save rasters
                  writeRaster(c, filename = paste(dir_cras, "CHELSA", j, k, 
                                                  paste0(paste(count_name, paste0("Bio", 1:19), 
                                                               j, k, sep="_"),".tif"), sep="/"),
                              overwrite=TRUE, filetype="GTiff") 
                  rm(c)
                  print("Files created")
                }
                ## If there are not SSP folders (passed periods)
                } else {
                print(paste("Folder", j, "doesn't have SSP", sep = " "))
                x  <- paste0(paste(dir_chelsa, j,"Bio", sep="/"), 1:19, "_", j,".tif") %>%
                  rast(.)
                ## Crop all 
                compareCRS(x, vcount)
                c <- crop(x, vcount, mask=TRUE)
                rm(x)
                ## Save all
                #setwd(paste(dir_cras, "CHELSA", sep="/"))
                if (file.exists(paste(dir_cras, "CHELSA", j, sep="/"))) {
                  print("The folder already exists")
                } else {
                  dir.create(paste(dir_cras, "CHELSA", j, sep="/")) }
                #setwd(file.path(j))
                writeRaster(c, filename = paste(dir_cras, "CHELSA", j, 
                                               paste0(paste(count_name, paste0("Bio", 1:19),
                                                            j, sep="_"),".tif"), sep="/"),
                            overwrite=TRUE, filetype="GTiff") 
                rm(c)
                print("Files created") 
                }
              }
          }
        } else {
              # If CHELSA doesn't exists, 
            print("CHELSA was not found")
            dir.create(paste(dir_cras, "CHELSA", sep="/") )
            #setwd(file.path("CHELSA"))
          for (j in periods) {
            print(paste0("For period ", j, " ..."))
            #setwd(paste(dir_chelsa, j, sep="/"))
            ## If there are SSP folders (future periods)
            if (length(list.files(path = (paste(dir_chelsa, j, sep="/")),
                                  pattern = "*ssp", full.names = F) ) > 0) {
              #print(paste("Folder", j, "have SSP", sep = " "))
              ## Loop 
              for (k in ssp) {
                print(paste0("Scenario ", k, " ..."))
                x <- paste0(paste(dir_chelsa, j, k,"Bio", sep="/"), 1:19, "_", j,"_", k,".tif") %>%
                  rast(.)
                ## Crop all 
                compareCRS(x, vcount)
                c <- crop(x, vcount, mask=TRUE)
                rm(x)
                ## Save all 
                #setwd(paste(dir_cras, "CHELSA", sep="/"))
                ## Folder 
                if (!file.exists(paste(dir_cras, "CHELSA", j, sep="/"))) { 
                  dir.create(paste(dir_cras, "CHELSA", j, sep="/")) }
                #setwd(file.path(j))
                ## SSP
                if (!file.exists(paste(dir_cras, "CHELSA", j, k, sep="/"))) { 
                  dir.create(paste(dir_cras, "CHELSA", j, k, sep="/")) }
                #setwd(file.path(k))
                ## Save rasters
                writeRaster(c, filename = paste(dir_cras, "CHELSA", j, k, 
                                                paste0(paste(count_name, paste0("Bio", 1:19),
                                                             j, k, sep="_"),".tif"), sep="/"),
                            overwrite=TRUE, filetype="GTiff") 
                rm(c)
                print("Files created")
              }
              # If there are not SSP folders (passed periods)
            } else {
              x  <- paste0(paste(dir_chelsa, j, "Bio", sep="/"), 1:19, "_", j,".tif") %>%
                rast(.)
              ## Crop all 
              compareCRS(x, vcount)
              c <- crop(x, vcount, mask=TRUE)
              rm(x)
              ## Save all
              #setwd(paste(dir_cras, "CHELSA", sep="/"))
              if (file.exists(paste(dir_cras, "CHELSA", j, sep="/"))) {
                print("The folder already exists")
              } else {
                dir.create(paste(dir_cras, "CHELSA", j, sep="/")) }
              #setwd(file.path(j))
              writeRaster(c, filename = paste(dir_cras, "CHELSA", j, 
                                             paste0(paste(count_name, paste0("Bio", 1:19), 
                                                          j, sep="_"),".tif"), sep="/"),
                          overwrite=TRUE, filetype="GTiff") 
              rm(c)
              print("Files created")
            }
          } 
        }
       print("Done")}  
    ### 3.2. Soil Grid variables ####
      if (is.null(soil)) { print("No soil variable defined. Please provide at least one (in soil) or continue only with bioclimatic variable(s)")
      } else { 
        ## Download SOIL GRIDS variables 
        print("Soil variables defined.")
        soil_avail <- character()
        for (s in soil){
          for (d in dep){
            if (!file.exists(paste0(dir_soil, "/", s,"_", d,"cm_mean_30s.tif"))) { #If file not yet exists, download it
              print("Download in progress...")
              depth2 <- str_extract(d,'\\b\\w+$')
              for(e in depth2) {
                print(paste("File", s,d, "not yet available", sep=" "))
                varsoil <- soil_world(var=s , depth=as.numeric(e), stat="mean", path=dir_soil)
              }
              rm(e)
            } else {
              print(paste("File", s,d, "already downloaded", sep=" "))
              soil_avail <- c(soil_avail, paste(s, d, sep="_"))
            } 
          }
        }
        soil <- unique(str_extract(soil_avail, "[^_]+"))
        dep <- unique(str_extract(soil_avail, "[^_]+$"))
        depth <- paste0(dep, "cm")
        
        ### Loop GIS in count to study
        if (file.exists(paste(dir_cras, "SoilGrids", sep="/"))) { 
          # If SoilGrids folder already exist
          if (isFALSE(all((do.call(paste, c(expand.grid(soil, depth), sep="_"))) %in% 
                          (str_extract(list.files(paste(dir_cras, "SoilGrids", sep="/")),
                                       "(?<=_)[^_]+_[^_]+(?=\\.tif$)")) )) ) {
            print("Soil variables missing in SoilGrids folder") 
            #} else { print("No missing")}
            
            x <- paste(dir_soil, (list.files(dir_soil, 
                                             pattern=paste0((do.call(paste, c(expand.grid(soil, depth), sep="_"))),
                                                            collapse="|"))), sep="/") %>%
              rast(.)
            ## Crop all 
            compareCRS(x, vcount)
            c <- crop(x, vcount, mask=TRUE)
            rm(x)
            ## Change extent of Soil Raster
            ext(c) <- ext_count
            ## Save rasters
            for (z in c@ptr$names) {
              writeRaster(c[z], filename = paste(dir_cras,"SoilGrids", 
                                                 paste0(paste(count_name, z, sep="_"), 
                                                        ".tif"), sep="/"),
                          overwrite=TRUE, filetype="GTiff") }
            rm(c, z)
            print("Files created")
          }
        } else { 
          print("The SoilGrids folder was not found, it must be created")
          dir.create(paste(dir_cras, "SoilGrids", sep="/"))
          #setwd(file.path("SoilGrids"))
          x <- paste(dir_soil, (list.files(dir_soil, 
                                           pattern=paste0((do.call(paste, c(expand.grid(soil, depth), sep="_"))),
                                                          collapse="|"))), sep="/") %>%
            rast(.)
          ## Crop all
          compareCRS(x, vcount)
          c <- crop(x, vcount, mask=TRUE)  
          rm(x)
          ## Change extent of Soil Raster
          ext(c) <- ext_count
          ## Save rasters
          for (z in c@ptr$names) {
            writeRaster(c[z], filename = paste(dir_cras,"SoilGrids", 
                                               paste0(paste(count_name, z, sep="_"), 
                                                      ".tif"), sep="/"),
                        overwrite=TRUE, filetype="GTiff") }
          rm(c, z)
          print("Files created") }
        print("Done")} 
    #### 4. Native Range of studied Species (Environmental layers) ####
      ## UPDATE: Rewrites, creates CHELSA/SoilGrids folders if they do not already exist,
      ## if they already exist, the number of bioclim and soil files inside is calculated and
      ## if nb of files < nb of var defined here then the files are recreated, otherwise not.
      period <- "1951-1980"
      #
    ### 4.1. Download Vector of Native Range ####
    ## First, looking for if NR shp for given species exists
    ## Create "range" one time, use after to crop CHELSA and SoilGRIDS var
    if (file.exists(paste(dir_set, i, sep="/"))) {
      ## Species folder
      d <- (paste(dir_set, i, sep="/"))
      file <- list.files(path = d, pattern = "_plg_clip.shp$", full.names = F)
      
      if (length(file) > 0) {
        print("clip Shapefile find")
      } else {
        print("Try to find shapefile ... ") 
        
        d <- (paste(dir_set, i, sep="/"))
        file <- list.files(path = d, pattern = "_plg.shp$", full.names = F) 
        
        if (length(file) > 0) {
          print("plg Shapefile find")
        } else {
          print("Try to find shapefile ... ") 
          
          d <- (paste(dir_set, i, sep="/"))
          file <- list.files(path = d, pattern = "\\.shp$", full.names = F) 
          if (length(file) < 0) {
            print("Error, no found")
          } else {
            print("Ok good, shp Shapefile was found") } } }
      
      range <- vect(paste(dir_set, i, file, sep="/"))
      crs(range) <- "epsg:4326"
      par(mfrow=c(1, 1))
      #plot(range) 
      } else {
        print(paste0("The Native Range Folder for ", i, " doesn't exists!")) }
    
    ## Define the extent of Bioclimatic Rasters to match with Soil Rasters
    x <- paste0(paste(dir_chelsa, period,"Bio", sep="/"), paste(1, period, sep="_"),".tif") %>% rast(.) 
    ## Crop all 
    # compareCRS(x, range)
    c <- crop(x, range, mask=TRUE)
    ext_nr <- ext(rast(c))
    rm(x, c)
    
    ### 4.2. Species Folder creation ####
    ## 4.2.1. Species Folder not already created ####
    if (!file.exists(paste(dir_nrr, i, sep="/"))) { 
      print(paste("Native Range of", i,"doesn't exist yet", sep=" ")) 
      dir.create(paste(dir_nrr, i, sep="/")) 
      ## CHELSA Bioclimatic variables 
      if (!is.null(varbio)) { 
        #setwd(paste(dir_nrr, i, "CHELSA", sep="/"))
          dir.create(paste(dir_nrr, i, "CHELSA", sep="/")) 
          #setwd(file.path("CHELSA")) 
          dir.create(paste(dir_nrr, i, "CHELSA", period, sep="/"))
          #setwd(file.path(period)) 
          for (v in 1:19) {
          x <- paste0(paste(dir_chelsa, period,"Bio", sep="/"), v, "_", 
                      period,".tif") %>% rast(.) 
          ## Crop all
          compareCRS(x, range)
          c <- crop(x, range, mask=TRUE)
          #plot(c[[1]])
          rm(x)
          ## Save rasters
          writeRaster(c, filename = paste(dir_nrr, i, "CHELSA", period, 
                                          paste0(paste(i, paste0("Bio", v), 
                                              period, sep="_"),".tif"), sep="/"),
                      overwrite=TRUE, filetype="GTiff") 
          rm(c) }
          print("Files created")
        }
      ## SOILGRIDS variables 
      if (!is.null(soil)) { 
        #setwd(paste(dir_nrr, i, sep="/"))
          dir.create(paste(dir_nrr, i, "SoilGrids", sep="/")) 
          #setwd(file.path("SoilGrids")) 
          y <- paste(dir_soil, (list.files(dir_soil, 
                                           pattern=paste0((do.call(paste, c(expand.grid(soil, depth), sep="_"))),
                                                          collapse="|"))), sep="/")  %>%
            rast(.) 
          ## Crop all 
          compareCRS(y, range)
          c <- crop(y, range, mask=TRUE)
          rm(y)
          ## Change extent of Soil Raster
          ext(c) <- ext_nr
          ## Save rasters
          for (z in c@ptr$names) {
            writeRaster(c[z], filename = paste(dir_nrr, i, "SoilGrids", 
                                               paste0(paste(i, z, sep="_"), 
                                                      ".tif"), sep="/"),
                        overwrite=TRUE, filetype="GTiff") }
          rm(c, z) 
          print("Files created")
        }
    } else {
      ### 4.2.2. If Species Folder already created ####
      print(paste("Native Range of", i,"already created", sep=" ")) 
      #setwd(paste(dir_nrr, i, sep="/"))
      ### 4.2.2.1. CHELSA Bioclimatic variables ####
      if (!is.null(varbio)) { 
        if (file.exists(paste(dir_nrr, i, "CHELSA", sep="/"))) { 
          #setwd(file.path("CHELSA")) 
          if(file.exists(paste(dir_nrr, i, "CHELSA", period, sep="/"))) { 
            if(isFALSE(length(list.files(paste(dir_nrr, i, "CHELSA", period, sep="/"))) == 19 )) { 
              ## If varbio =/= ones in CHELSA folder
              print(paste("CHELSA for", i, "doesn't have all variables", sep=" "))
              ## CHELSA world rasters
              for (v in 1:19) {
              x <- paste0(paste(dir_chelsa, period,"Bio", sep="/"), v, "_", period,".tif") %>%
                rast(.) 
              ## Crop all 
              compareCRS(x, range)
              c <- crop(x, range, mask=TRUE)
              rm(x)
              ## Save rasters
              #setwd(paste(dir_nrr, i, "CHELSA", period, sep="/"))
              writeRaster(c, filename = paste(dir_nrr, i, "CHELSA", period, 
                                              paste0(paste(i, paste0("Bio", v), 
                                                 period, sep="_"),".tif"), sep="/"),
                          overwrite=TRUE, filetype="GTiff")
              rm(c) }
              print("Files created")
            } else {print(paste("CHELSA for", i, "have all variables", sep=" "))}
          } else {
            print(paste("Folder", period, "doesn't exist yet", sep=" "))
            #setwd(paste(dir_nrr, i, "CHELSA", sep="/")) 
            dir.create(paste(dir_nrr, i, "CHELSA", period, sep="/"))
            #setwd(file.path(period)) 
            ## CHELSA world rasters
            for (v in 1:19) {
            x <- paste0(paste(dir_chelsa, period,"Bio", sep="/"), v, "_", period,".tif") %>%
              rast(.) 
            ## Crop all 
            compareCRS(x, range)
            c <- crop(x, range, mask=TRUE)
            rm(x)
            ## Save rasters
            writeRaster(c, filename = paste(dir_nrr, i, "CHELSA", period, 
                                            paste0(paste(i, paste0("Bio", v), 
                                              period, sep="_"),".tif"), sep="/"),
                        overwrite=TRUE, filetype="GTiff") 
            rm(c) }
            print("Files created") } 
        } else { 
          dir.create(paste(dir_nrr, i, "CHELSA", sep="/")) 
          #setwd(file.path("CHELSA")) 
          dir.create(paste(dir_nrr, i, "CHELSA", period, sep="/"))
          #setwd(file.path(period)) 
          ## CHELSA world rasters
          for (v in 1:19) {
          x <- paste0(paste(dir_chelsa, period,"Bio", sep="/"), v, "_", period,".tif") %>%
            rast(.) 
          ## Crop all 
          compareCRS(x, range)
          c <- crop(x, range, mask=TRUE)
          rm(x)
          ## Save rasters
          writeRaster(c, filename = paste(dir_nrr, i, "CHELSA", period, 
                                          paste0(paste(i, paste0("Bio", v), 
                                           period, sep="_"),".tif"), sep="/"),
                      overwrite=TRUE, filetype="GTiff") 
          rm(c) }
          print("Files created")
        }
      } 
      ### 4.2.1.2. SOIL GRIDS variables ####
      if (!is.null(soil)) { 
        if (file.exists(paste(dir_nrr, i, "SoilGrids", sep="/"))) { 
          #setwd(file.path("SoilGrids")) 
          ## If there are or not soil variables
          if (isFALSE(all((do.call(paste, c(expand.grid(soil, depth), sep="_"))) %in% 
                          (str_extract(list.files(paste(dir_nrr, i, "SoilGrids", sep="/")), 
                                       "(?<=_)(.+)(?=\\.)")) )) ) { 
            print(paste("SoilGrids for", i, "doesn't have all the variables", sep=" "))  
            ## SoilGRIDS world rasters
            y <- paste(dir_soil, (list.files(dir_soil, 
                                             pattern=paste0((do.call(paste, c(expand.grid(soil, depth), sep="_"))),
                                                            collapse="|"))), sep="/") %>% rast(.) 
            ## Crop all 
            compareCRS(y, range)
            c <- crop(y, range, mask=TRUE)
            rm(y)
            ## Change extent of Soil Raster
            ext(c) <- ext_nr
            ## Save rasters
            for (z in c@ptr$names) {
              writeRaster(c[z], filename = paste(dir_nrr, i, "SoilGrids", 
                                                 paste0(paste(i, z, sep="_"), 
                                                        ".tif"), sep="/"),
                          overwrite=TRUE, filetype="GTiff") }
            rm(c, z) 
            print("Files created") } 
        } else {
          dir.create(paste(dir_nrr, i, "SoilGrids", sep="/")) 
          #setwd(file.path("SoilGrids"))
          ## SoilGRIDS world rasters
          y <- paste(dir_soil, (list.files(dir_soil, 
                                           pattern=paste0((do.call(paste, c(expand.grid(soil, depth), sep="_"))),
                                                          collapse="|"))), sep="/") %>% rast(.) 
          ## Crop all 
          compareCRS(y, range)
          c <- crop(y, range, mask=TRUE)
          rm(y)
          ## Change extent of Soil Raster
          ext(c) <- ext_nr
          ## Save rasters
          for (z in c@ptr$names) {
            writeRaster(c[z], filename = paste(dir_nrr, i, "SoilGrids", 
                                               paste0(paste(i, z, sep="_"), 
                                                      ".tif"), sep="/"),
                        overwrite=TRUE, filetype="GTiff") }
          rm(c, z) 
          print("Files created")
        }
      }      
    }
    print ("Done") 
    #### 5. Load Rasters to Data Frame ####
    print(paste("Modelling and mapping climatic niche for", i, "in", count_name, "...", sep=" "))
    ### 5.1. Load Rasters in NR species (CHELSA + SoilGRIDS) ####
      if (is.null(varbio)) { 
        ## When there are only Soil variables 
         rast_spe <- paste0(paste(dir_nrr, i,"SoilGrids/", sep="/"), 
                            paste(i, (do.call(paste, c(expand.grid(soil, depth), 
                                                       sep="_"))), sep="_"), ".tif") %>%
           rast(.)
         
         ## Aggregate (returns the result in a convenient form)
         rast_spe <- aggregate(rast_spe, fact = 0.1/res(rast_spe), FUN=sum, na.rm=T, na.action=NULL)
         ### Create data.frame
         rdf_spe <- as.data.frame(rast_spe, na.rm=NA, xy=TRUE, cell=T)
         #rdf_spe <- rownames_to_column(rdf_spe, var = "cell")
         rm(rast_spe)
         } else {
           if (is.null(soil)) {
             ## When there are only Bioclimatic variables 
             rast_spe <- paste0(paste(dir_nrr, i,"CHELSA", "1951-1980", 
                                  i, sep="/"), "_Bio", varbio, "_1951-1980.tif") %>%
               rast(.)
        
             ## Aggregate (returns the result in a convenient form)
             rast_spe <- aggregate(rast_spe, fact = 0.1/res(rast_spe), FUN=sum, na.rm=TRUE, na.action=NULL)
             ### Create data.frame
             rdf_spe <- as.data.frame(rast_spe, na.rm=TRUE, xy=TRUE, cell=T)
             #rdf_spe <- rownames_to_column(rdf_spe, var = "cell")
             rm(rast_spe)
           } else {
             ## When there are both
             rast_soil <- paste0(paste(dir_nrr, i,"SoilGrids/", sep="/"), 
                                paste(i, (do.call(paste, c(expand.grid(soil, depth), 
                                                           sep="_"))), sep="_"), ".tif") %>%
               rast(.)
             rast_chel <- paste0(paste(dir_nrr, i,"CHELSA", "1951-1980", 
                                      i, sep="/"), "_Bio", varbio, "_1951-1980.tif") %>%
               rast(.)
             rast_chel <- resample(rast_chel, rast_soil, method = "bilinear")
             rast_spe <- c(rast_chel, rast_soil)
             rm(rast_soil, rast_chel)
             
             ## Aggregate (returns the result in a convenient form)
             rast_spe <- aggregate(rast_spe, fact = 0.1/res(rast_spe), FUN=sum, na.rm=TRUE, na.action=NULL)
             ### Create data.frame
             rdf_spe <- as.data.frame(rast_spe, na.rm=TRUE, xy=TRUE, cell=T)
             #rdf_spe <- rownames_to_column(rdf_spe, var = "cell")
             rm(rast_spe)
           } }
      
      ## Rename columns in function if varbio, soil or neither NULL
      if (is.null(varbio)) {
        colnames(rdf_spe) <- c("cell", "x", "y", 
                              paste0((do.call(paste, c(expand.grid(soil, depth), sep="_"))))) 
      } else { 
        if (is.null(soil)) { colnames(rdf_spe) <- c("cell", "x", "y", paste0("Bio",varbio)) } else {
          colnames(rdf_spe) <- c("cell", "x", "y", paste0("Bio",varbio),
                                paste0((do.call(paste, c(expand.grid(soil, depth), sep="_"))))) } }
      rdf_spe["Source"]="Aire indigénat"
      
      ## Add "nr" (native range) to cell values in data
      rdf_spe$cell <- sub("^","nr_",rdf_spe$cell)
      ## Remove column if only NA inside
      rdf_spe <- rdf_spe[,colSums(is.na(rdf_spe))<nrow(rdf_spe)]
      
    ### 5.2. Rasters in Country (CHELSA + SoilGRIDS) ####
    print("Convert rasters to Data Frames")
    ## Loop for each periods
    rdf_count <- data.frame() 
    if (is.null(varbio)) { 
      ## When there are only soil variables
      print("Only for soil variables")
      rast_c <- paste(dir_cras, "SoilGrids", paste0(paste(count_name, (do.call(paste, c(expand.grid(soil, depth), sep="_"))), sep="_"), 
                                                    ".tif"), sep="/") %>%
        rast(.)
      ## Aggregate (returns the result in a convenient form)
      rast_c<-aggregate(rast_c, fact = 0.1/res(rast_c), FUN=sum, na.rm=T, na.action=NULL)
      ## Create data.frame
      rdf <- as.data.frame(rast_c, na.rm=NA, xy=TRUE, cell=T)
      rm(rast_c)
      
      ## Rename columns in function if varbio, soil or neither NULL
      colnames(rdf) <- c("cell", "x", "y", 
                         paste0((do.call(paste, c(expand.grid(soil, depth), sep="_"))))) 
      rdf["Source"]= count_name
      ## Add country abbreviation to cell (rownames) in data
      rdf$cell <- sub("^",paste0( (substr(count_name, start = 1, stop = 2)), sep="_"),rdf$cell)
      ## Add 'rdf' in 'rdf_count' 
      rdf_count <- rbind(rdf, rdf_count)
      ## Remove column if only NA inside
      rdf_count <- rdf_count[,colSums(is.na(rdf_count))<nrow(rdf_count)]
    } else {
      for (j in periods) { 
        ## If there are SSP folders (future periods) ?
        if (length(list.files(path = paste(dir_cras,"CHELSA",j, sep="/"), 
                              pattern = "*ssp", full.names = F) ) > 0) {
          ### 5.2.1. Periods with Scenarios ####
          print(paste0("Period ", j, " ...")) 
          for (k in ssp) { 
            print(paste0("Scenario ", k, " ..."))
            
            ### Rasters in Country to study (CHELSA + SoilGRIDS)
              if (is.null(soil)) {
                ## Where there are only bioclimatic variables
                rast_c <- paste0(paste(paste(dir_cras, "CHELSA", j, k, count_name, sep="/"), paste0("Bio", 
                                                                                               varbio), j, k, sep="_"), ".tif") %>%
                  rast(.)
              } else {
                ## When there are both 
                rast_c <- c(paste0(paste(paste(dir_cras, "CHELSA", j, k, count_name, sep="/"), 
                                         paste0("Bio", varbio), j, k, sep="_"), ".tif"),  
                            paste(dir_cras, "SoilGrids", 
                                  paste0(paste(count_name, (do.call(paste, c(expand.grid(soil, depth), sep="_"))), 
                                               sep="_"), ".tif"), sep="/")) %>%
                  rast(.)
              } 
          ## Aggregate (returns the result in a convenient form)
          rast_c<-aggregate(rast_c, fact = 0.1/res(rast_c), FUN=sum, na.rm=T, na.action=NULL)
          ## Create data.frame
          rdf <- as.data.frame(rast_c, na.rm=T, xy=TRUE, cell=T)
          rm(rast_c)
    
          ## Rename columns in function if varbio, soil or neither NULL
            if (is.null(soil)) { colnames(rdf) <- c("cell", "x", "y", paste0("Bio",varbio)) } else {
              colnames(rdf) <- c("cell", "x", "y", paste0("Bio",varbio),
                                 paste0((do.call(paste, c(expand.grid(soil, depth), sep="_"))))) } 
          rdf["Source"]= paste0(count_name, "_", j, k)
          ## Add country abbreviation to cell (rownames) in data
          rdf$cell <- sub("^",paste0( (substr(count_name, start = 1, stop = 2)), j, k, sep="_"),rdf$cell)
          ## Add 'rdf' in 'rdf_count' 
          rdf_count <- rbind(rdf, rdf_count)
        } 
          } else {
          ### 5.2.2. Periods without Scenarios ####
          print(paste0("Period ", j, " ...")) 
            if (is.null(soil)) {
              ## When there are only bioclimatic variables
              rast_c <- paste0(paste(paste(dir_cras, "CHELSA", j, count_name, sep="/"), 
                                     paste0("Bio", varbio), j, sep="_"), ".tif") %>%
                rast(.)
            } else {
              ## When there are both
              rast_c <- c(paste0(paste(paste(dir_cras, "CHELSA", j, count_name, sep="/"), 
                                       paste0("Bio", varbio), j, sep="_"), ".tif"),  
                          paste(dir_cras, "SoilGrids", 
                                paste0(paste(count_name, (do.call(paste, c(expand.grid(soil, depth), sep="_"))), 
                                             sep="_"), ".tif"), sep="/")) %>%
                rast(.) 
            } 
      ## Aggregate (returns the result in a convenient form)
      rast_c<-aggregate(rast_c, fact = 0.1/res(rast_c), FUN=sum, na.rm=TRUE, na.action=NULL)
      ## Create data.frame
      rdf <- as.data.frame(rast_c, na.rm=TRUE, xy=TRUE, cell=T)
      rm(rast_c)
      
      ## Rename columns in function if varbio, soil or neither NULL
        if (is.null(soil)) { colnames(rdf) <- c("cell", "x", "y", paste0("Bio",varbio)) } else {
          colnames(rdf) <- c("cell", "x", "y", paste0("Bio",varbio),
                             paste0((do.call(paste, c(expand.grid(soil, depth), sep="_"))))) } 
      rdf["Source"]= paste(count_name ,j, sep="_")
      ## Add country abbreviation to cell (rownames) in data
      rdf$cell <- sub("^",paste0( (substr(count_name, start = 1, stop = 2)), j, sep="_"),rdf$cell)
      ## Add 'rdf' in 'rdf_count' 
      rdf_count <- rbind(rdf, rdf_count)
          }
        }  
      }
    #### 6. PCA and Select close points ####
    ### 6.1. PCA (Environmental space) ####
    print("Projecte in Environmental space (PCA)")
    ## Plots with PCA
    pca_list <- list()
    ## Data frame with coordinates (get_pca_ind)
    ind_list <- list()
    ### 6.1.1. For Soil variables ####
    if (is.null(varbio)) { 
    ## Merge / make the combine species and country data
    merg <-rbind(
      rdf_count[rdf_count$Source == count_name, ], 
      rdf_spe)
    merg <- na.omit(merg)
    ## tag individus
    rownames(merg) <- merg$cell
    
    ## Select quantitative variables in function if varbio, soil or neither NULL
    ## dudi.pca (ade4) to run the PCA on a centered and reduced data table.
    
    merg %>%
      dplyr::select(-c("cell", "x", "y", "Source")) %>%
      dudi.pca(scannf = FALSE, nf = 2) -> res_acp
    
    ## Scree plot of the PCA eigenvalues,
    plot_vp <- fviz_eig(res_acp, addlabels = TRUE)
    plot_vp
    #get_eigenvalue(res_acp)
    
    ## Visualisation of ind
    chemElement <- "ind_soil"
    ind <- get_pca_ind(res_acp)
    ind_list[[chemElement]]  <- ind
    
    ### Individuals PCA
    chemElement <- "pca_soil"
    plot_ind <- fviz_pca_biplot(res_acp,
                                geom.ind = "point",
                                # Montre les points seulement 
                                # (mais pas le "text")
                                col.ind = merg$Source, # colorer by groups
                                col.var = "black",
                                palette = c(
                                  "#E7B800",
                                  "#00AFBB"),
                                # select.ind = list(name = ),
                                addEllipses = TRUE, 
                                # Ellipses de concentration
                                legend.title = "Groups",
                                repel = TRUE) +   
      theme(legend.position = "bottom") +
      ggtitle("(a)") +
      theme(plot.title = element_text(size = 25, face = "bold"),
            axis.text=element_text(size=17),
            axis.title=element_text(size=17),
            legend.text = element_text(size=20)) 
    plot_ind 
    pca_list[[chemElement]]  <- plot_ind
    } else {
    ### 6.1.2. For Bioclimatic variables ####
    for (j in periods) {
      ## If there are SSP folders (future periods) ?
      if (length(list.files(path = paste(dir_cras,"CHELSA",j, sep="/"), 
                            pattern = "*ssp", full.names = F) ) > 0) {
        ## 6.1.1. Periods with Scenarios ####
        print(paste0("Period ", j, " ...")) 
        for (k in ssp) { 
          print(paste0("Scenario ", k, " ..."))
          ## Merge / make the combine species and country data
          merg <-rbind(
            rdf_count[rdf_count$Source == (paste0(count_name, "_", j, k)), ], 
            rdf_spe)
          ## tag individus
          rownames(merg) <- merg$cell
          ## Add column with country name and natif
          c <- sub("_.*", "", merg$Source)
          merg['Country'] <- c
          rm(c)
          
          ## Select quantitative variables in function if varbio, soil or neither NULL
          ## dudi.pca (ade4) to run the PCA on a centered and reduced data table.
          
          merg %>%
            dplyr::select(-c("cell", "x", "y", "Source", "Country")) %>%
            dudi.pca(scannf = FALSE, nf = 2) -> res_acp
        
          ## Scree plot of the PCA eigenvalues,
          plot_vp <- fviz_eig(res_acp, addlabels = TRUE)
          plot_vp
          #get_eigenvalue(res_acp)
          
          ## Visualisation of ind
          chemElement <- paste("ind", j, k, sep = "_")
          ind <- get_pca_ind(res_acp)
          ind_list[[chemElement]]  <- ind
        
          ### Individuals PCA
          chemElement <- paste("pca", j, k, sep = "_")
          plot_ind <- fviz_pca_biplot(res_acp,
                                      geom.ind = "point",
                                      # Montre les points seulement 
                                      # (mais pas le "text")
                                      col.ind = merg$Country, # colorer by groups
                                      col.var = "black",
                                      palette = c(
                                        "#E7B800",
                                        "#00AFBB"),
                                      # select.ind = list(name = ),
                                      addEllipses = TRUE, 
                                      # Ellipses de concentration
                                      legend.title = "Groups",
                                      repel = TRUE) + 
            theme(legend.position = "bottom") +
            ggtitle(j) +
            theme(plot.title = element_text(size = 15, face = "bold"),
                  axis.text=element_text(size=10),
                  axis.title=element_text(size=10),
                  legend.text = element_text(size=10)) 
          plot_ind 
          pca_list[[chemElement]]  <- plot_ind
        }
      } else {
          ## 6.1.2. Periods without Scenarios ####
          print(paste0("Period ", j, " ...")) 
          ## Merge / make the combine species and country data
          merg <-rbind(
            rdf_count[rdf_count$Source == (paste(count_name ,j, sep="_")), ],
            rdf_spe)
          ## tag individus
          rownames(merg) <- merg$cell
          ## Add column with country name and natif
          c <- sub("_.*", "", merg$Source)
          merg['Country'] <- c
          rm(c)
          
          ## Using 'prcomp' function
          # pca <- prcomp(merg[,4:11], scale.=TRUE)
          # biplot(pca)
          
          ## Select quantitative variables 
          ## PCA on a centered and reduced data table
          ## run PCA on all background points (PCA-env sensu Broennimann et al 2012)
          ## dudi.pca to retain equal balance between realized niches in the native 
          ## and introduced ranges.
          
          merg %>%
            dplyr::select(-c("cell", "x", "y", "Source", "Country")) %>%
            dudi.pca(scannf = FALSE, nf = 2) -> res_acp
          
          ## Scree plot of the PCA eigenvalues,
          plot_vp <- fviz_eig(res_acp, addlabels = TRUE)
          plot_vp
          #get_eigenvalue(res_acp)
          
          ## Visualisation of ind
          chemElement <- paste("ind", j, sep = "_")
          ind <- get_pca_ind(res_acp)
          ind_list[[chemElement]]  <- ind
          # ind
          # head(ind$coord)
          # tail(ind$coord)
          # str(ind$coord)
          #
          ### Individuals PCA
          chemElement <- paste("pca", j, sep = "_")
          plot_ind <- fviz_pca_biplot(res_acp,
                                      geom.ind = "point", 
                                      # Montre les points seulement 
                                      # (mais pas le "text")
                                      col.ind = merg$Country, # colorer by groups
                                      col.var = "black",
                                      palette = c(
                                        "#E7B800",
                                        "#00AFBB"),
                                      # select.ind = list(name = ),
                                      addEllipses = TRUE, 
                                      # Ellipses de concentration
                                      legend.title = "Groups",
                                      repel = TRUE) + 
            theme(legend.position = "bottom") +
            ggtitle(j) +
            theme(plot.title = element_text(size = 15, face = "bold"),
                  axis.text=element_text(size=10),
                  axis.title=element_text(size=10),
                  legend.text = element_text(size=10)) 
          plot_ind 
          pca_list[[chemElement]]  <- plot_ind
        }
      }
    }
    ### 6.2. Select close points ####
    print("Select Country points close to NR ones")
    ## Plots with select points in Envir. space
    dist_list <- list()
    ## Data Frame with select points
    map_list <- list()
    ### 6.2.1. For Soil variables ####
    if (is.null(varbio)) {
      ## Merge / make the combine species and country data
      merg <-rbind(
        rdf_count[rdf_count$Source == count_name, ], 
        rdf_spe)
      ## sample data, add point in er that would fall outside of the perimeter
      ind <- ind_list[[1]]
      er <- ind$coord[substr(rownames(ind$coord), 1, 
                             nchar(paste0( (substr(count_name, start = 1, stop = 2)),
                                           sep="_"))) == paste0( (substr(count_name, start = 1, stop = 2)), sep="_"), ]
      setDT(er, keep.rownames = "rowname")[]
      er <- as.data.frame(er)
      
      ## nr and threshold distance
      nr <- ind$coord[substr(rownames(ind$coord), 1, 3) == 'nr_', ]
      setDT(nr, keep.rownames = "rowname")[]
      nr <- as.data.frame(nr)
      
      ### Extract NR species and in country overlapping points 
      if(length(md)==1) {
        ### Without Gradient by distance
        dist <- distance_semi_join(er, nr,
                                   by = c("Dim.1","Dim.2"),
                                   max_dist=md,
                                   distance_col = "dist")
        # dfnp[nrow(dfnp) + 1,] <- c(nrow(dist), i, j, NA, length(varbio), 
        #                            paste(sort(c(varbio, soil)), collapse = ","))
        
        ## If overlap = 0
        if (nrow(dist) == 0) { 
          dist[nrow(dist) + 1,] = c("no_value",NA, NA) 
          
          ## View plot 
          chemElement <- "in_dist_sol"
          in_dist <- ggplot() +
            geom_point(data = er, aes(x = Dim.1, y = Dim.2, color = count_name))+
            geom_point(data = nr, aes(x = Dim.1, y = Dim.2, color = "Aire indigénat"))+
            
            #geom_point(data = dist, aes(x = Dim.1, y = Dim.2), shape = 1, size = 5 )+
            coord_fixed() +
            scale_colour_manual(values = c("Aire indigénat" = "#E7B800", 
                                          "France" = "#00AFBB")) + 
            theme(legend.position = "bottom") +
            ggtitle("(c)") +
            theme(plot.title = element_text(size = 25),
                  axis.text=element_text(size=17),
                  axis.title=element_text(size=17),
                  legend.text = element_text(size=20),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(),  axis.line = element_line(colour = "black")) +
            labs(fill = "", color ="")
          in_dist
          #assign(paste("ind_pca", j, sep = "_"), in_dist)
          dist_list[[chemElement]]  <- in_dist
          ### Points in the Country to study
          ## Merge data.frame
          map <- merge(dist, merg, by.x='rowname', by.y='cell') %>% insertRows(., 1, new = NA) 
        } else { 
          ## View plot 
          chemElement <- "in_dist_sol"
          in_dist <- ggplot() +
            geom_point(data = er, aes(x = Dim.1, y = Dim.2, color = count_name))+
            geom_point(data = nr, aes(x = Dim.1, y = Dim.2, color = "Aire indigénat"))+
            
            geom_point(data = dist, aes(x = Dim.1, y = Dim.2, fill = "Similaire"),
                       shape = 1, size = 5 )+
            coord_fixed() +
            scale_colour_manual(values = c("Aire indigénat" = "#E7B800", 
                                           "France" = "#00AFBB")) + 
            ggtitle("(c)") +
            theme(legend.position = "bottom") +
            theme(plot.title = element_text(size = 25),
                  axis.text=element_text(size=17),
                  axis.title=element_text(size=17),
                  legend.text = element_text(size=20),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(),  axis.line = element_line(colour = "black")) +
            labs(fill = "", color ="")
          in_dist
          #assign(paste("ind_pca", j, sep = "_"), in_dist)
          dist_list[[chemElement]]  <- in_dist
          ### Points in the Country to study
          ## Merge data.frame
          map <- merge(dist, merg, by.x='rowname', by.y='cell') }
        ## Adding a new column with period and scenarios
        map$Sol <- "Niche édaphique potentielle"
        
        ## Add map in list (or rename map)
        chemElement <- "map_sol"
        map_list[[chemElement]]  <- map
        rm(map) }
    } else {
    ### 6.2.2.For Bioclimatic variables ####
    for (j in periods) {
      ## If there are SSP folders (future periods) ?
      if (length(list.files(path = paste(dir_cras,"CHELSA",j, sep="/"), 
                            pattern = "*ssp", full.names = F) ) > 0) {
        ### 6.2.1. Periods with Scenarios ####
        print(paste0("Period ", j, " ...")) 
        for (k in ssp) {
          print(paste0("Scenario ", k, " ...")) 
          ## Merge / make the combine species and country data
          merg <-rbind(
            rdf_count[rdf_count$Source == (paste0(count_name, "_", j, k)), ], 
            rdf_spe)
          ## sample data, add point in er that would fall outside of the perimeter
          ind <- ind_list[[grep(paste(j, k, sep="_"), names(ind_list), value = TRUE)]]
          # sample data, add point in er (etablishment range) that would fall outside of the perimeter
          er <- ind$coord[substr(rownames(ind$coord), 1, 
                                 nchar(paste0( (substr(count_name, start = 1, stop = 2)), j, k, 
                                               sep="_"))) == paste0( (substr(count_name, start = 1, stop = 2)), j, k, sep="_"), ]
          setDT(er, keep.rownames = "rowname")[]
          er <- as.data.frame(er)
          
          ## nr (native range) and threshold distance
          nr <- ind$coord[substr(rownames(ind$coord), 1, 3) == 'nr_', ]
          setDT(nr, keep.rownames = "rowname")[]
          nr <- as.data.frame(nr)
          
          ### Extract NR species and in country overlapping points 
          if(length(md)==1) {
            ### 6.2.1.1. Without Distance Gradient ####
            dist <- distance_semi_join(er, nr, 
                                       by = c("Dim.1","Dim.2"), 
                                       max_dist= md,
                                       distance_col = "dist")
            dfnp[nrow(dfnp) + 1,] <- c(nrow(dist), i, j, k, length(varbio), 
                                       paste(sort(c(varbio, soil)), collapse = ","))
    
            ### 6.2.1.1.1. If overlap = 0 ####
            if (nrow(dist) == 0) { 
              dist[nrow(dist) + 1,] = c("no_value", NA, NA) 
              
              ## View plot 
              chemElement <- paste("in_dist", j, k, sep = "_")
              in_dist <- ggplot() +
                geom_point(data = er, aes(x = Dim.1, y = Dim.2, color = count_name))+
                geom_point(data = nr, aes(x = Dim.1, y = Dim.2, color = "Aire indigénat"))+
                
                #geom_point(data = dist, aes(x = Dim.1, y = Dim.2), shape = 1, size = 5 )+
                coord_fixed() +
                scale_colour_manual(values = c("Aire indigénat" = "#E7B800", 
                                               "France" = "#00AFBB")) + 
                theme(legend.position = "bottom") +
                ggtitle(j) +
                theme(plot.title = element_text(size = 25, face = "bold"),
                      axis.text=element_text(size=17),
                      axis.title=element_text(size=17),
                      legend.text = element_text(size=20),
                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_blank(),  axis.line = element_line(colour = "black")) +
                labs(fill = "", color ="")
              in_dist
              #assign(paste("ind_pca", j, k, sep = "_"), in_dist)
              dist_list[[chemElement]]  <- in_dist
              
              ### Points in the Country to study
              ## Merge data.frame
              map <- merge(dist, merg, by.x='rowname', by.y='cell') %>% insertRows(., 1, new = NA) 
            } else { 
              ### 6.2.1.1.2. If overlap =/= 0 ####
              ## View plot 
              chemElement <- paste("in_dist", j, k, sep = "_")
              in_dist <- ggplot() +
                geom_point(data = er, aes(x = Dim.1, y = Dim.2, color = count_name))+
                geom_point(data = nr, aes(x = Dim.1, y = Dim.2, color = "Aire indigénat"))+
                
                geom_point(data = dist, aes(x = Dim.1, y = Dim.2, fill = "Similaire"), 
                           shape = 1, size = 5 )+
                coord_fixed() +
                scale_colour_manual(values = c("Aire indigénat" = "#E7B800", 
                                               "France" = "#00AFBB")) + 
                theme(legend.position = "bottom") +
                ggtitle(j) +
                theme(plot.title = element_text(size = 25, face = "bold"),
                      axis.text=element_text(size=17),
                      axis.title=element_text(size=17),
                      legend.text = element_text(size=20),
                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_blank(),  axis.line = element_line(colour = "black")) +
                labs(fill = "", color ="")
              in_dist
              #assign(paste("ind_pca", j, k, sep = "_"), in_dist)
              dist_list[[chemElement]]  <- in_dist
              
              ### Points in the Country to study
              ## Merge data.frame
              map <- merge(dist, merg, by.x='rowname', by.y='cell')
            }
            ## Adding a new column with period and scenarios
            map$Period <- j
            map$Scenario <- k
            
            ## Add map in list (or rename map)
            chemElement <- paste("map", j, k, sep = "_")
            map_list[[chemElement]]  <- map
            #assign(  paste("map", j, k, sep = "_"), map ) 
            rm(map)
          } else {
            ### 6.2.1.2. With Distance Gradient ####
            dfdist <- data.frame()
            for(m in md) {
              dist <- distance_semi_join(er, nr, 
                                         by = c("Dim.1","Dim.2"), 
                                         max_dist= m,
                                         distance_col = "dist") 
              dfnp[nrow(dfnp) + 1,] <- c(nrow(dist), i, j, k, length(varbio), 
                                         paste(sort(c(varbio, soil)), collapse = ","))
              ### 6.2.1.2.1. If overlap = 0 ####
              if (nrow(dist) == 0) { 
                dist[nrow(dist) + 1,] = c("no_value", NA, NA) 
                
                ## View plot 
                chemElement <- paste("in_dist", j, k, m, sep = "_")
                in_dist <- ggplot() +
                  geom_point(data = er, aes(x = Dim.1, y = Dim.2, color = count_name))+
                  geom_point(data = nr, aes(x = Dim.1, y = Dim.2, color = "Native range"))+
                  
                  #geom_point(data = dist, aes(x = Dim.1, y = Dim.2), shape = 1, size = 5 )+
                  coord_fixed() +
                  scale_colour_manual(values = c("#00AFBB","#E7B800")) + 
                  theme(legend.position = "bottom") +
                  ggtitle(paste(j, sep=" ")) +
                  theme(plot.title = element_text(size = 20, face = "bold"))
                in_dist
                dist_list[[chemElement]]  <- in_dist
              } else { 
                ### 6.2.1.2.2. If overlap =/= 0 ####
                ## View plot 
                chemElement <- paste("in_dist", j, k, m, sep = "_")
                in_dist <- ggplot() +
                  geom_point(data = er, aes(x = Dim.1, y = Dim.2, color = count_name))+
                  geom_point(data = nr, aes(x = Dim.1, y = Dim.2, color = "Native range"))+
                  
                  geom_point(data = dist, aes(x = Dim.1, y = Dim.2), shape = 1, size = 5 )+
                  coord_fixed() +
                  scale_colour_manual(values = c("#00AFBB","#E7B800")) + 
                  theme(legend.position = "bottom") +
                  ggtitle(paste(j, m, sep=" ")) +
                  theme(plot.title = element_text(size = 20, face = "bold"))
                in_dist
                dist_list[[chemElement]]  <- in_dist 
              }
              dist["MaxDist"] <- m
              dfdist <- rbind(dfdist, dist) 
            }
            ## Gradient map (in function if 0 pnts or not)
            if (nrow(dfdist) == 0) { 
              gmap <- merge(dfdist, merg, by.x='rowname', by.y='cell') %>% insertRows(., 1, new = NA) } else {
                gmap <- merge(dfdist, merg, by.x='rowname', by.y='cell')
              }
            map <- as.data.frame(data.table(gmap)[data.table(gmap)[ , .I[which.min(MaxDist)], by = rowname]$V1])
            ## Adding a new column with period and scenarios
            map$Period <- j
            map$Scenario <- k
            ## Add map in list (or rename map)
            chemElement <- paste("map", j, k, sep = "_")
            map_list[[chemElement]]  <- map
            rm(gmap, map, dfdist)
          }
        }
      } else { 
        ## 6.2.2. Periods without Scenarios ####
        print(paste0("Period ", j, " ...")) 
        ## Merge / make the combine species and country data
        merg <-rbind(
          rdf_count[rdf_count$Source == (paste(count_name, j, sep="_")), ], 
          rdf_spe)
        ## sample data, add point in er that would fall outside of the perimeter
        ind <- ind_list[[grep(j, names(ind_list), value = TRUE)]]
        er <- ind$coord[substr(rownames(ind$coord), 1, 
                               nchar(paste0( (substr(count_name, start = 1, stop = 2)), j, 
                                             sep="_"))) == paste0( (substr(count_name, start = 1, stop = 2)), j, sep="_"), ]
        setDT(er, keep.rownames = "rowname")[]
        er <- as.data.frame(er)
        
        ## nr and threshold distance
        nr <- ind$coord[substr(rownames(ind$coord), 1, 3) == 'nr_', ]
        setDT(nr, keep.rownames = "rowname")[]
        nr <- as.data.frame(nr)
        
        ### Extract NR species and in country overlapping points 
        if(length(md)==1) {
          ### Without Gradient by distance
          dist <- distance_semi_join(er, nr,
                                     by = c("Dim.1","Dim.2"),
                                     max_dist=md,
                                     distance_col = "dist")
          dfnp[nrow(dfnp) + 1,] <- c(nrow(dist), i, j, NA, length(varbio), 
                                     paste(sort(c(varbio, soil)), collapse = ","))
    
          ## If overlap = 0
          if (nrow(dist) == 0) { 
            dist[nrow(dist) + 1,] = c("no_value",NA, NA) 
            
            ## View plot 
            chemElement <- paste("in_dist", j, sep = "_")
            in_dist <- ggplot() +
              geom_point(data = er, aes(x = Dim.1, y = Dim.2, color = count_name))+
              geom_point(data = nr, aes(x = Dim.1, y = Dim.2, color = "Aire indigénat"))+
              #geom_point(data = dist, aes(x = Dim.1, y = Dim.2), shape = 1, size = 5 )+
              coord_fixed() +
              scale_colour_manual(values = c("Aire indigénat" = "#E7B800", 
                                             "France" = "#00AFBB")) + 
              theme(legend.position = "bottom") +
              ggtitle(j) +
              theme(plot.title = element_text(size = 25, face = "bold"),
                    axis.text=element_text(size=17),
                    axis.title=element_text(size=17),
                    legend.text = element_text(size=20),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(),  axis.line = element_line(colour = "black")) +
              labs(fill = "", color ="")
            in_dist
            #assign(paste("ind_pca", j, sep = "_"), in_dist)
            dist_list[[chemElement]]  <- in_dist
            ### Points in the Country to study
            ## Merge data.frame
            map <- merge(dist, merg, by.x='rowname', by.y='cell') %>% insertRows(., 1, new = NA) 
          } else { 
            ## View plot 
            chemElement <- paste("in_dist", j, sep = "_")
            in_dist <- ggplot() +
              geom_point(data = er, aes(x = Dim.1, y = Dim.2, color = count_name))+
              geom_point(data = nr, aes(x = Dim.1, y = Dim.2, color = "Aire indigénat"))+
              geom_point(data = dist, aes(x = Dim.1, y = Dim.2, fill = "Similaire"),
                         shape = 1, size = 5 ) +
              coord_fixed() +
              scale_colour_manual(values = c("Aire indigénat" = "#E7B800", 
                                             "France" = "#00AFBB")) + 
              theme(legend.position = "bottom") +
              ggtitle(j) +
              theme(plot.title = element_text(size = 25, face = "bold"),
                    axis.text=element_text(size=17),
                    axis.title=element_text(size=17),
                    legend.text = element_text(size=20),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(),  axis.line = element_line(colour = "black")) +
              labs(fill = "", color ="")
            in_dist
            #assign(paste("ind_pca", j, sep = "_"), in_dist)
            dist_list[[chemElement]]  <- in_dist
            ### Points in the Country to study
            ## Merge data.frame
            map <- merge(dist, merg, by.x='rowname', by.y='cell') }
          ## Adding a new column with period and scenarios
          map$Period <- j
          
          ## Add map in list (or rename map)
          chemElement <- paste("map", j, sep = "_")
          map_list[[chemElement]]  <- map
          #assign(  paste("map", j, sep = "_"), map ) 
          rm(map)
          } else {
          ### With Gradient by distance
          dfdist <- data.frame()
          for(m in md) {
            dist <- distance_semi_join(er, nr, 
                                       by = c("Dim.1","Dim.2"), 
                                       max_dist= m,
                                       distance_col = "dist") 
            dfnp[nrow(dfnp) + 1,] <- c(nrow(dist), i, j, NA, length(varbio), 
                                       paste(sort(c(varbio, soil)), collapse = ","))
            
            ## If overlap = 0
            if (nrow(dist) == 0) { 
              dist[nrow(dist) + 1,] = c("no_value", NA, NA) 
              
              ## View plot 
              chemElement <- paste("in_dist", j, m, sep = "_")
              in_dist <- ggplot() +
                geom_point(data = nr, aes(x = Dim.1, y = Dim.2, color = "Native range"))+
                geom_point(data = er, aes(x = Dim.1, y = Dim.2, color = count_name))+
                #geom_point(data = dist, aes(x = Dim.1, y = Dim.2), shape = 1, size = 5 )+
                coord_fixed() +
                scale_colour_manual(values = c("#00AFBB","#E7B800")) + 
                theme(legend.position = "bottom") +
                ggtitle(paste(j, sep=" ")) +
                theme(plot.title = element_text(size = 20, face = "bold"),
                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_blank(),  axis.line = element_line(colour = "black"))
              in_dist
              #assign(paste("ind_pca", j, k, sep = "_"), in_dist)
              dist_list[[chemElement]]  <- in_dist
              
              ### Points in the Country to study
              ## Merge data.frame
              #map <- merge(dist, merg, by.x='rowname', by.y='cell') %>% insertRows(., 1, new = NA) 
            } else { 
              ## View plot 
              chemElement <- paste("in_dist", j, m, sep = "_")
              in_dist <- ggplot() +
                geom_point(data = nr, aes(x = Dim.1, y = Dim.2, color = "Native range"))+
                geom_point(data = er, aes(x = Dim.1, y = Dim.2, color = count_name))+
                geom_point(data = dist, aes(x = Dim.1, y = Dim.2), shape = 1, size = 5 )+
                coord_fixed() +
                scale_colour_manual(values = c("#00AFBB","#E7B800")) + 
                theme(legend.position = "none") +
                ggtitle(paste(j, m, sep=" ")) +
                theme(plot.title = element_text(size = 20, face = "bold"),
                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_blank(),  axis.line = element_line(colour = "black"))
              in_dist
              #assign(paste("ind_pca", j, k, sep = "_"), in_dist)
              dist_list[[chemElement]]  <- in_dist 
              ### Points in the Country to study
              ## Merge data.frame
              #map <- merge(dist, merg, by.x='rowname', by.y='cell')
            }
            dist["MaxDist"] <- m
            dfdist <- rbind(dfdist, dist) 
          }
          ## Gradient map (in function if 0 pnts or not)
          if (nrow(dfdist) == 0) { 
            gmap <- merge(dfdist, merg, by.x='rowname', by.y='cell') %>% insertRows(., 1, new = NA) } else {
              gmap <- merge(dfdist, merg, by.x='rowname', by.y='cell')
            }
          map <- as.data.frame(data.table(gmap)[data.table(gmap)[ , .I[which.min(MaxDist)], by = rowname]$V1])
          ## Adding a new column with period and scenarios
          map$Period <- j
          ## Add map in list (or rename map)
          chemElement <- paste("map", j, sep = "_")
          map_list[[chemElement]]  <- map
          rm(gmap, map, dfdist)
        }
        }
      }
    }
    #### 7. Mapping in Country ####
    ### Merge data maps
    ## Define there are not Scenario in "1951-1980" and "1981-2010" periods
    if (!is.null(varbio)) {
    map_list[[1]]["Scenario"]= NA
    map_list[[2]]["Scenario"]= NA
    }
    map <- map_list %>% 
      data.table::rbindlist(.)
    ## by ABbreviation
    AB <- map_data("world") %>% filter(region %in% count)
    ## Depending on the number of MD
    if (length(md)==1) {
      ### 7.1. Without Gradient distance 
      bplot_list <- list()
      if (is.null(varbio)) {
        chemElement <- "p_sol"
        p <- ggplot() +
          geom_polygon(data = AB, aes(x=long, y = lat, group = group), fill="#ECECEC") +
          geom_point( data=(map_list$map_sol), 
                      aes(fill=Sol, x=x, y=y),
                      colour = "#632742", size = 1, na.rm = FALSE) +
          theme_void() + coord_map() + ggtitle("(b)") +
          theme(legend.position = "none")  + 
          theme(plot.title = element_text(size = 25), 
                legend.text = element_text(size=15)) +
          labs(fill ="")
        p
        bplot_list[[chemElement]] <- p 
        rm(p)
      } else {
      ## Color of maps
      cols <- c("cyan2", "cyan4", 
                "#A5D875", "#FFB74D", "#E23D00",
                "#6AAC57", "#FB8C00", "#B41500",
                "#41850B", "#EF6C00", "#7D0025")
      ncols <- character(0)
      for(j in periods) {
        if(is.na(map[grep(j, map$Period),]$Scenario[[1]])) {
          ncols <- c(ncols, j)
        } else {
          for (k in ssp) {
            ncols <- c(ncols, paste(j, k, sep="_", collapse = ","))
          }
        }  
      }
      names(cols) <- ncols
    
      for (j in periods) {
        if(is.na(map[grep(j, map$Period),]$Scenario[[1]])) {
          # print("No scenario")
          chemElement <- paste("p", j, sep = "_")
          p <- ggplot() +
            geom_polygon(data = AB, aes(x=long, y = lat, group = group), fill="#ECECEC") +
            geom_point( data=(map[grep(j, map$Period),]), 
                        aes(fill=Period, x=x, y=y),
                        colour = cols[j], size = 1, na.rm = FALSE) +
            theme_void() + coord_map() + ggtitle(j) +
            theme(legend.position = "none")  + 
            theme(plot.title = element_text(size = 20, face = "bold"))
          
          bplot_list[[chemElement]] <- p 
          #assign(paste("p", j, sep = "_"), p ) 
          rm(p)
        } else {
          # print("Scenario")
          for (k in ssp) {
            chemElement <- paste("p", j, k, sep = "_")
            p <- ggplot() +
              geom_polygon(data = AB, aes(x=long, y = lat, group = group), fill="#ECECEC") +
              geom_point( data=(map[grep(j, map$Period),] %>% .[grep(k, .$Scenario),]), 
                          aes(fill=Period, x=x, y=y), 
                          colour = cols[paste(j,k, sep="_")], size = 1, na.rm = FALSE) +
              theme_void() + coord_map() + ggtitle(paste(j,
                                                         #toupper(k),
                                                         sep=" ")) +
              theme(legend.position = "none")  + 
              theme(plot.title = element_text(size = 20, face = "bold"))
            bplot_list[[chemElement]]  <- p
            #assign(paste("p", j, k, sep = "_"), p ) 
            rm(p)
            }
          }
        } 
      } 
    } else {
      ### 8.2. With Distance Gradient
      # Color
      cols <- c("cyan", "blue",
                "lime", "yellow", "orange",
                "light-green", "amber", "deep-orange",
                "green", "orange", "red")
      
      ncols <- character(0)
      for(j in periods) {
        if(is.na(map[grep(j, map$Period),]$Scenario[[1]])) {
          ncols <- c(ncols, j)
        } else {
          for (k in ssp) {
            ncols <- c(ncols, paste(j, k, sep="_", collapse = ","))
          }
        }  
      }
      names(cols) <- ncols
      
      bplot_list <- list()
      for (j in periods) {
        if(is.na(map[grep(j, map$Period),]$Scenario[[1]])) {
          # print("No scenario")
          chemElement <- paste("p", j, sep = "_")
          p <- ggplot() +
            geom_polygon(data = AB, aes(x=long, y = lat, group = group), fill="#ECECEC") +
            geom_point(data= map,
                       #(map[grep(j, map$Period),] %>% .[grep(k, .$Scenario),]), 
                       aes(x=x, y=y, colour=MaxDist), size = 1, na.rm = FALSE) +
            scale_color_material(cols[j], trans = 'reverse') +
            theme_void() + coord_map() + ggtitle(paste(j,k,
                                                       #toupper(k),
                                                       sep=" ")) +
            theme(plot.title = element_text(size = 20, face = "bold"))
          
          bplot_list[[chemElement]] <- p 
          #assign(paste("p", j, sep = "_"), p ) 
          rm(p)
        } else {
          # print("Scenario")
          for (k in ssp) {
            chemElement <- paste("p", j, k, sep = "_")
            p <- ggplot() +
              geom_polygon(data = AB, aes(x=long, y = lat, group = group), fill="#ECECEC") +
              geom_point(data= map,
                         #(map[grep(j, map$Period),] %>% .[grep(k, .$Scenario),]), 
                         aes(x=x, y=y, colour=MaxDist), size = 1, na.rm = FALSE) +
              scale_color_material(cols[paste(j,k, sep="_")], trans = 'reverse') +
              theme_void() + coord_map() + ggtitle(paste(j,k,
                                                         #toupper(k),
                                                         sep=" ")) +
              theme(plot.title = element_text(size = 20, face = "bold"))
            
            bplot_list[[chemElement]]  <- p
            #assign(paste("p", j, k, sep = "_"), p ) 
            rm(p)
          }
        }
      }
    }
    
    #### 8. Distance points between maps ####
    ## 8.1. Total Scenarios ####
    if (!is.null(varbio)) {
    map_dist_sc <- list()
    for (j in periods) {
      if(!is.na(map[grep(j, map$Period),]$Scenario[[1]])) {
        for (k in ssp)  {
          ## If dfs in list are equal or not to NA (0 point)
          if (all(sapply(map_list[grep(k, names( map_list ), value = T )], \(x) anyNA(x$rowname)))) {
            ## If 0 point, so create df empty with NA inside
            dist <- data.frame(matrix(NA, nrow = 1, ncol = (ncol(map_list[[grep(k, names(map_list), value = TRUE)[1]]])))) 
            colnames(dist) <- (colnames(map_list[[grep(k, names(map_list), value = TRUE)[1]]]))
            dist$Scenario <- k
            dist['ID'] <- NA
            dist['Presence'] <- NA
            chemElement <- paste("map_dist", k, sep = "_")
            map_dist_sc[[chemElement]]  <- dist
            rm(dist) 
          } else {
            ## If no empty, see overlap between maps
            total <- rbind(map_list[[grep(k, names(map_list), value = TRUE)[1]]],
                           map_list[[grep(k, names(map_list), value = TRUE)[2]]],
                           map_list[[grep(k, names(map_list), value = TRUE)[3]]] )
            total['XY'] <- paste(total$x, total$y, sep="/")
            total <- total %>% 
              add_count(XY, name = "Presence")
            total$Period <- NA
            total <- subset(total, select = -c(XY))
            total['ID'] <- sub("^[^_]*_", "", total$rowname)
            total <- total %>% drop_na(ID)
            ## Delete duplicated rows
            total = total[!duplicated(total$ID),]
          
            ## Add to the list
            chemElement <- paste("map_dist", k, sep = "_")
            map_dist_sc[[chemElement]]  <- total
            }
          } 
        }
      }
    }
    ## 8.2. Total Periods ####
    if (!is.null(varbio)) {
    map_dist_pe <- list()
    for (j in periods) {
      if(!is.na(map[grep(j, map$Period),]$Scenario[[1]])) {
      #   ## If dfs in list are equal or not to NA (0 point)
        if (all(sapply(map_list[grep(j, names(map_list), value = TRUE)], \(x) anyNA(x$rowname)))) {
          ## If 0 point, so create df empty with NA inside
          dist <- data.frame(matrix(NA, nrow = 1, ncol = (ncol(map_list[[grep(j, names(map_list), value = TRUE)[1]]])))) 
          colnames(dist) <- (colnames(map_list[[grep(j, names(map_list), value = TRUE)[1]]]))
          dist$Period <- j
          dist['ID'] <- NA
          dist['Presence'] <- NA
          chemElement <- paste("map_dist", j, sep = "_")
          map_dist_pe[[chemElement]]  <- dist
          rm(dist) 
        } else {
          ## If no empty, see overlap between maps
          total <- rbind(map_list[[grep(j, names(map_list), value = TRUE)[1]]],
                         map_list[[grep(j, names(map_list), value = TRUE)[2]]],
                         map_list[[grep(j, names(map_list), value = TRUE)[3]]] )
          total['XY'] <- paste(total$x, total$y, sep="/")
          total <- total %>% 
            add_count(XY, name = "Presence")
          total$Scenario <- NA
          total <- subset(total, select = -c(XY))
          total['ID'] <- sub("^[^_]*_", "", total$rowname)
          total <- total %>% drop_na(ID)
          ## Delete duplicated rows
          total = total[!duplicated(total$ID),]
          
          ## Add to the list
          chemElement <- paste("map_dist", j, sep = "_")
          map_dist_pe[[chemElement]]  <- total
         }
        } 
      }
    }
    ## 8.3. Total of totals ####
    if (!is.null(varbio)) {
    #map_dist_tot <- list()
    if (all(sapply(c(map_dist_sc,map_dist_pe), \(x) anyNA(x$rowname)))) {
      ## If 0 point, so create df empty with NA inside
      Total <- data.frame(matrix(NA, nrow = 1, ncol = (ncol(map_list[[grep(j, names(map_list), value = TRUE)[1]]])))) 
      colnames(Total) <- (colnames(map_list[[grep(j, names(map_list), value = TRUE)[1]]]))
      #chemElement <- paste0("map_dist_tot")
      #map_dist_tot[[chemElement]]  <- dist
      #rm(dist) 
    } else {
      ## If nb points >0
      Total <- rbind(map_dist_sc[[1]], map_dist_sc[[2]], map_dist_sc[[3]])
      Total['ID'] <- sub("^[^_]*_", "", Total$rowname)
      #map_dist_pe[[1]]['ID'] <- sub("^[^_]*_", "", map_dist_pe[[1]]$rowname)
      #Total <- Total %>% group_by(ID) %>% add_count(Presence, name = "Sum")
      ag <- aggregate(x = Total$Presence, by = list(Total$ID), FUN = sum)
      Total <- merge(x=Total, y=ag, by.x="ID", by.y="Group.1", all = TRUE)
      rm(ag)
      colnames(Total)[colnames(Total) == "x.y"] ="Sum"
      colnames(Total)[colnames(Total) == "x.x"] ="x"
      #Total$Sum <- ave(Total$ID,FUN=sum)
      }
    }
    #### 9. Mapping Total (Distance points between maps) ####
    ### 9.1. Map Total Scenarios ####
    if (!is.null(varbio)) {
    plot_dist_sc <- list()
    for (k in ssp) {
          ## If dfs in list are equal or not to NA (0 point)
          if (sapply(map_dist_sc[grep(k, names(map_dist_sc), value = TRUE)], \(x) anyNA(x$rowname))) { 
            ## If 0 point, just mapping empty country
            chemElement <- paste("plot", k, sep = "_")
          d <- ggplot() +
            geom_polygon(data = AB, aes(x=long, y = lat, group = group), fill="#ECECEC") +
            # geom_point( data=map_dist_sc[[grep(k, names(map_dist_sc), value = TRUE)]], 
            #             aes(fill=Period, x=x, y=y), 
            #             colour = "#6AB1D6", size = 1) +
            theme_void() + coord_map() + ggtitle("2011-2100") +
            theme(legend.position = "none")  + 
            theme(plot.title = element_text(size = 20, face = "bold"))
          plot_dist_sc[[chemElement]]  <- d
          #assign(paste("p", j, k, sep = "_"), p ) 
          rm(d)
          } else {
            ## If points,
            chemElement <- paste("plot", k, sep = "_")
            d <- ggplot() +
              geom_polygon(data = AB, aes(x=long, y = lat, group = group), fill="#ECECEC") +
              geom_point(data=map_dist_sc[[grep(k, names(map_dist_sc), value = TRUE)]],
                          aes(x=x, y=y,
                          colour = as.character(Presence)), size = 1) +
              theme_void() + coord_map() + ggtitle("Toutes périodes") +
              scale_color_manual(name = "Présence totale", values=c("#97CF8CFF", "#32A154FF", "#165D3CFF")) +
              theme(legend.position = "bottom")  + 
              theme(plot.title = element_text(size = 20, face = "bold"))
            d
            plot_dist_sc[[chemElement]]  <- d
            #assign(paste("p", j, k, sep = "_"), p ) 
            rm(d)
        }
      }
    }
    ### 9.2. Map Total Periods ####
    if (!is.null(varbio)) {
    plot_dist_pe <- list()
    for (j in periods) {
      ## Only keep periods with scenarios
      if(!is.na(map[grep(j, map$Period),]$Scenario[[1]])) { 
        ## If dfs in list are equal or not to NA (0 point)
        if (sapply(map_dist_pe[grep(j, names(map_dist_pe), value = TRUE)], \(x) anyNA(x$rowname))) { 
          ## If 0 point, just mapping empty country
          chemElement <- paste("plot", j, sep = "_")
          d <- ggplot() +
            geom_polygon(data = AB, aes(x=long, y = lat, group = group), fill="#ECECEC") +
            # geom_point( data=map_dist_pe[[grep(j, names(map_dist_pe), value = TRUE)]], 
            #             aes(fill=Period, x=x, y=y), 
            #             colour = "#6AB1D6", size = 1) +
            theme_void() + coord_map() + ggtitle(j) +
            #theme(legend.position = "none")  + 
            theme(plot.title = element_text(size = 20, face = "bold"))
          plot_dist_pe[[chemElement]]  <- d
          #assign(paste("p", j, k, sep = "_"), p ) 
          rm(d) } else {
            ## If points,
            chemElement <- paste("plot", j, sep = "_")
            d <- ggplot() +
              geom_polygon(data = AB, aes(x=long, y = lat, group = group), fill="#ECECEC") +
              geom_point( data=map_dist_pe[[grep(j, names(map_dist_pe), value = TRUE)]],
                          aes(x=x, y=y,
                          colour = as.character(Presence)), size = 1) +
              theme_void() + coord_map() + ggtitle(j) +
              scale_color_manual(name = "Présence totale", values=c("#97CF8CFF", "#32A154FF", "#165D3CFF")) +
              theme(legend.position = "bottom")  + 
              theme(plot.title = element_text(size = 20, face = "bold"))
            d
            plot_dist_pe[[chemElement]]  <- d
            #assign(paste("p", j, k, sep = "_"), p ) 
            rm(d)
          }
        }
      }
    }  
    ### 9.3. Map Total of totals ####
    if (!is.null(varbio)) {
    ## If total in list are equal or not to NA (0 point)
    if (any(is.na(Total$rowname))) { 
      ptot <- ggplot() +
        geom_polygon(data = AB, aes(x=long, y = lat, group = group), fill="#ECECEC") +
        # geom_point( data=Total, 
        #             aes(fill=Period, x=x, y=y), 
        #             colour = "#317197", size = 1) +
        theme_void() + coord_map() + ggtitle("Total") +
        theme(legend.position = "none")  + 
        theme(plot.title = element_text(size = 20, face = "bold"))
      } else {
        # If points >0,
        if(length(unique(Total$Sum)) > 3) {
      ptot <- ggplot() +
        geom_polygon(data = AB, aes(x=long, y = lat, group = group), fill="#ECECEC") +
        geom_point( data=Total, 
                    aes(x=x, y=y, 
                    colour = Sum), size = 1) +
        theme_void() + coord_map() + ggtitle("Total") +
        paletteer::scale_colour_paletteer_c("pals::ocean.algae", direction=-1, 
                                            name = "Présence totale",
                                            breaks = c(1:9))  + 
        theme(legend.position = "bottom")  + 
        theme(plot.title = element_text(size = 20, face = "bold")) 
      ptot
        } else {
          ptot <- ggplot() +
            geom_polygon(data = AB, aes(x=long, y = lat, group = group), fill="#ECECEC") +
            geom_point( data=Total, 
                        aes(x=x, y=y, 
                            colour = factor(Sum)), size = 1) +
            theme_void() + coord_map() + ggtitle("Total") +
            scale_color_manual(name = "Présence totale", values=c("#97CF8CFF", "#32A154FF", "#165D3CFF")) +
            theme(legend.position = "bottom")  + 
            theme(plot.title = element_text(size = 20, face = "bold")) 
          ptot
        }
      }
    }  
    #### 10. Arrange Maps together ####
    ### 10.1. Only soil variables ####
    if (is.null(varbio)) {
      print("Print all graphs with only soil variables")
      
      grid.arrange(pca_list[[1]], arrangeGrob(bplot_list[[1]], dist_list[[1]]), ncol = 2)
      
      library(cowplot)
      plot_grid(pca_list[[1]], dist_list[[1]], bplot_list[[1]], labels=c("A", "B", "C"), ncol = 1, nrow = 3)
      
      patch_sol <- (wrap_elements(pca_list[[1]]) + wrap_elements(dist_list[[1]]) + wrap_elements(bplot_list[[1]]) ) +
        plot_annotation(
          title = paste("Niches édaphiques potentielles projetées de", i, "en", count_name, sep=" "),
          subtitle = paste0("(Avec les variables édaphiques ", 
                                   paste(sort(soil), collapse = ","), 
                            " en profondeur ", paste(depth, collapse = ","),")"), 
          theme = theme(plot.title = element_text(size = 30, face="bold", hjust = 0.5),
                        plot.subtitle = element_text(size = 20, hjust = 0.5)))
      patch_sol
      ggsave((paste0(paste("Sol", i, sep="_"), ".pdf")),width=17, height=9, 
             patch_sol, path= dir_pc) #"Sol", sep="/"))
    } 
    ### 10.2. With bioclimatic variables ####
    if (!is.null(varbio)) {
    ### 10.2.1. Subtitles ####
    if (is.null(soil)) { sub = paste0("(Avec les variables bioclimatiques ", 
                                      paste(sort(varbio), collapse = ","), ")") } else {
        sub = paste0("(Avec les variables bioclimatiques ", paste(sort(varbio), collapse = ","),
                     " et sols ", paste(soil, collapse = ","),
                     " en profondeur ", paste(depth, collapse = ","), "cm)")
      }
    ### 10.2.2. Native Range map ####
    ## First, add Native Range in Reference maps
    ## World map
    world_coordinates <- map_data("world") %>% 
      st_as_sf(coords = c("long", "lat"), dim = "XY") %>% 
      st_set_crs(4326)
    ## Find min and max range coordinates
    coord_range <- st_as_sf(range)
    coord_range <- sf::st_cast(coord_range, "MULTIPOLYGON")
    coord_range <- data.frame(st_coordinates(coord_range$geometry))
    ## Do border of Native range 
    bb <-
      c(
        "xmin" = min(coord_range$X)-1,
        "xmax" = max(coord_range$X)+1,
        "ymin" = min(coord_range$Y)-1,
        "ymax" = max(coord_range$Y)+1
      ) %>%
      sf::st_bbox() %>%
      sf::st_as_sfc() %>%
      sf::st_as_sf(crs = 4326) %>%
      sf::st_transform(crs = 4326)
    
    ## Plot 1: Native Range in World
    world <-  ggplot() + 
      geom_map(
      data = map_data("world"), map = map_data("world"), fill = "#ECECEC",
      aes(long, lat, map_id = region) ) +
      #theme_void() + 
      #coord_map("ortho", orientation = c(90, 0, 0)) +
      geom_sf(data = bb, colour = "red", fill = NA, linewidth = 1) +
      tidyterra::geom_spatvector(data = range, fill = "#92D168FF", linewidth = NA) +
      coord_sf(xlim = c((min(coord_range$X)-20), (max(coord_range$X)+20)), 
               ylim = c((min(coord_range$Y)-20), (max(coord_range$Y)+20)), expand = FALSE) +
      theme(plot.title = element_text(size = 20, face="bold"),
            axis.text = element_text(size = 15), 
            axis.title = element_text(size = 15),
            #legend.title = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),  axis.line = element_line(colour = "black")) +
      labs(y = "Latitude",
           x = "Longitude") + ggtitle("Aire d'indigénat") 
    world
    #crds(range)
    
    ## Plot 2: only focus on Native Range
    prange <- ggplot() +
      geom_map(
        data = map_data("world"), map = map_data("world"), fill = "#ECECEC",
        aes(long, lat, map_id = region) ) +
      tidyterra::geom_spatvector(data = range, fill = "#92D168FF", linewidth = NA) + 
      theme(plot.title = element_text(size = 20, face = "bold")) +
      coord_sf(xlim = c((min(coord_range$X)-1), (max(coord_range$X)+1)), 
               ylim = c((min(coord_range$Y)-1), (max(coord_range$Y)+1)), expand = FALSE) +
      theme(text = element_text(size = 15), 
            legend.title = element_blank(),
            axis.text = element_text(size = 15), 
            axis.title = element_text(size = 15),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),  axis.line = element_line(colour = "black")) +
      labs(y = "Latitude",
           x = "Longitude") 
    prange
    
    ### 10.2.3. Arrange 1: Maps (no gradient) with points in country to study by periods and scenarios ####
    ## Arrange
    p1 <- cowplot::plot_grid(world, prange, bplot_list[[1]], bplot_list[[2]], ncol = 4) + 
      plot_annotation( title = paste0("Référence"), 
                       theme = theme(plot.title = element_text(size = 25, face="bold")))
    p2 <- cowplot::plot_grid(bplot_list[[3]], bplot_list[[6]], bplot_list[[9]], plot_dist_sc[[1]], ncol = 4) + 
      plot_annotation( title = paste0("Scénario SSP1-2.6"),
                       theme = theme(plot.title = element_text(size = 25, face="bold")))
    p3 <- cowplot::plot_grid(bplot_list[[4]], bplot_list[[7]], bplot_list[[10]], plot_dist_sc[[2]], ncol = 4) + 
      plot_annotation( title = paste0("Scénario SSP3-7.0"),
                       theme = theme(plot.title = element_text(size = 25, face="bold")))
    p4 <- cowplot::plot_grid(bplot_list[[5]], bplot_list[[8]], bplot_list[[11]], plot_dist_sc[[3]], ncol = 4) + 
      plot_annotation( title = paste0("Scénario SSP5-8.5"),
                       theme = theme(plot.title = element_text(size = 25, face="bold")))
    p5 <- cowplot::plot_grid(plot_dist_pe[[1]], plot_dist_pe[[2]], plot_dist_pe[[3]], ptot, ncol = 4) + 
      plot_annotation( title = paste0("Tous Scénarios"),
                       theme = theme(plot.title = element_text(size = 25, face="bold")))
    
    patch1 <- (wrap_elements(p1) / wrap_elements(p2) / wrap_elements(p3) / wrap_elements(p4) / wrap_elements(p5)) +
      plot_annotation(
        title = paste("Niche Bioclimatique Potentielle Projetée de", i, "en", count_name, sep=" "),
        subtitle = sub, 
        theme = theme(plot.title = element_text(size = 30, face="bold", hjust = 0.5),
                      plot.subtitle = element_text(size = 20, hjust = 0.5)))
    # patch1
    
    ### 10.2.4. Arrange 2: Arrange PCA plots ####
    p6 <- (pca_list[[1]] + pca_list[[2]]) +
      plot_annotation( title = paste0("Référence"),
                       theme = theme(plot.title = element_text(size = 15, face="bold")))
    p7 <- (pca_list[[3]]+ pca_list[[6]] + pca_list[[9]]) +
      plot_annotation( title = paste0("Scénario SSP1-2.6"),
                       theme = theme(plot.title = element_text(size = 15, face="bold")))
    p8 <- (pca_list[[4]]+ pca_list[[7]] + pca_list[[10]]) +
      plot_annotation( title = paste0("Scénario SSP3-7.0"),
                       theme = theme(plot.title = element_text(size = 15, face="bold")))
    p9 <- (pca_list[[5]]+ pca_list[[8]] + pca_list[[11]]) +
      plot_annotation( title = paste0("Scénario SSP5-8.5"),
                       theme = theme(plot.title = element_text(size = 15, face="bold")))
    
    patch2 <- (wrap_elements(p6)/wrap_elements(p7)/wrap_elements(p8)/wrap_elements(p9)) +
      plot_annotation(
        title = paste("ACP de", i, "en", count_name, sep=" "),
        subtitle = sub,
        theme = theme(plot.title = element_text(size = 20, face="bold", hjust = 0.5),
                      plot.subtitle = element_text(size = 15, hjust = 0.5)))
    #patch2
    
    ### 10.2.5. Arrange 3: Arrange Close points plots ####
    p10 <- (dist_list[[1]] + dist_list[[2]]) +
      plot_annotation( title = paste0("Référence"),
                       theme = theme(plot.title = element_text(size = 30, face="bold")))
    p11 <- (dist_list[[3]]+ dist_list[[6]] + dist_list[[9]]) +
      plot_annotation( title = paste0("Scénario SSP1-2.6"),
                       theme = theme(plot.title = element_text(size = 30, face="bold")))
    p12 <- (dist_list[[4]]+ dist_list[[7]] + dist_list[[10]]) +
      plot_annotation( title = paste0("Scénario SSP3-7.0"),
                       theme = theme(plot.title = element_text(size = 30, face="bold")))
    p13 <- (dist_list[[5]]+ dist_list[[8]] + dist_list[[11]]) +
      plot_annotation( title = paste0("Scénario SSP5-8.5"),
                       theme = theme(plot.title = element_text(size = 30, face="bold")))
    
    patch3 <- (wrap_elements(p10)/wrap_elements(p11)/wrap_elements(p12)/wrap_elements(p13)) +
      plot_annotation(
        title = paste("Espace environnemental de", i, "en", count_name, sep=" "),
        subtitle = sub,
        theme = theme(plot.title = element_text(size = 35, face="bold", hjust = 0.5),
                      plot.subtitle = element_text(size = 20, hjust = 0.5)))
    
    ## Remove all
    # rm(list = paste0("p", 1:9), bplot_list, AB, map, cols, chemElement,
    #    acp,rdf_spe,er,in_dist,ind,merg,nr,plot_ind,plot_vp,res_acp,
    #    dist_list, map_dist_pe, map_dist_sc, map_list, pca_list, plot_dist_pe, plot_dist_sc,
    #    ptot, Total, vcount, m, map_dist, sub)
    
    ### 10.2.6. Print the species map ####
    print(paste("Print global maps of ", i, "in", count_name, sep=" "))
    if (length(list.files(dir_pc, i))>0) {
      n <- max(as.numeric(na.omit(str_extract(list.files(dir_pc, i),
                                              "(?<=e)(.+)(?=\\_)"))))
      ggsave((paste0(paste(paste0("Carte", n+1), i, paste0("vars", length(varbio)), sep="_"), ".pdf")),width=20, height=31, patch1, 
             path= paste(dir_pc, "Cartes",
                         sep="/"))
      ggsave((paste0(paste(paste0("Space", n+1), i, paste0("vars", length(varbio)), sep="_"), ".pdf")),width=20, height=31, patch3, 
             path= paste(dir_pc, "Space", 
                         sep="/"))
      ggsave((paste0(paste(paste0("ACP", n+1), i, paste0("vars", length(varbio)), sep="_"), ".pdf")),width=11, height=16, patch2, 
             path= paste(dir_pc, "ACPs", 
                         sep="/"))
    } else {
      ggsave((paste0(paste("Carte1", i, paste0("vars", length(varbio)), sep="_"), ".pdf")),width=20, height=31, patch1, 
             path= paste(dir_pc, #"Cartes", 
                         sep="/"))
      ggsave((paste0(paste("Space1", i, paste0("vars", length(varbio)), sep="_"), ".pdf")),width=20, height=31, patch3,
             path= paste(dir_pc, #"Space",
                         sep="/"))
      ggsave((paste0(paste("ACP1", i, paste0("vars", length(varbio)), sep="_"), ".pdf")),width=11, height=16, patch2,
             path= paste(dir_pc, #"ACPs",
                         sep="/"))
          }
        } 
    }
#}
write.csv(dfnp, file="~/Documents/NicheEssenceExo/Scripts/dataf_npoints.csv", sep=",", dec=".")
