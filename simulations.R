# focus sparql endpoint
library(httr)
library(jsonlite)
library(SPARQL)
# GIS libraries
library(raster)
library(rgdal)
library(sp)
library("KernSmooth")
library(leaflet)

#---------------------
# mapping parameters:
#---------------------

library(leaflet)
HotspotColor = c("#040404B3","#080918B3","#0E0D24B3","#150F2EB3","#1D1135B3","#24123CB3","#2C1242B3","#341348B3","#3C134EB3","#451353B3","#4D1259B3",
                 "#56125DB3","#5F1162B3","#681066B3","#701069B3","#79106DB3","#82106FB3","#8A1172B3","#931373B3","#9B1674B3","#A31A75B3","#AB1E75B3",
                 "#B32375B3","#BA2973B3","#C12F71B3","#C8356FB3","#CF3B6BB3","#D64267B3","#DC4962B3","#E2505BB3","#E85752B3","#ED5F48B3","#F2673AB3",
                 "#F37133B3","#F47B2CB3","#F58426B3","#F58E23B3","#F69622B3","#F79F25B3","#F7A82CB3","#F7B134B3","#F8B93EB3","#F8C149B3","#F8CA54B3",
                 "#F9D25FB3","#F9DB6BB3","#FAE377B3","#FBEC84B3","#FDF490B3","#FFFE9EB3")
PatColor = c("#E24C80B3","#E4507EB3","#E5557DB3","#E6597BB3","#E75E7AB3","#E86278B3","#E96677B3","#EA6A76B3","#EB6E75B3","#EC7274B3","#ED7573B3"
             ,"#EE7972B3","#EE7D71B3","#EF8170B3","#F08470B3","#F1886FB3","#F18B6FB3","#F28F6FB3","#F3936FB3","#F3966FB3","#F4996FB3","#F49D6FB3"
             ,"#F5A070B3","#F5A470B3","#F6A771B3","#F6AA72B3","#F6AE74B3","#F7B175B3","#F7B476B3","#F7B878B3","#F8BB7AB3","#F8BE7CB3","#F8C17EB3"
             ,"#F9C581B3","#F9C883B3","#F9CB86B3","#F9CE88B3","#FAD18BB3","#FAD48EB3","#FAD891B3","#FADB95B3","#FBDE98B3","#FBE19BB3","#FBE49FB3"
             ,"#FBE7A2B3","#FCEAA6B3","#FCEDAAB3","#FDF0AEB3","#FDF3B1B3","#FDF6B5B3")
colours <- colorspace::choose_palette()
ObsColor <- colours(7, alpha=0.7)

#------------------------
# login to collect data
#------------------------

library(getPass)

URL <- "https://focus.sensingclues.org/"
url.login <- paste0(URL,"api/user/login")

#json_body <- jsonlite::toJSON(list(username = username,password=getPass()), auto_unbox = TRUE)
# we set up an authenticated session
#rl <- POST(url.login, body = json_body, encode = "raw",content_type_json())
rl <- POST(url.login, 
           body = jsonlite::toJSON(list(username = getPass("Enter your Cluey username:"),
                                        password=getPass()), 
                                   auto_unbox = TRUE), 
           encode = "raw",
           content_type_json())

## filters
#-----------
# concepts
q <- "  PREFIX skos:<http://www.w3.org/2004/02/skos/core#>
          SELECT DISTINCT ?Concept ?prefLabel
          WHERE
          { ?Concept ?x skos:Concept .
          { ?Concept skos:prefLabel ?prefLabel .
          FILTER regex(?prefLabel, 'Bear')}
          }
        "
focus.p <- "https://focus.sensingclues.org/v1/graphs/sparql?query="
ontology.lst <- content(GET(paste0(focus.p,URLencode(q, reserved=TRUE))))
names <- unlist(ontology.lst$head)
NROW <- length(ontology.lst$results$bindings)
NCOL <- length(names)
ontology.tbl <- matrix(nrow=NROW, ncol=NCOL)
for (a in 1:NROW) {
  for (b in 1:NCOL) {
    ontology.tbl[a,b] <- ontology.lst$results$bindings[[a]][[names[b]]]$value
  }
}
ontology.tbl <- data.frame(ontology.tbl)
names(ontology.tbl) <- names
# location
bounds <- list(north=90,east=40,south=30,west=0)
# date
DATE <- as.Date("2020-07-06")
input <- list(DateRange=c(as.Date("2018-05-01"),DATE))

#-----------
# data collection
#----------

q <- paste0('
{"query":
		{"and-query":
			{"queries":
				[{"range-constraint-query":{"constraint-name":"Concepts","range-operator":"EQ","value":["',ontology.tbl$Concept[4],'"],"range-option":[]}}
				,{"custom-constraint-query":{"constraint-name":"Incidents","point":[],"box":[{"south":',bounds$south,',"west":',bounds$west,',"north":',bounds$north,',"east":',bounds$east,'}],"circle":[],"polygon":[]}}
				,{"range-constraint-query":{"constraint-name":"Date","range-operator":"GE","value":["',input$DateRange[1],'"],"range-option":[]}}
				,{"range-constraint-query":{"constraint-name":"Date","range-operator":"LE","value":["',input$DateRange[2],'"],"range-option":[]}}
				]
			}
		 }
}')
url.incidents <- paste0(URL,"v1/values/IncidentLocations?options=all&start=1")
incidentDATA.L <- POST(url.incidents, body=q, encode="raw", content_type_json())
#incidentDATA.L <- POST(url.incidents, encode="raw", content_type_json())
incidentDATA <- content(incidentDATA.L)
Nobs <- length(incidentDATA[[1]]$tuple)
Nobs

DATA <- NULL
for(i in 1:Nobs) {
  coordname <- incidentDATA[[1]]$tuple[[i]]$`distinct-value`[[2]]$`_value`
  # Only read JSON files
  coords <- as.numeric(strsplit(coordname,",")[[1]])
  DATA <- rbind(DATA,coords)
}
DATA <- as.data.frame(DATA, row.names = NULL)
names(DATA) <- c("lat","lon")
row.names(DATA) <- NULL
coordinates(DATA) <- ~lon+lat
proj4string(DATA) <- "+proj=longlat +ellps=WGS84 +no_defs"

#####################################
#### IMPORT corine landcover file:  #
#####################################
setwd("C:/Users/konin056/Documents/Sensing Clues/CorineLandCover")
filename <- "CLC2012_CLC2006_V2018_20.tif"
CLC <- raster(filename)
EXTND <- extent(c(4587900,4678600,2061200,2133800))
CLCsmall <- crop(CLC, EXTND)

LEGEND <- read.table("CLC2012_CLC2006_V2018_20.txt", sep=",")
names(LEGEND) <- c("ID","R","G","B","max","Description")

plot(CLCsmall, col=rgb(LEGEND$R,LEGEND$G,LEGEND$B, maxColorValue = LEGEND$max), main="Corine land cover classification of study area")
plot(DATA.T, add=TRUE, col="red", pch=25)

levels(CLCsmall)[[1]]$Description <- LEGEND$Description

# EUNIS to CLC
library(tabulizer)
library(dplyr)

# source: https://www.eea.europa.eu/data-and-maps/data/eunis-habitat-classification/documentation/eunis-clc.pdf
filename <- 'EUNIS-CLC.TXT'
# import the table
out <- read.table(filename, header= TRUE, sep=";")
out$EUNIS.Code

# EUNIS Bear Habitat
library(readxl)
filename <- 'C:/Users/konin056/Documents/Sensing Clues/Bear factors/Factors_CA.xlsx'
BearHabitat <- read_xlsx(filename,sheet= "1. EUNIS Classes")

BearHabitat$EUNIS

# reduce EUNIS-CLC table:
EUNIStoCLC <- out[out$EUNIS.Code %in% BearHabitat$EUNIS,]

split <- strsplit(as.character(EUNIStoCLC$CLC.Code),split = "[.]")
EUNIStoCLC$CLCcode <- sapply(split,FUN=function(x){as.numeric(paste(x, collapse = ""))})
EUNIStoCLC %>% select(CLCcode,CLC.Code)

# merge files
BearHabitat.EC <- merge(BearHabitat, EUNIStoCLC, by.x="EUNIS", by.y="EUNIS.Code")

# fill in the EUNIS Bear habitat scores in the CLC legend
CLClegend <- levels(CLCsmall)[[1]]
CLCHabitatScores <- aggregate(Weight~CLCcode, BearHabitat.EC, "mean")
CLClegend$scores <- NA
CLClegend$scores[CLClegend$ID %in% CLCHabitatScores$CLCcode] <- CLCHabitatScores$Weight
CLCsmall@data@attributes[[1]] <- CLClegend

spplot(CLCsmall, zcol="CODE_06", main="Habitat types (Corine Landcover)",
       col.regions=rgb(LEGEND$R,LEGEND$G,LEGEND$B, maxColorValue = LEGEND$max),
       sp.layout = list("sp.points", DATA.T, pch = 15, cex=1.5,col="black"))
spplot(CLCsmall, zcol="scores", main="Bear habitat suitability score (Eunis based, reclassifid to Corine Landcover)",
       sp.layout = list("sp.points", DATA.T, pch = 15, cex=1.5,col="green"))

# make raster with habitat scores:
TEMP <- CLCsmall
values <- TEMP[]
values <- as.factor(values)
levels(values) <- CLClegend$scores[CLClegend$ID%in%levels(values)]
values <- as.numeric(as.character(values))
TEMP[] <- values

make_circ_filter<-function(radius, res){
  circ_filter<-matrix(NA, nrow=1+(2*radius/res), ncol=1+(2*radius/res))
  dimnames(circ_filter)[[1]]<-seq(-radius, radius, by=res)
  dimnames(circ_filter)[[2]]<-seq(-radius, radius, by=res)
  sweeper<-function(mat){
    for(row in 1:nrow(mat)){
      for(col in 1:ncol(mat)){
        dist<-sqrt((as.numeric(dimnames(mat)[[1]])[row])^2 +
                     (as.numeric(dimnames(mat)[[1]])[col])^2)
        if(dist<=radius) {mat[row, col]<-1}
      }
    }
    return(mat)
  }
  out<-sweeper(circ_filter)
  return(out)
}
filterradius <- 500
cf<-make_circ_filter(filterradius, 100)

r_filt<-focal(TEMP, w=cf, fun=function(x){mean(x, na.rm=TRUE)})

plot(TEMP, main="Prior Bear Habitat Suitability Map (Raw)") #original data
plot(r_filt, main=paste0("Prior Bear Habitat Suitability Map (",filterradius,"m filter radius)"))

#export data:
filepath <- "C:/Users/konin056/Documents/Sensing Clues/Paper 1/Data/"
FILEname <- paste0(DATE," Scientific prior")
writeRaster(r_filt, paste0(filepath,FILEname,".tif"),format="GTiff", overwrite=TRUE)

#----------------
# Observation likelihood map
#----------------

url.tracks <- paste0(URL,"v1/values/TrackLocations?options=all&start=1")

q <- paste0('
      {"query":
    		{"and-query":
	    		{"queries":
	    			[{"custom-constraint-query":{"constraint-name":"Tracks","point":[],"box":[{"south":',bounds$south,',"west":',bounds$west,',"north":',bounds$north,',"east":',bounds$east,'}],"circle":[],"polygon":[]}}
		    		,{"range-constraint-query":{"constraint-name":"Date","range-operator":"GE","value":["',input$DateRange[1],'"],"range-option":[]}}
		    		,{"range-constraint-query":{"constraint-name":"Date","range-operator":"LE","value":["',input$DateRange[2],'"],"range-option":[]}}
		    		]
		    	}
		     }
      }')
trackDATA.L <- POST(url.tracks, body=q, encode="raw", content_type_json())
trackDATA <- content(trackDATA.L)
Ntracks <- length(trackDATA[[1]]$tuple)

TRACKS <- NULL
if(Ntracks!=0) {
  print("Downloading tracks (this may take a while...)")
  for (i in 1:Ntracks) {
    filename <- trackDATA[[1]]$tuple[[i]]$`distinct-value`[[1]]$`_value`
    if(substr(filename,nchar(filename)-3,nchar(filename))=="json"){
      string <- paste0(URL,"v1/documents?format=json&uri=", filename)
      print(paste("Downloading track", i,"of",Ntracks))
      trackdata <- GET(string)
      if(trackdata$status_code==200) {
        trackdata <- content(trackdata)
        # get the detailed coordinates of the tracks
        coords <- trackdata[[1]]$attachments$original$track$geometry$coordinates
        coords <- t(matrix(unlist(coords),nrow=2,ncol=length(coords)))
        coords <- data.frame(coords)
        names(coords) <- c("lon","lat")
        # add timestamp data
        coords$time <- unlist(trackdata[[1]]$attachments$original$track$properties$timestamps)
        # remove duplicates
        coords <- coords[!duplicated(coords$time),]
        # calculate the time difference between subsequent fixes
        coords$time <- `substr<-`(coords$time,11,11," ")
        coords$time <- as.POSIXlt(coords$time)
        coords$dt <- c(as.numeric(coords$time[-nrow(coords)] - coords$time[-1]),0)
        # order the data so that it increases in time
        coords <- coords[order(coords$time),]
        # devide dataset into 5-minute time slots and save only 1 5-minute slot
        coords$timeslot <- cumsum(coords$dt)%/%(60*5)   
        coords <- coords[!duplicated(coords$timeslot),-4]
        TRACKS <- rbind(TRACKS,coords)
      }else {
        print(paste("Skipping track", i,"of",Ntracks))
      }
      
    } else {
      print(paste("Skipping track", i,"of",Ntracks))
    }
  }
  print("Successfully loaded the tracks")
  # make the data spatial
  if(!is.null(TRACKS)) {
    coordinates(TRACKS) <- ~lon+lat
    proj4string(TRACKS) <- "+proj=longlat +ellps=WGS84 +no_defs"
    TRACKS.T <- spTransform(TRACKS, crs(CLC))
  } else {
    print("all tracks skipped, no valid tracks loaded")
  }
} else {
  print("no tracks found")
}
TRACKS.T <- spTransform(TRACKS, crs(CLC))

StudyArea_Raster <- r_filt
xrange <- bbox(StudyArea_Raster)[3]-bbox(StudyArea_Raster)[1]
yrange <- bbox(StudyArea_Raster)[4]-bbox(StudyArea_Raster)[2]
scaled <- sqrt(ncell(StudyArea_Raster)/(xrange*yrange))

binsize <- 20/scaled

IncDensity <- bkde2D(coordinates(DATA.T), 
                     bandwidth=c(binsize,binsize), 
                     gridsize=c(StudyArea_Raster@ncols,StudyArea_Raster@nrows),
                     range.x=list(extent(StudyArea_Raster)[1:2],extent(StudyArea_Raster)[3:4]))
#IncDensity$fhat[IncDensity$fhat<(max(IncDensity$fhat,na.rm=TRUE)/100)] <- 0
IncDensity.raster <- raster(list(x=IncDensity$x1,y=IncDensity$x2,z=IncDensity$fhat))
proj4string(IncDensity.raster) <- proj4string(StudyArea_Raster)

PatDensity <- bkde2D(coordinates(TRACKS.T), 
                     bandwidth=c(binsize,binsize), 
                     gridsize=c(StudyArea_Raster@ncols,StudyArea_Raster@nrows),
                     range.x=list(extent(StudyArea_Raster)[1:2],extent(StudyArea_Raster)[3:4]))
PatDensity$fhat[PatDensity$fhat<(max(PatDensity$fhat,na.rm=TRUE)/100)] <- NA
PatDensity.raster <- raster(list(x=PatDensity$x1,y=PatDensity$x2,z=PatDensity$fhat))
proj4string(PatDensity.raster) <- proj4string(StudyArea_Raster)

Hotspots <- IncDensity.raster/PatDensity.raster

plot(Hotspots, main="Observation Likelihood Map")



#-----------------
# Bayesian model:
#-----------------

# normalise patrol map to 1 and calculate frequency of habitats patrolled
PatDensity.raster[] <- PatDensity.raster[]/sum(PatDensity.raster[], na.rm=TRUE)
P.habitat <- aggregate(PatDensity.raster[]~as.factor(CLCsmall[]), FUN="sum")
names(P.habitat) <- c("ID","freq")

# normalise observation heatmap to 1 and calculate the frequency of habitats visited by bear
Hotspots[] <- Hotspots[]/sum(Hotspots[], na.rm=TRUE)
P.habitat.bear <- aggregate(Hotspots[]~as.factor(CLCsmall[]), FUN=function(x){sum(x, na.rm=TRUE)})
names(P.habitat.bear) <- c("ID","freq")

# get the CLC habitat type at each bear observation location:
BearHabitat.OBS <- extract(CLCsmall, DATA.T)
BearHabitat.OBS <- BearHabitat.OBS[!is.na(BearHabitat.OBS)]
BearHabitat.OBS <- summary(factor(BearHabitat.OBS, levels=P.habitat.bear$ID))
P.habitat.bear$count <- BearHabitat.OBS
#this has to be corrected by the frequency of habitats patrolled
freq2 <- (P.habitat.bear$count/Nobs)/P.habitat$freq
freq2 <- freq2/sum(freq2)
P.habitat.bear$freq2 <- freq2

# frequency of habitats patrolled by humans
habitatfreq <- rep(0, length(freq2))  
habitatfreq[P.habitat.bear$ID %in% P.habitat$ID] <- P.habitat$freq
  
# Bayesian habitat suitability model with scientific evidence included
CLClegend$P.habitat <- NA
CLClegend$P.habitat.bear <- NA
CLClegend$P.habitat[CLClegend$ID %in% P.habitat$ID] <- P.habitat$freq
CLClegend$P.habitat.bear[CLClegend$ID %in% P.habitat.bear$ID] <- P.habitat.bear$freq2
CLClegend$P.bear.habitat <- sqrt(CLClegend$P.habitat.bear*CLClegend$scores)
  
# make raster with habitat scores:
TEMP <- CLCsmall
TEMP@data@attributes[[1]] <- CLClegend
values <- TEMP[]
values <- as.factor(values)
levels(values) <- CLClegend$P.bear.habitat[CLClegend$ID%in%levels(values)]
values <- as.numeric(as.character(values))
TEMP[] <- values
r_filt<-focal(TEMP, w=cf, fun=function(x){mean(x, na.rm=TRUE)})
plot(r_filt, main=paste0("Posterior Bear Habitat Suitability Map (",filterradius,"m filter radius) - CP = ",CP))

#export data:
filepath <- "C:/Users/konin056/Documents/Sensing Clues/Paper 1/Data/"
FILEname <- paste0(DATE," Baysian Posterior Nobs", Nobs)
writeRaster(r_filt, paste0(filepath,FILEname,".tif"),format="GTiff", overwrite=TRUE)

###############
# naive bayes #
###############

CLClegend$P.habitat <- NA
CLClegend$P.habitat.bear <- NA
CLClegend$P.habitat[CLClegend$ID %in% P.habitat$ID] <- P.habitat$freq
CLClegend$P.habitat.bear[CLClegend$ID %in% P.habitat.bear$ID] <- P.habitat.bear$freq2
CLClegend$P.bear.habitat <- CLClegend$P.habitat.bear

# make raster with habitat scores:
TEMP <- CLCsmall
TEMP@data@attributes[[1]] <- CLClegend
values <- TEMP[]
values <- as.factor(values)
levels(values) <- CLClegend$P.bear.habitat[CLClegend$ID%in%levels(values)]
values <- as.numeric(as.character(values))
TEMP[] <- values
r_filt<-focal(TEMP, w=cf, fun=function(x){mean(x, na.rm=TRUE)})
plot(r_filt, main=paste0("Naive bayes Habitat Suitability Map (",filterradius,"m filter radius)"))

#export data:
filepath <- "C:/Users/konin056/Documents/Sensing Clues/Paper 1/Data/"
FILEname <- paste0(DATE," Naive Bayes Habitat Suitability Nobs", Nobs)
writeRaster(r_filt, paste0(filepath,FILEname,".tif"),format="GTiff", overwrite=TRUE)


##########
# SIM V2 #
##########

for (ITER in 1:500) {
  
  starttime <- sample(0:250,1)
  endtime <- starttime + sample(0:400,1)
  TIMERANGE <- c(starttime,endtime)
  
  ID <- paste0(paste0(sample(letters,2),collapse=""),sample(10:99, 1))
  
  # date
  print(TIMERANGE)
  input <- list(DateRange=c(DATE-TIMERANGE[2],DATE-TIMERANGE[1]))
  print(input)
  
  #----------
  # data collection
  #----------
  q <- paste0('
{"query":
		{"and-query":
			{"queries":
				[{"range-constraint-query":{"constraint-name":"Concepts","range-operator":"EQ","value":["',ontology.tbl$Concept[4],'"],"range-option":[]}}
				,{"custom-constraint-query":{"constraint-name":"Incidents","point":[],"box":[{"south":',bounds$south,',"west":',bounds$west,',"north":',bounds$north,',"east":',bounds$east,'}],"circle":[],"polygon":[]}}
				,{"range-constraint-query":{"constraint-name":"Date","range-operator":"GE","value":["',input$DateRange[1],'"],"range-option":[]}}
				,{"range-constraint-query":{"constraint-name":"Date","range-operator":"LE","value":["',input$DateRange[2],'"],"range-option":[]}}
				]
			}
		 }
}')
  url.incidents <- paste0(URL,"v1/values/IncidentLocations?options=all&start=1")
  incidentDATA.L <- POST(url.incidents, body=q, encode="raw", content_type_json())
  #incidentDATA.L <- POST(url.incidents, encode="raw", content_type_json())
  incidentDATA <- content(incidentDATA.L)
  Nobs <- length(incidentDATA[[1]]$tuple)
  if (Nobs>0) {
    DATA <- NULL
    for(i in 1:Nobs) {
      coordname <- incidentDATA[[1]]$tuple[[i]]$`distinct-value`[[2]]$`_value`
      # Only read JSON files
      coords <- as.numeric(strsplit(coordname,",")[[1]])
      DATA <- rbind(DATA,coords)
    }
    DATA <- as.data.frame(DATA, row.names = NULL)
    names(DATA) <- c("lat","lon")
    row.names(DATA) <- NULL
    coordinates(DATA) <- ~lon+lat
    proj4string(DATA) <- "+proj=longlat +ellps=WGS84 +no_defs"
    DATA.T <- spTransform(DATA, crs(CLC))
    
    q <- paste0('
      {"query":
    		{"and-query":
	    		{"queries":
	    			[{"custom-constraint-query":{"constraint-name":"Tracks","point":[],"box":[{"south":',bounds$south,',"west":',bounds$west,',"north":',bounds$north,',"east":',bounds$east,'}],"circle":[],"polygon":[]}}
		    		,{"range-constraint-query":{"constraint-name":"Date","range-operator":"GE","value":["',input$DateRange[1],'"],"range-option":[]}}
		    		,{"range-constraint-query":{"constraint-name":"Date","range-operator":"LE","value":["',input$DateRange[2],'"],"range-option":[]}}
		    		]
		    	}
		     }
      }')
    trackDATA.L <- POST(url.tracks, body=q, encode="raw", content_type_json())
    trackDATA <- content(trackDATA.L)
    Ntracks <- length(trackDATA[[1]]$tuple)
    
    TRACKS <- NULL
    if(Ntracks!=0) {
      print("Downloading tracks (this may take a while...)")
      for (i in 1:Ntracks) {
        filename <- trackDATA[[1]]$tuple[[i]]$`distinct-value`[[1]]$`_value`
        if(substr(filename,nchar(filename)-3,nchar(filename))=="json"){
          string <- paste0(URL,"v1/documents?format=json&uri=", filename)
          print(paste("Downloading track", i,"of",Ntracks))
          trackdata <- GET(string)
          if(trackdata$status_code==200) {
            trackdata <- content(trackdata)
            # get the detailed coordinates of the tracks
            coords <- trackdata[[1]]$attachments$original$track$geometry$coordinates
            coords <- t(matrix(unlist(coords),nrow=2,ncol=length(coords)))
            coords <- data.frame(coords)
            names(coords) <- c("lon","lat")
            # add timestamp data
            coords$time <- unlist(trackdata[[1]]$attachments$original$track$properties$timestamps)
            # remove duplicates
            coords <- coords[!duplicated(coords$time),]
            # calculate the time difference between subsequent fixes
            coords$time <- `substr<-`(coords$time,11,11," ")
            coords$time <- as.POSIXlt(coords$time)
            coords$dt <- c(as.numeric(coords$time[-nrow(coords)] - coords$time[-1]),0)
            # order the data so that it increases in time
            coords <- coords[order(coords$time),]
            # devide dataset into 5-minute time slots and save only 1 5-minute slot
            coords$timeslot <- cumsum(coords$dt)%/%(60*5)   
            coords <- coords[!duplicated(coords$timeslot),-4]
            TRACKS <- rbind(TRACKS,coords)
          }else {
            print(paste("Skipping track", i,"of",Ntracks))
          }
          
        } else {
          print(paste("Skipping track", i,"of",Ntracks))
        }
      }
      print("Successfully loaded the tracks")
      # make the data spatial
      if(!is.null(TRACKS)) {
        coordinates(TRACKS) <- ~lon+lat
        proj4string(TRACKS) <- "+proj=longlat +ellps=WGS84 +no_defs"
        TRACKS.T <- spTransform(TRACKS, crs(CLC))
      } else {
        print("all tracks skipped, no valid tracks loaded")
      }
    } else {
      print("no tracks found")
    }
    
    StudyArea_Raster <- r_filt
    xrange <- bbox(StudyArea_Raster)[3]-bbox(StudyArea_Raster)[1]
    yrange <- bbox(StudyArea_Raster)[4]-bbox(StudyArea_Raster)[2]
    scaled <- sqrt(ncell(StudyArea_Raster)/(xrange*yrange))
    
    binsize <- 20/scaled
    
    IncDensity <- bkde2D(coordinates(DATA.T), 
                         bandwidth=c(binsize,binsize), 
                         gridsize=c(StudyArea_Raster@ncols,StudyArea_Raster@nrows),
                         range.x=list(extent(StudyArea_Raster)[1:2],extent(StudyArea_Raster)[3:4]))
    #IncDensity$fhat[IncDensity$fhat<(max(IncDensity$fhat,na.rm=TRUE)/100)] <- 0
    IncDensity.raster <- raster(list(x=IncDensity$x1,y=IncDensity$x2,z=IncDensity$fhat))
    proj4string(IncDensity.raster) <- proj4string(StudyArea_Raster)
    
    if(!is.null(TRACKS)) {
      PatDensity <- bkde2D(coordinates(TRACKS.T), 
                           bandwidth=c(binsize,binsize), 
                           gridsize=c(StudyArea_Raster@ncols,StudyArea_Raster@nrows),
                           range.x=list(extent(StudyArea_Raster)[1:2],extent(StudyArea_Raster)[3:4]))
      PatDensity$fhat[PatDensity$fhat<(max(PatDensity$fhat,na.rm=TRUE)/100)] <- NA
      PatDensity.raster <- raster(list(x=PatDensity$x1,y=PatDensity$x2,z=PatDensity$fhat))
      proj4string(PatDensity.raster) <- proj4string(StudyArea_Raster)
      
      Hotspots <- IncDensity.raster/PatDensity.raster
      
      plot(Hotspots, main="Observation Likelihood Map")
      
      # Bayesian model:
      #-----------------
      
      # normalise patrol map to 1 and calculate frequency of habitats patrolled
      PatDensity.raster[] <- PatDensity.raster[]/sum(PatDensity.raster[], na.rm=TRUE)
      P.habitat <- aggregate(PatDensity.raster[]~as.factor(CLCsmall[]), FUN="sum")
      names(P.habitat) <- c("ID","freq")
      
      # normalise observation heatmap to 1 and calculate the frequency of habitats visited by bear
      P.habitat.bear <- data.frame(ID=as.factor(rownames(HAmatrix)),freq=rep(0,nrow(HAmatrix)))
      Hotspots[] <- Hotspots[]/sum(Hotspots[], na.rm=TRUE)
      freq <- aggregate(Hotspots[]~as.factor(CLCsmall[]), FUN=function(x){sum(x, na.rm=TRUE)})$'Hotspots[]'
      P.habitat.bear$freq[P.habitat.bear$ID %in% P.habitat$ID] <- freq[levels(as.factor(CLCsmall[])) %in% P.habitat.bear$ID]
      
      # get the CLC habitat type at each bear observation location:
      BearHabitat.OBS <- extract(CLCsmall, DATA.T)
      BearHabitat.OBS <- BearHabitat.OBS[!is.na(BearHabitat.OBS)]
      BearHabitat.OBS <- summary(factor(BearHabitat.OBS, levels=P.habitat.bear$ID))
      P.habitat.bear$count <- BearHabitat.OBS
      #this has to be corrected by the frequency of habitats patrolled
      freq2 <- (P.habitat.bear$count/Nobs)[P.habitat.bear$ID %in% P.habitat$ID]/P.habitat$freq[P.habitat$ID %in% P.habitat.bear$ID]
      freq2 <- freq2/sum(freq2)
      P.habitat.bear$freq2 <- 0
      P.habitat.bear$freq2[P.habitat.bear$ID %in% P.habitat$ID] <- freq2
      
      # add prior to evidence and calculate posterior
      CLClegend$P.habitat <- NA
      CLClegend$P.habitat.bear <- NA
      CLClegend$P.habitat[CLClegend$ID %in% P.habitat$ID] <- P.habitat$freq
      CLClegend$P.habitat.bear[CLClegend$ID %in% P.habitat.bear$ID] <- P.habitat.bear$freq2
      CLClegend$P.bear.habitat <- sqrt(CLClegend$P.habitat.bear*CLClegend$scores)
      
      # make raster with habitat scores:
      TEMP <- CLCsmall
      TEMP@data@attributes[[1]] <- CLClegend
      values <- TEMP[]
      values <- as.factor(values)
      levels(values) <- CLClegend$P.bear.habitat[CLClegend$ID%in%levels(values)]
      values <- as.numeric(as.character(values))
      TEMP[] <- values
      r_filt<-focal(TEMP, w=cf, fun=function(x){mean(x, na.rm=TRUE)})
      
      #export data:
      filepath <- "C:/Users/konin056/Documents/Sensing Clues/Paper 1/Data/"
      FILEname <- paste0(DATE," HSRaster ID",ID," Nobs", Nobs)
      writeRaster(r_filt, paste0(filepath,FILEname,".tif"),format="GTiff", overwrite=TRUE)
      
    } else {print("Skipping due to lack of tracks")}
    
  } else {print("Skipping due to lack of observations")}
  
  #}
  
}


##################

m <- leaflet()
m <- addTiles(m)
m <- addRasterImage(m,r_filt,colors = ObsColor)
m <- addMarkers(m, data=DATA)
m


#################