


tkgrid(tklabel2(tt,text="    - Création de la Cartographie"),sticky="w")
#######CARTOGRAPHIE : WEBMAPPING
library(maptools)
gpclibPermit()
library(rgdal)
library(sp)

nrow(RLT_PCHFINALEXCEPT)
load(paste(racine,"/TEMPLATE/GEO_STATIONS/STATIONGEOPOINTWGS84_2.Rdata",sep=""))

  ##merge avec le "shp" des sations
PCHGEO<-merge( RLT_PCHFINALEXCEPT,STATIONGEOPOINTWGS84_2, by.y = "CDSTATIONM", by.x = "STATION")


   ##creation d'un fichier spatial
EPSG<-make_EPSG()
LAM93<-CRS(as.character(EPSG$prj4[EPSG$code == "2154" & !is.na(EPSG$prj4 )]))
WGS84<-CRS(as.character(EPSG$prj4[EPSG$code == "4326" & !is.na(EPSG$prj4 )]))
PCHGEO<-SpatialPointsDataFrame(PCHGEO[,c("COORDXSTAT", "COORDYSTAT")],data.frame(PCHGEO[,c("STATION" ,"ETATPCH")]), proj4string = LAM93  )
PCHGEO <- spTransform(PCHGEO, WGS84)

  ## creation des directory
dircarto<-paste(racine,"/CARTO/",gsub(".mdb","",FICHIERDATA),sep="")
fjson<-paste(dircarto,"/stations_pch.GeoJSON",sep="")
fol<-paste(dircarto,"/cartopch.html",sep="")
dircss<-paste(dircarto,"/CSS/",sep="")
if (!file.exists(dircarto)) {dir.create(dircarto)}
if (!file.exists(fol)) {file.copy(paste(racine,"/TEMPLATE/GEO_STATIONS/index.html",sep=""),fol)}
if (!file.exists(dircss)) {dir.create(dircss)}
if (!file.exists(dircss)) {file.copy(paste(racine,"/TEMPLATE/GEO_STATIONS/CSS/",sep=""),dircss)}

if (!file.exists(dircss)) {dir.create(dircss)}
srccss<-paste(racine,"/TEMPLATE/GEO_STATIONS/CSS/",sep="")
file.names <- dir(srccss)
sapply(file.names, function(x) {
                      file.copy(from=paste(srccss, x, sep=''),
                               to=paste(dircss, x, sep=''),
                              overwrite = FALSE) })


if (file.exists(fjson)) {file.remove(fjson)}
writeOGR(PCHGEO, fjson, "stations_pch.GeoJSON", driver="GeoJSON")


PCHnonGEO<-RLT_PCHFINALEXCEPT[!(RLT_PCHFINALEXCEPT$STATION %in% STATIONGEOPOINTWGS84_2$CDSTATIONM),]


####ouvrir la carto


### 
