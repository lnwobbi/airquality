library(gstat)
library(sp)
library(spacetime)
library(raster)
library(rgdal)
library(rgeos)
library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(raster)
library(readxl)
library(fields)
library(dismo)

if (!require("rspatial")) devtools::install_github('rspatial/rspatial')


AQY_AA_199_SG_CITY_OFFICE <- read_excel("~/CEHAT Clinic/AQY AA-199 SG CITY OFFICE.xlsx", skip = 5)
AQY_AA_200_SG_PARK <- read_excel("~/CEHAT Clinic/AQY AA-200 SG PARK.xlsx", skip = 5)
AQY_AA_202_SG_CITY_HALL <- read_excel("~/CEHAT Clinic/AQY AA-202 SG CITY HALL.xlsx", skip = 5)
AQY_AA_204_HOLLYDALE_PARK <- read_excel("~/CEHAT Clinic/AQY AA-204 HOLLYDALE PARK.xlsx", skip = 5)
AQY_AA_205_SOUTHEAST_SCHOOLS <- read_excel("~/CEHAT Clinic/AQY AA-205 SOUTHEAST SCHOOLS.xlsx", skip = 5)

AQY_AA_199_SG_CITY_OFFICE$Time <- as.POSIXlt(AQY_AA_199_SG_CITY_OFFICE$Time)
AQY_AA_200_SG_PARK$Time <- as.POSIXlt(AQY_AA_200_SG_PARK$Time)
AQY_AA_202_SG_CITY_HALL$Time <- as.POSIXlt(AQY_AA_202_SG_CITY_HALL$Time)
AQY_AA_204_HOLLYDALE_PARK$Time <- as.POSIXlt(AQY_AA_204_HOLLYDALE_PARK$Time)
AQY_AA_205_SOUTHEAST_SCHOOLS$Time <- as.POSIXlt(AQY_AA_205_SOUTHEAST_SCHOOLS$Time)


office <- data.frame(AQY_AA_199_SG_CITY_OFFICE[,c(1,4,7:8)])
park <- data.frame(AQY_AA_200_SG_PARK[,c(1,4,7:8)])
cityhall <- data.frame(AQY_AA_202_SG_CITY_HALL[,c(1,4,7:8)])
hollydale <- data.frame(AQY_AA_204_HOLLYDALE_PARK[,c(1,4,7:8)])
schools <- data.frame(AQY_AA_205_SOUTHEAST_SCHOOLS[,c(1,4,7:8)])

office_coord <- c(AQY_AA_199_SG_CITY_OFFICE$`LONG (°)`[1],AQY_AA_199_SG_CITY_OFFICE$`LAT (°)`[1])
park_coord <- c(AQY_AA_200_SG_PARK$`LONG (°)`[1],AQY_AA_200_SG_PARK$`LAT (°)`[1])
cityhall_coord <- c(AQY_AA_202_SG_CITY_HALL$`LONG (°)`[1],AQY_AA_202_SG_CITY_HALL$`LAT (°)`[1])
hollydale_coord <- c(AQY_AA_204_HOLLYDALE_PARK$`LONG (°)`[1],AQY_AA_204_HOLLYDALE_PARK$`LAT (°)`[1])
schools_coord <- c(AQY_AA_205_SOUTHEAST_SCHOOLS$`LONG (°)`[1],AQY_AA_205_SOUTHEAST_SCHOOLS$`LAT (°)`[1])

#assume that the sensor is not moved when the values of NA for long/lat are being read so that we can fill in missing coords
office$LAT.... <- office_coord[1]
office$LONG.... <- office_coord[2]
park$LAT.... <- park_coord[1]
park$LONG.... <- park_coord[2]
cityhall$LAT.... <- cityhall_coord[1]
cityhall$LONG.... <- cityhall_coord[2]
hollydale$LAT.... <- hollydale_coord[1]
hollydale$LONG.... <- hollydale_coord[2]
schools$LAT.... <- schools_coord[1]
schools$LONG.... <- schools_coord[2]


fullData <- bind_rows(office,park,cityhall,hollydale,schools)

fullData <- fullData[order(fullData$Time),]
colnames(fullData) <- c("Time", "PM2.5", "DP", "LAT", "LONG")
fullData <- fullData[,c(1:2,4:5)]

chunk <- subset(fullData, Time >= "2019-11-23 00:00:00 UTC" & Time <= "2019-11-30 23:00:00 UTC")


#Create a SpatialPointsDataFrame
coordinates(chunk)=~ LONG + LAT
projection(chunk)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#Transform into Mercator Projection
chunk.UTM <- spTransform(chunk,CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) 

chunkSP <- SpatialPoints(chunk.UTM@coords,CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

chunkDF <- data.frame(PM2.5=chunk.UTM$`PM2.5 (µg.m³)`) 

chunkTM <- as.POSIXct(chunk.UTM$Time,tz="UTC") 

timeDF <- STIDF(chunkSP,chunkTM,data=chunkDF) 



#dataset is too large
stplot(timeDF) 

#variogram portion
var <- variogramST(PM2.5~1,data=timeDF,tunit="hours",assumeRegular=F,na.omit=T) 
#different visualizations
plot(var,map=F) 
plot(var,wireframe=T) 

pars.l <- c(sill.s = 0, range.s = 40, nugget.s = 0,sill.t = 0, range.t = 1, nugget.t = 0,sill.st = 0, range.st = 10, nugget.st = 0, anis = 0)
pars.u <- c(sill.s = 200, range.s = 100, nugget.s = 100,sill.t = 10, range.t = 60, nugget.t = 100,sill.st = 200, range.st = 1000, nugget.st = 100,anis = 700) 

separable <- vgmST("separable", space = vgm(-60,"Sph", 50, 1),time = vgm(35,"Sph", 500, 1), sill=1) 


plot(var,separable,map=F) 
separableVgm <- fit.StVariogram(var, separable, fit.method=11,method="L-BFGS-B", stAni=5, lower=pars.l,upper=pars.u)
SimplesumMetric <- vgmST("simpleSumMetric",space = vgm(5,"Sph", 50, 0),time = vgm(15,"Sph", 50, 0), joint = vgm(1,"Sph", 500, 0), nugget=1, stAni=500) 
SimplesumMetricVgm <- fit.StVariogram(var, SimplesumMetric,method = "L-BFGS-B",lower=pars.l)
attr(SimplesumMetricVgm, "MSE")



#bringing in map of South Gate as a grid. This will be our estimation grid.
proj <-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

cities <- places(state = "CA", cb = TRUE, year=2019)
sg <- filter(cities, NAME == "South Gate")
ggplot(sg) +
  geom_sf(fill=NA,color = "black")

sg.city <- as(sg, "Spatial")
sg.city <- spTransform(sg.city, CRSobj = proj)
plot(sg.city)

long.range <- as.numeric(range(sg.city@bbox[1,]))
lat.range <- as.numeric(range(sg.city@bbox[2,]))

test.grid <- expand.grid(x = seq(from = long.range[1], to = long.range[2], by = 5.016533e-05), y = seq(from = lat.range[1], to = lat.range[2], by = 3.750733e-05))

sg.grid <- SpatialPoints(test.grid, proj4string = CRS(proj4string(sg.city)))

sg.grid <- SpatialPixels(sg.grid[sg.city,])
plot(sg.grid)

sg.R <- raster(sg)
res(sg.R) <- .1  # .5 km if your CRS's units are in km
g <- as(sg.R, 'SpatialGrid')
g@proj4string <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


tm.grid <- seq(as.POSIXct('2019-08-27 UTC'),as.POSIXct('2020-08-06 22:00:00 UTC'),length.out=5) 
grid.ST <- STF(sg.grid,tm.grid)

pred <- krigeST(PM2.5~1, data=timeDF, modelList=SimplesumMetricVgm, newdata=grid.ST) 

stplot(pred) 




###
# Exploring other interpolation methods with a slice of data from 1PM on Christmas Eve, 2019
###
slice <- subset(fullData, Time == "2019-12-24 08:00:00 UTC")

coordinates(slice)=~ LONG + LAT
projection(slice) <- CRS('+proj=longlat +datum=NAD83')
TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=km +ellps=GRS80")
library(rgdal)
sliceSP <- spTransform(slice, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

longlat <- data.frame(slice[,c(3,4)])
sliceSP <- SpatialPointsDataFrame( data = slice, coords = longlat,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


#gs <- gstat(formula=log(PM2.5)~1, data =sliceSP@data)
v <- variogram(PM2.5~1, sliceSP@coords, sliceSP, cloud=T, cressie=F, covariogram=F, cutoff = 20)
head(v)
#np is the number of point pairs
#since we are using cloud, we get the value of 1 datum in the pair (left) and the other (right)
plot(v)
fve <- fit.variogram(v, vgm(3, "Exp", 2.75, .05))
fve
fvs <- fit.variogram(v, vgm(3, "Sph", 2.75, .05))
fvs
plot(variogramLine(fve, 5), type='l', ylim=c(0,14))
points(v[,2:3], pch=20, col='red')

k <- gstat(formula=PM2.5~1, model=fve, data = sliceSP)
# predicted values
kp <- predict(k, sg.grid)
## [using ordinary kriging]
spplot(kp)

ok <- brick(kp)
ok <- mask(ok, sg.grid)
names(ok) <- c('PM2.5_concentraton_prediction', 'variance')
plot(ok$PM2.5_concentraton_prediction)

#change color to color-blind friendlyness
pal <- colorRampPalette(c("chartreuse1", "blue4"))

plot(ok, col = pal)


#Making a Thin Plate Spline surface to interpolate data

thinPlate <- Tps(sliceSP@coords, slice$`PM2.5`)
sgRaster <- raster(sg)

tps <- interpolate(sgRaster, thinPlate)
tps <- mask(tps, sg.city)
plot(tps, col = pal(13))

#disaggregate the resolution (make it finer) [SN: if you change factor to 10, the graph looks really cool]
sgRasterFine <- disaggregate(sgRaster, fact=10)
tpsFine <- interpolate(sgRasterFine, thinPlate)
tpsFine <- mask(tpsFine, sg.city)
plot(tpsFine, col = pal(13))
legend("bottomleft", legend = "PM2.5 (µg.m³)")
points(slice[,c(3,4)], cex=2, pch=20, col='red')

 



#### LEAVE ONE OUT ANALYSIS SPLINE
slicePartial <- slice[c(1:4),] # leave out the schools

longlatPartial <- data.frame(slicePartial[,c(3,4)])
coordinates(slicePartial)=~ LONG + LAT
slicePartialSP <- SpatialPointsDataFrame( data = slicePartial@data, coords = longlatPartial,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

leaveOutSchools <- Tps(slicePartialSP@coords, slicePartial$`PM2.5`)

tps.out <- as(tpsFine, "SpatialPixelsDataFrame")
tps.out@data$LONG <- tps.out@coords[,1]
tps.out@data$LAT <- tps.out@coords[,2]

# Leave One Out Cross Validation
library(Ecdat)
library(boot)
library(caret)

glm.fit <- glm(tps.out$layer ~ tps.out$LONG + tps.out$LAT, data = tps.out@data)
summary(glm.fit)
cv.error = cv.glm(tps.out@data, glm.fit)

#round(cv.error$delta,2)

cv.error = rep(0,5)

for (i in 1:5) {
  glm.fit = glm(tps.out$layer ~ poly(tps.out$LONG, i) + poly(tps.out$LAT,i),
                data=tps.out@data)
  cv.error[i] = cv.glm(tps.out@data, glm.fit)$delta[1]
}

cv.error


#Inverse Distance Weighting
idm <- gstat(formula=PM2.5~1, data = sliceSP)
## [inverse distance weighted interpolation]
sgRasterProj <- sgRaster
crs(sgRasterProj) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
sgRasterProj <- disaggregate(sgRasterProj, fact=15)

idp <- interpolate(sgRasterProj, idm)
idp <- mask(idp, sg.city)
par(mfrow = c(1, 1), mar = c(3.5, 3.5, .5, .5))
plot(idp, col = pal(13))
legend("bottomleft", legend = "PM2.5 (µg.m³)")
points(slice[,c(3,4)], cex=2, pch=20, col='red')



par(mfrow = c(3, 1), mar = c(3.5, 3.5, .5, .5))
plot(ok$prediction, col = pal(13))
legend("bottomleft", legend = "PM2.5 (µg.m³)")

plot(idp, col = pal(13))
legend("bottomleft", legend = "PM2.5 (µg.m³)")
points(slice[,c(3,4)], cex=2, pch=20, col='red')
plot(tpsFine, col = pal(13))


RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}
f1 <- function(x, test, train) {
  nmx <- x[1]
  idp <- x[2]
  if (nmx < 1) return(Inf)
  if (idp < .001) return(Inf)
  m <- gstat(formula=PM2.5~1, data= sliceSP, locations=train, nmax=nmx, set=list(idp=idp))
  p <- predict(m, newdata=test, debug.level=0)$var1.pred
  RMSE(test$PM2.5, p)
}
set.seed(20150518)
i <- sample(nrow(slice), 0.2 * nrow(slice))
tst <- slice[i,]
tstLL <- data.frame(tst[,c(3,4)])
coordinates(tst)=~ LONG + LAT
tst <- SpatialPointsDataFrame( data = tst@data, coords = tstLL,
                                proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
trn <- slice[-i,]
trnLL <- data.frame(trn[,c(3,4)])
coordinates(trn)=~ LONG + LAT
trn <- SpatialPointsDataFrame( data = trn@data, coords = trnLL,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

opt <- optim(c(8, .5), f1, test=tst, train=trn)
opt



library(dismo)
nfolds <- 5
k <- kfold(slice, nfolds)
ensrmse <- krigrmse <- idwrmse <- rep(NA, 5)
for (i in 1:nfolds) {
  test <- slice[k!=i,]
  testLL <- data.frame(test[,c(3,4)])
  coordinates(test)=~ LONG + LAT
  test <- SpatialPointsDataFrame( data = test@data, coords = testLL,
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  train <- slice[k==i,]
  trainLL <- data.frame(train[,c(3,4)])
  train <- SpatialPointsDataFrame( data = train, coords = trainLL,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  m <- gstat(formula=PM2.5~1, locations=train, nmax=opt$par[1], set=list(idp=opt$par[2]))
  p1 <- predict(m, newdata=test, debug.level=0)$var1.pred
  idwrmse[i] <-  RMSE(test$PM2.5, p1)
  m <- gstat(formula=PM2.5~1, locations=train, model=fve)
  p2 <- predict(m, newdata=test, debug.level=0)$var1.pred
  krigrmse[i] <-  RMSE(test$PM2.5, p2)
  #m <- Tps(coordinates(train), train$PM2.5)
  #p3 <- predict(m, coordinates(test))
  #tpsrmse[i] <-  RMSE(test$PM2.5, p3)
  w <- c(idwrmse[i], krigrmse[i])
  weights <- w / sum(w)
  ensemble <- p1 * weights[1] + p2 * weights[2] 
  ensrmse[i] <-  RMSE(test$PM2.5, ensemble)
}
rmi <- mean(idwrmse)
rmk <- mean(krigrmse)
#rmt <- mean(tpsrmse)
rms <- c(rmi, rmk)
rms

rme <- mean(ensrmse)
rme

okResample <- projectRaster(ok[[1]],idp,method = 'ngb')

weights <- ( rms / sum(rms) )
s <- stack(idp, okResample)
ensemble <- sum(s * weights)

s <- stack(idp,  okResample, ensemble)
names(s) <- c('IDW', 'OK', 'Ensemble')
plot(s, col = pal(15))


kf <- kfold(nrow(slice))

rmsenn <- rep(NA, 5)
for (k in 1:5) {
  test <- sliceSP@data[kf == k, ]
  testLL <- data.frame(test[,c(3,4)])
  coordinates(test)=~ LONG + LAT
  test <- SpatialPointsDataFrame( data = test@data, coords = testLL,
                                            proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  train <- sliceSP@data[kf != k, ]
  trainLL <- data.frame(train[,c(3,4)])
  coordinates(train)=~ LONG + LAT
  train <- SpatialPointsDataFrame( data = train@data, coords = trainLL,
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  #gscv <- gstat(formula=PM2.5~1, locations=train, nmax=5, set=list(idp = 0))
  #p <- predict(gscv, test)$var1.pred
  #rmsenn[k] <- RMSE(test$PM2.5, p)
  
  m <- Tps(coordinates(train), train$PM2.5)
  p <- predict(m, coordinates(test))
  tpsrmse[k] <-  RMSE(test$PM2.5, p)
}
tpsrmse
