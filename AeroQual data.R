#kriging test for Sept. 18, 9 AM
library(sp)
library(gstat)
suppressPackageStartupMessages({
        library(dplyr) # for "glimpse"
        library(ggplot2)
        library(scales) # for "comma"
        library(magrittr)
        library(geoR)
        library(gdata)
        library(gridExtra)
        library(lattice)
        #for the mapping
        library(tidyverse)
        library(tidycensus)
        library(sf)
        library(tigris)
        #you need to let R know to bring in the spatial data as sf objects
        options(tigris_class = "sf")
        library(tmap)
        library(raster)
        library(maptools)
        library(viridis)
})

X9_18_2019_9_00am_Aeroqual_data_2_ <- read_excel("C:/Users/jimzi/Downloads/9_18_2019 9_00am Aeroqual data (2).xlsx")
test <- data.frame(X9_18_2019_9_00am_Aeroqual_data_2_)
### Get long and lat from your data.frame. Make sure that the order is in lon/lat.
xy <- data.frame(test[,c(7,8)])
spdf <- SpatialPointsDataFrame(coords = xy, data = test,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

proj <-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
Longitude <- spdf@coords[,c(1)]
Latitude <- spdf@coords[,c(2)]

sensorLocations <- c("City Office", "Hollydale Park","South Gate Park", "Schools", "City Hall")

#plot with spatial point values for each sensor; points are diff sizes relative to their vconcentration alue
ggplot(spdf@data[,c(3,7,8)], aes(Longitude, Latitude)) + 
        geom_point(aes(size = `cPM2.5..µg.m³.`,color=`cPM2.5..µg.m³.`)) + 
        scale_color_gradient(low='green', high='red') + 
        guides(size=FALSE) +
        geom_text(aes(label=sensorLocations), check_overlap = T, show.legend = F, size = 3, vjust = 2)

#points we have measurements for
plot1 <- spdf@data[,c(3,7,8)] %>% as.data.frame %>%
        ggplot(aes(Longitude, Latitude), inherit.aes = FALSE) + geom_point(size=1, aes(Longitude, Latitude), inherit.aes = FALSE) + coord_equal() + 
        ggtitle("Points with measurements") + theme(
                plot.title = element_text(size=14, face="bold.italic"))

# points we want to estimate
plot2 <- sg.grid %>% as.data.frame %>%
        ggplot(aes(x, y), inherit.aes = FALSE) + geom_point(size=.5, aes(x, y), inherit.aes = FALSE) + coord_equal() + 
        ggtitle("Points at which to estimate") + xlab("Longitude") + ylab("Latitude") + theme(
                plot.title = element_text(size=14, face="bold.italic"))
#plot them
grid.arrange(plot1, plot2, ncol = 2)

#we get better results with the kriging setup in the geoR package

#geoR variogram for our points; the points are not very closely correlated
#classical is good for when you are working with bins
testVariogramGeoR <- variog(spdf, coords = spdf@coords, data = spdf$`cPM2.5..µg.m³.`, 
       uvec = "default", breaks = "default",
       trend = "cte", lambda = 1,
       option = "cloud",
       estimator.type = "classical", 
       bin.cloud = FALSE, direction = "omnidirectional", tolerance = pi/8,
       
       unit.angle = c("radians","degrees"), angles = FALSE)

#create a range of possible sill, nugget and range values
init.cov <- expand.grid(seq(10,.25,l=10), seq(0,.02,l=5))
#fit a line to the variogram (is the selected model the best fit? do some analysis to test)
geoFit <- variofit(testVariogramGeoR, cov.model="exponential",
                   ini.cov.pars=init.cov,
                   fix.nugget=FALSE, nugget=0)

plot(testVariogramGeoR, main = "Variogram: cPM2.5 (µg/m³)")
lines(geoFit)
grid()

#more success with the variogram in the geoR package, using the modulus estimator and the cloud option (not bin or smooth)
# cloud option means that all (n(n-1)/2) points are shown, there are no bins. Since we're only using 5 data entries, this is what we want

#plot(aero.vgm, aero.fit) # plot the sample values, along with the fit model

#bringing in map of South Gate as a grid. This will be our estimation grid.
cities <- places(state = "CA", cb = TRUE, year=2019)
sg <- filter(cities, NAME == "South Gate")
ggplot(sg) +
  geom_sf(fill=NA,color = "black")

sg.city <- as(sg, "Spatial")
sg.city <- spTransform(sg.city, CRSobj = proj)
plot(sg.city)

long.range <- as.numeric(range(sg.city@bbox[1,]))
lat.range <- as.numeric(range(sg.city@bbox[2,]))

test.grid <- expand.grid(x = seq(from = long.range[1], to = long.range[2], by = 0.0005), y = seq(from = lat.range[1], to = lat.range[2], by = 0.0005))

sg.grid <- SpatialPoints(test.grid, proj4string = CRS(proj4string(sg.city)))

sg.grid <- SpatialPixels(sg.grid[sg.city,])
plot(sg.grid)
        
#Simple Kriging interpolation with the gstat package
testKrige.gstat <- krige(spdf$`cPM2.5..µg.m³.`~1,spdf,sg.grid)
testKrige.gstat$direct <- testKrige.gstat$var1.pred

testKrige.gstat$log <- exp(krige(log(spdf$`cPM2.5..µg.m³.`)~1,spdf,sg.grid)$var1.pred)

spplot(testKrige.gstat[c("direct","log")],colorkey=TRUE, contour = FALSE,  col.regions = rev(get_col_regions()))


testKrigeDF <- data.frame(testKrige.gstat)
testKrige.gstat %>% as.data.frame %>%
        ggplot() + geom_tile(aes(x,y,fill=var1.pred)) + coord_equal() +
        scale_fill_continuous(type = "viridis") +
        scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
        theme_bw() 

krigePlot <- ggplot() + geom_tile(testKrigeDF, mapping = aes(x,y,fill=var1.pred)) + 
  geom_point(spdf@data[,c(3,7,8)], color ="red", size=2 , mapping =aes(Longitude, Latitude), inherit.aes = TRUE) + 
  coord_equal() +
  scale_fill_continuous(type = "viridis") +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  theme_bw() + 
  ggtitle("Simple Kriging PM2.5 Predictions")

krigePlot




###########################################
#BASIC PLOTTING STUFF
########################################### 

#convert time to a workable object class
times1 <-as.POSIXlt(AQY_AA_199_SG_CITY_OFFICE$Time)
times2 <-as.POSIXlt(AQY_AA_200_SG_PARK$Time)
times3 <-as.POSIXlt(AQY_AA_202_SG_CITY_HALL$Time)
times4 <-as.POSIXlt(AQY_AA_204_HOLLYDALE_PARK$Time)
times5 <-as.POSIXlt(AQY_AA_205_SOUTHEAST_SCHOOLS$Time)

AQY_AA_199_SG_CITY_OFFICE$Date <- as.Date(AQY_AA_199_SG_CITY_OFFICE$Time,"%Y/%m/%d")
AQY_AA_200_SG_PARK$Date <- as.Date(AQY_AA_200_SG_PARK$Time,"%Y/%m/%d")
AQY_AA_202_SG_CITY_HALL$Date <- as.Date(AQY_AA_202_SG_CITY_HALL$Time,"%Y/%m/%d")
AQY_AA_204_HOLLYDALE_PARK$Date <- as.Date(AQY_AA_204_HOLLYDALE_PARK$Time,"%Y/%m/%d")
AQY_AA_205_SOUTHEAST_SCHOOLS$Date <- as.Date(AQY_AA_205_SOUTHEAST_SCHOOLS$Time,"%Y/%m/%d")

AQY_AA_199_SG_CITY_OFFICE$Time = times1
AQY_AA_200_SG_PARK$Time = times2
AQY_AA_202_SG_CITY_HALL$Time = times3
AQY_AA_204_HOLLYDALE_PARK$Time = times4
AQY_AA_205_SOUTHEAST_SCHOOLS$Time = times5

#divisions are: night from midnight to 6AM. morning from 6Am to noon. afternoon from noon to 6PM. evening from 6pm to midnight
morning <- subset(AQY_AA_205_SOUTHEAST_SCHOOLS, AQY_AA_205_SOUTHEAST_SCHOOLS$Time$hour>=5 & AQY_AA_205_SOUTHEAST_SCHOOLS$Time$hour <= 11, select = -c(`LAT (°)`,`LONG (°)`))
afternoon <- subset(AQY_AA_205_SOUTHEAST_SCHOOLS, AQY_AA_205_SOUTHEAST_SCHOOLS$Time$hour>=11 & AQY_AA_205_SOUTHEAST_SCHOOLS$Time$hour <= 17, select = -c(`LAT (°)`,`LONG (°)`))
evening <- subset(AQY_AA_205_SOUTHEAST_SCHOOLS, AQY_AA_205_SOUTHEAST_SCHOOLS$Time$hour>=17 & AQY_AA_205_SOUTHEAST_SCHOOLS$Time$hour <= 23, select = -c(`LAT (°)`,`LONG (°)`))
night <- subset(AQY_AA_205_SOUTHEAST_SCHOOLS, AQY_AA_205_SOUTHEAST_SCHOOLS$Time$hour>=0 & AQY_AA_205_SOUTHEAST_SCHOOLS$Time$hour <= 5, select = -c(`LAT (°)`,`LONG (°)`))

#daily Averages
dailyAvgsOFFICE <- aggregate(AQY_AA_199_SG_CITY_OFFICE$`cPM2.5 (µg/m³)`, list(AQY_AA_199_SG_CITY_OFFICE$Date), mean)
dailyAvgsPARK <- aggregate(AQY_AA_200_SG_PARK$`cPM2.5 (µg/m³)`, list(AQY_AA_200_SG_PARK$Date), mean)
dailyAvgsCITYHALL <- aggregate(AQY_AA_202_SG_CITY_HALL$`cPM2.5 (µg/m³)`, list(AQY_AA_202_SG_CITY_HALL$Date), mean)
dailyAvgsHOLLYDALE <- aggregate(AQY_AA_204_HOLLYDALE_PARK$`cPM2.5 (µg/m³)`, list(AQY_AA_204_HOLLYDALE_PARK$Date), mean)
dailyAvgsSCHOOLS <- aggregate(AQY_AA_205_SOUTHEAST_SCHOOLS$`cPM2.5 (µg/m³)`, list(AQY_AA_205_SOUTHEAST_SCHOOLS$Date), mean)


allAvgs <- cbindX(dailyAvgsOFFICE,dailyAvgsPARK,dailyAvgsCITYHALL,dailyAvgsHOLLYDALE,dailyAvgsSCHOOLS)
allAvgs <- allAvgs[,-c(3,5,7,9)]
#daily Averages plot
colnames(allAvgs)<-c("Date","City Office average","Park average","City Hall average","Hollydale Park average","Middle/High School average")

names(dailyAvgsOFFICE)[names(dailyAvgsOFFICE) == "Group.1"] <- "Date"
names(dailyAvgsOFFICE)[names(dailyAvgsOFFICE)== "x"] <- "PM2.5 (µg/m³) mean"

names(dailyAvgsCITYHALL)[names(dailyAvgsCITYHALL) == "Group.1"] <- "Date"
names(dailyAvgsCITYHALL)[names(dailyAvgsCITYHALL)== "x"] <- "PM2.5 (µg/m³) mean"

names(dailyAvgsPARK)[names(dailyAvgsPARK) == "Group.1"] <- "Date"
names(dailyAvgsPARK)[names(dailyAvgsPARK)== "x"] <- "PM2.5 (µg/m³) mean"

names(dailyAvgsHOLLYDALE)[names(dailyAvgsHOLLYDALE) == "Group.1"] <- "Date"
names(dailyAvgsHOLLYDALE)[names(dailyAvgsHOLLYDALE)== "x"] <- "PM2.5 (µg/m³) mean"

names(dailyAvgsSCHOOLS)[names(dailyAvgsSCHOOLS) == "Group.1"] <- "Date"
names(dailyAvgsSCHOOLS)[names(dailyAvgsSCHOOLS)== "x"] <- "PM2.5 (µg/m³) mean"


plot(dailyAvgs$`cPM2.5 (µg/m³) mean`~as.Date(dailyAvgs$Date,"%Y/%m%/d%"), type="l", 
     xlab="Date", ylab=" (µg/m³)",
     main="Average Daily PM2.5 Concentrations")

plot(allAvgs$`Middle/High School average`~as.Date(allAvgs$Date,"%Y/%m%/d%"), type="l", 
     xlab="Date", ylab=" (µg/m³)",
     main="Average Daily PM2.5 Concentrations",
     lwd = 1.5)
lines(allAvgs$Date, allAvgs$`City Office average`, pch = 18, col = "brown", type = "l", lty = 1, lwd=1.5)
lines(allAvgs$Date, allAvgs$`Hollydale Park average`, pch = 18, col = "green", type = "l", lty = 1, lwd=1.5)
lines(allAvgs$Date, allAvgs$`Park average`, pch = 18, col = "red", type = "l", lty = 1, lwd =1.5)
lines(allAvgs$Date, allAvgs$`City Hall average`, pch = 18, col = "purple", type = "l", lty = 1,lwd=1.5)
abline(h = c(10,20,30,40), lty = 2, col = "grey")
legend("topleft", 
       legend = colnames(allAvgs)[2:6], 
       col = c("brown","red","purple","green","black") ,
       pch = "-", 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))



#add Date column to morning data
morning$Date <- as.Date(morning$Time,"%Y/%m/%d")

#create averages for the morning division
morningAvgs <- aggregate(morning$`cPM2.5 (µg/m³)`, list(morning$Date), mean)

#making morning plots
names(morningAvgs)[names(morningAvgs) == "Group.1"] <- "Date"
names(morningAvgs)[names(morningAvgs)== "x"] <- "cPM2.5 (?g/m?) mean"
plot(morningAvgs$`cPM2.5 (?g/m?) mean`~as.Date(morningAvgs$Date,"%Y/%m%/d%"), type="l", 
     xlab="Date", ylab=" PM2.5 (µg/m³)",
     main="Average PM2.5 Concentrations in the Morning")
grid()

weekAvg <- morningAvgs[c(37:49),c(1:2)]
plot(weekAvg$`cPM2.5 (?g/m?) mean`~as.Date(weekAvg$Date,"%Y/%m%/d%"), type="b", 
     xlab="Date", ylab=" (?g/m?)",
     main="Average PM2.5 Concentrations in the Morning (2-Week)")
grid()









