#######################################################################
# 
#  FISH 627: Statistical Computing with R
#    Data import example: Importing netCDF data. Data are from the NOAA
#             extended reconstructed sea-surface temperature data set
#             (ERSST version 4)
#
#######################################################################

library(spam)
library(ncdf4)
library(rnoaa)
library(maps)
library(mapdata)
library(fields)

# Data source for SST data at NOAA's ESRL site:
# http://www.esrl.noaa.gov/psd/cgi-bin/db_search/DBSearch.pl?Dataset=NOAA+Extended+Reconstructed+SST+V4&group=0&submit=Search
# Data request 9/22/16. Subset of monthly means over the following region:
# 49.00N to 65.00N, 179.00E to 225.00E
# Time frame: January 1950 - Aug 2016

########################################################################
# I downloaded and saved the file as "netCDF example NE Pacific.nc" 
# in my 'Data' folder

## 1. Open the file and store as an R object:
filename <- "Data/netCDF.nc"
sst.nc <- nc_open(filename) 
getwd()
setwd("C:/Users/Danielle/Desktop/StatcompR")

sst.nc  # Some basic info about the netCDF file
names(sst.nc)  # Some of the contents of the cdf
str(sst.nc)    # Fairly complex list of lists of lists of stuff
# For example, the units are stored in:
sst.nc$var$sst$units

## 2. Extract information (latitude, Longitude, Date/Time, and SST)
(lat <- ncvar_get(sst.nc, varid="lat"))
(lon <- ncvar_get(sst.nc, varid="lon"))
(time <- ncvar_get(sst.nc, varid="time"))

sst <- ncvar_get(sst.nc, varid="sst")

## 3. Explore structure of temperature data:
str(sst)    # 3-D array of SST values: 23 longitudes, 
            #     8 latitudes, 800 monthly means
sst[,,800]    # matrix of values for first month in the time series

## 4. Temporal structure of the data

# We'll first extract dates relative to the origin. Finding out what the origin is
# takes a little digging because of the complex strucutre, but after looking at:
str(sst.nc) 
# you can find the info in one of the list of lists:
sst.nc$var$sst$dim[[3]]$units  # Units are in dates since Jan 1, 1800

time_d <- as.Date(time, format="%j",origin=as.Date("1800-01-01"))
time_years <- format(time_d, "%Y")
time_months <- format(time_d, "%m")
time_YearMonth <- format(time_d, "%Y-%m")

# Often, netCDF files have missing values (for example for locations over land if
# you are downloading oceanographic data). The missing value designator is specific
# to the dataset, in this case it is: 
sst.nc$var$sst$missval    
# We can check if there are any missing values
any(sst == sst.nc$var$sst$missval) # This results in 'NA', which means 
                                   # there are NAs in the array
any(sst == sst.nc$var$sst$missval, na.rm=T) # There are no missing values with the
                                            # with the missing value designator
# It looks like the missing values are converted to NAs when importing the data
# into R (apparently a new feature in 'ncdf4') 

## 5. Check the data to make sure it is what you expect
# Check units. Again, this takes some digging (or you can look at online documentation):
nc$var$sst$units
# Look at distribution of values:
hist(sst) 
# The distribution looks reasonanble. All the really cold values (-1.8 degree C)
# are for regions that are ice-covered 

# 6. Plot the data:
dim(sst) # 800 SST values over a 23x8 grid gives dimensions 
n.lat <- length(lat)  # Number of latitudinal bands in the grid, which we use below
# Plot the results, converting longitude to degrees East (for adding a map) and 
# reversing latitudes, which need to be in ascending order for 'image.plot'. 
# 'image.plot' takes a vector of x-values (Longitude), a vector of y-values (Latitude),
# and a matrix of values to plot at each x/y combination (SST in this case):
# As an example, I selected the most recent point in time only (August 2016):
image.plot(-(360-lon), rev(lat), sst[, n.lat:1, 800], col = tim.colors(), #rev(lat) and longitude becasue needs to be in increasing order 
           xlab = "Longitude (W)", ylab = "Latitude (N)") #mapping one month of data
map("worldHires", add=T, xlim=c(-181, -135), ylim=c(49, 65), fill=T) #add=T adds coastline
?map
#map(database="world",fill=T)
#map('state', fill = TRUE, col = palette())
#image.plt(lat,long,time) in sst array subsetting by logical vectors 
# We can plot the long-term mean for the month of May by first averaging
# each grid point over all May values in the array:
# This uses the 'apply' function, which we will cover soon:
May.mean <- apply(sst[,,time_months == "05"], c(1,2), mean)
image.plot(-(360-lon), rev(lat), sst[, n.lat:1, 800], col = tim.colors(), 
           xlab = "Longitude (W)", ylab = "Latitude (N)")
map("worldHires", add=T, xlim=c(-181, -135), ylim=c(49, 65), fill=T)
title("May climatology")

## 7. Finally, we'll generate an animation of monthly means
# for one year of SST data as an HTML file:
saveHTML({
  ZLIM <- range(sst[,,1:12], na.rm=T)  # for fixing range of SST across plots
  dev.control("enable")  # enable recording
  for (i in 1:12) {
    image.plot(-(360-lon), rev(lat), sst[,n.lat:1,i],
               col = c(tim.colors(),"black"), main = time_YearMonth[i],
               xlab = "Longitude (W)", ylab = "Latitude (N)", zlim=ZLIM)
    map("worldHires", add=T, xlim=c(-181, -135), ylim=c(49, 65), fill=T)   
    ani.record(reset = TRUE, replay.cur = TRUE)  # record the current frame
  }
}, htmlfile = "monthlySST.html", ani.width = 720, ani.height = 480)


##########################################################################
# The rnoaa library facilitates reading a number of NOAA climate datasets
# into r and processsing / plotting the data directly from the web sources.
# See '?rnoaa' for basic description and the types of data available

# For example, to extract global ERSST data for a single year / month:
# (Data will be imported as ncdf4 object)
# YOu need to be online for this to work!
sst.2015.05.nc <- ersst(year = 2015, month = 5)

sst.2015.05 <- ncvar_get(sst.2015.05.nc, varid="sst")
(lat2 <- ncvar_get(sst.2015.05.nc, varid="lat"))
(lon2 <- ncvar_get(sst.2015.05.nc, varid="lon")) 

image.plot(lon2, lat2, sst.2015.05,
           col = c(tim.colors(),"black"),
           xlab = "Longitude (E)", ylab = "Latitude (N)")
map("world2", add=T, fill=T)



