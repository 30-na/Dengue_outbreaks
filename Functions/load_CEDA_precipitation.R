
# load libraries
library(ncdf4)
library(dplyr)
library(reshape2)
library(ggplot2)
library(viridis)

#open netCDF files precipitation
nc_precip = nc_open("Data/cru_ts4.06.1901.2021.pre.dat.nc")

# get the variable names
names(nc_precip$var)
names(nc_precip$dim)

# set the variables names
x = "lon"
y = "lat"
t = "time"

# get longitude, latitude and time
lon <- sort(ncvar_get(nc_precip,x))
lat <- sort(ncvar_get(nc_precip,y))
time <- ncvar_get(nc_precip,t)

# Convert numeric time values to date format
date <- as.Date("1900-01-01") + (time - 1)

# get precipitation
precip_array <- ncvar_get(nc_precip, "pre")
dim(precip_array)
    
# extract some information from file 
fullname <- ncatt_get(nc_precip,"pre","long_name")
dunits <- ncatt_get(nc_precip,"pre","units")
title <- ncatt_get(nc_precip,0,"title")
institution <- ncatt_get(nc_precip,0,"institution")
datasource <- ncatt_get(nc_precip,0,"source")
references <- ncatt_get(nc_precip,0,"references")
history <- ncatt_get(nc_precip,0,"history")
Conventions <- ncatt_get(nc_precip,0,"Conventions")

# replace netCDF fill values with NA's
fillvalue <- ncatt_get(nc_precip,"pre","_FillValue")
precip_array[precip_array==fillvalue$value] <- NA

# close the file
nc_close(nc_precip)

# Reshape the array to a long format
long_precip <- melt(precip_array)

# Rename the columns
colnames(long_precip) <- c("Longitude", "Latitude", "Date", "Precipitation")

# Create vectors for standard latitude and longitude ranges
standard_latitude <- seq(-89.75, 89.75, length.out = 360)
standard_longitude <- seq(-179.75, 179.75, length.out = 720)

# add date and standardize the cordination
precip_data = long_precip %>%
    mutate(Date = rep(date, each = (dim(precip_array)[1] * dim(precip_array)[2])),
           Latitude = standard_latitude[Latitude],
           Longitude = standard_longitude[Longitude]
    )


# save file as RDA
save(precip_data,
     file = "processedData/CEDA_precip_data.rda")


# Create the ggplot object and specify the aesthetics and geometry
g = ggplot(precip_data %>% filter(Date == "2005-06-15"),
           aes(x = Longitude,
               y = Latitude,
               color = Precipitation)) +
    geom_point() +
    scale_color_viridis(option = "magma", direction = -1) +
    labs(title = "Random Precipitation Sample from CEDA Dataset (2005-06-15)",
         x = "Longitude",
         y = "Latitude")

ggsave("Figures/CEDA_sample_precip.jpg",
       g,
       height=4,width=8,scale=1.65)
