# load library
library(dplyr)
library(raster)
library(ggplot2)
library(readxl)

######## Geolocalized Economic Data ##########

path = "Data"
url = "http://gecon.yale.edu/sites/default/files/files/Gecon40_post_final.xls"
filename = "Gecon40_post_final.xls"
destfile = paste(getwd(),
                 path,
                 filename,
                 sep = "/")

# download zip file
if(!file.exists(destfile)){
    download.file(url = url,
                  destfile = destfile)
}

# read the file
gEcon = read_excel("Data/Gecon40_post_final.xls",sheet = 1)


# Countries with G-Econ == NA in 2005
countries_NA2005 = gEcon %>%
    dplyr::filter(is.na(PPP2005_40)) %>%
    dplyr::select(COUNTRY) %>%
    distinct()


PPP = gEcon %>%
    dplyr::select(LAT, LONGITUDE, PPP2005_40, COUNTRY, PPP1990_40) %>%
    # replace G-Econ for 1990 if G-Econ value for 2005 is NA
    mutate(PPP2005_40 = if_else(COUNTRY %in% countries_NA2005$COUNTRY, PPP1990_40, PPP2005_40)) %>%
    mutate(PPP_tranformed = log(PPP2005_40*exp(0.47))) %>%
    mutate(risk_exposure = case_when(PPP_tranformed < 1.97 ~ 1,
                                     PPP_tranformed > 4.911 ~ 0,
                                     PPP_tranformed > 1.97 & PPP_tranformed < 4.911 ~ (1.67-(0.34*PPP_tranformed)))) %>%
    dplyr::select("x" = LONGITUDE,
                  "y" = LAT,
                  #"country" = COUNTRY,
                  "R_se" = risk_exposure)


# change it to raster format
RPP = rasterFromXYZ(PPP)

CEDA_resolution = raster(nrow = 360, ncol = 720,
                         xmn = -180, xmx = 180,
                         ymn = -90, ymx = 90)

RPP_CEDA = resample(RPP,
                    CEDA_resolution,
                    method = "bilinear")

#convert to dataframe
Rse = as.data.frame(RPP_CEDA, xy = TRUE)%>%
    rename("Longitude" = x,
           "Latitude" = y)

# save files 
save(Rse,
     file = "processedData/RiskOfExposure.rda")



# Create the ggplot object and specify the aesthetics and geometry
g = ggplot(Rse,
           aes(x = Longitude,
               y = Latitude,
               color = R_se)) +
    geom_point() +
    scale_color_viridis(option = "magma", direction = -1) +
    labs(title = "Risk of Exposure (0.50 x 0.50) Resolution",
         x = "Longitude",
         y = "Latitude")

ggsave("Figures/RiskOfExposure.jpg",
       g,
       height=4,width=8,scale=1.65)
