######## Global Aedes Distribution ##########
# Uncertainty estimates for mosquito distribution at 5 km x 5 km resolution
library(raster)
library(ggplot2)
path = "Data"
url = "https://www.dropbox.com/sh/bpxcmzmmpiiav8u/AAAl3CBKnBYwXb0n1s1C4-K-a?dl=0&preview=aegypti.tif"
filename = "aegypti.tif"
destfile = paste(getwd(),
                 path,
                 filename,
                 sep = "/")

# download zip file
if(!file.exists(destfile)){
    download.file(url = url,
                  destfile = destfile)
}



R = raster(paste(path, filename, sep = "/"))
 


CEDA_resolution = raster(nrow = 360, ncol = 720,
                         xmn = -180, xmx = 180,
                         ymn = -90, ymx = 90)


RPP_CEDA = resample(R,
                    CEDA_resolution,
                    method = "bilinear")

aegypti = as.data.frame(RPP_CEDA, xy = TRUE) %>%
    rename("Longitude" = x,
           "Latitude" = y,
           "P_ae" = aegypti)

    
# save list of files for each month
save(aegypti,
     file = "processedData/aegypti.rda")




# Create the ggplot object and specify the aesthetics and geometry
P_ae_map = ggplot(aegypti,
           aes(x = Longitude,
               y = Latitude,
               color = P_ae)) +
    geom_point() +
    scale_color_viridis(option = "magma", direction = -1) +
    labs(title = "Global Aedes Distribution (0.50 x 0.50) Resolution",
         x = "Longitude",
         y = "Latitude")

ggsave("Figures/global_Aedes_distribution.jpg",
       P_ae_map,
       height=4,width=8,scale=1.65)


