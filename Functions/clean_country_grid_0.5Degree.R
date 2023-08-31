# load library
library(dplyr)
library(ggplot2)
library(sf)


# Read the data from the CSV file and convert the 'geometry' column to sf object
grid_country <- data.table::fread("processedData/custom_grid_with_country_names.csv")
grid_country_sf <- st_as_sf(grid_country,
                            wkt = "geometry",
                            crs = 4326)



grid_country_clean = grid_country %>%
    dplyr::mutate(
        Longitude = st_coordinates(grid_country_sf$geometry)[,1],
        Latitude = st_coordinates(grid_country_sf$geometry)[,2],
        country = ifelse(country_name == "" | country_name == "Antarctica", NA, country_name)
    ) %>%
    dplyr::select(Longitude,
                  Latitude,
                  country)

save(grid_country_clean,
     file = "processedData/grid_country_clean.rda")