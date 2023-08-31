
library(dplyr)
load("processedData/R0_data.rda")
load("processedData/grid_country_clean.rda")

R0 = R0_data %>% 
    dplyr::rename(
        "Temperature" = t,
        "Precipitation" = r
    )%>%
    dplyr::mutate(
        Year = format(Date, "%Y"),
        Month = format(Date, "%B"),
        Month = factor(Month,
                       levels = c("January", "February", "March", "April", "May", "June",
                                  "July", "August", "September", "October", "November", "December"))
    )


R0MeanYearStat = R0 %>%
    group_by(Year) %>%
    dplyr::summarize(
        mean_temp = mean(Temperature, na.rm = T),
        mean_prec = mean(Precipitation, na.rm = T),
        mean_ber = mean(r0_briere, na.rm = T),
        mean_quad = mean(r0_quadratic, na.rm = T)
    ) 

save(R0MeanYearStat,
     file = "processedData/R0MeanYearStat.rda")




R0MeanMonthStat = R0 %>%
    group_by(Month) %>%
    summarize(
        mean_temp = mean(Temperature, na.rm = T),
        mean_prec = mean(Precipitation, na.rm = T),
        mean_ber = mean(r0_briere, na.rm = T),
        mean_quad = mean(r0_quadratic, na.rm = T)
    )

save(R0MeanMonthStat,
     file = "processedData/R0MeanMonthStat.rda")





R0MaxStatMap = R0 %>%
    group_by(Longitude ,
             Latitude) %>%
    summarize(
        max_temp = max(Temperature, na.rm = T),
        max_prec = max(Precipitation, na.rm = T),
        max_ber = max(r0_briere, na.rm = T),
        max_quad = max(r0_quadratic, na.rm = T)
    ) %>%
    dplyr::left_join(grid_country_clean,
                     by=c("Longitude", "Latitude"))


save(R0MaxStatMap,
     file = "processedData/R0MaxStatMap.rda")





R0MeanStatMap = R0 %>%
    group_by(Longitude ,
             Latitude) %>%
    summarize(
        mean_temp = mean(Temperature, na.rm = T),
        mean_prec = mean(Precipitation, na.rm = T),
        mean_ber = mean(r0_briere, na.rm = T),
        mean_quad = mean(r0_quadratic, na.rm = T)
    ) %>%
    dplyr::left_join(grid_country_clean,
                     by=c("Longitude", "Latitude"))


save(R0MeanStatMap,
     file = "processedData/R0MeanStatMap.rda")





R0MeanYearStatMap = R0 %>%
    group_by(Longitude ,
             Latitude,
             Year) %>%
    summarize(
        mean_temp = mean(Temperature, na.rm = T),
        mean_prec = mean(Precipitation, na.rm = T),
        mean_ber = mean(r0_briere, na.rm = T),
        mean_quad = mean(r0_quadratic, na.rm = T)
    ) %>%
    dplyr::left_join(grid_country_clean,
                     by=c("Longitude", "Latitude"))


save(R0MeanYearStatMap,
     file = "processedData/R0MeanYearStatMap.rda")





R0MeanMonthStatMap = R0 %>%
    group_by(Longitude ,
             Latitude,
             Month) %>%
    summarize(
        mean_temp = mean(Temperature, na.rm = T),
        mean_prec = mean(Precipitation, na.rm = T),
        mean_ber = mean(r0_briere, na.rm = T),
        mean_quad = mean(r0_quadratic, na.rm = T)
    ) %>%
    dplyr::left_join(grid_country_clean,
                     by=c("Longitude", "Latitude"))


save(R0MeanMonthStatMap,
     file = "processedData/R0MeanMonthStatMap.rda")

