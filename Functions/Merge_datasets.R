library("dplyr")


load("processedData/CEDA_temp_data.rda")
load("processedData/CEDA_precip_data.rda")
load("processedData/RiskOfExposure.rda")
load("processedData/aegypti.rda")

#merge all data sets
merged_data = temp_data %>%
    left_join(precip_data,
              by = c("Longitude", "Latitude","Date")) %>%
    left_join(Rse,
              by = c("Longitude", "Latitude")) %>%
    left_join(aegypti,
              by = c("Longitude", "Latitude"))



save(merged_data,
    file = "processedData/merged_input_Data.rda")

