
# load library
library(dplyr)
library(ggplot2)
library(reshape2)
library(viridis)
library(zoo)
library(ggpubr)
library(maps)

# load data
load("processedData/R0_data.rda")
load("processedData/R0MeanMonthStatMap.rda")
load("processedData/grid_country_clean.rda")
names(R0_data)

df_map_2015 = R0_data %>%
    dplyr::filter(Date >= "2015-02-01" & Date < "2016-02-01")%>% 
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
    ) %>%
    dplyr::left_join(grid_country_clean,
                     by=c("Longitude", "Latitude"))%>%
    dplyr::filter(country == "Brazil") %>%
    group_by(Year,
             Month,
             Longitude,
             Latitude)%>%
    dplyr::summarize(
        mean_ber_ElNino = mean(r0_briere, na.rm = T),
        mean_quad_ElNino = mean(r0_quadratic, na.rm = T),
        mean_temp_ElNino = mean(Temperature, na.rm = T),
        mean_prec_ElNino = mean(Precipitation, na.rm = T)
    ) %>%
    dplyr::left_join(
        R0MeanMonthStatMap , by=c("Longitude","Latitude", "Month")
    ) %>%
    dplyr::mutate(
        date = paste(Year, Month),
        diff_ber = mean_ber_ElNino - mean_ber,
        diff_quad = mean_quad_ElNino - mean_quad,
        diff_temp = mean_temp_ElNino - mean_temp,
        diff_prec = mean_prec_ElNino - mean_prec,
        per_ber = ifelse(diff_ber == 0 & mean_ber == 0, 0, (diff_ber / mean_ber)*100),
        per_quad = ifelse(diff_quad == 0 & mean_quad == 0, 0, (diff_quad / mean_quad)*100),
        per_temp = ifelse(diff_temp == 0 & mean_temp == 0, 0, (diff_temp / mean_temp)*100),
        per_prec = ifelse(diff_prec == 0 & mean_prec == 0, 0, (diff_prec / mean_prec)*100))








df_map_1997 = R0_data %>%
    dplyr::filter(Date >= "1997-06-01" & Date < "1998-06-01") %>% 
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
    ) %>%
    dplyr::left_join(grid_country_clean,
                     by=c("Longitude", "Latitude"))%>%
    dplyr::filter(country == "Brazil") %>%
    group_by(Year,
             Month,
             Longitude,
             Latitude)%>%
    dplyr::summarize(
        mean_ber_ElNino = mean(r0_briere, na.rm = T),
        mean_quad_ElNino = mean(r0_quadratic, na.rm = T),
        mean_temp_ElNino = mean(Temperature, na.rm = T),
        mean_prec_ElNino = mean(Precipitation, na.rm = T)
    ) %>%
    dplyr::left_join(
        R0MeanMonthStatMap , by=c("Longitude","Latitude", "Month")
    ) %>%
    dplyr::mutate(
        date = paste(Year, Month),
        diff_ber = mean_ber_ElNino - mean_ber,
        diff_quad = mean_quad_ElNino - mean_quad,
        diff_temp = mean_temp_ElNino - mean_temp,
        diff_prec = mean_prec_ElNino - mean_prec,
        per_ber = ifelse(diff_ber == 0 & mean_ber == 0, 0, (diff_ber / mean_ber)*100),
        per_quad = ifelse(diff_quad == 0 & mean_quad == 0, 0, (diff_quad / mean_quad)*100),
        per_temp = ifelse(diff_temp == 0 & mean_temp == 0, 0, (diff_temp / mean_temp)*100),
        per_prec = ifelse(diff_prec == 0 & mean_prec == 0, 0, (diff_prec / mean_prec)*100))




plot_map = function(month, fun, title1, title2, id, year){
    
    if(year == "2015-16"){
        temp_df = df_map_2015
    }
    if(year == "1997-98"){
        temp_df = df_map_1997
    }
    
    
    
    if(fun == "berier"){
        temp_df = temp_df %>%
            dplyr::rename(
                "var1" = per_ber,
                "var2" = per_temp,
                "var3" = per_prec
            )
    }
    
    if(fun == "quadratic"){
        temp_df = temp_df %>%
            dplyr::rename(
                "var1" = per_quad,
                "var2" = per_temp,
                "var3" = per_prec
            )
    }
    
    temp_df = temp_df %>%
        dplyr::filter(
            Month == month
        )
    
    date = unique(temp_df$date)
    
    custom_colors = c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','#e0e0e0','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061')
    
    custom_colors = c("#d73027",'#e0e0e0','#053061')
    
    
    g1 = ggplot(
        temp_df,
        aes(x = Longitude, y = Latitude, fill = var1)
    ) +
        geom_tile() +
        geom_polygon(data = map_data("world", region = "Brazil"),
                     aes(x = long, y = lat, group = group),
                     color = "black",
                     fill = NA,
                     size = 1) +
        scale_fill_gradientn(colors = rev(custom_colors),
                             limits = c(-150, 150),
                             #limits = c(-15, 15),
                             na.value = "transparent",
                             oob = scales::squish) +
        labs(
            #title = paste0(month, title),
            x = "",
            y = "",
            fill = "% R0"
        )+
        #ylim(c(-62, 90)) +
        
        theme_minimal()+
        guides(color = guide_legend(override.aes = list(size = 8, shape=15)))+
        theme(
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            plot.title = element_text(size = 12)
        )+
        coord_fixed()
    
    
    
    
    g2 = ggplot(
        temp_df,
        aes(x = Longitude, y = Latitude, fill = var2)
    ) +
        geom_tile() + 
        geom_polygon(data = map_data("world", region = "Brazil"), aes(x = long, y = lat, group = group),
                     color = "black", fill = NA, size = 1) +
        scale_fill_gradientn(colors = rev(custom_colors),
                             limits = c(-10, 10),
                             #limits = c(-3, 3),
                             na.value = "transparent",
                             oob = scales::squish) +
        labs(
            #title = paste0(month, title),
            x = "",
            y = "",
            fill = "% Temperature"
        )+
        #ylim(c(-62, 90)) +
        
        theme_minimal()+
        #guides(color = guide_legend(override.aes = list(size = 8, shape=15)))+
        theme(
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            plot.title = element_text(size = 12)
        )+
        coord_fixed()
    
    
    g3 = ggplot(
        temp_df,
        aes(x = Longitude, y = Latitude, fill = var3)
    ) +
        geom_tile() + 
        geom_polygon(data = map_data("world", region = "Brazil"), aes(x = long, y = lat, group = group),
                     color = "black", fill = NA, size = 1) +
        scale_fill_gradientn(colors = rev(custom_colors),
                             limits = c(-100, 100),
                             #limits = c(-150, 150),
                             na.value = "transparent",
                             oob = scales::squish) +
        labs(
            #title = paste0(month, title),
            x = "",
            y = "",
            fill = "% Precipitation"
        )+
        #ylim(c(-62, 90)) +
        
        theme_minimal()+
        #guides(color = guide_legend(override.aes = list(size = 8, shape=15)))+
        theme(
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            plot.title = element_text(size = 12)
        )+
        coord_fixed()
    
    
    g <- ggarrange(g1,
                   g2,
                   g3,
                   #labels=c(title1, title2),
                   font.label = list(size = 12,
                                     color = "black",
                                     face = "plain"),
                   ncol = 1,
                   nrow = 3,
                   hjust = -.02)
    
    
    g = annotate_figure(g,
                        top = text_grob(date,
                                        #color = "red",
                                        face = "bold",
                                        size = 14))
    
    ggsave(paste0("Figures/",year,"/", sprintf("%02d", id),".jpg"),
           g,
           height=9,width=5,scale=1.6)
}


months <- c("February", "March", "April", "May", "June", 
            "July", "August", "September", "October", "November", "December", "January" )


for(m in 1:length(months)){
    
    plot_map(month = months[m],
             fun = "berier",
             title1 = " R0 2015-16 El Nino ",
             title2 = "Temperature",
             id = m,
             year = "2015-16"
    )
}


for(m in 1:length(months)){
    
    plot_map(month = months[m],
             fun = "berier",
             title1 = " R0 1997-98 El Nino ",
             title2 = "Temperature",
             id = m,
             year = "1997-98"
    )
}

