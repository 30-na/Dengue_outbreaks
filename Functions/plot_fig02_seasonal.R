
# load library
library(dplyr)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(reshape2)
library(RColorBrewer)
library(mapproj)

# load data
load("processedData/R0MeanMonthStatMap.rda")
load("processedData/R0MeanStatMap.rda")
names(R0MeanMonthStatMap)

# calculate the seasonal R0 for quad and briere
df = R0MeanMonthStatMap %>%
    dplyr::mutate(
        season = case_when(Month %in% c("December", "January", "February") ~ "Winter",
                           Month %in% c("March", "April", "May") ~ "Spring",
                           Month %in% c("June", "July", "August") ~ "Summer",
                           Month %in% c("September", "October", "November") ~ "Autumn",),
        season = factor(season, levels = c("Winter", "Spring", "Summer", 'Autumn'))
    ) %>%
    group_by(
        Longitude,
        Latitude,
        season
    )%>%
    dplyr::summarize(
        mean_ber_s = mean(mean_ber, na.rm = T),
        mean_quad_s = mean(mean_quad, na.rm = T),
    ) %>%
    dplyr::left_join(
        R0MeanStatMap, by=c("Latitude", "Longitude")
    ) %>%
    #calculating relative change
    dplyr::mutate(
        diff_ber = mean_ber_s - mean_ber,
        diff_quad = mean_quad_s - mean_quad,
        per_ber = ifelse(diff_ber == 0 & mean_ber ==0, 0, (diff_ber / mean_ber)*100),
        per_quad = ifelse(diff_quad == 0 & mean_quad ==0, 0, (diff_quad / mean_quad)*100),
        mean_ber_disc = cut(per_ber,
                            breaks = c(-Inf, -80, -60, -40, -20, 0, 20, 40, 60,80, Inf),
                            labels = c("< -80", "-80 to -60", "-60 to -40","-40 to -20", "-20 to 0", "0 to 20", "20 to 40", "40 to 60", "60 to 80","> 80"),
                            include.lowest = TRUE),
        mean_quad_disc = cut(per_quad,
                            breaks = c(-Inf, -80, -60, -40, -20, 0, 20, 40, 60,80, Inf),
                            labels = c("< -80", "-80 to -60", "-60 to -40","-40 to -20", "-20 to 0", "0 to 20", "20 to 40", "40 to 60", "60 to 80","> 80"),
                            include.lowest = TRUE),
        mean_ber_disc = ifelse(per_ber == 0, "0", as.character(mean_ber_disc)),
        mean_quad_disc = ifelse(per_quad == 0, "0", as.character(mean_quad_disc)),
        mean_ber_disc = factor(mean_ber_disc, levels = c("< -80", "-80 to -60", "-60 to -40","-40 to -20", "-20 to 0", "0", "0 to 20", "20 to 40", "40 to 60", "60 to 80","> 80")),
        mean_quad_disc = factor(mean_quad_disc, levels = c("< -80", "-80 to -60", "-60 to -40","-40 to -20", "-20 to 0", "0", "0 to 20", "20 to 40", "40 to 60", "60 to 80","> 80"))
        
    )

hist(df$per_ber)

plot_map = function(fun, title){
    
    
    
    g0b = ggplot(df, aes(x = per_ber)) +
        geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
        labs(title = "Histogram of R0 seasonal change (Briere)",
             x = "%R0 Ralative change",
             y = "Frequency") +
        theme_minimal()
    ggsave("Figures/fig2HistB.jpg",
           g0b,
           height=5,width=8,scale=1)
    
    
    g0q = ggplot(df, aes(x = per_quad)) +
        geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
        labs(title = "Histogram of R0 seasonal change (Quadratic)",
             x = "%R0 Ralative change",
             y = "Frequency") +
        theme_minimal()
    ggsave("Figures/fig2HistQ.jpg",
           g0q,
           height=5,width=8,scale=1)
    
    
    
    if (fun == "briere"){
        temp_df = df %>%
            dplyr::select(
                Longitude,
                Latitude,
                season,
                "variable" = mean_ber_disc
            )
    }
        if (fun == "quadratic"){
            temp_df = df %>%
                dplyr::select(
                    Longitude,
                    Latitude,
                    season,
                    "variable" = mean_quad_disc
                )
    }
    
    
    
    custom_colors = rev(c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','#e0e0e0', '#d1e5f0','#92c5de','#4393c3','#2166ac','#053061'))
    
    
    g = ggplot(
        temp_df,
        aes(x = Longitude, y = Latitude, color = variable)
    ) +
        geom_point(size=.1) +
        borders(
            size=.2,
            colour = "black",
            xlim=c(-180, 180),
            ylim=c(-62, 90)
        )+ 
        #scale_color_viridis(discrete = TRUE, option = "D")+
        scale_color_manual(values = custom_colors,
                           na.value = "transparent",
                           drop = TRUE,
                           na.translate = F) +
        labs(
            title = title,
            x = "",
            y = "",
            color = "R0 %"
            #caption = "(A) Mean annual R0 calculated over the period 1950–2020. (B) R0 peak that represents the largest monthly value over period (1950–2020)"
        ) +
        facet_wrap(
            . ~ season,
            ncol = 2,
            labeller = as_labeller(c(Spring = "(B). Spring",
                                     Summer = "(C). Summer",
                                     Winter = "(A). Winter",
                                     Autumn = "(D). Autumn"))) +
        ylim(c(-62, 90)) +
        
        theme_minimal()+
        guides(color = guide_legend(override.aes = list(size = 10, shape=15)))+
        theme(
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            strip.text.x = element_text(hjust = 0, margin=margin(l=0)),
            strip.text = element_text(size = 12, angle = 0),
            plot.title = element_text(size = 20)
        )
    
    ggsave(paste0("Figures/fig2_", fun, ".jpg"),
           g,
           height=7,width=12,scale=1)
}

plot_map(fun = "briere", 
         title = "Fig. 02")


plot_map(fun = "quadratic", 
         title = "Fig. S02 (Quadratic)")
