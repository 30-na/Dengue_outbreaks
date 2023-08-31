
# load library
library(dplyr)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(reshape2)
library(RColorBrewer)
library(mapproj)

# load data
load("processedData/R0MeanStatMap.rda")
load("processedData/R0MaxStatMap.rda")


# merge datasets
df = R0MeanStatMap %>%
    dplyr::left_join(
        R0MaxStatMap, by=c("Latitude", "Longitude", "country")
    ) %>%
    dplyr::mutate(
        max_temp = ifelse(max_temp == -Inf, NA, max_temp),
        max_prec = ifelse(max_prec == -Inf, NA, max_prec),
        max_ber = ifelse(max_ber == -Inf, NA, max_ber),
        max_quad = ifelse(max_quad == -Inf, NA, max_quad),
        mean_ber_disc = cut(mean_ber,
                            breaks = c(-Inf, 0, 1, 2, 3, 4, 5, 6, 7, Inf),
                            labels = c("0","1","2","3","4","5","6","7","8"),
                                 include.lowest = TRUE),
        mean_quad_disc = cut(mean_quad,
                            breaks = c(-Inf, 0, 1, 2, 3, 4, 5, 6, 7, Inf),
                            labels = c("0","1","2","3","4","5","6","7","8"),
                            include.lowest = TRUE),
        max_ber_disc = cut(max_ber,
                           breaks = c(-Inf, 0, 1, 2, 3, 4, 5, 6, 7, Inf),
                           labels = c("0","1","2","3","4","5","6","7","8"),
                           include.lowest = TRUE),
        max_quad_disc = cut(max_quad,
                            breaks = c(-Inf, 0, 1, 2, 3, 4, 5, 6, 7, Inf),
                            labels = c("0","1","2","3","4","5","6","7","8"),
                            include.lowest = TRUE),
        
        # max_ber_disc = cut(max_ber,
        #                     breaks = c(-Inf, 0, .5, 1, 1.5, 2, Inf),
        #                    labels = c("0","0.50","1","1.50","2", "2.50"),
        #                     include.lowest = TRUE),
        # max_quad_disc = cut(max_quad,
        #                     breaks = c(-Inf, 0, .5, 1, 1.5, 2, Inf),
        #                     labels = c("0","0.50","1","1.50","2", "2.50"),
        #                     include.lowest = TRUE)
    ) %>%
    dplyr::select(
        Longitude, 
        Latitude,
        mean_ber,
        mean_quad,
        max_ber,
        max_quad,
        mean_ber_disc,
        mean_quad_disc,
        max_ber_disc,
        max_quad_disc
    ) %>%
    reshape2::melt(
        id.vars = c("Latitude", "Longitude")
    )

table(df$variable)
plot_map = function(variables, title){
    temp_df = df %>%
        dplyr::filter(
            variable %in% variables
            )
    
    custom_colors = c('#fff7ec','#fee8c8','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000')
    custom_colors2 = c('#ffffe5','#fff7bc','#fee391','#fec44f','#fe9929','#ec7014','#cc4c02','#993404','#662506')
    g = ggplot(
        temp_df,
        aes_string(x = "Longitude", y = "Latitude", color = "value")
    ) +
        geom_point(size=.1) +
        borders(
            size=.2,
            colour = "black",
            xlim=c(-180, 180),
            ylim=c(-62, 90)
        )+ 
        #scale_color_viridis(discrete = TRUE, option = "D")+
        scale_color_manual(values = custom_colors2,
                           na.value = "transparent",
                           drop = TRUE,
                           na.translate = F) +
        labs(
            title = title,
            x = "",
            y = "",
            color = "R0"
            #caption = "(A) Mean annual R0 calculated over the period 1950–2020. (B) R0 peak that represents the largest monthly value over period (1950–2020)"
        ) +
        facet_wrap(
            . ~ variable,
            ncol = 1,
            labeller = as_labeller(c(mean_quad_disc = "(A) Mean annual R0 calculated over the period 1950–2020",
                                     max_quad_disc = "(B) R0 peak that represents the largest monthly value over period 1950–2020"))) +
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
    
    ggsave(paste0("Figures/fig1s.jpg"),
           g,
           height=9,width=9,scale=1)
}

plot_map(variables = c("mean_ber_disc", "max_ber_disc" ), 
         title = "Fig. 01")


plot_map(variables = c("mean_quad_disc", "max_quad_disc" ), 
         title = "Fig. S01 (Quadratic)")

