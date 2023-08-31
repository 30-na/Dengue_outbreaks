
library(dplyr)
library(ggplot2)
library(ggpubr)
library(zoo)
library(reshape2)


load("processedData/R0MeanYearStatNoPrecip.rda")
df_mean_NoPrecip = R0MeanYearStat %>%
    dplyr::mutate(
        total_mean_ber_NoPrecip = mean(mean_ber, na.rm = T),
        total_mean_quad_NoPrecip = mean(mean_quad, na.rm = T),
        ber_diff_NoPrecip = mean_ber - total_mean_ber_NoPrecip,
        quad_diff_NoPrecip = mean_quad - total_mean_quad_NoPrecip,
        ber_per_NoPrecip = (ber_diff_NoPrecip / total_mean_ber_NoPrecip)*100,
        quad_per_NoPrecip = (quad_diff_NoPrecip / total_mean_quad_NoPrecip)*100
        )%>%
    dplyr::select(
        Year,
        ber_per_NoPrecip,
        quad_per_NoPrecip
        )%>%
    dplyr::filter(
        Year <= 2020
        )
    


load("processedData/R0MeanYearStat.rda")
names(R0MeanYearStat)
data = R0MeanYearStat %>%
    dplyr::mutate(
        total_mean_ber = mean(mean_ber, na.rm = T),
        total_mean_quad = mean(mean_quad, na.rm = T),
        total_mean_temp = mean(mean_temp, na.rm = T),
        total_mean_prec = mean(mean_prec, na.rm = T),
        
        ber_diff = mean_ber - total_mean_ber,
        quad_diff = mean_quad - total_mean_quad,
        temp_diff = mean_temp - total_mean_temp,
        prec_diff = mean_prec - total_mean_prec,
        
        ber_per = (ber_diff / total_mean_ber)*100,
        quad_per = (quad_diff / total_mean_quad)*100,
        temp_per = (temp_diff / total_mean_temp)*100,
        prec_per = (prec_diff / total_mean_prec)*100
        )%>%
    dplyr::select(
        Year,
        ber_per,
        quad_per,
        temp_per,
        prec_per
        )%>%
    dplyr::left_join(
        df_mean_NoPrecip, by=c("Year")
        )%>%
    reshape2::melt(
        id.vars = "Year"
        )%>%
    group_by(
        variable
        )%>%
    dplyr::mutate(
        smooth_value = rollmean(value,
                                k = 5,
                                fill = NA,
                                align = "center")
    )
    

plot_Fig4a = function(title1, title2, title3, title4){

    # G1
    g1 <- ggplot(data = data %>%
                     dplyr::filter(
                         variable %in% c("ber_per",
                                         #"temp_per",
                                         #"prec_per",
                                         "ber_per_NoPrecip"))%>%
                     mutate(variable = factor(variable)),
                 aes(x = as.numeric(Year),
                     y = value)) +  
        geom_bar(aes(fill = variable),
                 stat = "identity", position = "dodge", alpha=.5, width = 0.75, col="black", size=.02) +  
        geom_line(aes(y = smooth_value ,
                      col = variable),
                  size = 1,
                  show.legend=F) +
        
        #scale_fill_manual(values = c("FALSE" = "#d6604d", "TRUE" = "#4393c3")) +
        theme_minimal() +
        labs(title = title1,
            y = "Standardized R0 anomalies (%)",
            x = "") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8, face = "bold"),
              legend.position = "bottom") +
        
        scale_x_continuous(
            breaks = seq(1950, 2020, by = 5),
            labels = seq(1950, 2020, by = 5))+
        scale_fill_brewer(
            palette = "Dark2",
            labels = c("With Precipitaion", "Without Precipitation"),
            guide = guide_legend(
                title = "")
            )+
        
        scale_color_brewer(palette = "Dark2",
                           
                           #labels = c("With Precipitaion", "Without Precipitation")
                           ) +
        guides(
            col = guide_legend(title = NULL),
            
        )
    
    
    
    
    # G2
    g2 <- ggplot(data = data %>%
                     dplyr::filter(
                         variable %in% c("quad_per",
                                         #"temp_per",
                                         #"prec_per",
                                         "quad_per_NoPrecip")
                     )%>%
                     mutate(variable = factor(variable)),
                 aes(x = as.numeric(Year),
                     y = value)) +  
        geom_bar(aes(fill = variable),
                 stat = "identity",
                 position = "dodge",
                 alpha=.5,
                 width = 0.75,
                 col="black",
                 size=.02,
                 show.legend=F) +  
        geom_line(aes(y = smooth_value ,
                      col = variable),
                  size = 1,
                  show.legend=F) +
        
        #scale_fill_manual(values = c("FALSE" = "#d6604d", "TRUE" = "#4393c3")) +
        theme_minimal() +
        labs(title = title2,
             y = "Standardized R0 anomalies (%)",
             x = "") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8, face = "bold"),
              legend.position = "bottom") +
        
        scale_x_continuous(
            breaks = seq(1950, 2020, by = 5),
            labels = seq(1950, 2020, by = 5))+
        scale_fill_brewer(
            palette = "Dark2",
            labels = c("With Precipitaion", "Without Precipitation"),
            guide = guide_legend(
                title = "")
        )+
        
        scale_color_brewer(palette = "Dark2",
                           
                           #labels = c("With Precipitaion", "Without Precipitation")
        ) +
        guides(
            col = guide_legend(title = NULL),
            
        )
    
    
    p = "#ef3b2c"
    
    # G3
    g3 <- ggplot(data = data %>%
                     dplyr::filter(
                         variable %in% c("temp_per")
                     )%>%
                     mutate(variable = factor(variable)),
                 aes(x = as.numeric(Year),
                     y = value)) +  
        geom_bar(aes(fill = variable),
                 stat = "identity",
                 position = "dodge",
                 alpha=.7,
                 width = 0.75,
                 col="black",
                 size=.02,
                 show.legend=F,
                 fill = p ) +  
        geom_line(aes(y = smooth_value),
                  size = 1,
                  show.legend=F,
                  col = "black") +
        
        #scale_fill_manual(values = c("FALSE" = "#d6604d", "TRUE" = "#4393c3")) +
        theme_minimal() +
        labs(title = title3,
             y = "Standardized Temperature anomalies (%)",
             x = "") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8, face = "bold"),
              legend.position = "bottom") +
        
        scale_x_continuous(
            breaks = seq(1950, 2020, by = 5),
            labels = seq(1950, 2020, by = 5))+
        scale_fill_brewer(
            palette = p,
            #labels = c("With Precipitaion", "Without Precipitation"),
            guide = guide_legend(
                title = "")
        )+
        
        guides(
            col = guide_legend(title = NULL),
            
        )
    
    
    
    
    
    
    p = "#1d91c0"
    
    # G4
    g4 <- ggplot(data = data %>%
                     dplyr::filter(
                         variable %in% c("prec_per")
                     )%>%
                     mutate(variable = factor(variable)),
                 aes(x = as.numeric(Year),
                     y = value)) +  
        geom_bar(aes(fill = variable),
                 stat = "identity",
                 position = "dodge",
                 alpha=.7,
                 width = 0.75,
                 col="black",
                 size=.02,
                 show.legend=F,
                 fill = p ) +  
        geom_line(aes(y = smooth_value ,
                      col = p),
                  size = 1,
                  show.legend=F,
                  col="black") +
        
        #scale_fill_manual(values = c("FALSE" = "#d6604d", "TRUE" = "#4393c3")) +
        theme_minimal() +
        labs(title = title4,
             y = "Standardized Precipitation anomalies (%)",
             x = "") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8, face = "bold"),
              legend.position = "bottom") +
        
        scale_x_continuous(
            breaks = seq(1950, 2020, by = 5),
            labels = seq(1950, 2020, by = 5))+
        scale_fill_brewer(
            palette = p,
            #labels = c("With Precipitaion", "Without Precipitation"),
            guide = guide_legend(
                title = "")
        )+
        
        guides(
            col = guide_legend(title = NULL),
            
        )
    
    
    
    
    g <- ggarrange(g1,
                   g2,
                   g3,
                   g4,
                   heights = c(1.1,1,1,1),
                   font.label = list(size = 12,
                                     color = "black",
                                     face = "plain"),
                   ncol = 1,
                   nrow = 4,
                   hjust = -.02)
    
    g = annotate_figure(g,
                        top = text_grob("FIG 04",
                                        #face = "bold",
                                        size = 20,
                                        hjust=7)
    )
    

    ggsave(paste0("Figures/fig4.jpg"),
           g,
           height=16,width=12,scale=1)
}

plot_Fig4a(
    title1 = "A) Compare the model incorporating precipitation with the model excluding precipitation using the Briere function",
    title2 = "B) Compare the model incorporating precipitation with the model excluding precipitation using the Quadratic function",
    title3 = "C) Standardized Temperature anomalies with respect to the 1950-2020 period",
    title4 = "D) Standardized Precipitation anomalies with respect to the 1950-2020 period"
    )

           

