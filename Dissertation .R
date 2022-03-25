## DISSERTATION PROJECT ##

# Libraries ----
library(tidyverse)
library(readxl)
library(scales)
library(vegan)
library(gplots)
library(patchwork)
library(hrbrthemes)
library(car)
library(multcomp)
library(RColorBrewer)
library(gvlma)

# Data sets ----
ratio <- read_excel("root-shoot.xls")
field_capacity_data <- read_excel("Field_capacity_diss.xlsx")
moisture <- read_excel("moisture.xlsx")

# Data manip field capacity ----
# water content = grams of water contained in the soil per pot
# moisture = mean and sd per pot measured by probe every other day in %
field_capacity_data <- field_capacity_data %>% 
  mutate(soil_type = as_factor(soil_type), pot_number = as_factor(pot_number)) %>% 
  mutate(water_content_gr = total_fresh-total_dry) %>%  # creating a new column for amount of water in soil
  mutate(volumetric_water_content = water_content_gr/183,058)  # creating volumetric water content column in gr/cm3 
  # mutate(moisture_content = ((total_fresh/total_dry)-1))  # new column with soil moisture content
# should I add *100 or not??? 

# Graph Volumetric water content  ----
# Boxplot checking for differences of field_capacity between soil types
(volumetric_boxplot <- ggplot(field_capacity_data, aes(soil_type, volumetric_water_content)) +
    geom_boxplot(aes(color = soil_type)) +
    theme_bw() +
    scale_color_manual(values = c("#009E73", "#F0E442", "#0072B2")) +
    ylab("Volumetric water content (g/cm3)\n") +                             
    xlab("\nSoil Type")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),       
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  
          legend.position = "none"))

# ggsave(volumetric_boxplot, file = "outputs/volumetric_boxplot.png", width = 12, height = 7) 

# Data manip moisture ----
moisture <- moisture %>% 
  mutate(soil_type = as_factor(soil_type), pot = as_factor(pot)) %>% 
  tidyr::separate(date, c("year", "month", "day"), sep = "-", remove = FALSE) %>% 
  dplyr::select(-year,-month) %>% 
  mutate(day = case_when(day == "04" ~ 3, day == "06" ~ 5, day == "09" ~ 8, day == "11" ~ 10, 
                         day == "13" ~ 12, day == "16" ~ 15, day == "18" ~ 17, 
                         day == "20" ~ 19, day == "23" ~ 22)) %>% 
  mutate(irrigation_level = case_when(pot == 1 ~ "50",  # used to be drought_level but didn't make sense
                                   pot == 2 ~ "75",
                                   pot == 3 ~ "100",
                                   pot == 4 ~ "50",
                                   pot == 5 ~ "75",
                                   pot == 6 ~ "100")) %>% 
  mutate(irrigation_level = as_factor(irrigation_level)) %>% 
  filter(pot %in% c(1,2,3))  # only selecting pots that have plant data

# Graphs moisture ----
# Graph irrigation treatments 
(irrigation_graph <- ggplot(moisture) +
   geom_point(aes(day, irrigation, color = irrigation_level)) +
   geom_line(aes(day, irrigation, color = irrigation_level)) +
   theme_bw() +
   xlab("\nTime (days since start of experiment)") +
   ylab("Volume of water (ml)\n") +
   theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # making the dates at a bit of an angle
         axis.text.y = element_text(size = 10),
         axis.title = element_text(size = 12, face = "plain"),                        
         panel.grid = element_blank(),  
         plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"),  # Adding a margin around the plot
         legend.text = element_text(size = 10, face = "italic"),  
         legend.title = element_blank(),  # Removing the legend title 
         legend.position = "right")) 

# ggsave(irrigation_graph, file = "outputs/irrigation_graph.png", width = 12, height = 7) 

# Boxplot mean moisture per irrigation treatment 
(moisture_boxplot <- ggplot(moisture, aes(irrigation_level, mean_moisture)) +
   geom_boxplot(aes(color = irrigation_level)) +
   theme_bw() +
   scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
   ylab("Mean volumetric water content (%)\n") +                             
   xlab("\nSoil type")  +
   theme(axis.text = element_text(size = 12),
         axis.title = element_text(size = 14, face = "plain"),                     
         panel.grid = element_blank(),       
         plot.margin = unit(c(1,1,1,1), units = , "cm"),  
         legend.position = "right"))
moisture_model <- lm(mean_moisture ~ irrigation_level, data = moisture)
summary(moisture_model)
anova(moisture_model)

# Graph weight and irrigation just for interest 
(weight_time_series <- ggplot(moisture) +
   geom_point(aes(day, weight, color = soil_type)) +
   geom_smooth(formula = y ~ x, method = "lm", aes(day, weight, color = soil_type, fill = soil_type)) + # add se = FALSE to remove error shading
   geom_point(aes(day, irrigation/0.15, color = soil_type)) +
   scale_y_continuous(name = "Weight of pots (g)\n",
                      sec.axis = sec_axis(~.*0.15, name="Irrigation volume (ml)")) +
   theme_bw() +
   xlab("\nDay since start of experiment") +
   theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # making the dates at a bit of an angle
         axis.text.y = element_text(size = 10),
         axis.title = element_text(size = 12, face = "plain"),                        
         panel.grid = element_blank(),  
         plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"),  # Adding a margin around the plot
         legend.text = element_text(size = 10, face = "italic"),  
         legend.title = element_blank(),  # Removing the legend title 
         legend.position = "right")) 

# Graph of moisture across time 
(moisture_time_series <- ggplot(moisture, aes(day, mean_moisture, color = irrigation_level)) +
    geom_point() +
    facet_wrap(~ soil_type, scales = "fixed") +
    geom_smooth(formula = y ~ x, method = "lm", aes(fill = irrigation_level)) + # add se = FALSE to remove error shading
    theme_bw() +
    scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
    scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
    ylab("Mean volumetric water content (%)\n") +                             
    xlab("\nDays since start of experiment") +
    theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # making the dates at a bit of an angle
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "plain"),                        
          panel.grid = element_blank(),  
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"),  # Adding a margin around the plot
          legend.text = element_text(size = 10, face = "italic"),  
          legend.title = element_blank(), 
          legend.position = "bottom")) 

# ggsave(moisture_time_series, file = "outputs/moisture_time_series.png", width = 12, height = 7) 

# Graph of VWC % across time
(vwc_time_series <- ggplot(moisture, aes(day, mean_moisture, color = irrigation_level, fill = irrigation_level)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
    scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
    facet_wrap(~ soil_type, scales = "fixed") +
    ylab("Mean volumetric water content (%)\n") +                             
    xlab("\nTime (days since start of experiment)") +
    theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # making the dates at a bit of an angle
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "plain"),                        
          panel.grid = element_blank(),  
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"),  # Adding a margin around the plot
          legend.text = element_text(size = 10, face = "italic"),  
          legend.title = element_blank(),  # Removing the legend title 
          legend.position = "bottom")) 

# ggsave(vwc_time_series, file = "outputs/vwc_time_series.png", width = 12, height = 7) 

# Data manip ratio ----
# Rename columns and create factor levels for species, drought level and soil type #
ratio <- ratio %>% 
  rename(root_shoot = "Root/Shoot", irrigation_level = Drought_level, soil = Soil_Type, species = Species) %>% 
  mutate(species = as.factor(species), irrigation_level = as.factor(irrigation_level), soil = as.factor(soil)) %>% 
  filter(!root_shoot > 2.5, !Leaf_area > 5) %>%   # take out the outliers
  mutate(biomass_log = log(Dry_weight_total), root_shoot_log = log(root_shoot)) %>% 
  mutate(leaf_area_ratio = Leaf_area/Dry_weight_total) %>% 
  mutate(count = c(1:96))  # to help with checking which observation doesn't fit assumptions 
  # mutate(root_shoot = rescale(root_shoot, to = c(-1, 1)))  # to help with analysis 

# Graphs root/shoot ----
# Heatmap R/S + soil + drought
(root_shoot_heatmap <- ggplot(ratio, aes(soil, irrigation_level, fill = root_shoot)) +
   geom_tile())

# ggsave(root_shoot_heatmap, file = "outputs/root_shoot_heatmap.png", width = 12, height = 7) 

# Graph with log total biomass and log root/shoot 
(biomass_root_shoot_graph <- ggplot(ratio, aes(biomass_log, root_shoot_log)) +
   geom_point(aes(color = species)) +
   geom_smooth(aes(color = species), se = FALSE, method = "lm", formula = 'y ~ poly(x, 2)') +
   theme_bw() +
   ylab("Log root/shoot ratio\n") +                             
   xlab("\nLog total biomass")  +
   theme(axis.text = element_text(size = 12),
         axis.title = element_text(size = 14, face = "plain"),                     
         panel.grid = element_blank(),       
         plot.margin = unit(c(1,1,1,1), units = , "cm"),  
         legend.position = "right"))

# ggsave(biomass_root_shoot_graph, file = "outputs/biomass_root_shoot_graph.png", width = 12, height = 7) 

# log graph colored by soil 
(biomass_root_shoot2_graph <- ggplot(ratio_ratio, aes(biomass_log, root_shoot_log)) +
    geom_point(aes(color = soil)) +
    geom_smooth(aes(color = soil), se = FALSE, method = "lm", formula = 'y ~ poly(x, 2)') +
    facet_wrap(~ irrigation_level, scales = "fixed") +
    theme_bw() +
    scale_color_manual(values = c("#009E73", "#F0E442", "#0072B2")) +
    ylab("Log root/shoot ratio\n") +                             
    xlab("\nLog total biomass")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),       
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  
          legend.position = "right"))

# ggsave(biomass_root_shoot2_graph, file = "outputs/biomass_root_shoot2_graph.png", width = 12, height = 7) 

# Boxplot  root/shoot + drought level
(ratio_boxplot <- ggplot(ratio, aes(irrigation_level, root_shoot)) +
    geom_boxplot(aes(color = irrigation_level)) +
    theme_bw() +
    scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
    ylab("Root/shoot ratio\n") +                             
    xlab("\nIrrigation level")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),       
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  
          legend.position = "right"))

# ggsave(ratio_boxplot, file = "outputs/ratio_boxplot.png", width = 12, height = 7)

# Boxplot root/shoot and soil types + species
(ratio_soil_boxplot <- ggplot(ratio, aes(soil, root_shoot)) +
    geom_boxplot(aes(color = soil)) +
    theme_bw() +
    scale_color_manual(values = c("#009E73", "#F0E442", "#0072B2")) +
    ylab("Root/shoot ratio\n") +                             
    xlab("\nSoil type")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),       
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  
          legend.position = "none"))

# ggsave(ratio_soil_boxplot, file = "outputs/ratio_soil_boxplot.png", width = 12, height = 7)

# Boxplot root/shoot and species
(ratio_species_boxplot <- ggplot(ratio_ratio, aes(species, root_shoot)) +
    geom_boxplot(aes(color = species)) +
    theme_bw() +
    ylab("Root/shoot ratio\n") +                             
    xlab("\nSpecies")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),       
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  
          legend.position = "none"))

# ggsave(ratio_species_boxplot, file = "outputs/ratio_species_boxplot.png", width = 12, height = 7)

# Three plots for variation of root/shoot within species
(ratio_species_hist <- ggplot(ratio, aes(x = root_shoot)) +
   geom_histogram(aes(fill = irrigation_level), bins = 25) +
   theme_bw() +
   scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
   facet_wrap(~ species, scales = "fixed") +
   xlab("Root/shoot ratio")  +
   theme(axis.text = element_text(size = 12),
         axis.title = element_text(size = 14, face = "plain"),                     
         panel.grid = element_blank(),       
         plot.margin = unit(c(1,1,1,1), units = , "cm"),  
         legend.position = "bottom"))

# ggsave(ratio_species_hist, file = "outputs/ratio_species_hist.png", width = 12, height = 7)

# Boxplot root/shoot + drought level + soil types
(ratio_drought_soil_boxplot <- ggplot(ratio, aes(irrigation_level, root_shoot, color = soil)) +
    geom_boxplot() +
    theme_bw() +
    scale_color_manual(values = c("#009E73", "#F0E442", "#0072B2")) +
    ylab("Root/shoot ratio\n") +
    xlab("\nIrrigation level")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),       
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  
          legend.position = "bottom"))

# ggsave(ratio_drought_soil_boxplot, file = "outputs/ratio_drought_soil_boxplot.png", width = 12, height = 7)

# Graph root and shoot allometric regression 
slopes <- c(as.numeric(lm(Dry_weight_root ~ Dry_weight_shoot, ratio, species == "Basil")$coefficients[1]),
            as.numeric(lm(Dry_weight_root ~ Dry_weight_shoot, ratio, species == "Dill")$coefficients[1]),
            as.numeric(lm(Dry_weight_root ~ Dry_weight_shoot, ratio, species == "Parsley")$coefficients[1]))
slopes2 <- c(as.numeric(lm(Dry_weight_root ~ Dry_weight_shoot, ratio, species == "Basil")$coefficients[2]),
             as.numeric(lm(Dry_weight_root ~ Dry_weight_shoot, ratio, species == "Dill")$coefficients[2]),
             as.numeric(lm(Dry_weight_root ~ Dry_weight_shoot, ratio, species == "Parsley")$coefficients[2]))

coefficients <- data.frame(slopes, slopes2, c("Basil","Dill","Parlsey")) %>% 
  rename(intercept = slopes, slope = slopes2, species = c..Basil....Dill....Parlsey..)

(root_shoot_soil_graph <- ggplot(ratio_ratio, aes(Dry_weight_shoot, Dry_weight_root)) +
    geom_point(aes(color = soil)) +
    stat_smooth(aes(color = soil), se = FALSE, method = "lm") +
    scale_color_manual(values = c("#009E73", "#F0E442", "#0072B2")) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 1) +
    theme_bw() +
    facet_wrap(~ species, scales = "fixed") +
    ylab("Plant root biomass (g)\n") +                             
    xlab("\nPlant shoot biomass (g)") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),       
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  
          legend.position = "right"))

# ggsave(root_shoot_soil_graph, file = "outputs/root_shoot_soil_graph.png", width = 12, height = 7)

(root_shoot_irrigation_graph <- ggplot(ratio_ratio, aes(Dry_weight_shoot, Dry_weight_root)) +
    geom_point(aes(color = irrigation_level)) +
    stat_smooth(aes(color = irrigation_level), se = FALSE, method = "lm") +
    scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 1) +
    theme_bw() +
    facet_wrap(~ species, scales = "fixed") +
    ylab("Plant root biomass (g)\n") +                             
    xlab("\nPlant shoot biomass (g)") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),       
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  
          legend.position = "right"))
# ggsave(root_shoot_irrigation_graph, file = "outputs/root_shoot_irrigation_graph.png", width = 12, height = 7)

# Graphs leaf area ----
# Graph log leaf area ratio and log biomass
(biomass_leaf_ratio_graph <- ggplot(ratio_leaf_area, aes(biomass_log, log(leaf_area_ratio))) +
    geom_point(aes(color = soil)) +
    geom_smooth(aes(color = soil), se = FALSE, method = "lm", formula = 'y ~ poly(x, 2)') +
    theme_bw() +
    scale_color_manual(values = c("#009E73", "#F0E442", "#0072B2")) +
    facet_wrap(~ irrigation_level, scales = "fixed") +
    ylab("Log leaf area ratio\n") +                             
    xlab("\nLog total biomass")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),       
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  
          legend.position = "right"))

# ggsave(biomass_leaf_ratio_graph, file = "outputs/biomass_leaf_ratio_graph.png", width = 12, height = 7)

# Boxplot leaf area + species
(leaf_area_boxplot_species <- ggplot(ratio_leaf_area, aes(species, Leaf_area, color = species)) +
   geom_boxplot() +
   theme_bw() +
   ylab("Leaf area (cm2)\n") +                             
   xlab("\nSpecies")  +
   facet_wrap(~ soil, scales = "fixed") +
   theme(axis.text = element_text(size = 12),
         axis.title = element_text(size = 14, face = "plain"),                     
         panel.grid = element_blank(),       
         plot.margin = unit(c(1,1,1,1), units = , "cm"),  
         legend.position = "none"))

# ggsave(leaf_area_boxplot_species, file = "outputs/leaf_area_boxplot_species.png", width = 12, height = 7)

# Boxplot leaf area + soil 
(leaf_area_boxplot_soil <- ggplot(ratio, aes(soil, Leaf_area, color = soil)) +
   geom_boxplot() +
   theme_bw() +
   scale_color_manual(values = c("#009E73", "#F0E442", "#0072B2")) +
   ylab("Leaf area (cm2)\n") +                             
   xlab("\nSoil type")  +
   theme(axis.text = element_text(size = 12),
         axis.title = element_text(size = 14, face = "plain"),                     
         panel.grid = element_blank(),       
         plot.margin = unit(c(1,1,1,1), units = , "cm"),  
         legend.position = "none"))

# ggsave(leaf_area_boxplot_soil, file = "outputs/leaf_area_boxplot_drought.png", width = 12, height = 7)

# Boxplot leaf area + drought
(leaf_area_boxplot_drought <- ggplot(ratio, aes(irrigation_level, Leaf_area, color = irrigation_level)) +
   geom_boxplot() +
   theme_bw() +
   scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
   ylab("Leaf area (cm2)\n") +                             
   xlab("\nIrrigation level")  +
   theme(axis.text = element_text(size = 12),
         axis.title = element_text(size = 14, face = "plain"),                     
         panel.grid = element_blank(),       
         plot.margin = unit(c(1,1,1,1), units = , "cm"),  
         legend.position = "none"))

# ggsave(leaf_area_boxplot_drought, file = "outputs/leaf_area_boxplot_drought.png", width = 12, height = 7)

# Data manip biomass ----
biomass_data <- ratio %>% 
  select(irrigation_level, soil, species, Dry_weight_shoot, Dry_weight_root, Dry_weight_total) %>% 
  gather(., characteristic, biomass, c(4:6)) %>% 
  tidyr::separate(characteristic, c("moisture", "weight", "area"), sep = "_", remove = TRUE) %>% 
  select(-moisture,-weight)

# Graph biomass ----
# Heatmap biomass + soil + drought
(biomass_heatmap <- ggplot(ratio, aes(soil, irrigation_level, fill = Dry_weight_total)) +
   geom_tile())

# ggsave(biomass_heatmap, file = "outputs/biomass_heatmap.png", width = 12, height = 7) 

# Plot total biomass and soil types 
(total_biomass_drought_barplot <- ggplot(ratio, aes(irrigation_level, Dry_weight_total, fill = irrigation_level, color = irrigation_level)) +
    geom_bar(position = position_dodge(), stat = "identity") +
    theme_bw() +
    scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
    scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
    facet_wrap(~ species, scales = "fixed") +
    ylab("Total plant biomass (g)\n") +                             
    xlab("\nIrrigation level")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),       
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  
          legend.position = "none"))

# with the mean and error bars instead?
# biomass_subset <- biomass_data %>% 
#   filter(area %in% c("total")) %>% 
#   group_by(irrigation_level, species) %>% 
#   mutate(mean = mean(biomass), sd = sd(biomass)) %>% 
#   group_by(irrigation_level, species, mean, sd) %>% 
#   tally() %>% 
#   ungroup() 
#   
# (total_biomass_drought_barplot <- ggplot(biomass_subset, aes(irrigation_level, mean, fill = irrigation_level)) +
#    geom_bar(position = position_dodge(), stat = "identity") +
#    geom_errorbar(aes(x = drought, ymin = mean-sd, ymax = mean+sd, width=0.4)) +
#    facet_wrap(~ species, scales = "fixed") +
#    theme_bw() +
#    ylab("Total plant biomass (g)\n") +                             
#    xlab("\nIrrigation level")  +
#    theme(axis.text = element_text(size = 12),
#          axis.title = element_text(size = 14, face = "plain"),                     
#          panel.grid = element_blank(),       
#          plot.margin = unit(c(1,1,1,1), units = , "cm"),  
#          legend.position = "none"))

# ggsave(total_biomass_drought_barplot, file = "outputs/total_biomass_drought_barplot.png", width = 12, height = 7)

# # Barplot biomass in different parts of plant per drought treatment and species 
# (biomass_drought_species_barplot <- ggplot(biomass_data, aes(irrigation_level, biomass , fill = irrigation_level)) +
#   geom_bar(position = position_dodge(), stat = "identity") +
#   facet_wrap(~ species, scales = "fixed") +
#   theme_bw() +
#   ylab("Biomass (g)\n") +                             
#   xlab("\nIrrigation level")  +
#   theme(axis.text = element_text(size = 12),
#         axis.title = element_text(size = 14, face = "plain"),                     
#         panel.grid = element_blank(),       
#         plot.margin = unit(c(1,1,1,1), units = , "cm"),  
#         legend.position = "right"))
# 
# # ggsave(biomass_drought_species_barplot, file = "outputs/biomass_drought_species_barplot.png", width = 12, height = 7)

# Barplot biomass in different parts of plant per drought treatment and soil type
(biomass_drought_soil_barplot <- ggplot(biomass_data, aes(area, biomass, fill = irrigation_level)) +
    geom_bar(position = position_dodge(), stat = "identity") +
    facet_wrap(~ soil, scales = "fixed") +
    scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
    theme_bw() +
    ylab("Biomass (g)\n") +                             
    xlab("\nPlant part")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),       
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  
          legend.position = "right"))

# ggsave(biomass_drought_soil_barplot, file = "outputs/biomass_drought_soil_barplot.png", width = 12, height = 7)

# Barplot biomass in different parts of plant per species and soil type
(biomass_species_soil_barplot <- ggplot(biomass_data, aes(area, biomass, fill = species)) +
    geom_bar(position = position_dodge(), stat = "identity") +
    facet_wrap(~ soil, scales = "fixed") +
    theme_bw() +
    ylab("Biomass (g)\n") +                             
    xlab("\nPlant part")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),       
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  
          legend.position = "right"))

# ggsave(biomass_species_soil_barplot, file = "outputs/biomass_species_soil_barplot.png", width = 12, height = 7)

# Stats VWC ----
volumetric_model <- lm(volumetric_water_content ~ soil_type , data = field_capacity_data)
summary(volumetric_model)
anova(volumetric_model)
plot(volumetric_model)

# Stats ratio ----
ratio_ratio <- ratio %>% filter(!count %in% c(67,90))  # taking out outliers
ratio_model1 <- lm(root_shoot ~ irrigation_level*species + soil, data = ratio_ratio)
summary(ratio_model1)
anova(ratio_model1)
plot(ratio_model1)
summary(gvlma(ratio_model1))

# Stats biomass ----
total_biomass_model <- lm(Dry_weight_total ~ species + irrigation_level*soil, data = ratio)
summary(total_biomass_model)
anova(total_biomass_model)
plot(total_biomass_model)
summary(gvlma(total_biomass_model))

# Stats leaf area ----
ratio_leaf_area <- ratio %>%  filter(!count %in% c(84,53))  # taking out outliers
leaf_area_model <- lm(Leaf_area ~ species*soil + irrigation_level, data = ratio_leaf_area)
summary(leaf_area_model)
anova(leaf_area_model)
plot(leaf_area_model)
summary(gvlma(leaf_area_model))

leaf_area_ratio_model <- lm(leaf_area_ratio ~ species, data = ratio)
summary(leaf_area_ratio_model)
anova(leaf_area_ratio_model)
plot(leaf_area_ratio_model)

# Stats moisture ----
# ANCOVA
# Assumptions with chi-squared test and Levene's test
explanatory_table = table(moisture$day, moisture$soil_type)
print(explanatory_table)
print(chisq.test(explanatory_table))
# p-value > 0.05 so covariates are independent 
leveneTest(mean_moisture ~ irrigation_level, data = moisture)
# p-value > 0.05 so variance is equal 
# then actually fitting the model
ancova_model <- aov(mean_moisture ~ irrigation_level + day + soil_type, data = moisture)
Anova(ancova_model, type="III") 
# posthoc test 
postHocs <- glht(ancova_model, linfct = mcp(soil_type = "Tukey"))
summary(postHocs)
postHocs1 <- glht(ancova_model, linfct = mcp(irrigation_level = "Tukey"))
summary(postHocs1)
