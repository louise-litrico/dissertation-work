## Dissertation final code ##

# Libraries ----
library(tidyverse)
library(readxl)
library(scales)
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

# Data manipulation VWC ----
field_capacity_data <- field_capacity_data %>% 
  mutate(soil_type = as_factor(soil_type), pot_number = as_factor(pot_number)) %>% 
  mutate(water_content_gr = total_fresh-total_dry) %>%  
  mutate(volumetric_water_content = water_content_gr/183,058) 

# Stats VWC ----
volumetric_model <- lm(volumetric_water_content ~ soil_type , data = field_capacity_data)
summary(volumetric_model)
anova(volumetric_model)
plot(volumetric_model)

# Data manipulation moisture ----
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

# Data visualization moisture----
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

# Graph of VWC across time
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


# Data manipulation ratio ----
ratio_ratio <- ratio %>% 
  rename(root_shoot = "Root/Shoot", irrigation_level = Drought_level, soil = Soil_Type, species = Species) %>% 
  mutate(species = as.factor(species), irrigation_level = as.factor(irrigation_level), soil = as.factor(soil)) %>% 
  filter(!root_shoot > 2.5, !Leaf_area > 5) %>% 
  mutate(biomass_log = log(Dry_weight_total), root_shoot_log = log(root_shoot)) %>% 
  mutate(leaf_area_ratio = Leaf_area/Dry_weight_total) %>% 
  mutate(count = c(1:96)) %>% 
  mutate(root_log = log(Dry_weight_root), shoot_log = (log(Dry_weight_shoot)))
 
# Stats ratio ----
ratio_ratio <- ratio %>% filter(!count %in% c(67,90))  # taking out outliers
ratio_model1 <- lm(root_shoot ~ irrigation_level*species + soil, data = ratio_ratio)
summary(ratio_model1)
anova(ratio_model1)
plot(ratio_model1)
summary(gvlma(ratio_model1))

explanatory_model = aov(Dry_weight_shoot ~ irrigation_level, data = ratio_ratio)
summary(explanatory_model)
explanatory_model = aov(Dry_weight_shoot ~ soil, data = ratio_ratio)
summary(explanatory_model)
# p-value > 0.05 so covariates are independent 
leveneTest(Dry_weight_root ~ species, data = ratio_ratio)
leveneTest(Dry_weight_root ~ soil, data = ratio_ratio)
leveneTest(Dry_weight_root ~ irrigation_level, data = ratio_ratio)
# p-value > 0.05 so variance is equal 
# then actually fitting the model
ancova_model <- aov(Dry_weight_root ~ species + irrigation_level + Dry_weight_shoot, data = ratio_ratio)
Anova(ancova_model, type="III") 
ancova_model2 <- aov(Dry_weight_root ~ species + soil + Dry_weight_shoot, data = ratio_ratio)
Anova(ancova_model2, type="III") 
# posthoc test 
postHocs <- glht(ancova_model, linfct = mcp(soil_type = "Tukey"))
summary(postHocs)
postHocs1 <- glht(ancova_model, linfct = mcp(irrigation_level = "Tukey"))
summary(postHocs1)
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

# Graphs ratio ----
# Heatmap R/S + soil + drought
(root_shoot_heatmap <- ggplot(ratio, aes(soil, irrigation_level, fill = root_shoot)) +
   geom_tile())

# Log graph colored by soil 
(biomass_root_shoot2_graph <- ggplot(ratio_ratio, aes(biomass_log, root_shoot_log)) +
    geom_point(aes(color = soil)) +
    stat_smooth(aes(color = soil), se = FALSE, method = "lm", formula = 'y ~ poly(x, 2)') +
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

# Root against shoot for soils 
(root_shoot_soil_graph <- ggscatter(ratio_ratio, x = "shoot_log", y = "root_log", color = "soil", add = "reg.line") +
    stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = soil)) +
    scale_color_manual(values = c("#009E73", "#F0E442", "#0072B2")) +
    theme_bw() +
    facet_wrap(~ species, scales = "fixed") +
    ylab("Plant root biomass (g)\n") +                             
    xlab("\nPlant shoot biomass (g)") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),       
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  
          legend.position = "right"))

# Roots against shoots for irrigation levels 
(root_shoot_irrigation_graph <- ggscatter(ratio_ratio, x = "shoot_log", y = "root_log", color = "irrigation_level", add = "reg.line") +
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = irrigation_level)) +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
  theme_bw() +
  facet_wrap(~ species, scales = "fixed") +
  ylab("Plant root biomass (g)\n") +                             
  xlab("\nPlant shoot biomass (g)") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain"),                     
        panel.grid = element_blank(),       
        plot.margin = unit(c(1,1,1,1), units = , "cm"),  
        legend.position = "right"))

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

# Data manip biomass ----
biomass_data <- ratio %>% 
  select(irrigation_level, soil, species, Dry_weight_shoot, Dry_weight_root, Dry_weight_total) %>% 
  gather(., characteristic, biomass, c(4:6)) %>% 
  tidyr::separate(characteristic, c("moisture", "weight", "area"), sep = "_", remove = TRUE) %>% 
  select(-moisture,-weight)

# Stats biomass ----
total_biomass_model <- lm(Dry_weight_total ~ species + irrigation_level*soil, data = ratio)
summary(total_biomass_model)
anova(total_biomass_model)
plot(total_biomass_model)
summary(gvlma(total_biomass_model))

# Graph biomass ----
# Heatmap biomass + soil + drought
(biomass_heatmap <- ggplot(ratio, aes(soil, irrigation_level, fill = Dry_weight_total)) +
   geom_tile())

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

