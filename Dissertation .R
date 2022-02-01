## DISSERTATION PROJECT ##

# Libraries ----
library(tidyverse)
library(readxl)

# Import datasets ----
ratio <- read_excel("root-shoot.xls")
field_capacity <- read_excel("Field_capacity_diss.xlsx")
moisture <- read_excel("moisture.xlsx")

# Data manipulation field capacity dataset ----
# water content = grams of water contained in the soil per pot
# moisture = mean and sd per pot measured by probe every other day in %
field_capacity <- field_capacity %>% 
  mutate(soil_type = as_factor(soil_type), pot_number = as_factor(pot_number)) %>% 
  mutate(water_content_gr = total_fresh-total_dry) %>%  # creating a new column for amount of water in soil
  mutate(moisture_content_percent = ((total_fresh/total_dry)-1)*100)  # new column with moisture content in %

# Boxplot checking for differences of soil moisture between soil types ----
(soil_moisture_boxplot <- ggplot(field_capacity, aes(soil_type, moisture_content_percent)) +
    geom_boxplot(aes(color = soil_type)) +
    theme_bw() +
    ylab("Moisture content (%)\n") +                             
    xlab("\nSoil Type")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),       
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  
          legend.position = "none"))

# ggsave(soil_moisture_boxplot, file = "outputs/soil_moisture_boxplot.png", width = 12, height = 7) 

# Data manipulation for moisture measures ----
# moisture probe measures need to be plotted against time to see the progression 
# before relating them to the soil moisture content 
moisture <- moisture %>% 
  mutate(soil_type = as_factor(soil_type), pot = as_factor(pot)) %>% 
  tidyr::separate(date, c("year", "month", "day"), sep = "-", remove = FALSE) %>% 
  select(-year,-month) %>% 
  mutate(drought_level = case_when(pot == 1 ~ "50%",
                                   pot == 2 ~ "75%",
                                   pot == 3 ~ "100%",
                                   pot == 4 ~ "50%",
                                   pot == 5 ~ "75%",
                                   pot == 6 ~ "100%")) %>% 
  mutate(drought_level = as_factor(drought_level)) %>% 
  mutate(soil_type_moisture = c(field_capacity$moisture_content_percent,
                           field_capacity$moisture_content_percent,
                           field_capacity$moisture_content_percent,
                           field_capacity$moisture_content_percent,
                           field_capacity$moisture_content_percent,
                           field_capacity$moisture_content_percent,
                           field_capacity$moisture_content_percent)) %>% 
  mutate(drought_level_percent = (mean_moisture*100)/soil_type_moisture) %>% 
  filter(pot %in% c(1,2,3))  # only selecting pots that have plant data

# Graph of moisture accross time ----
(moisture_time_series <- ggplot(moisture, aes(date, mean_moisture, color = drought_level)) +
    geom_point() +
    facet_wrap(~ soil_type, scales = "free_y") +
    geom_smooth(formula = y ~ x, method = "lm", aes(fill = drought_level)) + # add se = FALSE to remove error shading
    theme_bw() +
    ylab("Moisture content (%)\n") +                             
    xlab("\nDate") +
    theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # making the dates at a bit of an angle
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "plain"),                        
          panel.grid = element_blank(),  
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"),  # Adding a margin around the plot
          legend.text = element_text(size = 10, face = "italic"),  
          legend.title = element_blank(),  # Removing the legend title 
          legend.position = "bottom")) 

# ggsave(moisture_time_series, file = "outputs/moisture_time_series.png", width = 12, height = 7) 

# Graph of field capacity % accross time ----
(field_capacity_time_series <- ggplot(moisture, aes(date(), drought_level_percent, color = drought_level)) +
    geom_point() +
    geom_smooth(formula = y ~ x, method = "lm", aes(fill = drought_level)) + # add se = FALSE to remove error shading
    theme_bw() +
    ylab("Moisture content (as a % of soil moisture content at field capacity)\n") +                             
    xlab("\nDate") +
    theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # making the dates at a bit of an angle
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "plain"),                        
          panel.grid = element_blank(),  
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"),  # Adding a margin around the plot
          legend.text = element_text(size = 10, face = "italic"),  
          legend.title = element_blank(),  # Removing the legend title 
          legend.position = "bottom")) 

# ggsave(field_capacity_time_series, file = "outputs/field_capacity_time_series.png", width = 12, height = 7) 

# Trying to figure out what is going on with soil 2 100% moisture measures = large differences according to pot ----
subset_moisture_soil_2 <- filter(moisture, soil_type == "2" & drought_level == "100%")

(field_capacity_time_series <- ggplot(subset_moisture_soil_2, aes(date, drought_level_percent, color = pot)) +
    geom_point() +
    geom_smooth(formula = y ~ x, method = "lm", aes(fill = drought_level)) + # add se = FALSE to remove error shading
    theme_bw() +
    ylab("Moisture content (as a % of soil moisture content at field capacity)\n") +                             
    xlab("\nDate") +
    theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # making the dates at a bit of an angle
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "plain"),                        
          panel.grid = element_blank(),  
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"),  # Adding a margin around the plot
          legend.text = element_text(size = 10, face = "italic"),  
          legend.title = element_blank(),  # Removing the legend title 
          legend.position = "bottom")) 

pot_model <- lm(drought_level_percent ~ pot, data = subset_moisture_soil_2)
summary(pot_model)
anova(pot_model)
plot(pot_model)

# Data manipulation ratio dataset ----
# Rename columns and create factor levels for species, drought level and soil type #
ratio <- ratio %>% 
  rename(root_shoot = "Root/Shoot", drought = Drought_level, soil = Soil_Type, species = Species) %>% 
  mutate(species = as.factor(species), drought = as.factor(drought), soil = as.factor(soil)) %>% 
  filter(!root_shoot > 2.5)  # take out the outliers

# Boxplot root/shoot and drought level ----
(ratio_boxplot <- ggplot(ratio, aes(drought, root_shoot)) +
    geom_boxplot(aes(color = drought)) +
    theme_bw() +
    ylab("Root/shoot ratio\n") +                             
    xlab("\nDrought level")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),       
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  
          legend.position = "none"))

# ggsave(ratio_boxplot, file = "outputs/ratio_boxplot.png", width = 12, height = 7)

# Boxplot root/shoot and soil types ----
(ratio_soil_boxplot <- ggplot(ratio, aes(soil, root_shoot)) +
    geom_boxplot(aes(color = soil)) +
    theme_bw() +
    ylab("Root/shoot ratio\n") +                             
    xlab("\nSoil type")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),       
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  
          legend.position = "none"))

# ggsave(ratio_soil_boxplot, file = "outputs/ratio_soil_boxplot.png", width = 12, height = 7)

# Boxplot root/shoot + drought level + soil types ----
(ratio_drought_soil_boxplot <- ggplot(ratio, aes(drought, root_shoot, color = soil)) +
    geom_boxplot() +
    theme_bw() +
    ylab("Root/shoot ratio\n") +                             
    xlab("\nDrought level")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),       
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  
          legend.position = "bottom"))

# ggsave(ratio_drought_soil_boxplot, file = "outputs/ratio_drought_soil_boxplot.png", width = 12, height = 7)

# Stats ----
ratio_model <- lm(root_shoot ~ drought*soil*species, data = ratio)
summary(ratio_model)
anova(ratio_model)
plot(ratio_model)

ratio_model2 <- lm(root_shoot ~ drought, data = ratio)
summary(ratio_model2)
anova(ratio_model2)
plot(ratio_model2)

leaf_area_model <- lm(Leaf_area ~ drought*soil*species, data = ratio)
summary(leaf_area_model)
anova(leaf_area_model)
plot(leaf_area_model)

# Verification of assumptions # 
leaf_resids <- resid(leaf_area_model)
shapiro.test(leaf_resids)
bartlett.test(Leaf_area ~ drought*soil*species, data = ratio)  # doesn't work with interaction terms? 


# To see which drought level drives the significant results #
ratio_aov <- aov(root_shoot ~ drought*soil*species, data = ratio)
TukeyHSD(ratio_aov)

