## DISSERTATION PROJECT ##

# Libraries ----
library(tidyverse)
library(readxl)

# Import datasets ----
ratio <- read_excel("root-shoot.xls")
field_capacity <- read_excel("Field_capacity_diss.xlsx")
moisture <- read_excel("moisture.xlsx")

# Data manipulation ratio dataset ----
# Rename columns and create factor levels for species, drought level and soil type #
ratio <- rename(ratio, root_shoot = "Root/Shoot", drought = Drought_level, soil = Soil_Type, species = Species)
ratio <- mutate(ratio, species = as.factor(species), drought = as.factor(drought), soil = as.factor(soil))

# Data manipulation field capacity dataset ----
# water content = grams of water contained in the soil per pot
# moisture = mean and sd per pot measured by probe every other day in %
field_capacity <- field_capacity %>% 
  mutate(soil_type = as_factor(soil_type), pot_number = as_factor(pot_number)) %>% 
  mutate(water_content_gr = total_fresh-total_dry) %>%  # creating a new column for amount of water in soiil
  mutate(moisture_content_percent = ((total_fresh/total_dry)-1)*100)  # new column with moisture content in %

# checking for differences of soil moisture between soil types 
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

ggsave(soil_moisture_boxplot, file = "outputs/soil_moisture_boxplot.png", width = 12, height = 7) 

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
  mutate(drought_level_percent = (mean_moisture*100)/soil_type_moisture)

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

(field_capacity_time_series <- ggplot(moisture, aes(date, drought_level_percent, color = drought_level)) +
    geom_point() +
    facet_wrap(~ soil_type, scales = "free_y") +
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

# Stats ----
ratio_model <- lm(root_shoot ~ drought*soil*species, data = ratio)
summary(ratio_model)
anova(ratio_model)
plot(ratio_model)

ratio_model2 <- lm(root_shoot ~ drought, data = ratio)
summary(ratio_model2)
anova(ratio_model2)
plot(ratio_model2)

# Verification of assumptions # 
ratio_resids <- resid(ratio_model2)
shapiro.test(ratio_resids)
bartlett.test(root_shoot ~ drought, data = ratio)

# To see which drought level drives the significant results #
ratio_aov <- aov(root_shoot ~ drought, data = ratio)
TukeyHSD(ratio_aov)
