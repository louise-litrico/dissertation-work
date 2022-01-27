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

#ggsave(soil_moisture_boxplot, file = "outputs/soil_moisture_boxplot.png", width = 5, height = 12) 

# Data manipulation for moisture measures ----
# moisture probe measures need to be plotted against time to see the progression 
# before relating them to the soil moisture content 
moisture <- moisture %>% 
  mutate(soil_type = as_factor(soil_type), pot = as_factor(pot)) %>% 
  tidyr::separate(date, c("year", "month", "day"), sep = "-", remove = FALSE) %>% 
  select(-year,-month) %>% 
  mutate(day = as_factor(day)) %>% 
  mutate(drought_level = case_when(grepl("1", pot) ~ "50%",
                                   grepl("2", pot) ~ "75%",
                                   grepl("3", pot) ~ "100%",
                                   grepl("4", pot) ~ "50%",
                                   grepl("5", pot) ~ "75%",
                                   grepl("6", pot) ~ "100%")) %>% 
  mutate(drought_level = as_factor(drought_level))

(moisture_time_series <- ggplot(moisture, aes(date, mean_moisture, color = drought_level)) +
    geom_point(size = 2) +
    facet_wrap(~ soil_type, scales = "free_y") +
    geom_smooth(formula = y ~ x, method = "lm", aes(fill = drought_level)) +
    theme_bw() +
    ylab("Moisture content (%)\n") +                             
    xlab("\nDay in August") +
    theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # making the dates at a bit of an angle
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "plain"),                        
          panel.grid = element_blank(),  
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"),  # Adding a margin around the plot
          legend.text = element_text(size = 10, face = "italic"),  
          legend.title = element_blank(),  # Removing the legend title 
          legend.position = "bottom")) 

#ggsave(moisture_time_series, file = "outputs/moisture_time_series.png", width = 5, height = 12) 

# I need to figure out a way to transform that moisture data into a percentage of the moisture content/field capacity of the soil types

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
