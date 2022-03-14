## DISSERTATION PROJECT ##

# Libraries ----
library(tidyverse)
library(readxl)
library(scales)
library(vegan)
library(gplots)

# Datasets ----
ratio <- read_excel("root-shoot.xls")
field_capacity_data <- read_excel("Field_capacity_diss.xlsx")
moisture <- read_excel("moisture.xlsx")

# Data manip field capacity ----
# water content = grams of water contained in the soil per pot
# moisture = mean and sd per pot measured by probe every other day in %
field_capacity_data <- field_capacity_data %>% 
  mutate(soil_type = as_factor(soil_type), pot_number = as_factor(pot_number)) %>% 
  mutate(water_content_gr = total_fresh-total_dry) %>%  # creating a new column for amount of water in soil
  mutate(moisture_content = ((total_fresh/total_dry)-1))  # new column with soil moisture content
# should I add *100 or not??? 

# Graph field capacity ----
# Boxplot checking for differences of field_capacity between soil types
(field_capacity_boxplot <- ggplot(field_capacity_data, aes(soil_type, moisture_content)) +
    geom_boxplot(aes(color = soil_type)) +
    theme_bw() +
    ylab("Field capacity\n") +                             
    xlab("\nSoil Type")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),       
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  
          legend.position = "none"))

# ggsave(soil_moisture_boxplot, file = "outputs/field_capacity_boxplot.png", width = 12, height = 7) 

# Data manip moisture ----
# moisture probe measures need to be plotted against time to see the progression 
# before relating them to the soil moisture content 
moisture <- moisture %>% 
  mutate(soil_type = as_factor(soil_type), pot = as_factor(pot)) %>% 
  tidyr::separate(date, c("year", "month", "day"), sep = "-", remove = FALSE) %>% 
  select(-year,-month) %>% 
  mutate(day = case_when(day == "09" ~ 1, day == "11" ~ 3, day == "13" ~ 5, 
                         day == "16" ~ 8, day == "18" ~ 10, day == "20" ~ 12, 
                         day == "23" ~ 15)) %>% 
  mutate(drought_level = case_when(pot == 1 ~ "50",
                                   pot == 2 ~ "75",
                                   pot == 3 ~ "100",
                                   pot == 4 ~ "50",
                                   pot == 5 ~ "75",
                                   pot == 6 ~ "100")) %>% 
  mutate(drought_level = as_factor(drought_level)) %>% 
  mutate(field_capacity = c(field_capacity_data$moisture_content,
                           field_capacity_data$moisture_content,
                           field_capacity_data$moisture_content,
                           field_capacity_data$moisture_content,
                           field_capacity_data$moisture_content,
                           field_capacity_data$moisture_content,
                           field_capacity_data$moisture_content)) %>% 
  mutate(field_capacity_percent = (mean_moisture*100)/field_capacity) %>% 
  mutate(field_capacity_percent_sd = (sd*100)/field_capacity) %>% 
  filter(pot %in% c(1,2,3))  # only selecting pots that have plant data

# Graphs moisture ----
# Graph of moisture across time 
(moisture_time_series <- ggplot(moisture, aes(date, mean_moisture, color = drought_level)) +
    geom_point() +
    facet_wrap(~ soil_type, scales = "fixed") +
    geom_smooth(formula = y ~ x, method = "lm", aes(fill = drought_level)) + # add se = FALSE to remove error shading
    theme_bw() +
    ylab("Mean moisture content (%)\n") +                             
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

# Graph of field capacity % across time
(field_capacity_time_series <- ggplot(moisture, aes(day, field_capacity_percent, color = drought_level, fill = drought_level)) +
    geom_point() +
    geom_smooth(formula = y ~ x, method = "lm") + # add se = FALSE to remove error shading
    theme_bw() +
    facet_wrap(~ soil_type, scales = "fixed") +
    ylab("Soil moisture (% of soil field capacity)\n") +                             
    xlab("\nTime (days since start of experiment)") +
    theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # making the dates at a bit of an angle
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12, face = "plain"),                        
          panel.grid = element_blank(),  
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"),  # Adding a margin around the plot
          legend.text = element_text(size = 10, face = "italic"),  
          legend.title = element_blank(),  # Removing the legend title 
          legend.position = "bottom")) 

# ggsave(field_capacity_time_series, file = "outputs/field_capacity_time_series.png", width = 12, height = 7) 
# whole figure tells us that there seems to be significant differences between soil types AND between drought treatments 

# Trying to figure out what is going on with soil 2 100% moisture measures = large differences according to pot
subset_moisture_soil_2 <- filter(moisture, soil_type == "2" & drought_level == "100%")

(field_capacity_time_series <- ggplot(subset_moisture_soil_2, aes(date, field_capacity_percent, color = pot)) +
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

# Data manip ratio ----
# Rename columns and create factor levels for species, drought level and soil type #
ratio <- ratio %>% 
  rename(root_shoot = "Root/Shoot", drought = Drought_level, soil = Soil_Type, species = Species) %>% 
  mutate(species = as.factor(species), drought = as.factor(drought), soil = as.factor(soil)) %>% 
  filter(!root_shoot > 2.5, !Leaf_area > 5) %>%   # take out the outliers
  mutate(biomass_log = log(Dry_weight_total), root_shoot_log = log(root_shoot))
  # mutate(root_shoot = rescale(root_shoot, to = c(-1, 1)))  # to help with analysis 

# Graphs root/shoot ----
# Graph with log total biomass and log root/shoot 
(biomass_root_shoot_graph <- ggplot(ratio, aes(log(Dry_weight_total), log(root_shoot))) +
   geom_point(aes(color = soil)) +
   geom_smooth(aes(color = soil), se = FALSE, method = "lm") +
   facet_wrap(~ drought, scales = "fixed") +
   theme_bw() +
   ylab("Log root/shoot ratio\n") +                             
   xlab("\nLog total biomass")  +
   theme(axis.text = element_text(size = 12),
         axis.title = element_text(size = 14, face = "plain"),                     
         panel.grid = element_blank(),       
         plot.margin = unit(c(1,1,1,1), units = , "cm"),  
         legend.position = "right"))

# ggsave(biomass_root_shoot_graph, file = "outputs/biomass_root_shoot_graph.png", width = 12, height = 7) 

# Boxplot log root/shoot + drought level
(ratio_boxplot <- ggplot(ratio, aes(drought, root_shoot)) +
    geom_boxplot(aes(color = drought)) +
    theme_bw() +
    ylab("Root/shoot ratio\n") +                             
    xlab("\nDrought level")  +
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
    ylab("Root/shoot ratio\n") +                             
    xlab("\nSoil type")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),       
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  
          legend.position = "none"))

# ggsave(ratio_soil_boxplot, file = "outputs/ratio_soil_boxplot.png", width = 12, height = 7)

# Boxplot root/shoot and species
(ratio_species_boxplot <- ggplot(ratio, aes(species, root_shoot)) +
    geom_boxplot(aes(color = species)) +
    theme_bw() +
    ylab("Root/shoot ratio\n") +                             
    xlab("\nSpecies")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),       
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  
          legend.position = "none"))

# Three plots for variation of root/shoot within species
(ratio_species_hist <- ggplot(ratio, aes(x = root_shoot)) +
   geom_histogram(aes(fill = drought), bins = 25) +
   theme_bw() +
   facet_wrap(~ species, scales = "fixed") +
   xlab("Root/shoot ratio")  +
   theme(axis.text = element_text(size = 12),
         axis.title = element_text(size = 14, face = "plain"),                     
         panel.grid = element_blank(),       
         plot.margin = unit(c(1,1,1,1), units = , "cm"),  
         legend.position = "bottom"))

# ggsave(ratio_species_hist, file = "outputs/ratio_species_hist.png", width = 12, height = 7)

# Boxplot root/shoot + drought level + soil types
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

# Graph root and shoot allometric regression 
(root_shoot_graph <- ggplot(ratio, aes(Dry_weight_root, Dry_weight_shoot)) +
    geom_point(aes(color = drought)) +
    geom_smooth(aes(color = drought), se = FALSE, method = "lm") +
    theme_bw() +
    ylab("Plant shoot biomass (g)\n") +                             
    xlab("\nPlant root biomass (g)")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),       
          plot.margin = unit(c(1,1,1,1), units = , "cm"),  
          legend.position = "right"))

# ggsave(root_shoot_graph, file = "outputs/root_shoot_graph.png", width = 12, height = 7)
# facet_wrap(~ drought, scales = "fixed") +
  
# Graphs leaf area ----
# Boxplot leaf area + species
(leaf_area_boxplot_species <- ggplot(ratio, aes(species, Leaf_area, color = species)) +
   geom_boxplot() +
   theme_bw() +
   ylab("Leaf area (cm2)\n") +                             
   xlab("\nSpecies")  +
   theme(axis.text = element_text(size = 12),
         axis.title = element_text(size = 14, face = "plain"),                     
         panel.grid = element_blank(),       
         plot.margin = unit(c(1,1,1,1), units = , "cm"),  
         legend.position = "none"))

ggsave(leaf_area_boxplot_species, file = "outputs/leaf_area_boxplot_species.png", width = 12, height = 7)

# Boxplot leaf area + soil 
(leaf_area_boxplot_soil <- ggplot(ratio, aes(soil, Leaf_area, color = soil)) +
   geom_boxplot() +
   theme_bw() +
   ylab("Leaf area (cm2)\n") +                             
   xlab("\nSoil type")  +
   theme(axis.text = element_text(size = 12),
         axis.title = element_text(size = 14, face = "plain"),                     
         panel.grid = element_blank(),       
         plot.margin = unit(c(1,1,1,1), units = , "cm"),  
         legend.position = "none"))

# ggsave(leaf_area_boxplot_soil, file = "outputs/leaf_area_boxplot_drought.png", width = 12, height = 7)

# Boxplot leaf area + drought
(leaf_area_boxplot_drought <- ggplot(ratio, aes(drought, Leaf_area, color = drought)) +
   geom_boxplot() +
   theme_bw() +
   ylab("Leaf area (cm2)\n") +                             
   xlab("\nDrought level")  +
   theme(axis.text = element_text(size = 12),
         axis.title = element_text(size = 14, face = "plain"),                     
         panel.grid = element_blank(),       
         plot.margin = unit(c(1,1,1,1), units = , "cm"),  
         legend.position = "none"))

# ggsave(leaf_area_boxplot_drought, file = "outputs/leaf_area_boxplot_drought.png", width = 12, height = 7)

# Data manip biomass ----
biomass_data <- ratio %>% 
  select(drought, soil, species, Dry_weight_shoot, Dry_weight_root, Dry_weight_total) %>% 
  gather(., characteristic, biomass, c(4:6)) %>% 
  tidyr::separate(characteristic, c("moisture", "weight", "area"), sep = "_", remove = TRUE) %>% 
  select(-moisture,-weight)

# Graph biomass ----
# Plot total biomass with ≠ drought 
biomass_subset <- biomass_data %>% 
  filter(area %in% c("total")) %>% 
  group_by(drought, species) %>% 
  mutate(mean = mean(biomass), sd = sd(biomass)) %>% 
  group_by(drought, species, mean, sd) %>% 
  tally()
  ungroup() 
  
(total_biomass_drought_barplot <- ggplot(biomass_subset, aes(drought, mean, fill = drought)) +
   geom_bar(position = position_dodge(), stat = "identity") +
   geom_errorbar(aes(x = drought, ymin = mean-sd, ymax = mean+sd, width=0.4)) +
   facet_wrap(~ species, scales = "fixed") +
   theme_bw() +
   ylab("Total plant biomass (g)\n") +                             
   xlab("\nDrought treatment")  +
   theme(axis.text = element_text(size = 12),
         axis.title = element_text(size = 14, face = "plain"),                     
         panel.grid = element_blank(),       
         plot.margin = unit(c(1,1,1,1), units = , "cm"),  
         legend.position = "none"))

# ggsave(total_biomass_drought_barplot, file = "outputs/total_biomass_drought_barplot.png", width = 12, height = 7)

# Barplot biomass in different parts of plant per drought treatment and species 
(biomass_drought_species_barplot <- ggplot(biomass_data, aes(drought, biomass , fill = drought)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  facet_wrap(~ species, scales = "fixed") +
  theme_bw() +
  ylab("Biomass (g)\n") +                             
  xlab("\nPlant part")  +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain"),                     
        panel.grid = element_blank(),       
        plot.margin = unit(c(1,1,1,1), units = , "cm"),  
        legend.position = "right"))

# ggsave(biomass_drought_species_barplot, file = "outputs/biomass_drought_species_barplot.png", width = 12, height = 7)

# Barplot biomass in different parts of plant per drought treatment and soil type
(biomass_drought_soil_barplot <- ggplot(biomass_data, aes(area, biomass, fill = drought)) +
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

# Stats moisture and field capacity ----
moisture_model <- lm(field_capacity ~ soil_type, data = moisture)
summary(moisture_model)

moisture_model_2 <- lm(field_capacity_percent ~ day + soil_type, data = moisture)
summary(moisture_model_2)

moisture_model_3 <- lm(field_capacity_percent ~ drought_level + soil_type, data = moisture)
summary(moisture_model_3)

moisture_model_4 <- lm(mean_moisture ~ drought_level + soil_type, data = moisture)
summary(moisture_model_4)

# Stats ratio ----
ratio_model <- lm(root_shoot ~ drought, data = ratio)
summary(ratio_model)
anova(ratio_model)
plot(ratio_model)

ratio_model2 <- lm(root_shoot ~ soil, data = ratio)
summary(ratio_model2)
anova(ratio_model2)
plot(ratio_model2)

ratio_model3 <- lm(root_shoot ~ drought*species, data = ratio)
summary(ratio_model3)
anova(ratio_model3)
plot(ratio_model3)

ratio_model4 <- lm(root_shoot ~ soil + species, data = ratio)
summary(ratio_model4)
anova(ratio_model4)
plot(ratio_model4)

ratio_model5 <- lm(root_shoot ~ species, data = ratio)
summary(ratio_model5)
anova(ratio_model5)
plot(ratio_model5)

AIC(ratio_model, ratio_model2, ratio_model3)  # can I use AIC for lm???

# Verification of assumptions
ratio_resids <- resid(ratio_model2)
shapiro.test(ratio_resids)
bartlett.test(root_shoot ~ drought*species + soil, data = ratio)  # doesn't work with interaction terms? 
fligner.test(root_shoot ~ drought*species + soil, data = ratio) 

# Stats biomass ----
total_biomass_model <- lm(biomass ~ drought, data = biomass_data)
summary(total_biomass_model)

total_biomass_model2 <- lm(biomass ~ species, data = biomass_data)
summary(total_biomass_model2)

# Stats leaf area ----
leaf_area_model <- lm(Leaf_area ~ species, data = ratio)
summary(leaf_area_model)
anova(leaf_area_model)
plot(leaf_area_model)

leaf_area_model2 <- lm(Leaf_area ~ soil, data = ratio)
summary(leaf_area_model2)
anova(leaf_area_model2)
plot(leaf_area_model2)

leaf_area_model3 <- lm(Leaf_area ~ drought, data = ratio)
summary(leaf_area_model3)
anova(leaf_area_model3)
plot(leaf_area_model3)

# Verification of assumptions # 
leaf_resids <- resid(leaf_area_model)
shapiro.test(leaf_resids)
bartlett.test(Leaf_area ~ drought*soil*species, data = ratio)  # doesn't work with interaction terms? 

# To see which drought level drives the significant results #
ratio_aov <- aov(root_shoot ~ drought*soil*species, data = ratio)
TukeyHSD(ratio_aov)

# Generalized linear models ----
# check family 
hist(ratio$root_shoot)

generalized_ratio <- glm(root_shoot ~ drought*soil*species, family = gaussian, data = ratio)
summary(generalized_ratio)

generalized_ratio2 <- glm(root_shoot ~ drought*soil + species, family = gaussian, data = ratio)
summary(generalized_ratio2)

generalized_ratio3 <- glm(root_shoot ~ drought + soil + species, family = gaussian, data = ratio)
summary(generalized_ratio3)

AIC(generalized_ratio,generalized_ratio2, generalized_ratio3)
# weird because I get different results than for the ANOVA...?

# NMDS + permanova ----
(ratio.fit <- adonis(root_shoot ~ drought*soil*species, ratio, 
                      permutations = 500, method = "bray"))  # weird because shows effect of drought + species + soil which is different from ANOVA results

(leaf_area.fit <- adonis(Leaf_area ~ drought*soil*species, ratio, 
                     permutations = 500, method = "bray"))  # only species have significant difference which is same result as ANOVA

# pairwise.adonis(invert[,5:18], invert$Site)  # post hoc test 
ratio.NMDS <- metaMDS(ratio$root_shoot, distance = "bray", k = 3, trymax=100)  # works but stress nearly 0 so may have insufficient data 
# par(mfrow=c(1,1))
# ratio.NMDS$stress

group <- as.character(invert$Distance)
colours <- as.character(invert$Distance)

as.character(invert$Distance) %>% 
  replace(colours=="0", "#ddffa2") %>% 
  replace(colours=="6", "#a9c3e1") %>% 
  replace(colours=="12", "#ff4f4f") %>% 
  replace(colours=="18", "#fffcbf") -> colours

# create the NMDS plot #

par(mfrow=c(1,1))
ordiplot(ratio.NMDS, type = "n", cex.axis = 2, cex.lab=2)

for(i in unique(group)) {
  ordihull(ratio.NMDS$points[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colours[grep(i,group)],label=F) 
  }  # doesn't work 

orditorp(ratio.NMDS, display = "species", col = "red", air = 0.01)
orditorp(invert.NMDS, display = "sites", label=F, air = 0.01, cex = 1.25)
legend('bottomright', legend=c("0m","6m","12m","18m"), col=unique(colours), 
       title = "Distance from road", bty = "n", pch = 16)
legend('bottomleft', legend="stress = 0.042", bty = "n")

# MANOVA ----
(manova1 <- manova(cbind(root_shoot, Leaf_area) ~ drought + soil + species, ratio))
summary(manova1)
summary.aov(manova1)

plotmeans(ratio$root_shoot ~ ratio$drought)
plotmeans(ratio$root_shoot ~ ratio$species)
plotmeans(ratio$root_shoot ~ ratio$soil)
