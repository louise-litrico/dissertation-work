# Data used for the dissertation experiment # 

The root_shoot.xls file contains the treatment information of each plant individual: 
- irrigation level (50%; 75% or 100%)
- soil type (1 for compost, 2 for compost-vermiculite, or 3 for compost-vermiculte-sand)
- herb species (Basil, Dill or Parsley)

This file also contains the measures taken from those plant individuals and used in the data analysis available [here](https://github.com/louise-litrico/dissertation-work/blob/main/code/Dissertation_final_code.R):
- the root:shoot ratio values (calculated by creating root and shoot percentages of the total plant by using the dry_weight values of the root, shoot and total parts of the plant, and then dividing those two percentages together) 
- the moisture content was created by substracting the total dry weight to the total fresh weight. But this value was not used in the final data analysis. 
- the leaf area of the individuals was calculated with the Fiji software using the leaf pictures references in the column next to it. The leaf picture sare available [here](https://github.com/louise-litrico/dissertation-work/tree/main/leaf_pictures)

The moisture.xlsx contains the mean volumetric water content measures taken in the pots throughout the growth period (mean_moisture column) and the standard deviation measure associated with this mean (sd column). This file also contains the volume of water added to each pot throughout the growth period (irrigation colum), the date associated with each volume, the weight of the pots (these values were not used in the data analysis), as well as the soil type (1 for compost, 2 for compost-vermiculite, and 3 for compost-vermiculte-sand). Pots 4-6 were not included in the data analysis (see code) since they were to be used for an essential oil extraction which did not succeed. Pots number 1 were all under 50% irrigation, Pots number 2 were all under 75% irrigation, and Pots number 3 were all under 100% irrigation level.  

The sample_size file was created in the main code available [here](https://github.com/louise-litrico/dissertation-work/blob/main/code/Dissertation_final_code.R) and contains the sample sizes used to create Appendix B of the main report. The germination rate was calculated by dividing the sample size by 8 (the number of seeds planted at the beginning of the experiment) and multiplying this value by 100 to get a percentage. 
