
library(haven)
library(tidyverse)
library(dplyr)
library(readr)
library(survey)
library(ggplot2)
lvl3 = read_sav('lvl3.sav')
lvl15 = read_sav('lvl15.sav')

common_data = lvl3 %>% subset(select = c(Common_ID))

common_data$hh_size_c = lvl15[lvl15$Questionnaire_No == 'C',]$Household_size 
common_data$hh_size_d = lvl15[lvl15$Questionnaire_No == 'D',]$Household_size 
common_data$hh_size = lvl3$hh_size
lvl14 = read_sav("lvl14.sav")


lvl14 = lvl14 %>% left_join(common_data %>% subset(select = c(Common_ID, hh_size, hh_size_c, hh_size_d)), by = 'Common_ID')

item_365 = as.character(c(409, 419, 899, 379, 399, 389, 629, 609, 619, 599, 579, 559, 569, 639, 649))
item_365 = c(item_365, '099')


item_7 = as.character(c(309, 319, 329, 169, 219, 239, 249, 199, 189, 269, 279, 289, 299))
lvl14[lvl14$Item_Code %in% '539',]$Value = 0

lvl14[lvl14$Item_Code %in% item_365,]$Value = lvl14[lvl14$Item_Code %in% item_365,]$Value * 30/365


lvl14[lvl14$Item_Code %in% item_7,]$Value = lvl14[lvl14$Item_Code %in% item_7,]$Value * 30/7


lvl14[lvl14$Questionnaire_No == 'C',]$Value = lvl14[lvl14$Questionnaire_No == 'C',]$Value * lvl14[lvl14$Questionnaire_No == 'C',]$hh_size/lvl14[lvl14$Questionnaire_No == 'C',]$hh_size_c 
lvl14[lvl14$Questionnaire_No == 'D',]$Value = lvl14[lvl14$Questionnaire_No == 'D',]$Value * lvl14[lvl14$Questionnaire_No == 'D',]$hh_size/lvl14[lvl14$Questionnaire_No == 'D',]$hh_size_d 


# Define features to combine and sum
combine_features14 <- unique(c(lvl14$Item_Code))

# Separate the features to be combined and summed
combined_data14 <- lvl14 %>%
  filter(Item_Code %in% combine_features14) %>%
  group_by(Common_ID) %>%
  dplyr:::summarize(Value = sum(Value), .groups = 'drop') 
common_data$total_lvl14 = combined_data14$Value
common_data$MPCE_lvl14 = common_data$total_lvl14 / common_data$hh_size

new_data = common_data %>% left_join(lvl1, by = "Common_ID")
common_data$Multiplier = new_data$Multiplier
common_data$Sector = new_data$Sector
common_data$State = new_data$State



df_r = common_data[common_data$Sector == '1',]
df_u = common_data[common_data$Sector == '2',]





df_r$Multiplier = df_r$Multiplier/100

df_u$Multiplier = df_u$Multiplier/100



ggplot(df_r[df_r$MPCE_lvl14<20000,], aes(x = MPCE_lvl14, weight = Multiplier)) +
  geom_histogram(binwidth = 500, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of MPCE_lvl14 with Weighted Multiplier", 
       x = "MPCE_lvl14", 
       y = "Weighted Count") +
  theme_minimal()

sum(df_r$MPCE_lvl14*df_r$hh_size*df_r$Multiplier)/sum(df_r$hh_size*df_r$Multiplier)
sum(df_u$MPCE_lvl14*df_u$hh_size*df_u$Multiplier)/sum(df_u$hh_size*df_u$Multiplier)