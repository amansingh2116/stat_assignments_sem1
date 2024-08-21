library(tidyverse)
library(dplyr)
library(readr)
library(survey)
library(ggplot2)

# Load data from CSV files
lvl3 <- read_csv("data_new\\lvl3.csv")
lvl15 <- read_csv("data_new\\lvl15.csv")
lvl14 <- read_csv("data_new\\lvl14.csv")

# Extract the 'Common_ID' column from lvl3
common_data <- lvl3 %>% subset(select = c(Common_ID))

# Add household size columns from lvl15
common_data$hh_size_c <- lvl15[lvl15$Questionnaire_No_ == 'C', ]$Household_size
common_data$hh_size_d <- lvl15[lvl15$Questionnaire_No_ == 'D', ]$Household_size

# Add the household size column from lvl3
common_data$hh_size <- lvl3$`HH_Size_(For_FDQ)`

# Join the household size data with lvl14 based on 'Common_ID'
lvl14 <- lvl14 %>%
  left_join(common_data %>% subset(select = c(Common_ID, hh_size, hh_size_c, hh_size_d)), by = 'Common_ID')

# Define the item codes for different groups
item_365 <- as.character(c(409, 419, 899, 379, 399, 389, 629, 609, 619, 599, 579, 559, 569, 639, 649))
item_365 <- c(item_365, '099')

item_7 <- as.character(c(309, 319, 329, 169, 219, 239, 249, 199, 189, 269, 279, 289, 299))

# Set 'Value' to 0 for specific item code
lvl14[lvl14$Item_Code == '539', ]$`Value_(in_Rs)` <- 0

# Adjust 'Value' for item_365 group
lvl14[lvl14$Item_Code %in% item_365, ]$`Value_(in_Rs)` <- lvl14[lvl14$Item_Code %in% item_365, ]$`Value_(in_Rs)` * 30 / 365

# Adjust 'Value' for item_7 group
lvl14[lvl14$Item_Code %in% item_7, ]$`Value_(in_Rs)` <- lvl14[lvl14$Item_Code %in% item_7, ]$`Value_(in_Rs)` * 30 / 7

# Adjust 'Value' based on household size
lvl14[lvl14$Questionnaire_No. == 'C', ]$`Value_(in_Rs)` <- lvl14[lvl14$Questionnaire_No. == 'C', ]$`Value_(in_Rs)` * lvl14[lvl14$Questionnaire_No. == 'C', ]$hh_size / lvl14[lvl14$Questionnaire_No. == 'C', ]$hh_size_c
lvl14[lvl14$Questionnaire_No. == 'D', ]$`Value_(in_Rs)` <- lvl14[lvl14$Questionnaire_No. == 'D', ]$`Value_(in_Rs)` * lvl14[lvl14$Questionnaire_No. == 'D', ]$hh_size / lvl14[lvl14$Questionnaire_No. == 'D', ]$hh_size_d

# Combine and sum the features
combine_features14 <- unique(lvl14$Item_Code)

# Summarize data
combined_data14 <- lvl14 %>%
  filter(Item_Code %in% combine_features14) %>%
  group_by(Common_ID) %>%
  dplyr::summarize(Value = sum(`Value_(in_Rs)`), .groups = 'drop')

# Calculate MPCE for lvl14
common_data$total_lvl14 <- combined_data14$Value
common_data$MPCE_lvl14 <- common_data$total_lvl14 / common_data$hh_size

# Assuming lvl1 is another data frame that needs to be loaded as well
# Load lvl1 (CSV) and join with common_data based on 'Common_ID'
lvl1 <- read_csv("data_new\\lvl1.csv")
new_data <- common_data %>% left_join(lvl1, by = "Common_ID")

# Add columns from new_data to common_data
common_data$Multiplier <- new_data$Multiplier
common_data$Sector <- new_data$Sector
common_data$State <- new_data$State

# Filter data for rural and urban sectors
df_r <- common_data[common_data$Sector == '1', ]
df_u <- common_data[common_data$Sector == '2', ]

# Adjust Multiplier
df_r$Multiplier <- df_r$Multiplier / 100
df_u$Multiplier <- df_u$Multiplier / 100

# Plot histogram for rural data
ggplot(df_r[df_r$MPCE_lvl14 < 20000,], aes(x = MPCE_lvl14, weight = Multiplier)) +
  geom_histogram(binwidth = 500, fill = "blue", color = "black", alpha = 0.7) +
  scale_x_continuous(labels = scales::comma) +  # This line formats the x-axis
  scale_y_continuous(labels = scales::comma) +  # This line formats the y-axis
  labs(title = "Histogram of MPCE_lvl14 with Weighted Multiplier for rural", 
       x = "MPCE_lvl14", 
       y = "Weighted Count") +
  theme_minimal()

# Plot histogram for urban data
ggplot(df_u[df_u$MPCE_lvl14 < 20000,], aes(x = MPCE_lvl14, weight = Multiplier)) +
  geom_histogram(binwidth = 500, fill = "red", color = "black", alpha = 0.7) +
  scale_x_continuous(labels = scales::comma) +  # This line formats the x-axis
  scale_y_continuous(labels = scales::comma) +  # Format y-axis to normal notation
  labs(title = "Histogram of MPCE_lvl14 for Urban Sector with Weighted Multiplier for urban", 
       x = "MPCE_lvl14", 
       y = "Weighted Count") +
  theme_minimal()


# Overlay Plots

# Combine rural and urban data into one data frame with a sector label
combined_data <- rbind(
  df_r %>% mutate(Sector = "Rural"),
  df_u %>% mutate(Sector = "Urban")
)

# Plot histograms with legend
ggplot(combined_data[combined_data$MPCE_lvl14 < 20000,], aes(x = MPCE_lvl14, weight = Multiplier, fill = Sector)) +
  geom_histogram(binwidth = 500, color = "black", alpha = 0.5) +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis to normal notation
  scale_fill_manual(values = c("Rural" = "blue", "Urban" = "red"), name = "Sector") +
  labs(title = "Overlay Histogram of MPCE_lvl14 with Weighted Multiplier", 
       x = "MPCE_lvl14", 
       y = "Weighted Count") +
  theme_minimal()

# defining weighted mean and variance functions

weighted_median <- function(x, w) {
  o <- order(x)
  x <- x[o]
  w <- w[o]
  cumsum_w <- cumsum(w)
  total_weight <- sum(w)
  median_idx <- which(cumsum_w >= total_weight / 2)[1]
  x[median_idx]
}

weighted.var <- function(x, w) {
  wm <- weighted.mean(x, w)
  sum(w * (x - wm)^2) / sum(w)
}

# Compute summary statistics for rural data
summary_rural <- df_r %>%
  filter(MPCE_lvl14 < 20000) %>%
  summarise(
    Mean = weighted.mean(MPCE_lvl14, Multiplier),
    Median = weighted_median(MPCE_lvl14, Multiplier),
    SD = sqrt(weighted.var(MPCE_lvl14, Multiplier))
  )

# Compute summary statistics for urban data
summary_urban <- df_u %>%
  filter(MPCE_lvl14 < 20000) %>%
  summarise(
    Mean = weighted.mean(MPCE_lvl14, Multiplier),
    Median = weighted_median(MPCE_lvl14, Multiplier),
    SD = sqrt(weighted.var(MPCE_lvl14, Multiplier))
  )

# Print the results
print("Rural Data Summary:")
print(summary_rural)

# "Rural Data Summary:"
#    Mean  Median  SD
#    5362.  4293. 3751.

print("Urban Data Summary:")
print(summary_urban)

# "Urban Data Summary:"
#    Mean  Median   SD
#    5766.  4570. 4084.

# Calculate weighted MPCE

weighted_mpce_rural <- sum(df_r$MPCE_lvl14 * df_r$hh_size * df_r$Multiplier) / sum(df_r$hh_size * df_r$Multiplier)
print("Rural weighted MPCE:")
print(weighted_mpce_rural)
#  4734.548

weighted_mpce_urban <- sum(df_u$MPCE_lvl14 * df_u$hh_size * df_u$Multiplier) / sum(df_u$hh_size * df_u$Multiplier)
print("Urban weighted MPCE:")
print(weighted_mpce_urban)
#  5146.665