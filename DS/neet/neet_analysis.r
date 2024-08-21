data <- read.csv("DS\\neet\\neet_data.csv", header = TRUE, sep = ",")
n = 1048576
score = data[,6]
id = data[,1]
city = data[,3]

hist(score,n=50)

(med = median(score)) # [1] 164

(avg = sum(score)/n) # [1] 217.4961

# Find the median of scores for each unique ID
medians <- aggregate(score, by = list(id), FUN = median)
# Rename the columns for better readability
colnames(medians) <- c("ID", "Median_Score")
# Print the results
print(medians)
# Plot a histogram of the median scores
hist(medians$Median_Score, main="Histogram of Median Scores", xlab="Median Score",
ylab="Frequency", col="blue", breaks=30)

# Find the median of scores for each center ID
medians_by_center <- aggregate(score ~ center_id, data, median)
# Rename the columns for better readability
colnames(medians_by_center) <- c("Center_ID", "Median_Score")
# Calculate the overall median of the center medians
overall_median <- median(medians_by_center$Median_Score)
# Plot a bar plot of the median scores by center ID
barplot(medians_by_center$Median_Score, names.arg = medians_by_center$Center_ID, main =
"Median Scores by Center ID", xlab = "Center ID", ylab = "Median Score", col = "blue", las = 2,
cex.names = 0.8)
# Add a horizontal line representing the overall median of medians
abline(h = overall_median, col = "red", lwd = 2, lty = 2)
# Add a legend
legend("topright", legend = paste("Median of Medians:", round(overall_median, 2)), col = "red", lwd
= 2, lty = 2)


# Calculate the median score for each center
median_scores_by_center <- aggregate(score ~ center_id + city, data, median)
colnames(median_scores_by_center) <- c("Center_ID", "City", "Median_Score_Center")
# Calculate the median score for each city
median_scores_by_city <- aggregate(score ~ city, data, median)
colnames(median_scores_by_city) <- c("City", "Median_Score_City")
# Merge the center and city median scores
merged_data <- merge(median_scores_by_center, median_scores_by_city, by = "City")
# Find the centers whose median score is more than the median score of the city they belong to
centers_above_city_median <- merged_data[merged_data$Median_Score_Center >
1.5*(merged_data$Median_Score_City), ]
# Print the results
print(centers_above_city_median)

#                       City Center_ID Median_Score_Center Median_Score_City
# 9                     AGRA    440108               186.5               115
# 331                BHIWANI    230507               403.0               249
# 395                 BUDAUN    442802               161.0               100
# 400            BULANDSHAHR    442902               177.0               116
# 788                 HISSAR    230614               370.0               235
# 1069 KHARGONE (WEST NIMAR)    302201               133.0                87
# 1717                RAMPUR    445402               155.0               102

# Calculate the frequency of each score
score_frequencies <- table(score)
# Convert the table to a data frame
score_frequencies_df <- as.data.frame(score_frequencies)
colnames(score_frequencies_df) <- c("Score", "Frequency")
# Ensure the columns are numeric
score_frequencies_df$Score <- as.numeric(as.character(score_frequencies_df$Score))
score_frequencies_df$Frequency <- as.numeric(as.character(score_frequencies_df$Frequency))
# Calculate the correlation between scores and their frequencies
correlation <- cor(score_frequencies_df$Score, score_frequencies_df$Frequency)
# Print the correlation
print(paste("Correlation between scores and their frequencies:", correlation))
## "Correlation between scores and their frequencies: -0.475063476702362"

# Create a data frame with score and city columns
data_df <- data.frame(City = city, Score = score)
# Function to calculate correlation for each city
calculate_correlation <- function(df) {
# Calculate the frequency of each score
score_frequencies <- table(df$Score)
# Convert the table to a data frame
score_frequencies_df <- as.data.frame(score_frequencies)
colnames(score_frequencies_df) <- c("Score", "Frequency")
# Ensure the columns are numeric
score_frequencies_df$Score <- as.numeric(as.character(score_frequencies_df$Score))
score_frequencies_df$Frequency <- as.numeric(as.character(score_frequencies_df$Frequency))
# Calculate the correlation between scores and their frequencies
correlation <- cor(score_frequencies_df$Score, score_frequencies_df$Frequency)
return(correlation)
}
# Apply the function to each city and store results
library(dplyr)
correlations_by_city <- data_df %>%
group_by(City) %>%
summarize(Correlation = calculate_correlation(cur_data())) %>%
arrange(Correlation) # Arrange in increasing order of correlation
# Print the results
print(correlations_by_city, n=370)
# only TANUKU correlation was found positive 0.0486


# Load necessary packages
library(ggplot2)
# Extract necessary columns
score <- data[,6]
center_id <- data[,1] # Assuming center ID is in the 1st column
# Create a dataframe to store results
results <- data.frame(Center_ID = unique(center_id), Mean = NA, SD = NA)
# Loop through each center to fit a normal distribution
for (center in unique(center_id)) {
# Filter data for the current center
center_data <- data[data$center_id == center,]
# Fit a normal distribution
fit <- fitdistr(center_data$score, "normal")
# Store the mean and standard deviation
results[results$Center_ID == center, "Mean"] <- fit$estimate["mean"]
results[results$Center_ID == center, "SD"] <- fit$estimate["sd"]
# Plot the histogram and normal distribution fit
hist(center_data$score, probability = TRUE, main = paste("Histogram of Scores for Center", center),
xlab = "Score", ylab = "Density", col = "lightblue", breaks = 30)
curve(dnorm(x, mean = fit$estimate["mean"], sd = fit$estimate["sd"]), add = TRUE, col = "red")
}
# Display the results
print(results)
