# Assuming you've already loaded the necessary libraries and read the data
library(readxl)
library(readr)
library(dplyr)
library(haven)

layout <- read_excel("layouts\\Layout_HCES_Level1.xlsx", sheet = 1)

print(colnames(layout))
print(head(layout))

read_data_file <- function(file_path, layout) {
  col_positions <- fwf_positions(start = layout$start, end = layout$end, col_names = layout$col_names)
  data <- read_fwf(file_path, col_positions)
  return(data)
}

file <- "data\\hces22_lvl_01.TXT"

data <- read_data_file(file, layout)
lvl15 = data

# Save the data frame as a CSV file
write.csv(lvl15, "data_new\\lvl1.csv", row.names = FALSE)
