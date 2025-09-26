library(tidyr)
library(dplyr)


# Read the Excel data. Assuming your excel sheet name is "Table 6"
foodbook_raw <- readxl::read_excel(
  here::here(
    "data",
    "Toolkit-binomial-probability-calculation-tool-2.0.xlsx"
  ),
  sheet = "Table 6",
  skip = 1
)

# Remove the first column (row numbers)
foodbook_raw <- foodbook_raw[, -1]

# Reshape the data into tidy format, excluding rows with no "Canada" value
foodbook_data <- foodbook_raw %>%
  pivot_longer(cols = -Exposure, 
               names_to = "Province.Territory", 
               values_to = "Proportion") %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  filter(!is.na(Proportion))  #Remove any rows that might have been created due to merged titles


#Final checks
foodbook_data$Proportion <- as.numeric(foodbook_data$Proportion)  # Ensure Proportion is numeric
foodbook_data$Proportion[is.na(foodbook_data$Proportion)] <- 0    # Replace NA with 0

# Print the head of the data to check
print(head(foodbook_data))

# Save the tidy data to a CSV file
write.csv(foodbook_data, here::here("data", "foodbook_data.csv"), row.names = FALSE)

skimr::skim(foodbook_data)