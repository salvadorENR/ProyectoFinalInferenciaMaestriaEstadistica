# Load necessary libraries
library(dplyr)

# Step 1: Load the dataset
# Replace 'house_prices.csv' with the correct file path if it's in a specific directory
data <- read.csv("house_prices.csv", stringsAsFactors = FALSE)

# Step 2: Check for missing values and identify variables without missing values
variables_no_missing <- colnames(data)[colSums(is.na(data)) == 0]

# Step 3: Classify variables into categorical, numerical, or neither
variable_types <- sapply(data, function(x) {
  if (is.factor(x) || is.character(x)) {
    "Categorical"
  } else if (is.numeric(x)) {
    "Numerical"
  } else {
    "Neither"
  }
})

# Step 4: Filter only variables without missing values
variable_summary <- data.frame(
  Variable = colnames(data),
  Type = variable_types,
  Missing_Values = colSums(is.na(data))
) %>%
  filter(Missing_Values == 0) %>%
  select(Variable, Type)

# Step 5: Display the results
cat("Summary of variables without missing values:\n")
print(variable_summary)

# Optional: Save the summary to a CSV file
write.csv(variable_summary, "variables_no_missing_summary.csv", row.names = FALSE)
