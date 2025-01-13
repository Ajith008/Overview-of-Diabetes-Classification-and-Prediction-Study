# Load necessary libraries
library(ggplot2)
library(dplyr)
library(corrplot)

# Load the dataset
data <- read.csv("Dataset.csv")

# Remove leading and trailing spaces from the CLASS column
data$CLASS <- trimws(data$CLASS)

# Verify the changes
table(data$CLASS)


# Standardize column names for clarity
colnames(data) <- c(
  "ID", "Patient_Number", "Gender", "AGE", "Urea", "Creatinine",
  "HbA1c", "Cholesterol", "Triglycerides", "HDL", "LDL",
  "VLDL", "BMI", "CLASS"
)

# Summary of the dataset
print("Structure of the Dataset:")
str(data)

print("Summary of the Dataset:")
summary(data)

# 1. Distribution of numeric columns
# Create histograms for numeric columns
numeric_columns <- c("AGE", "Urea", "Creatinine", "HbA1c", "Cholesterol", "BMI", "HDL", "LDL","VLDL" )

for (col in numeric_columns) {
  p <- ggplot(data, aes_string(x = col)) +
    geom_histogram(fill = "blue", color = "black", bins = 30, alpha = 0.7) +
    geom_density(color = "red", alpha = 0.5) +
    ggtitle(paste("Distribution of", col)) +
    xlab(col) + ylab("Frequency") +
    theme_minimal()
  
  # Explicitly print the plot
  print(p)
}


# Load necessary library
library(ggplot2)

# Create a list of variables to plot
lipid_variables <- c("LDL", "HDL", "VLDL")

# Loop through the lipid variables and create boxplots
for (var in lipid_variables) {
  p <- ggplot(data, aes_string(x = "CLASS", y = var, fill = "CLASS")) +
    geom_boxplot(alpha = 0.7) +
    ggtitle(paste(var, "by CLASS")) +
    xlab("CLASS") +
    ylab(var) +
    theme_minimal()
  
  # Print the plot
  print(p)
}


# 2. Correlation heatmap for numeric columns
numeric_data <- data %>% select(where(is.numeric))
cor_matrix <- cor(numeric_data, use = "complete.obs")
print("Correlation Matrix:")
print(cor_matrix)

pairs.panels(data)

# Visualize the correlation matrix
corrplot(cor_matrix, method = "color", tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7)

# 3. Gender distribution
ggplot(data, aes(x = Gender)) +
  geom_bar(fill = "steelblue") +
  ggtitle("Gender Distribution") +
  xlab("Gender") +
  ylab("Count") +
  theme_minimal()

# 4. Class distribution
ggplot(data, aes(x = CLASS)) +
  geom_bar(fill = "steelblue") +
  ggtitle("Class Distribution") +
  xlab("CLASS") +
  ylab("Count") +
  theme_minimal()

# 5. Boxplots of numeric features grouped by CLASS
for (col in numeric_columns) {
  p <- ggplot(data, aes(x = CLASS, y = !!sym(col), fill = CLASS)) +
    geom_boxplot(alpha = 0.7) +
    ggtitle(paste(col, "by CLASS")) +
    xlab("CLASS") +
    ylab(col) +
    theme_minimal()
  print(p)
}

