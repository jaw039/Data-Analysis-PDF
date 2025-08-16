# ==============================================================
#               Data Loading and Initial Exploration
# ==============================================================

# Load the dataset
babies_data <- read.table("babies.txt", header = TRUE)

# Display the first few rows of the data to verify loading was successful
head(babies_data)

# Examine the structure of the dataset to see variable types and missing data
str(babies_data)

# Generate summary statistics for the dataset to get an overview
summary(babies_data)

# ==============================================================
#             Exploratory Data Analysis (EDA) - Histograms
# ==============================================================

# Set up a 2x3 grid for plotting multiple histograms
par(mfrow = c(2, 3))

# Plot histogram for Birth Weight
# Visualizes the distribution of birth weights in ounces
hist(babies_data$bwt, 
     main = "Histogram of Birth Weight", 
     xlab = "Birth Weight (oz)", 
     col = "lightblue", 
     breaks = sqrt(length(babies_data$bwt)))

# Plot histogram for Gestation Length
# Visualizes the distribution of gestation length in days
hist(babies_data$gestation, 
     main = "Histogram of Gestation Length", 
     xlab = "Gestation Length (days)", 
     col = "lightgreen", 
     breaks = sqrt(length(babies_data$gestation)))

# Plot histogram for Maternal Age
# Visualizes the distribution of the age of mothers
hist(babies_data$age, 
     main = "Histogram of Mother's Age", 
     xlab = "Age (years)", 
     col = "lightpink", 
     breaks = sqrt(length(babies_data$age)))

# Plot histogram for Maternal Height
# Visualizes the distribution of the mothers' height in inches
hist(babies_data$height, 
     main = "Histogram of Mother's Height", 
     xlab = "Height (inches)", 
     col = "lightyellow", 
     breaks = sqrt(length(babies_data$height)))

# Plot histogram for Maternal Weight
# Visualizes the distribution of the mothers' weight in pounds
hist(babies_data$weight, 
     main = "Histogram of Mother's Weight", 
     xlab = "Weight (lbs)", 
     col = "lightcoral", 
     breaks = sqrt(length(babies_data$weight)))

# ==============================================================
#               Data Cleaning - Handling Missing Values and Outliers
# ==============================================================

# Check for missing values in the dataset
# This will calculate the total number of missing values (NA) in each column
na_counts <- colSums(is.na(babies_data))
print(na_counts)  # Display the number of missing values per column

# --------------------------------------------------------------
# Handle Missing Values
# --------------------------------------------------------------

# Remove rows with any NA values
# This function omits rows that contain missing values in any column
cleaned_df <- na.omit(babies_data)

# Print the number of rows after cleaning missing values
cat("Number of rows after removing missing values:", nrow(cleaned_df), "\n")

# --------------------------------------------------------------
# Remove Outliers
# --------------------------------------------------------------

# Removing outliers with erroneous placeholder values (e.g., 999 or 99) in specific columns
cleaned_df <- cleaned_df[cleaned_df$bwt != 999, ]         # Remove invalid birth weights
cleaned_df <- cleaned_df[cleaned_df$gestation != 999, ]   # Remove invalid gestation values
cleaned_df <- cleaned_df[cleaned_df$age != 99, ]          # Remove invalid maternal age
cleaned_df <- cleaned_df[cleaned_df$height != 99, ]       # Remove invalid maternal height
cleaned_df <- cleaned_df[cleaned_df$weight != 999, ]      # Remove invalid maternal weight

# Print the number of rows after cleaning outliers
cat("Number of rows after removing outliers:", nrow(cleaned_df), "\n")

# --------------------------------------------------------------
# Final Summary of the Cleaned Dataset
# --------------------------------------------------------------

# Print the summary statistics of the cleaned dataset to verify data integrity
summary(cleaned_df)

# ==============================================================
#               Exploratory Data Analysis (EDA) - Histograms
# ==============================================================

# Set up a 2x3 grid for plotting multiple histograms on one page
par(mfrow = c(2, 3))

# --------------------------------------------------------------
# Plot Histogram for Birth Weight (Cleaned Data)
# --------------------------------------------------------------
# Visualize the distribution of birth weights (in ounces) after cleaning the data
hist(cleaned_df$bwt, 
     main = "Histogram of Cleaned Birth Weight", 
     xlab = "Birth Weight (oz)", 
     col = "lightblue", 
     breaks = sqrt(length(cleaned_df$bwt)))

# --------------------------------------------------------------
# Plot Histogram for Gestation Length (Cleaned Data)
# --------------------------------------------------------------
# Visualize the distribution of gestation lengths (in days) after cleaning the data
hist(cleaned_df$gestation, 
     main = "Histogram of Cleaned Gestation Length", 
     xlab = "Gestation Length (days)", 
     col = "lightgreen", 
     breaks = sqrt(length(cleaned_df$gestation)))

# --------------------------------------------------------------
# Plot Histogram for Maternal Age (Cleaned Data)
# --------------------------------------------------------------
# Visualize the distribution of maternal age (in years) after cleaning the data
hist(cleaned_df$age, 
     main = "Histogram of Cleaned Mother's Age", 
     xlab = "Age (years)", 
     col = "lightpink", 
     breaks = sqrt(length(cleaned_df$age)))

# --------------------------------------------------------------
# Plot Histogram for Maternal Height (Cleaned Data)
# --------------------------------------------------------------
# Visualize the distribution of maternal height (in inches) after cleaning the data
hist(cleaned_df$height, 
     main = "Histogram of Cleaned Mother's Height", 
     xlab = "Height (inches)", 
     col = "lightyellow", 
     breaks = sqrt(length(cleaned_df$height)))

# --------------------------------------------------------------
# Plot Histogram for Maternal Weight (Cleaned Data)
# --------------------------------------------------------------
# Visualize the distribution of maternal weight (in pounds) after cleaning the data
hist(cleaned_df$weight, 
     main = "Histogram of Cleaned Mother's Weight", 
     xlab = "Weight (lbs)", 
     col = "lightcoral", 
     breaks = sqrt(length(cleaned_df$weight)))

# ==============================================================
#             End of Exploratory Data Analysis (EDA)
# ==============================================================

# ==============================================================
#   Analysis of Birth Weight for Smokers and Non-Smokers
# ==============================================================

# --------------------------------------------------------------
# Subset the data for smokers and non-smokers
# --------------------------------------------------------------
smokers <- subset(cleaned_df, smoke == 1)
non_smokers <- subset(cleaned_df, smoke == 0)

# --------------------------------------------------------------
# a. Minimum and Maximum Values for Birth Weight
# --------------------------------------------------------------
# Calculate minimum and maximum birth weights for smokers and non-smokers
smoker_min_bwt <- min(smokers$bwt, na.rm = TRUE)
smoker_max_bwt <- max(smokers$bwt, na.rm = TRUE)
nonsmoker_min_bwt <- min(non_smokers$bwt, na.rm = TRUE)
nonsmoker_max_bwt <- max(non_smokers$bwt, na.rm = TRUE)

# Print Minimum and Maximum Values
cat("Smokers - Min Birth Weight:", smoker_min_bwt, 
    "\nSmokers - Max Birth Weight:", smoker_max_bwt, 
    "\nNon-Smokers - Min Birth Weight:", nonsmoker_min_bwt, 
    "\nNon-Smokers - Max Birth Weight:", nonsmoker_max_bwt, "\n")

# --------------------------------------------------------------
# b. Mean Birth Weight
# --------------------------------------------------------------
# Calculate mean birth weights for smokers and non-smokers
smoker_mean_bwt <- mean(smokers$bwt, na.rm = TRUE)
nonsmoker_mean_bwt <- mean(non_smokers$bwt, na.rm = TRUE)

# --------------------------------------------------------------
# c. Median Birth Weight
# --------------------------------------------------------------
# Calculate median birth weights for smokers and non-smokers
smoker_median_bwt <- median(smokers$bwt, na.rm = TRUE)
nonsmoker_median_bwt <- median(non_smokers$bwt, na.rm = TRUE)

# --------------------------------------------------------------
# Plot Histograms for Smokers and Non-Smokers
# --------------------------------------------------------------
# Set up side-by-side histograms
par(mfrow = c(1, 2))

# Histogram for Smokers
hist(smokers$bwt, 
     main = "Histogram of BWT - Smokers", 
     xlab = "Birth Weight (BWT)", 
     col = "lightyellow", 
     breaks = sqrt(length(smokers$bwt)),
     xlim = range(c(smokers$bwt, non_smokers$bwt))) 

# Histogram for Non-Smokers
hist(non_smokers$bwt, 
     main = "Histogram of BWT - Non-Smokers", 
     xlab = "Birth Weight (BWT)", 
     col = "lightblue", 
     breaks = sqrt(length(non_smokers$bwt)),
     xlim = range(c(smokers$bwt, non_smokers$bwt)))

# Print Mean and Median Values for Birth Weight
cat("Smokers - Mean Birth Weight:", smoker_mean_bwt, 
    "\nNon-Smokers - Mean Birth Weight:", nonsmoker_mean_bwt,
    "\nSmokers - Median Birth Weight:", smoker_median_bwt, 
    "\nNon-Smokers - Median Birth Weight:", nonsmoker_median_bwt, "\n")

# --------------------------------------------------------------
# d. Quartiles for Birth Weight
# --------------------------------------------------------------
# Calculate quartiles for smokers and non-smokers
# Smokers
smoker_q1_bwt <- quantile(smokers$bwt, 0.25, na.rm = TRUE)
smoker_q2_bwt <- quantile(smokers$bwt, 0.5, na.rm = TRUE)  # Median
smoker_q3_bwt <- quantile(smokers$bwt, 0.75, na.rm = TRUE)

# Non-Smokers
nonsmoker_q1_bwt <- quantile(non_smokers$bwt, 0.25, na.rm = TRUE)
nonsmoker_q2_bwt <- quantile(non_smokers$bwt, 0.5, na.rm = TRUE)  # Median
nonsmoker_q3_bwt <- quantile(non_smokers$bwt, 0.75, na.rm = TRUE)

# Print Quartile Values for Smokers
cat("Smokers - Q1 Birth Weight:", smoker_q1_bwt, 
    "\nSmokers - Q2 (Median) Birth Weight:", smoker_q2_bwt,
    "\nSmokers - Q3 Birth Weight:", smoker_q3_bwt, "\n")

# Print Quartile Values for Non-Smokers
cat("Non-Smokers - Q1 Birth Weight:", nonsmoker_q1_bwt, 
    "\nNon-Smokers - Q2 (Median) Birth Weight:", nonsmoker_q2_bwt,
    "\nNon-Smokers - Q3 Birth Weight:", nonsmoker_q3_bwt, "\n")

# --------------------------------------------------------------
# e. Standard Deviation for Birth Weight
# --------------------------------------------------------------
# Calculate standard deviation for smokers and non-smokers
smoker_std_bwt <- sd(smokers$bwt, na.rm = TRUE)
nonsmoker_std_bwt <- sd(non_smokers$bwt, na.rm = TRUE)

# Print Standard Deviation Values
cat("Smokers - Standard Deviation of Birth Weight:", smoker_std_bwt,
    "\nNon-Smokers - Standard Deviation of Birth Weight:", nonsmoker_std_bwt, "\n")

# ==============================================================
#             End of Birth Weight Analysis
# ==============================================================

# ==============================================================
#   Analysis of Low Birth Weight (Under 100 Ounces) for Smokers and Non-Smokers
# ==============================================================

# --------------------------------------------------------------
# Subset the data for smokers and non-smokers
# --------------------------------------------------------------
smokers <- subset(cleaned_df, smoke == 1)
non_smokers <- subset(cleaned_df, smoke == 0)

# --------------------------------------------------------------
# a. Percentage of Babies Weighing Under 100 Ounces for Smokers
# --------------------------------------------------------------
# Calculate percentage of babies born to smokers weighing less than 100 ounces
low_bwt_smoker <- mean(smokers$bwt < 100) * 100

# Print the percentage of low birth weight babies for smokers
cat("Percentage of Low Birth Weight Babies for Smokers (under 100 oz):", low_bwt_smoker, "%\n")

# --------------------------------------------------------------
# b. Percentage of Babies Weighing Under 100 Ounces for Non-Smokers
# --------------------------------------------------------------
# Calculate percentage of babies born to non-smokers weighing less than 100 ounces
low_bwt_nonsmoker <- mean(non_smokers$bwt < 100) * 100

# Print the percentage of low birth weight babies for non-smokers
cat("Percentage of Low Birth Weight Babies for Non-Smokers (under 100 oz):", low_bwt_nonsmoker, "%\n")

# --------------------------------------------------------------
# c. Percentage of Babies Weighing Under 105 Ounces
# --------------------------------------------------------------
# Calculate percentage of babies born to smokers and non-smokers weighing less than 105 ounces
low_bwt_smoker2 <- mean(smokers$bwt < 105) * 100
low_bwt_nonsmoker2 <- mean(non_smokers$bwt < 105) * 100

# Print the percentage of low birth weight babies for smokers and non-smokers (under 105 oz)
cat("Percentage of Low Birth Weight Babies for Smokers (under 105 oz):", low_bwt_smoker2, "%\n")
cat("Percentage of Low Birth Weight Babies for Non-Smokers (under 105 oz):", low_bwt_nonsmoker2, "%\n")

# --------------------------------------------------------------
# d. Percentage of Babies Weighing Under 95 Ounces
# --------------------------------------------------------------
# Calculate percentage of babies born to smokers and non-smokers weighing less than 95 ounces
low_bwt_smoker3 <- mean(smokers$bwt < 95) * 100
low_bwt_nonsmoker3 <- mean(non_smokers$bwt < 95) * 100

# Print the percentage of low birth weight babies for smokers and non-smokers (under 95 oz)
cat("Percentage of Low Birth Weight Babies for Smokers (under 95 oz):", low_bwt_smoker3, "%\n")
cat("Percentage of Low Birth Weight Babies for Non-Smokers (under 95 oz):", low_bwt_nonsmoker3, "%\n")

# ==============================================================
#             End of Low Birth Weight Analysis
# ==============================================================


# ==============================================================
#   Kurtosis Analysis for Birth Weights - Smokers vs Non-Smokers
# ==============================================================

# --------------------------------------------------------------
# Subset the data for smokers and non-smokers
# --------------------------------------------------------------
smokers <- subset(cleaned_df, smoke == 1)
non_smokers <- subset(cleaned_df, smoke == 0)

# --------------------------------------------------------------
# Custom Kurtosis Function
# --------------------------------------------------------------
# Define a custom function to calculate kurtosis
kurtosis <- function(x) {
  n <- length(x)
  m4 <- mean((x - mean(x))^4)
  m2 <- mean((x - mean(x))^2)
  # Adjust kurtosis formula to ensure normal distribution has kurtosis = 3
  kurt <- (n-1) / ((n-2)*(n-3)) * ((n+1)*m4 / m2^2 - 3*(n-1))
  return(kurt + 3)  # Adding 3 to match standard definition
}

# --------------------------------------------------------------
# Calculate Kurtosis for Smokers and Non-Smokers
# --------------------------------------------------------------
kurtosis_smokers <- kurtosis(smokers$bwt)
kurtosis_nonsmokers <- kurtosis(non_smokers$bwt)

# Print the calculated kurtosis values
cat("Kurtosis for smokers' babies birth weights:", kurtosis_smokers, "\n")
cat("Kurtosis for non-smokers' babies birth weights:", kurtosis_nonsmokers, "\n")

# --------------------------------------------------------------
# Visualize Birth Weight Distributions (Histograms and Boxplot)
# --------------------------------------------------------------
par(mfrow = c(2, 2))  # Set up a 2x2 plotting layout

# Histogram for Smokers
hist(smokers$bwt, 
     main = "Birth Weights - Smokers", 
     xlab = "Birth Weight")

# Histogram for Non-Smokers
hist(non_smokers$bwt, 
     main = "Birth Weights - Non-Smokers", 
     xlab = "Birth Weight")

# Boxplot for Comparing Birth Weights
boxplot(smokers$bwt, non_smokers$bwt, 
        names = c("Smokers", "Non-Smokers"), 
        main = "Birth Weight Distribution by Smoking Status")

# --------------------------------------------------------------
# Permutation Test for Difference in Kurtosis
# --------------------------------------------------------------
n_permutations <- 1000  # Number of permutations
observed_diff <- kurtosis_smokers - kurtosis_nonsmokers  # Observed difference in kurtosis
permutation_diffs <- numeric(n_permutations)  # Store permutation differences

set.seed(123)  # Ensure reproducibility

# Permutation test loop
for(i in 1:n_permutations) {
  permuted_smoke <- sample(cleaned_df$smoke)  # Randomly permute smoke status
  perm_smokers <- cleaned_df$bwt[permuted_smoke == 1]
  perm_nonsmokers <- cleaned_df$bwt[permuted_smoke == 0]
  permutation_diffs[i] <- kurtosis(perm_smokers) - kurtosis(perm_nonsmokers)
}

# Calculate p-value (two-tailed test)
p_value <- mean(abs(permutation_diffs) >= abs(observed_diff))
cat("Permutation test p-value:", p_value, "\n")

# --------------------------------------------------------------
# Store Results for Reporting
# --------------------------------------------------------------
# Store the kurtosis results in a list for easy access in the report
kurtosis_results <- list(
  kurtosis_smokers = kurtosis_smokers,
  kurtosis_nonsmokers = kurtosis_nonsmokers,
  observed_diff = observed_diff,
  p_value = p_value
)

# Print the stored results (optional)
print(kurtosis_results)

# ==============================================================
#             End of Kurtosis Analysis
# ==============================================================



