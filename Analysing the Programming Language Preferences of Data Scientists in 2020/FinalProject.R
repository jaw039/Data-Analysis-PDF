# ================================
# Setup
# ================================
# Load the dataset
knitr::opts_chunk$set(echo = TRUE)
data <- read.csv("kaggle_survey_2020_responses.csv", header = TRUE)

# ================================
# 2.1 Programming Language Usage and Educational Background Analysis
# ================================
# Function to process columns that contain specific question prefixes
process_matching_columns <- function(data, question_prefixes) {
  # Combine prefixes into a single regex pattern
  pattern <- paste(question_prefixes, collapse = "|")
  
  # Get column names that contain any of the prefixes
  columns_to_process <- grep(pattern, colnames(data), value = TRUE)
  
  if (length(columns_to_process) == 0) {
    stop(paste("No columns found with the specified prefixes:", paste(question_prefixes, collapse = ", ")))
  }
  
  # Process each matching column
  for (column_name in columns_to_process) {
    data[[column_name]][-1] <- ifelse(
      trimws(data[[column_name]][-1]) != "", 1, 0
    )
  }
  
  return(data)
}

# Specify the prefixes to match
question_prefixes <- c("Q7", "Q9", "Q10", "Q12", "Q14", "Q16", "Q17", "Q18", "Q19" , "Q23", "Q26", "Q27", "Q28", "Q29", "Q31", "Q33", "Q34", "Q35")

# Process all columns that contain these prefixes
data <- process_matching_columns(data, question_prefixes)

data <- data[, !names(data) %in% data]
# To save the updated dataset
write.csv(data, "kaggle_survey_2020_responses_with_binary.csv", row.names = FALSE)

# Function to create improved visualization
plot_regional_distribution <- function(data) {
  # Remove the header row
  data <- data[-1,]
  
  # Group countries into regions
  regions <- list(
    "North America" = c("United States of America", "Canada"),
    "Europe" = c("United Kingdom", "Germany", "France", "Spain", "Italy", 
                 "Netherlands", "Poland", "Russian Federation"),
    "Asia" = c("India", "China", "Japan", "South Korea", "Taiwan", "Singapore"),
    "Other" = NULL  # Catch-all for countries not listed
  )
  
  # Create region mapping function
  get_region <- function(country) {
    for (region_name in names(regions)) {
      if (!is.null(regions[[region_name]]) && country %in% regions[[region_name]]) {
        return(region_name)
      }
    }
    return("Other")
  }
  
  # Add region column
  data$Region <- sapply(data$Q3, get_region)
  
  # Check unique job roles and include missing roles explicitly
  all_job_roles <- c(
    "Business Analyst", "Currently not employed", "Data Analyst",
    "Data Engineer", "Data Scientist", "DBA/Database Engineer",
    "Machine Learning Engineer", "Other", "Product/Project Manager",
    "Research Scientist", "Software Engineer", "Statistician", "Student"
  )
  
  # Get job role counts by region
  job_counts <- table(factor(data$Region, levels = names(regions)),
                      factor(data$Q5, levels = all_job_roles))
  
  # Calculate percentages
  job_percentages <- prop.table(job_counts, margin = 1) * 100
  
  # Define a custom color palette
  colors <- c(
    "#1b9e77",  # Green
    "#d95f02",  # Orange
    "#7570b3",  # Purple
    "#e7298a",  # Pink
    "#66a61e",  # Lime
    "#e6ab02",  # Yellow
    "#a6761d",  # Brown
    "#666666",  # Gray
    "#1f78b4",  # Blue
    "#6a3d9a",  # Dark Purple
    "#b15928",  # Dark Brown
    "#ff7f00",  # Bright Orange
    "#33a02c"   # Bright Green
  )
  
  # Create the first plot (percentages)
  par(mar = c(10, 4, 4, 2))
  barplot(t(job_percentages),
          beside = TRUE,
          col = colors,
          main = "Regional Distribution of Job Roles (Percentage)",
          ylab = "Percentage",
          las = 2,
          cex.names = 0.7,
          ylim = c(0, max(job_percentages, na.rm = TRUE) * 1.2))
  
  # Add legend with all job roles
  legend("topright",
         legend = all_job_roles,
         fill = colors,
         cex = 0.6)
  
  # Add horizontal reference lines
  abline(h = seq(0, max(job_percentages, na.rm = TRUE), by = 5), 
         col = "gray", 
         lty = "dotted")
  
  # Create the second plot (counts)
  par(mar = c(10, 4, 4, 2))
  uniform_value <- mean(as.vector(job_counts), na.rm = TRUE)
  barplot(t(job_counts),
          beside = TRUE,
          col = colors,
          main = "Distribution of Job Roles vs Uniform Distribution",
          ylab = "Count",
          las = 2,
          cex.names = 0.7,
          ylim = c(0, max(job_counts, na.rm = TRUE) * 1.2))
  
  # Add uniform distribution reference line
  abline(h = uniform_value, 
         col = "red", 
         
         lty = "dashed")
  # Add legend with all job roles
  legend("topright",
         legend = 'Uniform Distribution',
         fill = 'red',
         cex = 0.6)
  
  # Reset plotting parameters
  par(mar = c(5, 4, 4, 2))
}

# Run the visualization
plot_regional_distribution(data)


# ================================
# 2.2 Regional Distribution of Programming Language Preferences
# ================================
# Remove header row
clean_data <- data[-1, ]
clean_data <- clean_data[!is.na(clean_data$Q4) & clean_data$Q4 != "", ]

# Get language columns
language_cols <- grep("^Q7", names(clean_data), value = TRUE)

# Aggregate by education level
education_levels <- unique(clean_data$Q4)
language_usage <- matrix(0, nrow = length(education_levels), 
                         ncol = length(language_cols))
total_respondents <- numeric(length(education_levels))

# Calculate totals for each education level
for(i in seq_along(education_levels)) {
  edu_subset <- clean_data[clean_data$Q4 == education_levels[i], ]
  total_respondents[i] <- nrow(edu_subset)
  
  for(j in seq_along(language_cols)) {
    language_usage[i, j] <- sum(as.numeric(edu_subset[[language_cols[j]]]), 
                                na.rm = TRUE)
  }
}

# Create data frame with results
education_language_usage <- data.frame(
  Q4 = education_levels,
  total_respondents = total_respondents
)
education_language_usage[language_cols] <- language_usage

legend_cols <- c('Python', 'R', 'SQL', 'C', 'C++', 'Java','Javascript','Julia','Swift','Bash','MATLAB','None', 'Other')

# Plot language usage
barplot(t(as.matrix(education_language_usage[, language_cols])),
        beside = TRUE,
        col = rainbow(length(language_cols)),
        names.arg = education_language_usage$Q4,
        legend.text = legend_cols,
        main = "Programming Language Usage by Education Level",
        las = 2,
        cex.names = 0.7)

# Calculate average languages per person using respective education level counts
avg_languages <- sapply(1:nrow(education_language_usage), function(i) {
  sum(as.numeric(education_language_usage[i, language_cols])) / 
    education_language_usage$total_respondents[i]
})

names(avg_languages) <- education_language_usage$Q4

barplot(avg_languages,
        names.arg = education_levels,
        main = "Average Number of Languages Used by Education Level",
        las = 2,
        cex.names = 0.7,
        col = "skyblue",
        ylab = "Average Number of Languages")
# Create contingency table for chi-square test
contingency_table <- matrix(0, 
                            nrow = length(unique(clean_data$Q4)), 
                            ncol = length(language_cols))

rownames(contingency_table) <- unique(clean_data$Q4)
colnames(contingency_table) <- gsub("Q7_Part_", "", language_cols)

# Fill contingency table
for(i in seq_along(rownames(contingency_table))) {
  edu_level <- rownames(contingency_table)[i]
  edu_subset <- clean_data[clean_data$Q4 == edu_level, ]
  
  for(j in seq_along(language_cols)) {
    contingency_table[i,j] <- sum(as.numeric(edu_subset[[language_cols[j]]]), 
                                  na.rm = TRUE)
  }
}

# Perform chi-square test
chi_square_result <- chisq.test(contingency_table)

# Create nicely formatted tables for chi-square results
chi_square_table <- knitr::kable(data.frame(
  Statistic = chi_square_result$statistic,
  'Degrees of Freedom' = chi_square_result$parameter,
  'P-value' = chi_square_result$p.value
), caption = "Chi-Square Test Results")


# Convert residuals to a data frame and include row names as a column
residuals_data <- as.data.frame(chi_square_result$residuals)
colnames(residuals_data) <- c('Python', 'R', 'SQL', 'C', 'C++', 'Java',
                              'Javascript', 'Julia', 'Swift', 'Bash',
                              'MATLAB', 'None', 'Other')
residuals_data <- cbind(Category = rownames(residuals_data), residuals_data)

# Ensure row names are removed after being added as a column
rownames(residuals_data) <- NULL

# Create a properly formatted table for residuals
residuals_table <- knitr::kable(
  residuals_data[1:7],
  caption = "Contribution to Chi-Square (Standardized Residuals)",
  digits = 2,
  align = c("l", rep("c", ncol(residuals_data) - 1))
)

# Create a properly formatted table for residuals
residuals_table2 <- knitr::kable(
  residuals_data[8:14],
  caption = "Continuation Contribution to Chi-Square (Standardized Residuals)",
  digits = 2,
  align = c("l", rep("c", ncol(residuals_data) - 1))
)


# Print results
print(chi_square_result)

residuals_table
residuals_table2

# ================================
# 2.3 Early Career Programming Language Adoption Patterns
# ================================
# Get programming library columns (Q14)
lib_cols <- paste0("Q14_Part_", 1:11)
lib_names <- c("Matplotlib", "Seaborn", "Plotly / Plotly Express", "Ggplot / ggplot2", "Shiny", "D3js", "Altair", 
               "Bokeh", "Geoplotlib", "Leaflet / Folium", "None")

# Create matrix of library usage
lib_matrix <- matrix(0, nrow=nrow(clean_data), ncol=length(lib_cols))
for(i in 1:length(lib_cols)) {
  lib_matrix[,i] <- as.numeric(as.character(clean_data[[lib_cols[i]]]))
}
colnames(lib_matrix) <- lib_names

# Calculate average libraries by education level
avg_libs <- tapply(rowSums(lib_matrix), clean_data$Q4, mean, na.rm=TRUE)
avg_libs_df <- data.frame(
  Education = names(avg_libs),
  Average = as.numeric(avg_libs)
)

# Sort by average number of libraries
avg_libs_df <- avg_libs_df[order(-avg_libs_df$Average),]

# Calculate proportion of each library by education level
edu_levels <- unique(clean_data$Q4)
edu_lib_props <- matrix(0, nrow=length(edu_levels), ncol=length(lib_names))
rownames(edu_lib_props) <- edu_levels
colnames(edu_lib_props) <- lib_names

for(i in 1:length(edu_levels)) {
  edu_rows <- clean_data$Q4 == edu_levels[i]
  edu_lib_props[i,] <- colMeans(lib_matrix[edu_rows,], na.rm=TRUE) * 100
}

# Set up the plot
par(mar=c(10,15,4,2))  # Increased left margin for education labels

# Create heatmap
image(1:ncol(edu_lib_props), 
      1:nrow(edu_lib_props), 
      t(edu_lib_props),
      col=hcl.colors(100, "Blues", rev=TRUE),
      xlab="",
      ylab="",
      main="Library Usage by Education Level (%)",
      axes=FALSE)

# Add axes with rotated labels
axis(1, 1:ncol(edu_lib_props), colnames(edu_lib_props), las=2, cex.axis=0.8)
axis(2, 1:nrow(edu_lib_props), rownames(edu_lib_props), las=2, cex.axis=0.8)

# Add percentages text to heatmap
for(i in 1:nrow(edu_lib_props)) {
  for(j in 1:ncol(edu_lib_props)) {
    if(edu_lib_props[i,j] > 1) {  # Only show if > 1%
      text(j, i, sprintf("%.0f%%", edu_lib_props[i,j]), 
           col=ifelse(edu_lib_props[i,j] > 50, "white", "black"))
    }
  }
}

# Reset plot parameters
par(mfrow=c(1,1), mar=c(5,4,4,2))

# Create table for average libraries by education level
avg_libs_table <- knitr::kable(
  avg_libs_df,
  col.names = c("Education Level", "Average Number of Libraries"),
  caption = "Average Number of Libraries by Education Level",
  digits = 2
)

# Create a separate longer format data frame for top libraries
top_libs_long <- data.frame(
  Education = character(),
  Rank = character(),
  Library = character(),
  Percentage = numeric(),
  stringsAsFactors = FALSE
)

for(i in 1:nrow(edu_lib_props)) {
  top_libs <- sort(edu_lib_props[i, ], decreasing = TRUE)[1:3]
  for(j in 1:3) {
    top_libs_long <- rbind(top_libs_long, data.frame(
      Education = rownames(edu_lib_props)[i],
      Rank = paste(j),
      Library = names(top_libs)[j],
      Percentage = top_libs[j]
    ))
  }
}

# Generate the longer format table
long_libs_table <- knitr::kable(
  top_libs_long,
  col.names = c("Education Level", "Rank", "Library", "Usage %"),
  caption = "Top Libraries by Education Level (Longer Format)",
  digits = 1,
  align = c('l', 'l', 'l', 'r'),
  row.names = FALSE  # This removes the row names column
)

# Display both tables
avg_libs_table
long_libs_table
# ================================
# 2.4 Early Career Programming Language Patterns
# ================================
# Remove header and filter early-career professionals
data <- data[-1,]
early_career <- data[data$Q6 %in% c("< 1 years", "1-2 years", "2-5 years"), ]

# Extract and process language data
lang_cols <- paste0("Q7_Part_", 1:12)
lang_matrix <- as.matrix(early_career[, lang_cols])
mode(lang_matrix) <- "numeric"

# Name the columns
colnames(lang_matrix) <- c("Python", "R", "SQL", "C", "C++", "Java", 
                           "Javascript", "Julia", "Swift", "Bash", 
                           "MATLAB", "None")

# Perform k-means clustering
set.seed(123)
kmeans_result <- kmeans(lang_matrix, centers=5, nstart=25)

# Create the heatmap
par(mar=c(8, 4, 4, 2))  # Adjust margins for longer labels
image(1:12, 1:5, 
      t(kmeans_result$centers),
      col=colorRampPalette(c("white", "darkblue"))(100),
      main="Programming Language Usage Patterns by Cluster",
      xlab="", ylab="Cluster",
      axes=FALSE)
axis(1, at=1:12, labels=colnames(lang_matrix), las=2, cex.axis=0.9)
axis(2, at=1:5, labels=paste("Cluster", 1:5), las=1)

# Calculate average languages per cluster
avg_langs_per_cluster <- sapply(1:5, function(i) {
  mean(rowSums(lang_matrix[kmeans_result$cluster == i,]))
})

# Prepare data frame for kable table
cluster_table <- data.frame(
  Cluster = integer(),
  Size = integer(),
  Percentage = numeric(),
  Avg_Languages = numeric(),
  Primary_Languages = character(),
  Secondary_Languages = character(),
  stringsAsFactors = FALSE
)

# Populate the table with cluster analysis
for(i in 1:5) {
  cluster_size <- sum(kmeans_result$cluster == i)
  cluster_percent <- round(cluster_size / nrow(lang_matrix) * 100, 1)
  avg_langs <- avg_langs_per_cluster[i]
  
  lang_percentages <- kmeans_result$centers[i,] * 100
  major_langs <- colnames(lang_matrix)[which(lang_percentages > 30)]
  minor_langs <- colnames(lang_matrix)[which(lang_percentages > 10 & lang_percentages <= 30)]
  
  cluster_table <- rbind(cluster_table, data.frame(
    Cluster = i,
    Size = cluster_size,
    Percentage = cluster_percent,
    Avg_Languages = avg_langs,
    Primary_Languages = paste(major_langs, collapse = ", "),
    Secondary_Languages = paste(minor_langs, collapse = ", ")
  ))
}

# Generate kable table
kable_cluster_table <- knitr::kable(
  cluster_table,
  col.names = c("Cluster", "Size", "Percentage (%)", "Avg. Languages",
                "Primary Languages (>30%)", "Secondary Languages (10-30%)"),
  caption = "Cluster Analysis of Programming Language Usage Patterns",
  digits = 1,
  align = c('c', 'r', 'r', 'r', 'l', 'l')
)

# Display the kable table
kable_cluster_table
# ================================
# 3. Advanced Analysis
# ================================
# Load required libraries
# Suppress package startup messages
suppressPackageStartupMessages(library(corrplot))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(randomForest))

# Remove header row and process data
data <- data[-1,]

# Step 1: Create numeric education column
# Map education levels to numeric values
education_mapping <- c(
  "No formal education past high school" = 1,
  "Some college/university study without earning a bachelor's degree" = 2,
  "Bachelor's degree" = 3,
  "Master's degree" = 4,
  "Doctoral degree" = 5,
  "Professional degree" = 4  # Assuming equivalent to Master's
)
education_numeric <- as.numeric(factor(data$Q4, levels = names(education_mapping)))

# Step 2: Identify and process Q7 and Q14 columns
lang_cols <- grep("^Q7", names(data), value = TRUE)
lib_cols <- grep("^Q14", names(data), value = TRUE)
relevant_columns <- c(lang_cols, lib_cols)

# Create numeric dataset
numeric_data <- data[, relevant_columns]
numeric_data[] <- lapply(numeric_data, as.numeric)

# Add education column
numeric_data$Education <- education_numeric

# Step 3: Calculate correlations with education
correlations <- sapply(numeric_data[, -ncol(numeric_data)], function(x) {
  cor(x, numeric_data$Education, use = "complete.obs")
})

# Create meaningful labels
lang_labels <- c("Python", "R", "SQL", "C", "C++", "Java", 
                 "Javascript", "Julia", "Swift", "Bash", "MATLAB", "None", "Other")
lib_labels <- c("Matplotlib", "Seaborn", "Plotly / Plotly Express", "Ggplot / ggplot2", "Shiny", "D3js", "Altair", 
                "Bokeh", "Geoplotlib", "Leaflet / Folium", "None")
names(correlations) <- c(lang_labels, lib_labels)

# Sort correlations by absolute value
sorted_correlations <- sort(abs(correlations), decreasing = TRUE)
actual_correlations <- correlations[names(sorted_correlations)]


# Create correlation summary table
correlation_df <- data.frame(
  Feature = names(correlations),
  Correlation = round(correlations, 3)
) %>%
  arrange(desc(abs(Correlation)))

# Print formatted table
correlation_table <- (knitr::kable(correlation_df, 
                                   caption = "Correlations with Education Level",
                                   col.names = c("Feature", "Correlation")))

# Function to prepare data for classification
prepare_data <- function(data) {
  # Remove header row and any rows with missing values
  clean_data <- data
  clean_data <- clean_data[!is.na(clean_data$Q4) & clean_data$Q4 != "", ]
  
  # Get programming language columns (Q7) and library columns (Q14)
  lang_cols <- grep("^Q7", names(clean_data), value = TRUE)
  lib_cols <- grep("^Q14", names(clean_data), value = TRUE)
  
  # Combine features and convert to numeric
  features <- clean_data[, c(lang_cols, lib_cols)]
  features[] <- lapply(features, as.numeric)
  
  # Convert education levels to factor
  labels <- factor(clean_data$Q4)
  
  return(list(features = features, labels = labels))
}

# Function to train and evaluate the model
train_evaluate_model <- function(features, labels) {
  # Set up cross-validation
  set.seed(123)
  train_control <- trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE
  )
  
  # Train random forest model
  rf_model <- train(
    x = features,
    y = labels,
    method = "rf",
    trControl = train_control,
    importance = TRUE
  )
  
  # Get feature importance
  importance <- varImp(rf_model)
  
  return(list(model = rf_model, importance = importance))
}

# Main execution function
run_classification <- function(data) {
  # Prepare data
  prepared_data <- prepare_data(data)
  
  # Train and evaluate model
  results <- train_evaluate_model(prepared_data$features, prepared_data$labels)
  
  # Extract accuracy and Kappa from results
  metrics <- results$model$results[, c("mtry", "Accuracy", "Kappa")]
  
  # Create a kable table for the metrics
  library(knitr)
  kable_table <- knitr::kable(
    metrics,
    col.names = c("mtry", "Accuracy", "Kappa"),
    caption = "Model Performance Metrics by mtry",
    digits = 5
  )
  
  # Return the kable table
  return(kable_table)
}

# Example usage:

run_classification(data)
correlation_table

# ================================
# End of Analysis
# ================================