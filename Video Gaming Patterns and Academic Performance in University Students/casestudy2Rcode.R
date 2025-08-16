# ================================
# Setup
# ================================
# Load both datasets
videodata <- read.table("videodata.txt", header = TRUE)

# ================================
# Load Data
# ================================
# Remove 99 values in video
videodata <- videodata[videodata$time != 99,]

# Remove rows with NA values
cleaned_videodata <- na.omit(videodata)

# ================================
# Data Cleaning
# ================================

videodata <- videodata[videodata$time != 99, ]
cleaned_videodata <- na.omit(videodata)

# ================================
# Point and Interval Estimates
# ================================

# Calculate the number of students who played video games
players <- sum(cleaned_videodata$time > 0)
total_students <- nrow(cleaned_videodata)

# Point estimate
point_estimate_fraction <- players / total_students

# Interval estimate (95% confidence interval)
z <- qnorm(0.975)  
se <- sqrt((point_estimate_fraction * (1 - point_estimate_fraction)) / total_students)
margin_of_error <- z * se

lower_interval_estimate_fraction <- max(0, point_estimate_fraction - margin_of_error)
upper_interval_estimate_fraction <- min(1, point_estimate_fraction + margin_of_error)

# Save results
point_estimate_fraction <- point_estimate_fraction
lower_interval_estimate_fraction <- lower_interval_estimate_fraction
upper_interval_estimate_fraction <- upper_interval_estimate_fraction

# ================================
# Frequency Analysis
# ================================

# Create a frequency mapping
freq_mapping <- c("1" = "Daily", "2" = "Weekly", "3" = "Monthly", "4" = "semesterly")

# Add a new column with mapped frequency labels
cleaned_videodata$freq_label <- factor(freq_mapping[as.character(cleaned_videodata$freq)], levels = freq_mapping)

# Calculate average time spent for each frequency category
avg_time_by_freq <- tapply(cleaned_videodata$time, cleaned_videodata$freq_label, function(x) c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE), count = length(x)))

# Convert the results to a data frame
avg_time_df <- data.frame(
  Frequency = names(avg_time_by_freq),
  Mean_Time = sapply(avg_time_by_freq, `[`, "mean"),
  Median_Time = sapply(avg_time_by_freq, `[`, "median"),
  Count = sapply(avg_time_by_freq, `[`, "count")
)

# Create and print the kable
library(knitr)
knitr::kable(avg_time_df, 
             caption = "Average time spent by frequency",
             col.names = c("Frequency", "Mean Time", "Median Time", "Count"),
             digits = 2,
             align = c('l', 'c', 'c', 'c'),
             row.names = FALSE)

figure1 <- cleaned_videodata

# ================================
# Bootstrapping Analysis
# ================================

point_estimate_average <- mean(cleaned_videodata$time, na.rm = TRUE)

par(mfrow = c(1, 2))

hist(cleaned_videodata$time, 
     main = "Histogram of # of Hours", 
     xlab = "Time (Hours)", 
     col = "lightyellow", 
     breaks = sqrt(length(cleaned_videodata$time)))

n <- length(cleaned_videodata$time)
n_boot <- 1000 

# Bootstrapping
bootstrap_samples <- replicate(n_boot, mean(sample(cleaned_videodata$time, n, replace = TRUE), na.rm = TRUE))

hist(bootstrap_samples, 
     main = "Bootstrapped # of Hours",
     xlab = "Time (Hours)", 
     col = "pink", 
     breaks = sqrt(length(bootstrap_samples)))

lower_interval_estimate_average <- quantile(bootstrap_samples, 0.025, names = FALSE)
upper_interval_estimate_average <- quantile(bootstrap_samples, 0.975, names = FALSE)

par(mfrow = c(1, 1))

# ================================
# Gaming Behavior During Busy Periods
# ================================

# Find if students enjoy video games or not
cleaned_videodata <- cleaned_videodata[cleaned_videodata$like != 99, ]

never_percentage <- sum(cleaned_videodata$like == 1) / nrow(cleaned_videodata) * 100

like_percentage <- (sum(cleaned_videodata$like %in% c(2, 3))
                    / nrow(cleaned_videodata) * 100)

dislike_percentage <- (sum(cleaned_videodata$like %in% c(4, 5))
                       / nrow(cleaned_videodata) * 100)

# ================================
# Attitudes Towards Video Games
# ================================

# Load attribute dataset
videoMultiple <- read.table("videoMultiple.txt", header = TRUE)

rows_with_na <- sum(apply(videoMultiple, 1, function(x) any(is.na(x))))

removed_na <- na.omit(videoMultiple)

# Corrected ifelse statement to create a new column
removed_na$other <- ifelse(removed_na$other != " " & !is.na(removed_na$other), 1, 0)

# Select the first 12 rows of the videoMultiple data frame
likeliness_rows <- removed_na[6:12 ]

likeliness_rows_percentages <- colMeans(likeliness_rows, na.rm=TRUE)

# Convert the result to a data frame for better formatting
likeliness_rows_percentages_df <- as.data.frame(t(likeliness_rows_percentages))  # Transpose to make it a single row

# Display the percentages as a kable table
library(knitr)
kable(likeliness_rows_percentages_df)

reasons_for_likeliness <- c('Relaxation', 'Feeling of mastery', 'Bored', 'Graphics/Realism')

# ================================
# Demographic Analysis
# ================================

# Create binary gaming preference (collapsing the scale)
cleaned_videodata$gamer_type <- ifelse(cleaned_videodata$like %in% c(1, 4, 5), "Dislike Playing", ifelse(cleaned_videodata$like %in% c(2, 3), "Enjoy Playing", NA))

# Filter out invalid gender codes
gender_videodata <- cleaned_videodata[cleaned_videodata$sex != 99,]
gender_videodata$sex <- factor(gender_videodata$sex,
                               levels = c(0, 1),
                               labels = c("Female", "Male"))

# Create contingency table
gender_table <- table(gender_videodata$sex, gender_videodata$gamer_type)

# Calculate proportions
gender_props <- prop.table(gender_table, 2)  # Note: changed margin to 2

par(mfrow = c(1, 2))
# Create barplot with proper labels
barplot(gender_props,  # Removed transpose
        beside = TRUE,
        main = "Gaming Enjoyment by Gender",
        ylab = "Proportion",
        col = c("lightpink", "lightblue"),
        legend.text = c("Female", "Male"))

# Filter out invalid work codes
work_videodata <- cleaned_videodata[cleaned_videodata$work != 99,]

# Create binary work status and convert to factor with labels
work_videodata$work <- factor(ifelse(work_videodata$work > 0, 1, 0),
                              levels = c(0, 1),
                              labels = c("Don't Work", "Work"))

# Create contingency table with gamer_type as rows
work_table <- table(work_videodata$work, work_videodata$gamer_type)

# Calculate proportions (by work status)
work_props <- prop.table(work_table, 2)

# Create barplot with proper labels
barplot(work_props,
        beside = TRUE,
        main = "Gaming Enjoyment by Work",
        ylab = "Percentage",
        col = c("lavender", "palegreen"),
        legend.text = c("Don't Work", "Work"))

# Filter out invalid work codes
own_videodata <- cleaned_videodata[cleaned_videodata$own != 99,]

# Convert computer ownership to factor BEFORE creating the table
own_videodata$own <- factor(own_videodata$own,
                            levels = c(0, 1),
                            labels = c("Don't Own", "Own"))

# Create contingency table with gamer_type as columns
own_table <- table(own_videodata$own, own_videodata$gamer_type)

# Calculate proportions by gaming preference
own_props <- prop.table(own_table, 2) 

# Create barplot with proper labels
barplot(own_props,
        beside = TRUE,
        main = "Gaming Enjoyment by Computer",
        ylab = "Percentage",
        col = c("salmon", "steelblue"),
        legend.text = c("Don't Own", "Own"))
par(mfrow = c(1, 1))

# ================================
# Grade Distribution Analysis
# ================================

# Prepare the data
# First, let's remove any invalid grades (99s or NAs)
grade_data <- cleaned_videodata[!is.na(cleaned_videodata$grade) & cleaned_videodata$grade != 99, ]

# Combine D's and F's (0s and 1s)
grade_data$grade <- factor(ifelse(grade_data$grade <= 1, 1, grade_data$grade), levels = 1:4)

# Create a table of observed frequencies
observed_freq <- table(grade_data$grade)
n <- sum(observed_freq)

# Convert observed frequencies to proportions
observed_prop <- prop.table(observed_freq)

# Define target proportions (in same order as grades 1(D/F),2(C),3(B),4(A))
target_prop <- c(0.10, 0.40, 0.30, 0.20)

# Calculate observed TVD
tvd_obs <- 0.5 * sum(abs(observed_prop - target_prop))

# Perform bootstrap test
n_boot <- 10000
tvd_boot <- numeric(n_boot)

for(i in 1:n_boot) {
  # Generate bootstrap sample from target distribution
  boot_sample <- table(factor(sample(1:4, size = n, prob = target_prop, replace = TRUE), levels = 1:4))
  boot_prop <- prop.table(boot_sample)
  tvd_boot[i] <- 0.5 * sum(abs(boot_prop - target_prop))
}

# Calculate p-value
p_value <- mean(tvd_boot >= tvd_obs)

# Create visual comparison

# Bar plot of observed vs expected proportions
barplot(rbind(observed_prop, target_prop), 
        beside = TRUE,
        col = c("lightblue", "lightpink"),
        names.arg = c("D/F", "C", "B", "A"),
        legend.text = c("Observed", "Target"),
        main = "Grade Distribution Comparison",
        ylab = "Proportion")

# Distribution of bootstrap TVDs with observed TVD marked
hist(tvd_boot, 
     main = "Bootstrap Distribution of TVD",
     xlab = "TVD",
     xlim = range(c(tvd_boot, tvd_obs)),)
abline(v = tvd_obs, col = "red", lwd = 2)


# ================================
# Advanced Analysis
# ================================

# Remove invalid entries for math
math_data <- cleaned_videodata[cleaned_videodata$math != 99, ]

# Create a factor for math focus
math_data$math_label <- factor(math_data$math, 
                               levels = c(0, 1),
                               labels = c("Non-Math Focus", "Math Focus"))

# Create the boxplot comparing gaming time distribution
boxplot(time ~ math_label, data = math_data,
        main = "Gaming Hours Distribution by Math Focus",
        ylab = "Hours Spent Gaming",
        xlab = "math_label",
        col = c("lightblue", "black"))

# Perform Wilcoxon test
test_result <- wilcox.test(time ~ math_label, data = math_data)

# Calculate Z score 
n1 <- sum(math_data$math == 0)
n2 <- sum(math_data$math == 1)
Z <- -2.76  # From the result
P <- 0.006  # From the result

# Add text for Z and P values
legend("topright", 
       bty="o",
       bg="white",
       legend=c(paste("Z =", Z),
                paste("P =", P)))

# ================================
# Appendix: Additional Figures
# ================================

par(mfrow = c(1, 2))
# Create a boxplot to visualize the distribution
boxplot(time ~ freq_label, data = figure1, 
        main = "Time by Frequency of Play",
        xlab = "Frequency Category", ylab = "Time Spent (hours)")

# Calculate the mean time spent for each reported frequency
avg_time_per_freq <- aggregate(time ~ freq_label, data = figure1, FUN = mean)

# Create a barplot to visualize the average time spent for each reported frequency
barplot(avg_time_per_freq$time, names.arg = avg_time_per_freq$freq_label,
        main = "Average Time by Frequency",
        xlab = "Frequency Category", ylab = "Average Time Spent (hours)", 
        col = "lightblue", ylim = c(0, max(avg_time_per_freq$time)))
par(mfrow = c(1, 1))


# Filter out the 99 values
filtered_data <- figure1[figure1$busy != 99, ]

# Create a contingency table to count the number of 0s and 1s for each frequency category
counts <- table(filtered_data$freq_label, filtered_data$busy)

# Create a barplot to visualize the counts of 0s and 1s for each frequency category
barplot(t(counts), beside = TRUE, col = c("lightblue", "lightgreen"),
        legend = c("0 - Don't play if busy", "1 - Play if busy"), 
        main = "Count of 1s and 0s by Frequency of Play",
        xlab = "Frequency Category", ylab = "Count")

# Print the percentage table
print(round(gender_props, 2))
# Print the percentage table
print(round(work_props, 2))
# Print the percentage table
print(round(own_props, 2))

# ================================
# End of Analysis
# ================================