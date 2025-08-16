# ================================
# Setup
# ================================
# Load the dataset
gauge_data <- read.table("gauge.txt", header = TRUE)
# ================================
# 2.1 Raw Data Analysis
# ================================
# Enhanced scatter plot of density vs. gain
plot(gauge_data$density, gauge_data$gain,
     main = "Enhanced Scatter Plot: Gain vs. Density",
     xlab = "Density (g/cm^3)",
     ylab = "Gain",
     pch = 16, # Solid dots
     col = rgb(0.2, 0.4, 0.6, 0.7), # Semi-transparent blue
     cex = 1.5) # Larger point size

# Add a grid for better readability
grid(col = "gray", lty = "dotted")

# Fit linear regression to raw data
raw_model <- lm(gain ~ density, data = gauge_data)

# Calculate n for standard error
n <- nrow(gauge_data)

# Create plots
par(mfrow=c(1,2))

# 1. Scatter plot with regression line and prediction bands
plot(gauge_data$density, gauge_data$gain,
     xlab = "Density (g/cm^3)", 
     ylab = "Gain",
     main = "Raw Data: Gain vs Density",
     pch = 19)
abline(raw_model, col = "red", lwd = 2)

# Add confidence and prediction bands
newX <- seq(min(gauge_data$density), max(gauge_data$density), length.out=100)
conf.bands <- predict(raw_model, newdata = data.frame(density=newX), interval = 'confidence')
pred.bands <- predict(raw_model, newdata = data.frame(density=newX), interval = 'prediction')

lines(newX, conf.bands[,2], lwd = 2, lty = 2, col = 'red')
lines(newX, conf.bands[,3], lwd = 2, lty = 2, col = 'red')
lines(newX, pred.bands[,2], lwd = 2, lty = 2, col = 'blue')
lines(newX, pred.bands[,3], lwd = 2, lty = 2, col = 'blue')

# 2. Residuals vs Fitted values
plot(fitted(raw_model), residuals(raw_model),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted",
     pch = 19)
abline(h = 0, col = "red", lty = 2)


# Calculate and print correlation coefficient
cat("Correlation coefficient:", cor(gauge_data$density, gauge_data$gain))

# ================================
# 2.2 Transformed Data Analysis
# ================================
# Create transformed data
gauge_data$log_gain <- log(gauge_data$gain)

# Enhanced scatter plot of density vs. log(gain)
plot(gauge_data$density, gauge_data$log_gain,
     main = "Enhanced Scatter Plot: Log(Gain) vs. Density",
     xlab = "Density (g/cm^3)",
     ylab = "Log(Gain)",
     pch = 16, # Solid dots
     col = rgb(0.2, 0.4, 0.6, 0.7), # Semi-transparent blue
     cex = 1.5) # Larger point size

# Add a grid for better readability
grid(col = "gray", lty = "dotted")

# Fit linear regression to transformed data
transformed_model <- lm(log_gain ~ density, data = gauge_data)

# Create plots in a 2x2 layout
par(mfrow=c(1,2))

# 1. Scatter plot with regression line
plot(gauge_data$density, gauge_data$log_gain,
     xlab = "Density (g/cm^3)", 
     ylab = "Log(Gain)",
     main = "Transformed Data: Log(Gain) vs Density",
     pch = 19)
abline(transformed_model, col = "red", lwd = 2)

# Add confidence and prediction bands
newX <- seq(min(gauge_data$density), max(gauge_data$density), length.out=100)
conf.bands <- predict(transformed_model, newdata = data.frame(density=newX), interval = 'confidence')
pred.bands <- predict(transformed_model, newdata = data.frame(density=newX), interval = 'prediction')

lines(newX, conf.bands[,2], lwd = 2, lty = 2, col = 'red')
lines(newX, conf.bands[,3], lwd = 2, lty = 2, col = 'red')
lines(newX, pred.bands[,2], lwd = 2, lty = 2, col = 'blue')
lines(newX, pred.bands[,3], lwd = 2, lty = 2, col = 'blue')

# 2. Residuals vs Fitted values
plot(fitted(transformed_model), residuals(transformed_model),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted",
     pch = 19)
abline(h = 0, col = "red", lty = 2)

# Calculate and print correlation coefficient
cat("Correlation coefficient:", cor(gauge_data$density, gauge_data$log_gain))

# ================================
# 2.3 Robustness Testing
# ================================
# Set seed for reproducibility
set.seed(123)
# Original data
x <- gauge_data$density
y <- log(gauge_data$gain)
# Number of simulations
n_sims <- 1000
# Standard deviation for measurement error
sigma <- 0.02
# Storage for simulation results
slopes <- numeric(n_sims)
intercepts <- numeric(n_sims)
# Run simulations
for(i in 1:n_sims) {
  # Add random normal noise to x values
  x_noise <- x + rnorm(length(x), mean=0, sd=sigma)
  # Fit model with noisy data
  sim_model <- lm(y ~ x_noise)
  # Store coefficients
  slopes[i] <- coef(sim_model)[2]
  intercepts[i] <- coef(sim_model)[1]
}

# Calculate confidence intervals
slope_ci <- quantile(slopes, c(0.025, 0.975))
intercept_ci <- quantile(intercepts, c(0.025, 0.975))

# Original model fit
original_fit <- lm(y ~ x)
# Create plots in a 2x2 layout
par(mfrow=c(2,2))
# 1. Original data with true fit
plot(x, y, 
     main="Original Data with True Fit",
     xlab="Density (g/cm^3)",
     ylab="Log(Gain)",
     pch=19)
abline(original_fit, col="red", lwd=2)
# 2. Example of one noisy dataset
x_example <- x + rnorm(length(x), mean=0, sd=sigma)
plot(x_example, y,
     main="Example of Noisy Data",
     xlab="Density with Noise (g/cm^3)",
     ylab="Log(Gain)",
     pch=19)
abline(lm(y ~ x_example), col="blue", lwd=2)
abline(original_fit, col="red", lwd=2, lty=2)
legend("topright", legend=c("True Fit", "Noisy Fit"),
       col=c("red", "blue"), lty=c(2,1), lwd=2)
# 3. Histogram of slopes
hist(slopes, 
     main=paste("Distribution of Slopes\nMean =", round(mean(slopes),3),
                "\nSD =", round(sd(slopes),3)),
     xlab="Slope",
     breaks=30)
abline(v=coef(original_fit)[2], col="red", lwd=2)
abline(v=slope_ci, col="blue", lwd=2, lty=2)
legend("topright", 
       legend=c("True Value", "95% CI"),
       col=c("red", "blue"), 
       lty=c(1,2), 
       lwd=2)

# 4. Histogram of intercepts
hist(intercepts,
     main=paste("Distribution of Intercepts\nMean =", round(mean(intercepts),3),
                "\nSD =", round(sd(intercepts),3)),
     xlab="Intercept",
     breaks=30)
abline(v=coef(original_fit)[1], col="red", lwd=2)
abline(v=intercept_ci, col="blue", lwd=2, lty=2)
legend("topright", 
       legend=c("True Value", "95% CI"),
       col=c("red", "blue"), 
       lty=c(1,2), 
       lwd=2)

# Reset plotting layout
par(mfrow=c(1,1))

# Update summary table to include confidence intervals
sim_summary <- data.frame(
  Metric = c("True Value", "Simulated Mean", "Standard Deviation", 
             "95% CI Lower", "95% CI Upper", "Bias (%)"),
  Value_Slope = c(
    coef(original_fit)[2],
    mean(slopes),
    sd(slopes),
    slope_ci[1],
    slope_ci[2],
    (mean(slopes) - coef(original_fit)[2])/coef(original_fit)[2] * 100
  ),
  Value_Intercept = c(
    coef(original_fit)[1],
    mean(intercepts),
    sd(intercepts),
    intercept_ci[1],
    intercept_ci[2],
    (mean(intercepts) - coef(original_fit)[1])/coef(original_fit)[1] * 100
  )
)

# Format the table using kable
library(knitr)
kable(sim_summary,
      caption = "Simulation Results Summary with Confidence Intervals",
      col.names = c("Metric", "Slope", "Intercept"),
      align = c('l', 'r', 'r'),
      digits = 4,
      format.args = list(big.mark = ","))

# ================================
# 2.4 Forward Prediction
# ================================
# Create prediction points
new_densities <- data.frame(density = c(0.508, 0.001))

# Get predictions in log scale with prediction intervals
log_predictions <- predict(transformed_model, newdata = new_densities, interval = "prediction")

# Transform back to original scale
predictions <- exp(log_predictions)

# Create formatted table of predictions
prediction_table <- data.frame(
  Density = new_densities$density,
  Predicted_Gain = predictions[,"fit"],
  Lower_PI = predictions[,"lwr"],
  Upper_PI = predictions[,"upr"],
  Interval_Width = predictions[,"upr"] - predictions[,"lwr"]
)

# Print formatted table
library(knitr)
kable(prediction_table,
      caption = "Predictions with Intervals",
      digits = 2)

# Create visualization with prediction bands across density range
density_seq <- seq(min(gauge_data$density), max(gauge_data$density), length.out = 100)
log_pred_bands <- predict(transformed_model, 
                          newdata = data.frame(density = density_seq), 
                          interval = "prediction")
pred_bands <- exp(log_pred_bands)

# Plot predictions
plot(gauge_data$density, gauge_data$gain,
     main = "Gain vs Density with Prediction Bands",
     xlab = "Density (g/cm^3)",
     ylab = "Gain",
     pch = 19)

# Add prediction bands
lines(density_seq, pred_bands[,"fit"], col = "red", lwd = 2)
lines(density_seq, pred_bands[,"lwr"], col = "blue", lty = 2, lwd = 2)
lines(density_seq, pred_bands[,"upr"], col = "blue", lty = 2, lwd = 2)

# Add points for specific predictions
points(prediction_table$Density, prediction_table$Predicted_Gain, 
       col = "red", pch = 19, cex = 1.2)

# Add legend
legend("topright", 
       legend = c("Observed", "Predicted", "Prediction Interval"),
       col = c("black", "red", "blue"),
       lty = c(NA, 1, 2),
       pch = c(19, NA, NA),
       lwd = c(NA, 2, 2))

# ================================
# 2.5 Backward Prediction
# ================================
# Create function for reverse prediction (gain to density)
reverse_predict <- function(gain, model) {
  # Transform gain to log scale
  log_gain <- log(gain)
  
  # Extract model coefficients
  beta0 <- coef(model)[1]  # intercept
  beta1 <- coef(model)[2]  # slope
  
  # Calculate predicted density
  density_pred <- (log_gain - beta0) / beta1
  
  # Calculate standard error for reverse prediction
  sigma <- summary(model)$sigma
  x_bar <- mean(gauge_data$density)
  n <- length(gauge_data$density)
  SSx <- sum((gauge_data$density - x_bar)^2)
  
  # Standard error for reverse prediction
  se_rev <- sigma * sqrt(1/beta1^2 * (1 + 1/n + (density_pred - x_bar)^2/SSx))
  
  # Calculate prediction interval
  t_val <- qt(0.975, df = n-2)
  pi_lower <- density_pred - t_val * se_rev
  pi_upper <- density_pred + t_val * se_rev
  
  return(data.frame(
    Predicted_Density = density_pred,
    Lower_PI = pi_lower,
    Upper_PI = pi_upper,
    PI_Width = pi_upper - pi_lower
  ))
}

# Perform reverse predictions for specific points
gains <- c(38.6, 426.7)
true_densities <- c(0.508, 0.001)
reverse_results <- data.frame(
  Gain = gains,
  True_Density = true_densities
)

# Add predictions
for(i in 1:length(gains)) {
  pred <- reverse_predict(gains[i], transformed_model)
  reverse_results[i, c("Predicted_Density", "Lower_PI", "Upper_PI", "PI_Width")] <- 
    pred[1, c("Predicted_Density", "Lower_PI", "Upper_PI", "PI_Width")]
}

# Calculate prediction errors
reverse_results$Abs_Error <- abs(reverse_results$Predicted_Density - reverse_results$True_Density)
reverse_results$Relative_Error <- reverse_results$Abs_Error / reverse_results$True_Density * 100

# Format table
library(knitr)
kable(reverse_results, 
      caption = "Reverse Prediction Results",
      digits = 4)

# Create sequence of gain values for prediction bands
gain_seq <- seq(min(gauge_data$gain), max(gauge_data$gain), length.out = 100)
pred_bands <- data.frame(Gain = gain_seq)

# Calculate prediction bands
for(i in 1:length(gain_seq)) {
  pred <- reverse_predict(gain_seq[i], transformed_model)
  pred_bands$Density[i] <- pred$Predicted_Density
  pred_bands$Lower[i] <- pred$Lower_PI
  pred_bands$Upper[i] <- pred$Upper_PI
}

# Visualization
plot(gauge_data$gain, gauge_data$density,
     xlab = "Gain",
     ylab = "Density (g/cm^3)",
     main = "Reverse Prediction: Density vs Gain",
     pch = 19)

# Add prediction bands
lines(pred_bands$Gain, pred_bands$Density, col = "red", lwd = 2)
lines(pred_bands$Gain, pred_bands$Lower, col = "blue", lty = 2, lwd = 2)
lines(pred_bands$Gain, pred_bands$Upper, col = "blue", lty = 2, lwd = 2)

# Add reference points
points(gains, reverse_results$Predicted_Density, 
       col = "red", pch = 19, cex = 1.2)

# Add legend
legend("topright",
       legend = c("Observed", "Predicted", "Prediction Interval"),
       col = c("black", "red", "blue"),
       pch = c(19, 19, NA),
       lty = c(NA, 1, 2),
       lwd = c(NA, 2, 2))
# ================================
# 2.6 Cross-Validation
# ================================
# Reverse prediction function
reverse_predict <- function(gain, model) {
  # Transform gain to log scale
  log_gain <- log(as.numeric(gain))  # Ensure numeric
  
  # Extract model coefficients
  beta0 <- coef(model)[1]  # intercept
  beta1 <- coef(model)[2]  # slope
  
  # Calculate predicted density
  density_pred <- (log_gain - beta0) / beta1
  
  # Get model summary statistics
  model_summary <- summary(model)
  sigma <- model_summary$sigma
  x_bar <- mean(model$model$density)
  n <- nrow(model$model)
  SSx <- sum((model$model$density - x_bar)^2)
  
  # Standard error for reverse prediction
  se_rev <- sigma / abs(beta1) * sqrt(1 + 1/n + ((density_pred - x_bar)^2) / SSx)
  
  # Calculate prediction interval
  t_val <- qt(0.975, df = n-2)
  pi_lower <- density_pred - t_val * se_rev
  pi_upper <- density_pred + t_val * se_rev
  
  return(data.frame(
    Predicted_Density = density_pred,
    Lower_PI = pi_lower,
    Upper_PI = pi_upper,
    PI_Width = pi_upper - pi_lower
  ))
}

# Cross-validation for density 0.508
gauge_data_omit_0508 <- subset(gauge_data, density != 0.508)
model_omit_0508 <- lm(log_gain ~ density, data = gauge_data_omit_0508)
prediction_0508 <- reverse_predict(38.6, model_omit_0508)

# Cross-validation for density 0.001
gauge_data_omit_0001 <- subset(gauge_data, density != 0.001)
model_omit_0001 <- lm(log_gain ~ density, data = gauge_data_omit_0001)
prediction_0001 <- reverse_predict(426.7, model_omit_0001)

# Combine results
cross_validation_results <- data.frame(
  True_Density = c(0.508, 0.001),
  Gain = c(38.6, 426.7),
  Predicted_Density = c(prediction_0508$Predicted_Density, prediction_0001$Predicted_Density),
  Lower_PI = c(prediction_0508$Lower_PI, prediction_0001$Lower_PI),
  Upper_PI = c(prediction_0508$Upper_PI, prediction_0001$Upper_PI),
  PI_Width = c(prediction_0508$PI_Width, prediction_0001$PI_Width)
)

# Calculate absolute and relative errors
cross_validation_results$Absolute_Error <- abs(cross_validation_results$Predicted_Density - 
                                                 cross_validation_results$True_Density)
cross_validation_results$Relative_Error <- (cross_validation_results$Absolute_Error / 
                                              cross_validation_results$True_Density) * 100

# Print results
knitr::kable(cross_validation_results, 
             caption = "Cross-Validation Results: Predicted Density and Prediction Intervals",
             digits = 4)

# Visualize results
par(mfrow=c(1,2))

# Plot for density 0.508
plot(gauge_data_omit_0508$density, log(gauge_data_omit_0508$gain),
     main="Cross-validation: density = 0.508",
     xlab="Density (g/cm³)", ylab="Log(Gain)",
     pch=19, col="blue")
abline(model_omit_0508, col="red")
points(0.508, log(38.6), col="green", pch=19, cex=2)

# Plot for density 0.001
plot(gauge_data_omit_0001$density, log(gauge_data_omit_0001$gain),
     main="Cross-validation: density = 0.001",
     xlab="Density (g/cm³)", ylab="Log(Gain)",
     pch=19, col="blue")
abline(model_omit_0001, col="red")
points(0.001, log(426.7), col="green", pch=19, cex=2)

# ================================
# 3. Advanced Analysis
# ================================
# Split the dataset by median density
median_density <- median(gauge_data$density)
low_density_data <- subset(gauge_data, density <= median_density)
high_density_data <- subset(gauge_data, density > median_density)

# Fit log-linear models to each subset
low_density_model <- lm(log(gain) ~ density, data = low_density_data)
high_density_model <- lm(log(gain) ~ density, data = high_density_data)

# Compare model coefficients
low_coef <- coef(low_density_model)
high_coef <- coef(high_density_model)

# Visualize the fits
par(mfrow = c(1, 2))  # Side-by-side layout

# Plot for low-density data
plot(
  low_density_data$density, low_density_data$gain,
  pch = 16, col = "blue", xlab = "Density (g/cm³)", ylab = "Gain",
  main = "Low-Density Model Fit"
)
lines(
  low_density_data$density, exp(predict(low_density_model, newdata = low_density_data)),
  col = "red", lwd = 2
)

# Plot for high-density data
plot(
  high_density_data$density, high_density_data$gain,
  pch = 16, col = "green", xlab = "Density (g/cm³)", ylab = "Gain",
  main = "High-Density Model Fit"
)
lines(
  high_density_data$density, exp(predict(high_density_model, newdata = high_density_data)),
  col = "orange", lwd = 2
)
# ================================
# End of Analysis
# ================================