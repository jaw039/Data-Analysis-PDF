# ================================
# Setup
# ================================
# Load the dataset
palindromes_data <- as.numeric(read.table("hcmv.txt", header = TRUE)$location)

# ================================
# Global Parameters
# ================================
L <- 229354  # DNA sequence length
n <- 296     # Number of palindromes
nsim <- 1000 # Number of simulations for random scatter
set.seed(123)

# ================================
# 2.1 Random Scatter Analysis
# ================================
# Matrix to store all simulation results
all_sims <- matrix(nrow = nsim, ncol = n)

# Run simulations and store results
for(i in 1:nsim) {
  all_sims[i,] <- sort(sample(1:L, size = n, replace = F))
}

# Calculate average positions
avg_positions <- colMeans(all_sims)

# Plot average positions and Q-Q plot
par(mfrow=c(1,2))
# Plot 1: Average positions
plot(1, type="n", 
     xlim=c(1, n), 
     ylim=c(1, L),
     xlab="Palindrome Index",
     ylab="Position",
     main="Average Palindrome Positions")
lines(avg_positions, col = "black", lwd = 2)
lines(sort(palindromes_data), col = "red", lwd = 2)
legend("topleft", 
       legend = c("Average Simulated", "Actual Data"),
       col = c("black", "red"),
       lty = c(1, 1),
       lwd = c(2, 2))

# Plot 2: Q-Q plot
qqplot(avg_positions, sort(palindromes_data),
       main="Q-Q Plot: Simulated vs Observed Palindrome Positions",
       xlab="Theoretical Quantiles (Simulated)",
       ylab="Sample Quantiles (Observed)")
abline(0, 1, col="red", lwd=2)

# KS test
ks_result <- ks.test(sort(palindromes_data), avg_positions)

# Summary statistics
obs_summary <- summary(palindromes_data)
sim_summary <- summary(avg_positions)

# ================================
# 2.2 Graphical Analysis
# ================================
# Calculate observed patterns
obs_spacings <- diff(palindromes_data)
obs_pair_sums <- sapply(1:(length(obs_spacings)-1), 
                        function(i) obs_spacings[i] + obs_spacings[i+1])
obs_triplet_sums <- sapply(1:(length(obs_spacings)-2),
                           function(i) sum(obs_spacings[i:(i+2)]))

# Calculate expected patterns
exp_spacings <- diff(avg_positions)
exp_pair_sums <- sapply(1:(length(exp_spacings)-1), 
                        function(i) exp_spacings[i] + exp_spacings[i+1])
exp_triplet_sums <- sapply(1:(length(exp_spacings)-2),
                           function(i) sum(exp_spacings[i:(i+2)]))

# Perform KS tests
ks_spacings <- ks.test(obs_spacings, exp_spacings)
ks_pairs <- ks.test(obs_pair_sums, exp_pair_sums)
ks_triplets <- ks.test(obs_triplet_sums, exp_triplet_sums)

# Set up plotting area (3x2 grid)
par(mfrow=c(3,2))

# Distribution plots
hist(obs_spacings, probability=TRUE, col = adjustcolor("blue", alpha=0.5),
     main="Distribution of Spacings",
     xlab="Spacing between palindromes",
     breaks=30)
hist(exp_spacings, probability=TRUE, col = adjustcolor("red", alpha=0.5), add=TRUE)

# Spacing vs Position plots
plot(obs_spacings, type="l", col="red",
     main="Spacing vs Position",
     xlab="Position Index",
     ylab="Spacing")
lines(exp_spacings, col="blue")

# Pair and Triplet sum plots
hist(obs_pair_sums, probability=TRUE, col = adjustcolor("blue", alpha=0.5),
     main="Distribution of Pair Sums",
     xlab="Sum of adjacent pairs",
     breaks=30)
hist(exp_pair_sums, probability=TRUE, col = adjustcolor("red", alpha=0.5), add=TRUE)

plot(obs_pair_sums, type="l", col="red",
     main="Pair Sums vs Position",
     xlab="Position Index",
     ylab="Pair Sum")
lines(exp_pair_sums, col="blue")

# ================================
# 2.3 Count Analysis
# ================================
# Function to analyze palindrome counts in regions
analyze_region_counts <- function(palindromes, genome_length, n_region) {
  count_int <- table(cut(palindromes, 
                         breaks = seq(1, genome_length, length.out=n_region+1), 
                         include.lowest=TRUE))
  count_vector <- as.vector(count_int)
  count_tab <- table(factor(count_vector, levels=0:max(count_vector)))
  lambda <- length(palindromes)/n_region
  trunc <- 7
  
  obs_trunc <- c(count_tab[1:min(trunc, length(count_tab))], 
                 if(length(count_tab) > trunc) sum(count_tab[(trunc+1):length(count_tab)]) else 0)
  p <- c(dpois(0:(trunc-1), lambda), 1-sum(dpois(0:(trunc-1), lambda)))
  E <- p * n_region
  chi_sq_comp <- (obs_trunc - E)^2/E
  chi_sq <- chisq.test(obs_trunc, p=p, simulate.p.value=TRUE)
  
  list(table = data.frame(
    Count = c(0:(trunc-1), paste(">=", trunc, sep="")),
    Observed = obs_trunc,
    Expected = E,
    Chi_Square_Component = chi_sq_comp
  ),
  chi_square = chi_sq,
  lambda = lambda)
}

# Analyze different region sizes
region_sizes <- c(50, 100, 200, 300, 400, 500, 600, 700, 800, 900)
results <- list()
for(n_region in region_sizes) {
  results[[as.character(n_region)]] <- analyze_region_counts(
    palindromes_data, 229354, n_region
  )
}

# ================================
# 2.4 Largest Cluster Analysis
# ================================
# Function to compute max hits
compute_max_hits <- function(positions, L, M) {
  breaks <- seq(0, ceiling(L/M)*M, by = M)
  hist_result <- hist(positions, breaks = breaks, plot = FALSE)
  return(list(
    max_count = max(hist_result$counts),
    counts = hist_result$counts,
    interval = which.max(hist_result$counts),
    start_pos = breaks[which.max(hist_result$counts)],
    end_pos = breaks[which.max(hist_result$counts) + 1]
  ))
}

M <- 1000  # interval length
observed <- compute_max_hits(palindromes_data, L, M)

# Visualization
hist(palindromes_data, 
     breaks = seq(0, ceiling(L/M)*M, by = M),
     main = "Observed Palindrome Distribution",
     xlab = "Genome Position",
     ylab = "Number of Palindromes",
     col = "lightgreen")

# ================================
# 3. Advanced Analysis
# ================================
# Calculate spacings between consecutive palindromes
spacings <- diff(palindromes_data)

# Define regions
start_region <- c(1, 76451)
middle_region <- c(76452, 152902)
end_region <- c(152903, 229354)

# Find close pairs
close_pairs_indices <- which(spacings < 50)
close_pairs_positions <- palindromes_data[close_pairs_indices]

# Count pairs in each region
start_pairs <- sum(close_pairs_positions >= start_region[1] & 
                     close_pairs_positions <= start_region[2])
middle_pairs <- sum(close_pairs_positions >= middle_region[1] & 
                      close_pairs_positions <= middle_region[2])
end_pairs <- sum(close_pairs_positions >= end_region[1] & 
                   close_pairs_positions <= end_region[2])

# Create and plot pair counts
pair_counts <- c(start_pairs, middle_pairs, end_pairs)
names(pair_counts) <- c("Start", "Middle", "End")

barplot(pair_counts,
        main = "Distribution of Close Palindrome Pairs by Region",
        ylab = "Number of Close Pairs (<50 bp apart)",
        col = c("lightblue", "lightgreen", "pink"))

# ================================
# End of Analysis
# ================================