# The array containing all the scores
scores <- c(6, 8, 6, 6, 6, 2, 1, 2, 7, 3, 3, 7, 5, 7, 9, 6, 7, 7, 2, 6)

# The value of the mean
mean_val <- mean(scores)
# The value of the median
median_val <- median(scores)

# A function which you can call on an array
# which returns the most occuring number in an array
# a.k.a. the modus
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# The value of the modus
mode_val <- get_mode(scores)

# The Median Absolute Value
mad_val <- mean(abs(scores - mean(scores)))

# The variance and standard deviation value
variance_val <- var(scores)
sd_val <- sd(scores)

# Print the results using the cat function
cat("Mean:", mean_val, "\nMedian:", median_val, "\nMode:", mode_val, 
    "\nMAD:", mad_val, "\nVariance:", variance_val, "\nSD:", sd_val)

# We plot the histogram
hist(scores, breaks = seq(0, 10, 1), col = "skyblue", border = "black",
     main = "Histogram of User Satisfaction Scores", xlab = "Score", ylim = c(0, 7))

# Add lines for the asked values and add them to a legend
abline(v = mean_val - 0.05, col = "red", lwd = 2, lty = 2)
abline(v = median_val + 0.05, col = "green", lwd = 2, lty = 2)
abline(v = mode_val, col = "purple", lwd = 2, lty = 2)
legend("topleft", legend = c("Mean", "Median", "Mode"), 
       col = c("red", "green", "purple"), lty = 2, lwd = 2)

# We calculate the Z-scores 
z_scores <- (scores - mean_val) / sd_val

# |z| > 1.5
outliers_count <- sum(abs(z_scores) > 1.5)
cat("\nNumber of observations with |z| > 1.5:", outliers_count)

# We calculate the probabilities using pnorm()
probabilities <- pnorm(scores, mean = mean_val, sd = sd_val)

# We combine these two into a data frame for viewing
results_table <- data.frame(Score = scores, Z_Score = z_scores, Prob_Lower = probabilities)

View(results_table)