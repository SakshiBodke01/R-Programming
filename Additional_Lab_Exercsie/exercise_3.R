#Part A: Descriptive Statistics & Visual Summaries
#Dataset: mtcars (built-in R dataset)
library(ggplot2)
library(dplyr)

data(mtcars)

# 1. Descriptive stats for mpg
mpg <- mtcars$mpg
mean_mpg <- mean(mpg)
median_mpg <- median(mpg)
# mode function (may be multimodal) - returns the smallest most frequent value
get_mode <- function(v) {
  tv <- table(v)
  as.numeric(names(tv)[which.max(tv)])
}
mode_mpg <- get_mode(mpg)
var_mpg <- var(mpg)
sd_mpg <- sd(mpg)
range_mpg <- range(mpg)

list(mean = mean_mpg,
     median = median_mpg,
     mode = mode_mpg,
     variance = var_mpg,
     sd = sd_mpg,
     range = range_mpg)

# 2. Frequency table of cylinders
table(mtcars$cyl)

# 3. Histogram of mpg with density curve
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(aes(y = ..density..), bins = 10, alpha = 0.6) +
  geom_density(size = 1) +
  labs(title = "Histogram of mpg with Density", x = "mpg", y = "Density")

# 4. Boxplot of mpg by cyl and short interpretation printed
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_boxplot() +
  labs(title = "mpg by Number of Cylinders", x = "Cylinders", y = "mpg")

# Interpretation (print)
by(mpg, mtcars$cyl, summary)
# The boxplot and the summaries show spread (IQR) and medians per cyl group. Typically 4-cyl cars have higher median mpg and less spread; 8-cyl often have lower median and larger spread.

# 5. summary() of dataset
summary(mtcars)

#Part B: Probability & Distributions
#Dataset: iris (built-in R dataset)
data(iris)

# 1. Normal curve using Sepal.Length mean & sd (plot over range)
mu <- mean(iris$Sepal.Length)
sigma <- sd(iris$Sepal.Length)
x <- seq(min(iris$Sepal.Length) - 1, max(iris$Sepal.Length) + 1, length.out = 200)
df_norm <- data.frame(x = x, y = dnorm(x, mean = mu, sd = sigma))
ggplot(df_norm, aes(x = x, y = y)) +
  geom_line() +
  labs(title = "Normal Curve: Sepal.Length", x = "Sepal.Length", y = "Density") +
  geom_vline(xintercept = mu, linetype = "dashed")

# 2. Shapiro–Wilk test for normality
shapiro_test <- shapiro.test(iris$Sepal.Length)
shapiro_test

# 3. Simulate 1000 binomial samples (n=10, p=0.5) and histogram
set.seed(123)
bin_samps <- rbinom(1000, size = 10, prob = 0.5)
hist(bin_samps, breaks = ( -0.5 : 10.5 ), main = "Binomial(n=10,p=0.5) - 1000 samples", xlab = "Successes")

# 4. Compare sample mean/variance with theoretical values
sample_mean <- mean(bin_samps)
sample_var <- var(bin_samps)
theo_mean <- 10 * 0.5
theo_var <- 10 * 0.5 * (1 - 0.5)
list(sample_mean = sample_mean, sample_var = sample_var,
     theoretical_mean = theo_mean, theoretical_variance = theo_var)

#Part C: Estimation & Confidence Intervals
#Dataset: mtcars
# 1. 95% CI for mean of mpg (t-based)
t.test(mtcars$mpg, conf.level = 0.95)$conf.int

# 2. Bootstrap CI for hp using boot package
library(boot)
set.seed(123)
hp_stat <- function(data, indices) {
  d <- data[indices]
  mean(d)
}
boot_out <- boot(mtcars$hp, statistic = hp_stat, R = 2000)
boot_out
# 95% bootstrap percentile CI
boot.ci(boot_out, type = c("perc","bca"))

# 3. Compare CIs of mpg for automatic vs manual (am: 0 = automatic, 1 = manual)
mtcars$am <- factor(mtcars$am, levels = c(0,1), labels = c("Automatic","Manual"))
t_auto <- t.test(mpg ~ am, data = mtcars)  # Welch two-sample by default
t_auto$conf.int
# For clarity: CI for Automatic group and Manual group individually
t.test(mtcars$mpg[mtcars$am=="Automatic"])$conf.int
t.test(mtcars$mpg[mtcars$am=="Manual"])$conf.int



# 1. Pearson correlation between mpg and hp (mtcars)
cor_mpg_hp <- cor(mtcars$mpg, mtcars$hp, method = "pearson", use = "complete.obs")
cor_mpg_hp

# 2. Scatterplot with regression line mpg ~ hp
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = paste0("mpg vs hp (Pearson r = ", round(cor_mpg_hp,3), ")"))

# 3. Correlation matrix for numeric columns in mtcars
num_mtcars <- mtcars %>% select_if(is.numeric)
cor_matrix <- cor(num_mtcars, use = "pairwise.complete.obs")
cor_matrix

# 4. Spearman rank correlation between Sepal.Length and Petal.Length
cor_spear <- cor(iris$Sepal.Length, iris$Petal.Length, method = "spearman")
cor_spear

# 5. Association rule mining (arules) — small example market-basket dataset
library(arules)
# Example transactions
trans_list <- list(
  c("bread","milk"),
  c("bread","diaper","beer","eggs"),
  c("milk","diaper","beer","cola"),
  c("bread","milk","diaper","beer"),
  c("bread","milk","diaper","cola")
)
trans <- as(trans_list, "transactions")
summary(trans)
rules <- apriori(trans, parameter = list(supp = 0.2, conf = 0.6))
inspect(sort(rules, by = "lift"))
