# Rationale of one-sample t-tests

# Create population
# Pretend that it is unknown
set.seed(2018)
unknown.population <- rnorm(100)
hist(unknown.population, main="", ylim=c(0, 25), lty=2)

# Calculate population mean
# Symbol: Greek mu
(unknown.mu <- mean(unknown.population))
abline(v=unknown.mu, lty=2, lwd=2)
text(-1, 23, "unknown.mu", cex=0.7)

# Set sample size n 
n <- 10

# Extract sample from population
set.seed(2018)
(some.sample <- sample(unknown.population, n))

# Calculate mean x_bar and standard deviation s
# of sample
(x.bar <- mean(some.sample))
(s <- sd(some.sample))

# Present sample graphically 
hist(some.sample, add=TRUE, col="darkgreen")
abline(v=x.bar, lwd=2, col="darkgreen")
text(0.5, 21, labels=expression(bar(x)), cex=0.7, col="darkgreen")

# Estimate deviation of sample mean 
# from unknown population mean
# according to SEM 
# (standard error of the mean): 
# Rationale: the wider the sample distribution (s) 
# and the smaller the sample size (n), 
# the greater the uncertainty that x_bar reproduces 
# the mean mu and the larger the SEM.
(SEM <- s / sqrt(n))

# Question: Does the sample mean x_bar correspond to
# a given mu_question? In other words, does my sample 
# derive from a population with mean mu_question?
# Null hypothesis: x_bar is equal to mu_question.
mu.question <- 1.7 
abline(v=mu.question, lwd=2, col="turquoise")
text(mu.question + 0.6, 21, "mu.question", cex=0.7, col="turquoise")
# 1. Determine difference of mu_question and x_bar 
(difference.x.bar.mu.question <- x.bar - mu.question)
# 2. If the answer is 'Yes', this
# difference should approach 0. It is necessary to take into consideration
# that x_bar may not be a good estimate of the true mu.
# That is why we divide this difference by the 
# standard error of x_bar. 
# Since the resulting quotient has a t-distribution,
# it is also called the t-value:
(t.some.sample <- difference.x.bar.mu.question / SEM)
# 3. We can now determine using the t-distribution whether
# this t.some.sample-value is more extreme than the t-value, which sets 
# the bounds of the most extreme 2.5% of the probability measure under the 
# density function of the t-distribution
# ( 2 * 2.5% = 5% -> the significance level alpha
# or the significance level p; "2 *" because 
# both ends of the distribution must be cropped
# because we reject the null hypothesis
# when the t-value is larger or smaller than 0 -> two-sided test)
# First determine the degrees of freedom:
df <- n - 1
# Then determine the critical t-value for the outermost 2.5%
# in the negative direction, thus 0.025 rather than 0.975
(t.crit <- qt(0.025, df))

# t.some.sample = -3.53 < t.crit = -2.26, thus the 
# t.some.sample-value is more extreme than the critical 
# t-value, therefore:
# Reject null hypothesis (with p < 0.05) and answer question negatively.

# For info: Calculate p-value
(p.some.sample <- 2 * min (pt(t.some.sample, df), 1 - pt(t.some.sample, df)))

# Comparable with:
t.test(some.sample, mu=mu.question, alternative="two.sided")

c(round(t.some.sample, 4),
  round(df, 1),
  t.crit, 3,
  p.some.sample)
