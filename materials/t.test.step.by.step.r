## One-sample t-test step by step

# DBH of 15 randomly selected spruce trees
dbh <-c(8, 11, 8, 6, 9, 8, 7, 12, 14, 7, 9, 12, 10, 8, 12)

# t-test step by step
n <- length(dbh)  # sample size
df <- n - 1  # degrees of freedom
(t.crit <- qt(0.95, df))  # critial t-value for a significance level of 0.05
m <- mean(dbh)  # sample mean
mu <- 11.5  # hypothesized population mean (to be tested)
sem <- sqrt(var(dbh) / n)  # Standard error of the mean
(t.sample <- (m - mu) / sem)
(p.sample <- pt(t.sample, df))  # default: lower.tail=TRUE, 
# thus null hypothesis: True mean is equal to or larger than "mu"
# and alternative hypothesis: True mean is smaller than "mu"

# Comparative value calculated in one step with t.test()
(result <- t.test(dbh, mu=11.5, alternative="less"))  # Caution! default would be here alternative="two-sided",
# alternative must therefore be set at "less" 
# explicitly for comparability 


## Two-sample t-test

# Fundamental question: Do the two sample means derive from different populations 
#             or from the same population?

# dbhs of 15 randomly selected spruces in two stands
dbh.1 <-c(8, 11, 8, 6, 9, 8, 7, 12, 14, 7, 9, 12, 10, 8, 12)
dbh.2 <-c(15, 9, 13, 12, 10, 8, 10, 15, 7, 9, 13, 6, 10, 8, 6)

# Sample sizes
n1 <- length(dbh.1)
n2 <- length(dbh.2)

# Degrees of freedom for two-sample t-test
df <- n1 + n2 - 2

# Sample means
m1 <- mean(dbh.1)
m2 <- mean(dbh.2)

# Sample variances
(var1 <- var(dbh.1))
(var2 <- var(dbh.2))

# Standard error of difference SED
sed <- sqrt(var1 / n1 + var2 / n2)

# t-test statistic for Welch test with unequal variances
(t.sample.welch <- (m1 - m2) / sed)
# --> t-test statistic can be interpreted as the quantity 
#     Standard error of the difference that separates the two sample means

# p-values for the different null hypotheses
(p.sample.less <- pt(t.sample.welch, df, lower.tail=TRUE))
(p.sample.greater <- pt(t.sample.welch, df, lower.tail=FALSE))  # = (1 - p.sample.less)
(p.sample.twosided <- 2 * min(p.sample.less, p.sample.greater))

## Comparative value
t.test(dbh.1, dbh.2, alternative="less")
t.test(dbh.1, dbh.2, alternative="greater")
t.test(dbh.1, dbh.2, alternative="two.sided")


# t-test statistic with equal variances
(t.sample <- sqrt(n1 * n2 / (n1 + n2)) * (m1 - m2) / 
    sqrt(((n1 - 1) * var1 + (n2 - 1) * var2) / df))

# p-values for the different null hypotheses
(p.sample.less <- pt(t.sample, df, lower.tail=TRUE))
(p.sample.greater <- pt(t.sample, df, lower.tail=FALSE))  # = (1 - p.sample.less)
(p.sample.twosided <- 2 * min(p.sample.less, p.sample.greater))

## Comparative value
t.test(dbh.1, dbh.2, var.equal=TRUE, alternative="less")
t.test(dbh.1, dbh.2, var.equal=TRUE, alternative="greater")
t.test(dbh.1, dbh.2, var.equal=TRUE, alternative="two.sided")


