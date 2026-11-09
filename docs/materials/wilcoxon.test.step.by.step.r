# Wilcoxon signed rank test step by step

# Wilcoxon signed rank test (one sample) # ----

# Exampl: Generate sample
height <-c(21, 34, 28, 36, 31, 34, 33, 34, 32, 31, 29, 20, 35, 30, 23)

# H0: "The true heights are larger than or equal to mu where mu is the mean."
mu <- 31.4

# Sample sizes
n <- length(height)

# 1. Determine difference from mu 
(x <- height - mu)

# 1. Determine ranks of absolute differences
(ranks <- rank(abs(x)))

# 3. Calculate test statistic V via rank-sum for all 
# positive differences 
(V <- sum(ranks[x > 0]))

# 4. Calculate p-values
psignrank(V, n, lower.tail=TRUE)  
# lower.tail=TRUE mirrors the alternative hypothesis H1: 
# "The true heights are smaller than mu"

# For comparison in one step with wilcox.test()
# (p-values are not entirely equal, as wilcox.text() includes some corrections)
wilcox.test(height, mu=31.4, alternative='less')


# Wilcoxon Rank-Sum Test (two-sample; also called Mann-Whitney U-test) # ----

# Model data
height.1 <-c(21, 34, 28, 36, 31, 34, 33, 34, 32, 31, 29, 20, 35, 30, 23)
height.2 <-c(31, 32, 29, 29, 21, 32, 32, 27, 29, 26, 28, 30, 23, 33, 35)

# Sample sizes
n1 <- length(height.1)
n2 <- length(height.2)

# 1. Combine into one common vector 
(heights <- c(height.1, height.2))

# 2. Determine ranks
(ranks <- rank(heights))

# 3. Calculate test statistic W via rank-sum and sample sizes for height.1 
# (the result of this algorithm is equal to the number of 
# all possible x-y pairs, for which y is smaller than x)
(W <- sum(ranks[1:length(height.1)]) - n1 * (n1 + 1) / 2)

# 4. Calculate p-values
pwilcox(W, n1, n2, lower.tail=FALSE)

# For comparison in one step with wilcox.test()
# (p-values are not entirely equal, as wilcox.text() includes some corrections)
wilcox.test(height.1, height.2, alternative='greater')


