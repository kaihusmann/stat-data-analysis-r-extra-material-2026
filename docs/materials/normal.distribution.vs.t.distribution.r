# comparing the standard normal distribution and t distribution

# preparation: create an RStudio Project

# call up a graphics window
windows()

# draw from a standard normal distribution
set.seed(1356)
rn <- rnorm(1000, mean=0, sd=1)
hist(rn, xlim=c(-4, 4), freq=FALSE, xlab="x", 
     main="standard normal distribution vs. t distribution")    

# standard normal distribution
curve(dnorm(x, mean=0, sd=1), col="red", add=TRUE)

# normal distribution fitted to the randomly drawn values
curve(dnorm(x, mean=mean(rn), sd=sd(rn)), col="pink", add=TRUE)

# t distribution applied to the degrees of freedom of the randomly drawn values
curve(dt(x, 999), col="purple", add=TRUE)

# t distribution with fewer degrees of freedom
curve(dt(x, 8), col="green", add=TRUE)

legend('topleft', inset=0.01, bty='n', lty=1, lwd=2, cex=0.8,
       legend=c('standard normal distribution', 'fitted normal distribution', 
                't distribution with df=999', 't distribution with df=8'), 
       col=c('red', 'pink', 'purple', 'green'))

# comparing the normal and the t distributions applied to the height data
treedata <- read.csv ("treedata.csv")
hist(treedata$height, xlab='tree height [m]', ylab='f(x)', main='', breaks=40, freq=FALSE)
mue <- mean(treedata$height)
sigm <- sd(treedata$height)
df <- length(treedata$height - 1)
curve(dnorm(x, mean=mue, sd=sigm), col="red", add=TRUE)
curve(dt(x - mue, df), col="purple", add=TRUE)
legend("topright", legend=c("normal distribution", "t distribution"), col=c("red", "purple"), lty=1)
