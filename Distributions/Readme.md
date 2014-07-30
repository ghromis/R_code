```R
sample <- rnorm(100, mean=1, sd=2)
hist(sample, prob=TRUE, yaxt='n', main="Best fitting normal curve", ylab="", xlab="Data sample")
curve(dnorm(x, mean=mean(sample), sd=sd(sample)), add=TRUE, col="blue", lwd=2.5)
```

![ScreenShot](https://raw.github.com/ghromis/DataProjects/master/Distributions/Normal.jpeg)

```R
# Student's t distribution
x <- seq(-5,5, by=0.01)
probt <- dt(x, df=1)
plot(x, probt, type='l', col='blue', main = "Student's t distribution",
     xlab = "Random Variable",xlim = c(-5,5), ylim=c(0, 0.4), lwd = 2.5, ylab = "Probability")
par(new=TRUE)
probt <- dt(x, df=5)
plot(x, probt, type='l', col='red', xlab = "", ylab='',xaxt = "n", yaxt="n",
     lwd = 2.5, xlim = c(-5,5), ylim=c(0, 0.4))
par(new=TRUE)
probt <- dt(x, df=100)
plot(x, probt, type='l', col='dark green', xlab = "", ylab='', xaxt = "n", yaxt="n",lwd = 2.5,
     xlim = c(-5,5), ylim=c(0, 0.4))
legend(2.7, 0.4, c("df=1", "df=5", "df=100"), lty=1, lwd= 2.5,col=c("blue","red", "dark green"))
```
![ScreenShot](https://raw.github.com/ghromis/DataProjects/master/Distributions/Students.jpeg)

```R
# Chi-Squared
curve(dchisq(x, 2), main = "Chi squared distribution", xlim = c(0,25), ylim=c(0, 0.5), col='blue',
      lwd = 2.5, ylab='')
par(new=TRUE)
curve(dchisq(x, 4), xlim = c(0,25), ylim=c(0, 0.5),col='dark green',
      lwd = 2.5, xaxt = "n", yaxt="n", xlab = "", ylab='')
par(new=TRUE)
curve(dchisq(x, 9), xlim = c(0,25), ylim=c(0, 0.5),col='red',
      lwd = 2.5, xaxt = "n", yaxt="n", xlab = "", ylab='')
par(new=TRUE)
curve(dchisq(x, 14), xlim = c(0,25), ylim=c(0, 0.5),col='purple',
      lwd = 2.5, xaxt = "n", yaxt="n", xlab = "", ylab='')
legend(15, 0.5, title = "Degrees of Freedom",
       c("k=2", "k=4", "k=9", "k=14"),
       lty=1, lwd= 2.5,col=c("blue","green", "red", "purple"))
```
![ScreenShot](https://raw.github.com/ghromis/DataProjects/master/Distributions/ChiSquared.jpeg)
```R
# Geometric
x <- seq(0,10, by=1)
probg <- dgeom(x, 0.2)
plot(x, probg, type='l', col='blue', lwd = 2, main = "Geometric Distribution",
     xlab = "Random Variable", ylab="Probability", ylim=c(0, 0.8),)
par(new=TRUE)
probg<- dgeom(x, 0.5)
plot(x, probg, type='l', col='red', lwd = 2, xlab="",ylab ="", ylim=c(0, 0.8))
par(new=TRUE)
probg <- dgeom(x, 0.8)
plot(x, probg, type='l', col='dark green', lwd =2, xlab="",ylab="", ylim=c(0, 0.8))
legend(8, 0.8, c("p=0.2","p=0.5","p=0.8"), lty=1, lwd= 2,col=c("blue","red", "dark green"))
```
![ScreenShot](https://raw.github.com/ghromis/DataProjects/master/Distributions/Geometric.jpeg)
```R
# Binomial
x <- seq(1,100, by=1)
probx <- dbinom(x, 20, 0.5)
plot(x, probx, type='h', col='blue', main = "Binomial Distribution", xlab = "Random Variable",
     ylab="", ylim=c(0, 0.18))
par(new=TRUE)
probx <- dbinom(x, 50, 0.5)
plot(x, probx, type='h', col='red', main = "Binomial Distribution", xlab = "",
     ylab="", ylim=c(0, 0.18))
par(new=TRUE)
probx <- dbinom(x, 100, 0.5)
plot(x, probx, type='h', col='dark green', main = "Binomial Distribution", xlab = "", 
     ylab="", ylim=c(0, 0.18))
legend(66, 0.18, c("n=20, p=0.5","n=50, p=0.5","n=100, p=0.5"), lty=1, lwd= 2,
       col=c("blue","red", "dark green"))
```
![ScreenShot](https://raw.github.com/ghromis/DataProjects/master/Distributions/Binomial.jpeg)
```R
# Negative Binomial
x <- seq(1,100, by=1)
probx <- dnbinom(x, 10, 0.5)
plot(x, probx, type='h', col='blue', main = " Negative Binomial Distribution", xlab = "Random
Variable", ylab="", ylim=c(0, 0.1))
par(new=TRUE)
probx <- dnbinom(x, 40, 0.5)
plot(x, probx, type='h', col='red', xlab = "", ylab="", ylim=c(0, 0.1))
par(new=TRUE)
probx <- dnbinom(x, 70, 0.5)
plot(x, probx, type='h', col='dark green', yaxt='n', xlab = "",
     ylab="Probability", ylim=c(0, 0.1))
legend(66, 0.1, c("n=10, p=0.5","n=40, p=0.5","n=70, p=0.5"), lty=1, lwd= 2,
       col=c("blue","red", "dark green"))
```
![ScreenShot](https://raw.github.com/ghromis/DataProjects/master/Distributions/NegBinomial.jpeg)

```R
# Poisson
x <- seq(0,100, by=1)
probp <- dpois(x, 1)
plot(x, probp, type='l', col='blue', main = "Poisson Distribution",xlab = "Number of Occurances",
     xlim = c(0,20), ylim=c(0, 0.4), lwd = 2.5, ylab = "Probability")
par(new=TRUE)
probp <- dpois(x, 5)
plot(x, probp, type='l', col='red', xlab = "", ylab='',xaxt = "n", yaxt="n",
     xlim = c(0,20), ylim=c(0, 0.4), lwd = 2.5)
par(new=TRUE)
probp <- dpois(x, 10)
plot(x, probp, type='l', col='dark green', xlab = "", ylab='', xaxt = "n", yaxt="n",
     xlim = c(0,20), ylim=c(0, 0.4),lwd = 2.5)
legend(15, 0.4, c(expression(lambda~"=1"),expression(lambda~"=5"),expression(lambda~"=10")),
       lty=1, lwd= 2.5,col=c("blue","red", "dark green"))
```
![ScreenShot](https://raw.github.com/ghromis/DataProjects/master/Distributions/Poisson.jpeg)
