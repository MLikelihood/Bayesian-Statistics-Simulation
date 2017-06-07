#Bayesian statistics homework 1
install.packages("plyr")
install.packages("ggplot2")
library(plyr)
library(ggplot2)
library(reshape2)
n = 10000
d = data.frame(a=2^(-1:1), b=2^(-1:1))
# simulation of 1000 realization of inverse gamma
dinvgamma = function(x,a,b) dgamma(1/x,a,b)/x^2
dsqrtinvgamma = function(x, a, b) dinvgamma(x^2, a, b)*2*x
par(mfrow=c(3,3), mar=c(5,4,0,0)+.1)
d_ply(d, .(a), function(x) {
      a = x$a
      b = x$b
      phi = rgamma(n,a,b)
      sigma2 = 1/phi
      sigma = sqrt(sigma2)
      hist(phi, 100, freq=F, main="", ylab=paste("a=b=",a))
      curve(dgamma(x,a,b), add=TRUE, col="red", lwd=2)
      hist(sigma2, 100, freq=F, main="", ylab="")
      curve(dinvgamma(x,a,b), add=TRUE, col="red", lwd=2)
      hist(sigma, 100, freq=F, main="", ylab="")
      curve(dsqrtinvgamma(x, a, b), add=TRUE, col="red", lwd=2)
      }, .inform=TRUE)



#poisson ---prior and posterior
alpha=beta=1
y = c(4, 4, 5, 8, 3)
curve(dgamma(x, alpha+sum(y), beta+length(y)), 0, 10, col="red", lwd=2, main="Poisson model, gamma prior",
      xlab=expression(lambda), ylab="Density", ylim=c(0,1))
curve(dgamma(x, 1, 1), col="blue", lwd=2, add=TRUE)
legend("topright", c("Prior","Posterior"), col=c("blue","red"), lwd=2)


#negative binomial---prior and posterior 
xx = 0:15
alpha.n = alpha+sum(y)
beta.n = beta+length(y)
# Be careful with the parameterization,
# size is the number of successful, rather than unsuccessful, attempts.
# I fix this by taking prob = 1-p = beta/(beta+1)
plot(xx, dnbinom(xx, alpha, beta/(beta+1)),
     pch=19, col="blue", main="Negative binomial",
     xlab=expression(tilde(y)), ylab="PMF")
points(xx, dnbinom(xx, alpha.n, beta.n/(beta.n+1)), pch=19, col="red")
legend("topright", c("Prior","Posterior"), col=c("blue","red"), pch=19)



#homework 2
library(plyr)
library(reshape2)
set.seed(1)
#initialize the simulation constants
J = 1000
n = 5
a = b = 1
#Draw J lambda values from a gamma(a,b)
lambda = rgamma(J, a, b)
#get five y values for each lambda (in basic data form)
dat = adply(lambda, 1, function(x){
            data.frame(lambda=x, y=rpois(n, x))
            })

#for each lambda, get the posterior gamma parameters
sum = ddply(dat, .(X1), summarize,
            alpha = a + sum(y),
            beta = b + length(y))
#get posterior statistics
sum$mean = sum$alpha/sum$beta
sum$median = qgamma(.5, sum$alpha, sum$beta)
sum$mode = (sum$alpha-1)/sum$beta
#identify the true value for each iteration
sum$lambda = lambda
#create table comparing the true value to the posterior statistics
e = 0.01
res = ddply(melt(sum, id.vars=c("X1","lambda","alpha","beta")),
            .(variable), summarize,
            mse = mean((lambda-value)^2),
            abs = mean(abs(lambda-value)),
            ste = mean(abs(lambda-value)>e))
#output this result
res






