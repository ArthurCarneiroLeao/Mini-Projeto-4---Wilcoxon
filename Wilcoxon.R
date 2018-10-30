
###-------------------Estatística Wilconxon------------###

r <- 1000           
mu0 <- 0       
mu.test <- c(seq(-3.5, 4, .3))   
M <- length(mu.test)             
power <- numeric(M)               
nobs <- c(20, 60, 100, 200)        
power_nobs <- matrix(0,length(nobs),M)  
cont <- 1
for(j in nobs) {
  for (i in 1:M) {
    mu <- mu.test[i]
    p_value <- replicate(r, expr = {
      x <- rnorm(j, mu0, 1)
      y <- rnorm(j, mu, 1)
      w_test <- wilcox.test(x, y, paired = T, alternative = "two.sided")
      w_test$p.value})
    power[i] <- mean(p_value <= 0.05)
  }
  power_nobs[cont,] <- power
  cont = cont+1
}

x11()
par(mfrow=c(2,2))
plot(mu.test, power_nobs[1,], type = "l", xlab = bquote(theta), ylab = "Power", main = "n = 20")
abline(v = 0.0, lty = 1)

plot(mu.test, power_nobs[2,], type = "l", xlab = bquote(theta), ylab = "Power", main = "n = 60")
abline(v = 0.0 , lty = 1)

plot(mu.test, power_nobs[3,], type = "l", xlab = bquote(theta), ylab = "Power", main = "n = 100")
abline(v = 0.0, lty = 1)

plot(mu.test, power_nobs[4,], type = "l", xlab = bquote(theta), ylab = "Power", main = "n = 200", ylim = c(-.001, 1))
abline(v = 0.0, lty = 1)
