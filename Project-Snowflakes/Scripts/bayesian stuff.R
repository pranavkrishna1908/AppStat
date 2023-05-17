#bayesian stuff
#we want to do monte carlo
library(ggplot2)

#sett he value of y
Y = 2;sqr = sqrt(0.6)
unnormalised_density = function(x,y){
  likelihood = dnorm(Y, mean = x, sd = 0.6)
  prior = dweibull(x, shape = 2, scale = 1.2)
  return(prior * likelihood)
}#here, sd is o.6
candidate = function(Y){
  return(rnorm(n = 1, mean = Y, sd = sqr))
}#here, variance in 0.6
alpha = function(present_state, candidate_state){
  temp1 = unnormalised_density(candidate_state,Y) * dnorm(present_state, mean = Y, sd = sqr)
  temp2 = unnormalised_density(present_state,Y) * dnorm(candidate_state, mean = Y, sd = sqr)
  return(min(1, temp1 / temp2))
  #beta<-log(dnorm(x=Y, mean=X_prop, sd=0.6))+log(dweibull(x=X_prop, shape=2, scale = 1.2, log = FALSE))+log(dnorm(x=X_actual, mean=Y, sd=sqrt(0.6))) - log(dnorm(x=Y, mean=X_actual, sd=0.6))-log(dweibull(x=X_actual, shape=2, scale = 1.2, log = FALSE))-log(dnorm(x=X_prop, mean=Y, sd=sqrt(0.6)))
  #return(beta)
}
#acceptance-rejection
next_state = function(present_state){
  candidate_state = candidate(present_state)
  alpha = alpha(present_state, candidate_state)
  rv = runif(1)
  if (rv < alpha) return(candidate_state)
  return(present_state)
}
#taken from slides
dposterior <- function(x, y, scale = 1.2, sd = .6) {
  # x: evaluation points of the density
  # y: observation Y=y (length 1),
  # scale: scale parameter of Weibull prior (shape=2 fixed)
  # sd: standard deviation of Gaussian error (mean=0 fixed)
  a <- 1/2*1/sd^2; c <- 1/scale^2
  erf <- function(x) 2*pnorm(x*sqrt(2)) - 1
  k <- ifelse(x >= 0, x * exp( -a * (x-y)^2 - c*x^2 ), 0)
  n <- exp(-a*(y^2)) *     (sqrt(pi) * a * y * exp(a^2*y^2 / (a+c)) *
                              (erf(a*y/sqrt(a+c)) + 1) +
                              sqrt(a + c) ) / (2* (a+c)^(3/2))
  k/n
}
#starting the chain now
chain = rep(0,21001)
chain[1] = Y
for(i in 1:21000){
  chain[i+1] = next_state(chain[i])
}
#remove burn-in
chain = chain[10001:21001]
thing1 = density(chain)
x_vals = thing1$x
y_vals = thing1$y
otherx = seq(0,5, by = 0.001)
othery = dposterior(otherx ,y = Y)
ggplot()+
  geom_point(data = as.data.frame(cbind(x_vals, y_vals)), aes(x = x_vals, y = y_vals))+
  geom_point(data = as.data.frame(cbind(otherx, othery)), aes(x = otherx, y = othery, colour = 'red'))
"plot(x_vals, y_vals)
points(otherx, othery)"