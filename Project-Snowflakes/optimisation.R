#optimisation thing
thingy = snowdata2
snowdata3 = snowdata2
snowdata3$number = as.integer(snowdata3$retained....* snowdata3$particles.detected/100)
reallikelihood = function( params,x = snowdata3){
  mu1 = params[1]
  mu2 = params[2]
  sigma1_sq = params[3]
  sigma2_sq = params[4]
  tau = params[5]
  temp = 0
  for(i in 1:nrow(x)){
    temp = temp +x[i,7] * log(plnorm(x[i,3]) - plnorm(x[i,2]))
  }
  return(temp)
}
final = optim(par = params, reallikelihood)
final$par
