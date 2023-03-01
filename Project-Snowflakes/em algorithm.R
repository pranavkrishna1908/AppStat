dmixlnorm <- function(x, mu1, mu2, sigma1, sigma2, tau){
  y <- (1-tau)*dlnorm(x,mu1,sigma1) + tau*dlnorm(x,mu2,sigma2)
  return(y)
}
likelihood <- function(x, mu1, mu2, sigma1, sigma2, tau){
  temp = 0
  for (i in 1:length(x)) {
    temp = temp + log(dmixlnorm(x[i], mu1,mu2,sigma1, sigma2, tau))
  }
  return(temp)
}
num = 2000
temp1 = rlnorm(num,1,4)
tau = runif(num)
tau = tau<0.5
sample = rlnorm(num)*tau + temp1 *(1-tau)
mu1 = 0
mu2 = 1
sigma1_sq = 1
sigma2_sq = 4
tau = 0.5
N = num
likelihood_obs = likelihood(sample,  mu1, mu2, sigma1_sq, sigma2_sq, tau)
p = rep(0, num)
piece1 = dlnorm(sample ,mean = mu2, sd = sqrt(sigma2_sq))*tau
piece2 = dmixlnorm(sample, mu1, mu2, sqrt(sigma1_sq), sqrt(sigma2_sq), tau)
p = piece1/piece2
next_tau = sum(p)/N
next_mu1 = (sum((1-p)*log(sample)))/(N - sum(p))
next_mu2 = sum(p*log(sample))/sum(p)
next_sigma1_sq = sum((1-p)*((log(sample) - next_mu1)**2))/(N - sum(p))
next_sigma2_sq = sum(p*(log(sample) - next_mu2)**2)/sum(p)
likelihood_obs_next = likelihood(sample, next_mu1,next_mu2,sqrt(next_sigma1_sq),sqrt(next_sigma2_sq),next_tau)
print(likelihood_obs_next)
tau = next_tau
mu1 = next_mu1
mu2 = next_mu2
sigma1_sq = next_sigma1_sq
sigma2_sq = next_sigma2_sq
iter = 1
tol = 0.0001
while (abs(likelihood_obs_next - likelihood_obs) > tol) {
  print(11)
  piece1 = dlnorm(sample ,mean = mu2, sd = sqrt(sigma2_sq))*tau
  piece2 = dmixlnorm(sample, mu1, mu2, sqrt(sigma1_sq), sqrt(sigma2_sq), tau)
  p = piece1/piece2
  next_tau = sum(p)/N
  next_mu1 = (sum((1-p)*log(sample)))/(N - sum(p))
  next_mu2 = sum(p*log(sample))/sum(p)
  next_sigma1_sq = sum((1-p)*((log(sample) - next_mu1)**2))/(N - sum(p))
  next_sigma2_sq = sum(p*(log(sample) - next_mu2)**2)/sum(p)
  likelihood_obs = likelihood_obs_next
  likelihood_obs_next = likelihood(sample, next_mu1,next_mu2,sqrt(next_sigma1_sq),sqrt(next_sigma2_sq),next_tau)
  tau = next_tau
  mu1 = next_mu1
  mu2 = next_mu2
  sigma1_sq = next_sigma1_sq
  sigma2_sq = next_sigma2_sq
  iter = iter + 1

}
print(c(next_mu1,next_mu2,sqrt(next_sigma1_sq),sqrt(next_sigma2_sq),next_tau))
print(iter)

