data {
  int<lower=0> N;
  vector[N] partial_evac_cost;
  vector[N] partial_stay_cost;
  vector<lower=0,upper=1>[N] safety_prob;
  int<lower=0,upper=1> evac_decision[N];
  int<lower=1> L;
  int<lower=1,upper=L> group[N];
}
parameters {
  real mu_safety_util;
  real mu_noise_util;
  real<lower=0> sigma_safety_util;
  real<lower=0> sigma_noise_util;
  real safety_util[L];
  real noise_util[L];
}
model {
  real evac_eu;
  real stay_eu;
  real max_eu = 0;
  real p_evac;
  
  //safety_util ~ normal(3, 0.5);
  //noise_util ~ normal(3, 0.5);
  
  mu_safety_util ~ normal(0, 1);
  mu_noise_util  ~ normal(0, 1);
  sigma_safety_util ~ cauchy(0, 2.5);
  sigma_noise_util  ~ cauchy(0, 2.5);
  
  for(l in 1:L)
  {
    safety_util[l] ~ normal(mu_safety_util, sigma_safety_util);
    noise_util[l]  ~ normal(mu_noise_util, sigma_noise_util);
  }
  
  for(n in 1:N)
  {
    evac_eu = -1*(partial_evac_cost[n] + noise_util[group[n]]);
    stay_eu = -1*(partial_stay_cost[n] + safety_prob[n]*safety_util[group[n]]);
    
    //max_eu  = fmax(evac_eu, stay_eu);
    evac_eu = exp( (evac_eu - max_eu));
    stay_eu = exp( (stay_eu - max_eu));
    
    p_evac  = evac_eu/(evac_eu + stay_eu);
    
    evac_decision[n] ~ bernoulli( p_evac );
  }
}
