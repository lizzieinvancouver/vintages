
data {
  int<lower=1> N;
  vector[N] avg_rank; 		// response
  int<lower=1> n_location;
  int<lower=1, upper=n_location> location[N];
  int<lower=1> n_variety;
  int<lower=1, upper=n_variety> variety[N];
  vector[N] precip;
  vector[N] gdd;
  vector[N] year;
  
}
	
parameters {
  real<lower=0, upper=100> base_rank;
  real<lower=0> sigma_location;
  real<lower=0> sigma_variety;
  real<lower=0> sigma_rank; 
  real a_location[n_location]; // intercept for location
  real a_variety[n_variety]; // intercept for variety
  real b_precip;
  real b_gdd;
  real b_year;
}

transformed parameters {
  real mu_y[N];
  real rank_location[n_location];
  real rank_variety[n_variety];

  for(i in 1:N){
    mu_y[i] = base_rank + a_location[location[i]] + a_variety[variety[i]] + b_precip * precip[i] + b_gdd * gdd[i] + b_year * (year[i] - 1);
  }

  for(i in 1:n_location){
    rank_location[i] = base_rank + a_location[i];
  }

  for(i in 1:n_variety){
    rank_variety[i] = base_rank + a_variety[i];
  }
  
}
	
model {   	
  // likelihood
  for(i in 1:N){
    avg_rank[i] ~ normal(mu_y[i], sigma_rank);
  }

  a_variety ~ normal(0, sigma_variety);
  a_location ~ normal(0, sigma_location);
  
  // priors
  base_rank ~ normal(90, 5);
  sigma_rank ~ normal(5, 2);
  sigma_location ~ normal(5, 2);
  sigma_variety ~ normal(5, 2);
  b_precip ~ normal(0, .1);
  b_gdd ~ normal(0, .1);
  b_year ~ normal(0, 1);
}

generated quantities {
  vector[N] pred_rank;

  for(i in 1:N){
    pred_rank[i] = normal_rng(mu_y[i], sigma_rank);
  }
  
}
