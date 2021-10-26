
data {
  int<lower=1> N;
  vector[N] avg_rank; 		// response
  int<lower=1> n_location;
  int<lower=1, upper=n_location> location[N];
  int<lower=1> n_variety;
  int<lower=1, upper=n_variety> variety[N];
  vector[N] precip;
  vector[N] gdd;
  // General
  int<lower=1> N_gen;
  vector[N_gen] avg_rank_gen;
  int location_gen[N_gen];
  vector[N_gen] precip_gen;
  vector[N_gen] gdd_gen;
  
}
	
parameters {
  real<lower=0, upper=100> base_rank;
  real<lower=0> sigma_location;
  real<lower=0> sigma_variety;
  real<lower=0> sigma_rank; 
  real a_location[n_location]; // intercept for location
  real a_variety[n_variety]; // intercept for varieties
  real b_precip;
  real b_gdd;
}

transformed parameters {
  real mu_y[N];
  real mu_y_gen[N_gen];
  
  for(i in 1:N){
    mu_y[i] = base_rank + a_location[location[i]] + a_variety[variety[i]] + b_precip * precip[i] + b_gdd * gdd[i];
  }

  for(i in 1:N_gen){
    mu_y_gen[i] = base_rank + a_location[location_gen[i]] + b_precip * precip_gen[i] + b_gdd * gdd_gen[i];
  }

}
	
model {   	
  // likelihood
  for(i in 1:N){
    avg_rank[i] ~ normal(mu_y[i], sigma_rank);
  }

  for(i in 1:N_gen){
    avg_rank_gen[i] ~ normal(mu_y_gen[i], sigma_rank);
  }

  // priors
  base_rank ~ normal(85, 5);
  sigma_rank ~ normal(5, 2);
  a_location ~ normal(0, sigma_location);
  sigma_location ~ normal(5, 2);
  a_variety ~ normal(0, sigma_variety);
  sigma_variety ~ normal(5, 2);
  b_precip ~ normal(0, .5);
  b_gdd ~ normal(0, .5);

}

generated quantities {
  vector[N] pred_rank;
  vector[N_gen] pred_rank_gen;

  for(i in 1:N){
    pred_rank[i] = normal_rng(mu_y[i], sigma_rank);
  }

  for(i in 1:N_gen){
    pred_rank_gen[i] = normal_rng(mu_y_gen[i], sigma_rank);
  }
  
}
