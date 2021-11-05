data {
  int<lower=0> p;     // number of persons
  int<lower=0> s[p];  // number of signal trials == old trials of person p
  int<lower=0> n[p];  // number of noise  trials == new trials of person p
  int<lower=0> h[p];  // number of hits of person p
  int<lower=0> fa[p]; // number of false alarms of person p
}

parameters {
  real mu_d;
  real<lower=0> sigma_d;
  
  real mu_c;
  real<lower=0> sigma_c;
  
  vector[p] d;
  vector[p] c;
  
  real<lower=1, upper = 100> noise_sigma;
}


transformed parameters {
  vector<lower=0,upper=1> [p] hit_rate;
  vector<lower=0,upper=1> [p] fa_rate;
  real<lower=0> n_sigma_d;
  real<lower=0> n_sigma_c;
 
  for (i in 1:p){
    hit_rate[i] = Phi_approx(d[i]/2-c[i]);
    fa_rate[i]  = Phi_approx(-d[i]/2-c[i]);
  }
  
  
  
  n_sigma_d = sigma_d * noise_sigma;
  n_sigma_c = sigma_c * noise_sigma;
  
}

model {
// Priors
  mu_d ~ normal(0, 1);
  mu_c ~ normal(0, 1);
  
  sigma_d ~ exponential(1);
  sigma_c ~ exponential(1);

// Individual level parameters
  d ~ normal(mu_d, n_sigma_d);
  c ~ normal(mu_c, n_sigma_c);

// Likelihood
  h  ~ binomial(s, hit_rate);
  fa ~ binomial(n, fa_rate);
} 


generated quantities {

  vector<lower=0> [p] h_pred;
  vector<lower=0> [p] fa_pred;
 
  for (i in 1:p){
    h_pred[i]  = binomial_rng(s[i], hit_rate[i]);
    fa_pred[i] = binomial_rng(n[i], fa_rate[i]);
  }
}