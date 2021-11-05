data {
  int<lower=0> p;     // number of persons
  int<lower=0> s[p];  // number of signal trials == old trials of person p
  int<lower=0> n[p];  // number of noise  trials == new trials of person p
  int<lower=0> h[p];  // number of hits of person p
  int<lower=0> fa[p]; // number of false alarms of person p
}


parameters {
  vector[p] d;
  vector[p] c;
}


transformed parameters {
  vector<lower=0,upper=1> [p] hit_rate;
  vector<lower=0,upper=1> [p] fa_rate;
 
  for (i in 1:p){
    hit_rate[i] = Phi_approx(d[i]/2-c[i]);
    fa_rate[i]  = Phi_approx(-d[i]/2-c[i]);
  }
}


model {
// Priors
  d ~ normal(0, 1);
  c ~ normal(0, 1);

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