---
layout: single
title: "Hierarchical Signal Detection Model with STAN - Part 2"
excerpt: "Modeling recognition data with a hierarchical signal detection model using Stan"
date: 2021-10-15
tags:
  - cognitive modeling
  - STAN
  - R
permalink: /posts/2021/10/SDT-2/
classes: wide
editor_options: 
  chunk_output_type: console
---




## About this blog post 

This blog post is supposed to be the second post in a series of blog post comparing the *signal detection theory * model (SDT) with a *two-high-threshold* model (2HTM) of recognition. I wrote this blog post for three reasons. First, I wanted to learn more about the modeling of recognition and memory data. Second, I wanted to learn more about STAN, since I mostly use JAGS in my own research. Finally, I also wanted to practice writing, since I take forever when writing my own articles. So the reasons for this blog are rather selfish. However, if anyone ever finds this blog post and finds it helpful, that would be even better!


In this blog, I will build upon the *non-hierarchical SDT* model introduced in the first blog post and make a *hierarchical SDT* model.  Hierarchical Bayesian approaches have become a very popular tool to  estimate latent parameters of cognitive models. There are multiple reasons for this (see Lee & Wagenmakers, 2014 and McElreath for detailed informations). For me, the two most important reasons are:

1. The hierarchical structure of the model naturally reflects the hierarchical structure data, where each participants does several trials of the same task 
2.  Hierarchical models use the information from each individual to inform the estimates of other individuals which leads to more informative and reliable estimates on average (McElreath, 2020).


## Setup                

Before we start with the fun part, we have to load the packages we need for this analysis. This time, I will use the `cmdstanr` package instead of the `stan` package.


```r
library(tidyverse) # contains ggplot, dplyr etc
library(cmdstanr)
library(bayesplot)
library(patchwork)
library(kableExtra)
library(knitr)
library(latex2exp)
library(tidybayes)
library(posterior)

# Set bayesplot theme
bayesplot_theme_set(theme_bw())

theme_set(theme_bw() + theme(plot.title = element_text(hjust = 0.5)))
```


## The hierarchical Signal-Detection-Theory (SDT) model

In the non-hierarchical SDT model, we assumed that each person $p$ had their own $d$ (discriminability parameter)  and $c$ (bias parameter) as defined by the normal prior distributions with $\mu = 0$ and $\sigma = 1$:

$$
\begin{aligned}
d_p &\sim Normal(0,1)\\[.5em]
c_p &\sim Normal(0,1)
\end{aligned}

$$
To implement a hierarchical structure in our model, I now assume that distributions of the individual $d$ and $c$ parameters are defined by four **hyperparameters**: $\mu_d$, $\sigma_d$, $\mu_c$, and $\sigma_c$. The individual level parameters are then based on the distributions defined by the hyperparameters:

$$
\begin{aligned}
d_p &\sim Normal(\mu_d,\sigma_d)\\[.5em]
c_p &\sim Normal(\mu_c,\sigma_c)
\end{aligned}
$$



Now lets quickly recap the data on which we want to use this model.

## The Experiment       

The experiment was conducted as part of a pre-test to select stimuli for a subsequent multiple-cue judgment experiment. The experiment consisted of five blocks, with two phases each. In the learning phase, participants saw 12 different pictures of either plants, houses, aliens, bugs, or plates, (a different stimulus set in each block) with features differing on five cues. The same 12 pictures were presented 4 times for 5 seconds to each participant. After the learning phase, participants were presented again with all 12 old pictures as well as 20 new pictures in the testing phase. In each trial of the testing phase, participants had to indicate if the picture was an old picture or a new one.  These two phases were repeated for each of the five stimulus sets. 

## The Data set         

The data contains the following variables:

- `ID`: participants ID
- `block`: block number 
- `trial`: trial number
- `stimulus`: type of stimulus (house, alien, plant, plate, or bug)
- `response`: response given in this trial (old or new)
- `correctResponse`: correct response in this trial
- `correct`: is `true` if response is equal to `correctResponse`, if not is it `false`


|ID               | block| trial|stimulus |response |correctResponse |correct |
|:----------------|-----:|-----:|:--------|:--------|:---------------|:-------|
|hcpibo8hk7bz2itt |     1|     1|houses   |old      |new             |false   |
|hcpibo8hk7bz2itt |     1|     2|houses   |new      |new             |true    |
|hcpibo8hk7bz2itt |     1|     3|houses   |old      |new             |false   |

Based on these variables, I calculated the number of hits, false alarms, false negatives, and misses, as well as their corresponding rates for each person, in each block. In addition, I calculated $d'$  as $d' = z(HR) - z(FR)$ (Snodgrass &  Corwin, 1988; Stanislaw & Todorov,1999), where HR again is the hit rate and FR is the false-alarm rate.


```r
hits <- dataSDT %>%  
          mutate(IDn            = as.numeric(as.factor(ID)), 
                 hit            = ifelse(response == "old" & correctResponse == "old",1,0),
                 false_positive = ifelse(response == "old" & correctResponse == "new",1,0)) %>% 
          group_by(ID,IDn,stimulus,block) %>% 
          summarize(n_old    = sum(correctResponse == "old"),
                    n_new    = sum(correctResponse == "new"),
                    h        = sum(hit),
                    fa       = sum(false_positive),
                    hit_rate = sum(hit)/n_old, 
                    fa_rate  = sum(false_positive)/n_new) %>% 
          mutate(hit_rate = case_when(
                               hit_rate == 1 ~ .9999999,
                               hit_rate == 0 ~ .0000001,
                               TRUE ~ hit_rate
                            ),
                 fa_rate = case_when(
                               fa_rate == 1 ~ .9999999,
                               fa_rate == 0 ~ .0000001,
                               TRUE ~ fa_rate
                            ),
                 z_h_rate  = qnorm(hit_rate),
                 z_fa_rate = qnorm(fa_rate),
                 dprime    = z_h_rate-z_fa_rate)  

```

Which then gives us the following data structure:


| IDn|stimulus | block| n_old| n_new|  h| fa|  hit_rate| fa_rate|  z_h_rate| z_fa_rate|     dprime|
|---:|:--------|-----:|-----:|-----:|--:|--:|---------:|-------:|---------:|---------:|----------:|
|   1|aliens   |     5|    12|    20|  7| 11| 0.5833333|    0.55| 0.2104284| 0.1256613|  0.0847670|
|   1|bugs     |     2|    12|    20|  7| 13| 0.5833333|    0.65| 0.2104284| 0.3853205| -0.1748921|
|   1|houses   |     4|    12|    20|  7| 14| 0.5833333|    0.70| 0.2104284| 0.5244005| -0.3139721|
|   1|plants   |     3|    12|    20|  7| 12| 0.5833333|    0.60| 0.2104284| 0.2533471| -0.0429187|
|   1|plates   |     1|    12|    20| 11| 11| 0.9166667|    0.55| 1.3829941| 0.1256613|  1.2573328|


Again, we will use only use the data from one stimulus set:


```r
temp <- hits %>% filter(stimulus == "plants")
```

## SDT - Hierarchical - One Stimulus Set 

Now let us begin with the fun part of building our model, which I will build using STAN. I will first present the complete STAN code of the model and then will go through it, step by step.

### The STAN-Model   

This is how our hierarchical SDT model in STAN looks like:


```stan
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
  mu_d ~ normal(0, 1);
  mu_c ~ normal(0, 1);
  
  sigma_d ~ exponential(1);
  sigma_c ~ exponential(1);

// Individual level parameters
  d ~ normal(mu_d, sigma_d);
  c ~ normal(mu_c, sigma_c);

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
```


### The Data       

In the `data` block, I define the data I am using in the model, which is:

- `p` the number of participants
- `s` a vector containing the number of signal trials (i.e., trials with a picture already shown in the learning phase)
- `n` a vector containing the number of noise trials (i.e., the number of new pictures)
- `h` a vector containing the number of hits of each person
- `fa` a vector containing the number if false alarms of each person



```r
stan_data <- list(
  s  = temp$n_old,
  n  = temp$n_new,
  h  = temp$h,
  fa = temp$fa,
  p  = length(unique(temp$ID))
)

stan_data
#> $s
#>  [1] 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12
#> [26] 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12
#> 
#> $n
#>  [1] 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20
#> [26] 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20
#> 
#> $h
#>  [1]  7  9  7 10 11 12 10  7  6  7  7  8  8  7  6 12  6 11  9 12  7  6 10  8  8
#> [26]  6 11  5  7  8  9 10  6  9  6  6  7  5  6  9
#> 
#> $fa
#>  [1] 12 10 16 10 11 20 12 10 13  7 11 12 11 10  6 20 11 11 14 11 10 14 14 17 10
#> [26] 15  9 11 11 13 16 10 12 13 16 11  8 11 11 13
#> 
#> $p
#> [1] 40
```

### The Parameters 

In the `parameters` block, I define the four hyperparameters  $\mu_d$, $\sigma_d$, $\mu_c$, and $\sigma_c$, as well as the individual level parameters $d_p$ and $c_p$ for each person $p$.  These individual level parameters are then transformed in to the *hit rate* and the *false-alarm rate* in the `transformed parameters` block.


### The Model      

In the `model` block, I then specify the model structure, this is how the parameters relate to each other, the prior distributions of the parameters, as well as the binomial likelihood function connecting our parameters and our data. This can be written as: 


$$
\begin{aligned}
\mu_d &\sim Normal(0,1)\\[.5em]
\mu_c &\sim Normal(0,1)\\[.5em]
\sigma_d &\sim Exp(1)\\[.5em]
\sigma_c &\sim Exp(1)
\\[1.5em]
d_p &\sim Normal(\mu_d,\sigma_d)\\[.5em]
c_p &\sim Normal(\mu_c,\sigma_c)
\\[1.5em]
HR_p &= \phi(0.5 \times d_p - c_p) \\[.5em]
FR_p &= \phi(- 0.5 \times d_p - c_p) 
\\[1.5em]
h_p & \sim binomial(HR_p,s_p)\\[.5em]
fa_p &\sim binomial(FR_p,n_p)
\end{aligned}
$$




In the `generated quantities` block I also include variables for later posterior predictive analysis, capturing the predictions of `h` and `fa` based on the current parameter values of each step of the MCMC-chains. 

### The Priors       

As evident from the model block we use a weak Normal($\mu = 0$,$\sigma = 1$) prior for $\mu_d$ and $\mu_c$, as well as an Exp($\lambda$ = 1) prior for $\sigma_d$ and $\sigma_c$. Both distributions are shown here:


<img src="/figs/2021-10-15-SDT2-hierarchical/unnamed-chunk-3-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />


The resulting prior distributions of the individual $d_p$ and $c_p$ parameters and the corresponding distribution after transforming them into hit and false-alarm rates then looks like this:



<img src="/figs/2021-10-15-SDT2-hierarchical/unnamed-chunk-4-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />



## Running the Model    
  
Now that we have defined our STAN model, we are ready to go. I sample from the model using 10.000 iterations, with a rather small warm-up of 2000 iterations and thinning = 4. 


```r
options(mc.cores=4)

mod  <- cmdstan_model("_R/Stan Models/StanModel_oneStim_Hier.stan")

fit2 <- mod$sample(
          data           = stan_data, # named list of data
          chains         = 2,         # number of Markov chains
          iter_warmup    = 2000,      # number of warmup iterations per chain
          iter_sampling  = 10000,     # total number of iterations per chain
          refresh        = 0,         # no progress shown
          thin           = 4,
          adapt_delta    = 0.9)
#> Running MCMC with 2 chains, at most 4 in parallel...
#> 
#> Chain 2 finished in 20.1 seconds.
#> Chain 1 finished in 34.0 seconds.
#> 
#> Both chains finished successfully.
#> Mean chain execution time: 27.0 seconds.
#> Total execution time: 34.6 seconds.

# save results 
posterior_SDT_oS_H <- as_draws_df(fit2$draws())
summary_oS_H       <- fit2$summary() %>% as.data.frame()
```

### Inspect MCMC, Rhat, ESS                 


As a first check of the convergence of our MCMC-chains, let us look at the MCMC-traces of the four hyperparameters:


```r
 mcmc_trace(posterior_SDT_oS_H,
            pars       = vars("mu_c","mu_d","sigma_d","sigma_c"),
            facet_args = list(nrow = 2, labeller = label_parsed))
```

<img src="/figs/2021-10-15-SDT2-hierarchical/unnamed-chunk-6-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />

Although this trace plots look very similar to the hairy caterpillars we had when using the non-hierarchical model, if you look closely you will see that at some point the chains seem to be stuck at some value for a few iterations. To avoid this behavior, we can extend our original Stan model by using a so called *parameter expansion* (see Lee & Wagenmakers, p. XX for detailed information). We will introduce an additional parameter called `noise_sigma`. We will then expand the model with this new parameter in the following way:


$$
\begin{aligned}
noise_{sigma} &\sim Uniform(1,100)
\\[.5em]
\sigma_d &\sim Exp(1)\\[.5em]
\sigma_c &\sim Exp(1)
\\[.5em]
new-\sigma_d &= \sigma_d \times noise_{sigma}\\[.5em]
new-\sigma_c &= \sigma_c \times noise_{sigma}
\\[1.5em]
d_p &\sim Normal(\mu_d,new-\sigma_d)\\[.5em]
c_p &\sim Normal(\mu_c,new-\sigma_c)
\end{aligned}
$$

We basically just add some additional noise, or randomness into the standard deviation of our group-level normal distributions for $d_p$ and $c_p$ by multiplying $\sigma$ with `noise_sigma`.

### The STAN-Model with parameter expansion 

This is how our hierarchical SDT model in STAN then looks like:


```stan

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
  
  real<lower=0, upper = 100> noise_sigma;
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
```


### Running the Model ... again ...         

Now that we have defined our new and hopefully better model, lets run it again.


```r
options(mc.cores=4)

mod  <- cmdstan_model("_R/Stan Models/StanModel_oneStim_Hier_exp.stan")

fit3 <- mod$sample(
          data           = stan_data, # named list of data
          chains         = 2,         # number of Markov chains
          iter_warmup    = 2000,      # number of warmup iterations per chain
          iter_sampling  = 10000,     # total number of iterations per chain
          refresh        = 0,         # no progress shown
          thin           = 4,
          adapt_delta    = 0.9)
#> Running MCMC with 2 chains, at most 4 in parallel...
#> 
#> Chain 1 finished in 55.4 seconds.
#> Chain 2 finished in 60.5 seconds.
#> 
#> Both chains finished successfully.
#> Mean chain execution time: 57.9 seconds.
#> Total execution time: 60.7 seconds.

fit3$save_object(file = "_R/Data/fit_StanModel_Hier.RDS")
```

### Inspect MCMC, Rhat, ESS                 


Let's check our MCMC-traces again:


```r
 mcmc_trace(as_draws_df(fit3$draws()),
            pars = vars("mu_c","mu_d","sigma_d","sigma_c"),
            facet_args = list(nrow = 2, labeller = label_parsed))
```

<img src="/figs/2021-10-15-SDT2-hierarchical/unnamed-chunk-9-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />

So far so good, this looks exactly as you would like it, some nice hairy caterpillars. So our parameter expansion seems to have helped. We could run the chains for a little bit longer, to get better sampling for $\sigma_d$, however, for now this is good enough for me. We can also plot the distributions of $\hat{R}$  and the *effective sample size* again:


```r

p1 <- ggplot(summary_oS_H,aes(x = ess_bulk))+
        geom_histogram(bins = 40, color = "black", fill = "skyblue2")+
        theme_bw() +
        xlim(0,6000)+
        labs(x = "Effective Sample Size",
             y = "Count")

p2 <- ggplot(summary_oS_H,aes(x = rhat))+
        geom_histogram(bins = 40, color = "black", fill = "tomato2")+
        theme_bw() +
        xlim(0.99,2) + 
        labs(x = "Rhat",
             y = "Count")

p1+p2 #patchwork package
```

<img src="/figs/2021-10-15-SDT2-hierarchical/unnamed-chunk-10-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />

This looks also fine. The effective sample sizes are always > 400, which is often recommended, and are near the total sample size of  5000. The parameter with the lowes effective sample size was $sigma_d$ with ESS = 415. In addition, $\hat{R}$ is always very close to 1.


### Posterior Summaries & Densities         

Next, we can look at the summary statistics and plots of the posterior density distributions. First we have a look at the hyperparameters. 


```r
fit2 %>%
  gather_draws(mu_d,sigma_d,mu_c,sigma_c) %>% 
  ggplot(aes(y = .variable, x = .value)) +
    stat_halfeye() +
    labs(x = "Value",
         y = "Parameter")
```

<img src="/figs/2021-10-15-SDT2-hierarchical/unnamed-chunk-11-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />

```r


fit2 %>%
  gather_draws(mu_d,sigma_d,mu_c,sigma_c) %>% 
  mean_hdci() %>% 
  mutate(HDI=paste0("[",round(.lower,2),", ",round(.upper,2),"]")) %>% 
  select(.variable,.value,HDI) %>% 
  kable(format    = "markdown",
        digits    = 2,
        align     = "c",
        col.names = c("Parameter","$M$","95%-HDI"))
```



| Parameter |  $M$  |    95%-HDI     |
|:---------:|:-----:|:--------------:|
|   mu_c    | -0.37 | [-0.49, -0.24] |
|   mu_d    | 0.18  |  [0.02, 0.36]  |
|  sigma_c  | 0.30  |  [0.17, 0.43]  |
|  sigma_d  | 0.24  |  [0.02, 0.46]  |


### The posterior predictive values         

We can also look at the posterior predictive values of `h` and `fa`.  For this, I first combine and then plot the actual observed values from our original data.frame with the MCMC estimates. So lets first create a tidy data.frame only containing the empirical variables we need.



```r
pp_emp <- hits  %>%  filter(stimulus == "plants") %>% ungroup() %>% select(.,ID=IDn,h,fa)

head(pp_emp)
#> # A tibble: 6 x 3
#>      ID     h    fa
#>   <dbl> <dbl> <dbl>
#> 1     1     7    12
#> 2     2     9    10
#> 3     3     7    16
#> 4     4    10    10
#> 5     5    11    11
#> 6     6    12    20
```

Next, we extract the posterior draws for our predicted values. To do this, I use the `spread_draws()` function from the `tidybayes` package, since it makes it very easy to extract a tidy data .frame when we have parameter names like `fa_pred[12]`, where `fa_pred` is the name of our parameter (the predicted number of false alarms), and `[12]` indicates the ID of the participant. 


```r
pp_pred <- fit3 %>% spread_draws(fa_pred[ID],h_pred[ID])

head(pp_pred)
#> # A tibble: 6 x 6
#> # Groups:   ID [1]
#>      ID fa_pred .chain .iteration .draw h_pred
#>   <int>   <dbl>  <int>      <int> <int>  <dbl>
#> 1     1      17      1          1     1      9
#> 2     1       9      1          2     2      5
#> 3     1      16      1          3     3     11
#> 4     1      11      1          4     4      6
#> 5     1      14      1          5     5      8
#> 6     1      11      1          6     6      8
```

Now I can simply join both data.frames together:



```r
pp_df <- pp_pred %>% left_join(.,pp_emp,by = c("ID")) 

head(pp_df)
#> # A tibble: 6 x 8
#> # Groups:   ID [1]
#>      ID fa_pred .chain .iteration .draw h_pred     h    fa
#>   <dbl>   <dbl>  <int>      <int> <int>  <dbl> <dbl> <dbl>
#> 1     1      17      1          1     1      9     7    12
#> 2     1       9      1          2     2      5     7    12
#> 3     1      16      1          3     3     11     7    12
#> 4     1      11      1          4     4      6     7    12
#> 5     1      14      1          5     5      8     7    12
#> 6     1      11      1          6     6      8     7    12
```



This data.frame now contains the empirically observed number of hits and false-alarms, as well as the corresponding posterior predicted draws for each participant. We can now plot everything together, to get an idea of how good our (in-sample) posterior predictions capture the (empirical) reality. To safe some space, I will only do the plots for four participants. 



```r

pp1 <- ggplot(filter(pp_df,ID %in% 1:4),aes(x = h_pred)) +
        geom_histogram(bins=10,color = "black",fill = "skyblue2",alpha=0.5) +
        geom_vline(aes(xintercept =  h),color = "black",lwd = 1.5, lty = "dashed")+
        facet_wrap(.~ID,scales="free") + 
        theme_bw() + 
        labs(x = "(Posterior) Predicted False Alarms",
             y = "Frequency",
             title = "False Alarms")

pp2 <- ggplot(filter(pp_df,ID %in% 1:4),aes(x = fa_pred)) +
        geom_histogram(bins=10,color = "black",fill = "tomato2",alpha=0.5) +
        geom_vline(aes(xintercept =  fa),color = "black",lwd = 1.5, lty = "dashed")+
        facet_wrap(.~ID,scales="free") + 
        theme_bw() + 
        labs(x = "(Posterior) Predicted False Alarms",
             y = "Frequency",
             title = "False Alarms")

pp1 + pp2 #patchwork package
```

<img src="/figs/2021-10-15-SDT2-hierarchical/unnamed-chunk-15-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />

This looks also fine so far. The posterior distribution is symmetrically distributed around the empirical values. In addition, let me also calculate how often the 95% credible interval of the predicted values contains the true values:


```r
fit3 %>%
  spread_draws(fa_pred[ID],h_pred[ID]) %>%
  mean_hdci() %>% 
  left_join(.,pp_emp,by = c("ID"))  %>% 
  summarize(pp_h  = mean(h  > h_pred.lower  & h  < h_pred.upper),
            pp_fa = mean(fa > fa_pred.lower & fa < fa_pred.upper)) %>% 
  as.data.frame() # better output in .md
#>   pp_h pp_fa
#> 1  0.9  0.95
```


Let us also plot the observed against the predicted (i.e., median of the posterior predictive distributions) hits and false alarms for each participant.


```r
ph <- fit3 %>%
        spread_draws(h_pred[ID]) %>%
        mean_hdci() %>% 
        left_join(.,pp_emp,by = c("ID")) %>% 
        ggplot(aes(x = h,h_pred)) +
          geom_smooth(method="lm") +
          geom_point(size=2,shape=21,color="black",fill="grey") +
          labs(x = "Observed Hits",
               y = "Predicted Hits",
               title = "Hits")


pfa <- fit3 %>%
        spread_draws(fa_pred[ID]) %>%
        mean_hdci() %>% 
        left_join(.,pp_emp,by = c("ID")) %>% 
        ggplot(aes(x = fa,fa_pred)) +
          geom_smooth(method="lm") +
          geom_point(size=2,shape=21,color="black",fill="grey") +
          labs(x = "Observed False Alarms",
               y = "Predicted False Alarms",
               title = "False Alarms")

ph + pfa + plot_annotation(tag_levels = "A")
```

<img src="/figs/2021-10-15-SDT2-hierarchical/unnamed-chunk-17-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />

As evident from the plots and the 95% credible interval checks of the posterior predictives, the hierarchical model is also able to recover our data well, however, a little bit worse then the non-hierarchical version of the model. So how can this be ? 

### Comparing non-hierarchical and hierarchical estimates

The following plot shows the number of hits (Plot A) and false alarms (Plot B) as predicted by the hierarchical and the non-hierarchical SDT model. To do this, we first load the old fitted non-hierarchical model and bind the predictions of the non-hierarchical model to the predictions to the hierarchical model. The result is a convenient data.frame for plotting the different estimates:




```r
# load old data
fit_non_hier <- readRDS("_R/Data/fit_StanModel_nonHier.RDS")

# extract what we need and join with predicitons from non-hierarchical model
ph <- fit3 %>%
        spread_draws(h_pred[ID]) %>%
        mean_hdci() %>% 
        left_join(.,pp_emp,by = c("ID")) %>% 
        bind_cols("h_pred_non_h" = fit_non_hier %>%
                                      spread_draws(h_pred[ID]) %>%
                                      mean_hdci() %>%  pull(h_pred)
                  ) %>% 
        select(ID,h_pred,h_emp = h,h_pred_non_h)


pfa <- fit3 %>%
        spread_draws(fa_pred[ID]) %>%
        mean_hdci() %>% 
        left_join(.,pp_emp,by = c("ID")) %>% 
        bind_cols("fa_pred_non_h" = fit_non_hier %>%
                                      spread_draws(fa_pred[ID]) %>%
                                      mean_hdci() %>%  pull(fa_pred)
                  )%>% 
        select(ID,fa_pred,fa_emp = fa,fa_pred_non_h)


head(ph,5)
#> # A tibble: 5 x 4
#>      ID h_pred h_emp h_pred_non_h
#>   <dbl>  <dbl> <dbl>        <dbl>
#> 1     1   7.69     7         6.93
#> 2     2   8.06     9         8.49
#> 3     3   8.23     7         7.07
#> 4     4   8.42    10         9.36
#> 5     5   8.92    11        10.2
```


What we see in the plot is a property of hierarchical models know as **shrinkage**. The non-hierarchical estimates are all shrunken together (i.e., pulled towards general mean) and the farther the individual estimates are away from the mean, the more their are shrunken towards it (as indicated by the longer arrows). With the hierarchical structure we implemented in our model, we assume that that there is some similarity between individuals and, thus, the model uses the information from each individual to  inform the estimates of other individuals (McElreath, 2020).  It has been shown that this pooling of information can lead to more accurate estimates (Efron & Morris, 1977; McElreath, 2020). However, in our case, this lead to slightly worse predictions by the model. However, if we wanted to predict data from new, so far unobserved participants, the predictions by the hierarchical model would lead to better results on average.



```r
p1 <- ggplot(ph,aes(y = factor(ID))) +
        geom_point(aes(x = h_pred,color = "hierarchical",shape="hierarchical"),show.legend = F,size=3) +
        geom_segment(aes(y = factor(ID), yend = factor(ID),
                         x = h_pred_non_h, xend = h_pred,
                         group = factor(ID)), 
                     colour = "black", 
                     arrow  = arrow(length = unit(0.2,"cm")),
                     lty    = "solid",
                     size   = 0.5) +
        geom_point(aes(x = h_pred_non_h,color = "non-hierarchical",shape="non-hierarchical"),show.legend = F,size=3) +
        labs(x = "Predicted Hits",
             y = "ID",
             title = "Hits")



p2 <- ggplot(pfa,aes(y = factor(ID))) +
        geom_point(aes(x = fa_pred,color = "hierarchical",shape="hierarchical"),size=3) +
        geom_segment(aes(y = factor(ID), yend = factor(ID),
                       x = fa_pred_non_h, xend = fa_pred,
                       group = factor(ID)), 
                   colour = "black", 
                   arrow  = arrow(length = unit(0.2,"cm")),
                   lty    = "solid",
                   size   = 0.5) +
        geom_point(aes(x = fa_pred_non_h,color = "non-hierarchical",shape="non-hierarchical"),size=3) +
          labs(x = "Predicted False Alarms",
               y = "ID",
               title = "False Alarms",
               color = "Estimate Type",
               shape = "Estimate Type")


p1 + p2 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect") & theme(legend.position = "bottom")
```

<img src="/figs/2021-10-15-SDT2-hierarchical/unnamed-chunk-19-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />


In the next part in this series of blog posts, I will now use a different type of model to model the recognition data: the two-high treshold model. 

---

### References 

- Lee, M. D., & Wagenmakers, E. J. (2014). *Bayesian cognitive modeling: A practical course.* Cambridge university press.

- Efron, B., & Morris, C. (1977). *Stein’s Paradox in Statistics*. Scientific American,
857 236 (5), 119–127. 

- McElreath, R. (2020). *Statistical Rethinking: A Bayesian Course with Examples in R and Stan (2nd ed.)*.



***

*Last knitted on 2021-11-05.*[^si] 

[^si]: 
    
    ```r
    sessioninfo::session_info()
    #> - Session info ---------------------------------------------------------------
    #>  setting  value                       
    #>  version  R version 4.1.0 (2021-05-18)
    #>  os       Windows 10 x64              
    #>  system   x86_64, mingw32             
    #>  ui       RTerm                       
    #>  language (EN)                        
    #>  collate  German_Germany.1252         
    #>  ctype    German_Germany.1252         
    #>  tz       Europe/Berlin               
    #>  date     2021-11-05                  
    #> 
    #> - Packages -------------------------------------------------------------------
    #>  ! package        * version  date       lib source                           
    #>    abind            1.4-5    2016-07-21 [1] CRAN (R 4.1.0)                   
    #>    arrayhelpers     1.1-0    2020-02-04 [1] CRAN (R 4.1.0)                   
    #>    assertthat       0.2.1    2019-03-21 [1] CRAN (R 4.1.0)                   
    #>    backports        1.2.1    2020-12-09 [1] CRAN (R 4.1.0)                   
    #>    bayesplot      * 1.8.1    2021-06-14 [1] CRAN (R 4.1.0)                   
    #>    broom            0.7.10   2021-10-31 [1] CRAN (R 4.1.1)                   
    #>    callr            3.7.0    2021-04-20 [1] CRAN (R 4.1.0)                   
    #>    cellranger       1.1.0    2016-07-27 [1] CRAN (R 4.1.0)                   
    #>    checkmate        2.0.0    2020-02-06 [1] CRAN (R 4.1.0)                   
    #>    cli              3.0.1    2021-07-17 [1] CRAN (R 4.1.1)                   
    #>    cmdstanr       * 0.4.0    2021-09-20 [1] local                            
    #>    coda             0.19-4   2020-09-30 [1] CRAN (R 4.1.0)                   
    #>    codetools        0.2-18   2020-11-04 [2] CRAN (R 4.1.0)                   
    #>    colorspace       2.0-1    2021-05-04 [1] CRAN (R 4.1.0)                   
    #>    crayon           1.4.2    2021-10-29 [1] CRAN (R 4.1.1)                   
    #>    curl             4.3.1    2021-04-30 [1] CRAN (R 4.1.0)                   
    #>    data.table       1.14.2   2021-09-27 [1] CRAN (R 4.1.1)                   
    #>    DBI              1.1.1    2021-01-15 [1] CRAN (R 4.1.0)                   
    #>    dbplyr           2.1.1    2021-04-06 [1] CRAN (R 4.1.0)                   
    #>    digest           0.6.27   2020-10-24 [1] CRAN (R 4.1.0)                   
    #>    distributional   0.2.2    2021-02-02 [1] CRAN (R 4.1.0)                   
    #>    dplyr          * 1.0.6    2021-05-05 [1] CRAN (R 4.1.0)                   
    #>    ellipsis         0.3.2    2021-04-29 [1] CRAN (R 4.1.0)                   
    #>    evaluate         0.14     2019-05-28 [1] CRAN (R 4.1.0)                   
    #>    fansi            0.5.0    2021-05-25 [1] CRAN (R 4.1.0)                   
    #>    farver           2.1.0    2021-02-28 [1] CRAN (R 4.1.0)                   
    #>    fastmap          1.1.0    2021-01-25 [1] CRAN (R 4.1.0)                   
    #>    forcats        * 0.5.1    2021-01-27 [1] CRAN (R 4.1.0)                   
    #>    fs               1.5.0    2020-07-31 [1] CRAN (R 4.1.0)                   
    #>    generics         0.1.1    2021-10-25 [1] CRAN (R 4.1.0)                   
    #>    ggdist           3.0.0    2021-07-19 [1] CRAN (R 4.1.1)                   
    #>    ggplot2        * 3.3.5    2021-06-25 [1] CRAN (R 4.1.1)                   
    #>    ggridges         0.5.3    2021-01-08 [1] CRAN (R 4.1.0)                   
    #>    glue             1.4.2    2020-08-27 [1] CRAN (R 4.1.0)                   
    #>    gridExtra        2.3      2017-09-09 [1] CRAN (R 4.1.0)                   
    #>    gtable           0.3.0    2019-03-25 [1] CRAN (R 4.1.0)                   
    #>    haven            2.4.3    2021-08-04 [1] CRAN (R 4.1.1)                   
    #>    HDInterval       0.2.2    2020-05-23 [1] CRAN (R 4.1.0)                   
    #>    here             1.0.1    2020-12-13 [1] CRAN (R 4.1.1)                   
    #>    highr            0.9      2021-04-16 [1] CRAN (R 4.1.0)                   
    #>    hms              1.1.1    2021-09-26 [1] CRAN (R 4.1.1)                   
    #>    htmltools        0.5.2    2021-08-25 [1] CRAN (R 4.1.1)                   
    #>    httr             1.4.2    2020-07-20 [1] CRAN (R 4.1.0)                   
    #>    inline           0.3.19   2021-05-31 [1] CRAN (R 4.1.0)                   
    #>    jsonlite         1.7.2    2020-12-09 [1] CRAN (R 4.1.0)                   
    #>    kableExtra     * 1.3.4    2021-02-20 [1] CRAN (R 4.1.0)                   
    #>    knitr          * 1.36     2021-09-29 [1] CRAN (R 4.1.1)                   
    #>    labeling         0.4.2    2020-10-20 [1] CRAN (R 4.1.0)                   
    #>    latex2exp      * 0.5.0    2021-03-18 [1] CRAN (R 4.1.0)                   
    #>    lattice          0.20-44  2021-05-02 [2] CRAN (R 4.1.0)                   
    #>    lifecycle        1.0.1    2021-09-24 [1] CRAN (R 4.1.1)                   
    #>    loo              2.4.1    2020-12-09 [1] CRAN (R 4.1.0)                   
    #>    lubridate        1.7.10   2021-02-26 [1] CRAN (R 4.1.0)                   
    #>    magrittr         2.0.1    2020-11-17 [1] CRAN (R 4.1.0)                   
    #>    Matrix           1.3-3    2021-05-04 [2] CRAN (R 4.1.0)                   
    #>    matrixStats      0.59.0   2021-06-01 [1] CRAN (R 4.1.0)                   
    #>    mgcv             1.8-35   2021-04-18 [2] CRAN (R 4.1.0)                   
    #>    modelr           0.1.8    2020-05-19 [1] CRAN (R 4.1.0)                   
    #>    munsell          0.5.0    2018-06-12 [1] CRAN (R 4.1.0)                   
    #>    nlme             3.1-152  2021-02-04 [2] CRAN (R 4.1.0)                   
    #>    patchwork      * 1.1.1    2020-12-17 [1] CRAN (R 4.1.0)                   
    #>    pillar           1.6.4    2021-10-18 [1] CRAN (R 4.1.0)                   
    #>    pkgbuild         1.2.0    2020-12-15 [1] CRAN (R 4.1.0)                   
    #>    pkgconfig        2.0.3    2019-09-22 [1] CRAN (R 4.1.0)                   
    #>    plyr             1.8.6    2020-03-03 [1] CRAN (R 4.1.0)                   
    #>    posterior      * 1.1.0    2021-09-09 [1] CRAN (R 4.1.1)                   
    #>    prettyunits      1.1.1    2020-01-24 [1] CRAN (R 4.1.0)                   
    #>    processx         3.5.2    2021-04-30 [1] CRAN (R 4.1.0)                   
    #>    ps               1.6.0    2021-02-28 [1] CRAN (R 4.1.0)                   
    #>    purrr          * 0.3.4    2020-04-17 [1] CRAN (R 4.1.0)                   
    #>    R6               2.5.1    2021-08-19 [1] CRAN (R 4.1.1)                   
    #>    ragg             1.2.0    2021-10-30 [1] CRAN (R 4.1.1)                   
    #>    Rcpp             1.0.7    2021-07-07 [1] CRAN (R 4.1.1)                   
    #>  D RcppParallel     5.1.4    2021-05-04 [1] CRAN (R 4.1.0)                   
    #>    readr          * 2.0.2    2021-09-27 [1] CRAN (R 4.1.1)                   
    #>    readxl           1.3.1    2019-03-13 [1] CRAN (R 4.1.0)                   
    #>    reprex           2.0.1    2021-08-05 [1] CRAN (R 4.1.1)                   
    #>    reshape2         1.4.4    2020-04-09 [1] CRAN (R 4.1.0)                   
    #>    rlang            0.4.11   2021-04-30 [1] CRAN (R 4.1.0)                   
    #>    rmarkdown        2.11     2021-09-14 [1] CRAN (R 4.1.1)                   
    #>    rprojroot        2.0.2    2020-11-15 [1] CRAN (R 4.1.0)                   
    #>    rstan            2.21.2   2020-07-27 [1] CRAN (R 4.1.0)                   
    #>    rstudioapi       0.13     2020-11-12 [1] CRAN (R 4.1.0)                   
    #>    rvest            1.0.2    2021-10-16 [1] CRAN (R 4.1.1)                   
    #>    scales           1.1.1    2020-05-11 [1] CRAN (R 4.1.0)                   
    #>    sessioninfo      1.1.1    2018-11-05 [1] CRAN (R 4.1.0)                   
    #>    StanHeaders      2.21.0-7 2020-12-17 [1] CRAN (R 4.1.0)                   
    #>    stringi          1.6.1    2021-05-10 [1] CRAN (R 4.1.0)                   
    #>    stringr        * 1.4.0    2019-02-10 [1] CRAN (R 4.1.0)                   
    #>    svglite          2.0.0    2021-02-20 [1] CRAN (R 4.1.0)                   
    #>    svUnit           1.0.6    2021-04-19 [1] CRAN (R 4.1.0)                   
    #>    systemfonts      1.0.3    2021-10-13 [1] CRAN (R 4.1.1)                   
    #>    tensorA          0.36.2   2020-11-19 [1] CRAN (R 4.1.0)                   
    #>    textshaping      0.3.6    2021-10-13 [1] CRAN (R 4.1.1)                   
    #>    tibble         * 3.1.2    2021-05-16 [1] CRAN (R 4.1.0)                   
    #>    tidybayes      * 3.0.1    2021-10-27 [1] Github (mjskay/tidybayes@031b63d)
    #>    tidyr          * 1.1.4    2021-09-27 [1] CRAN (R 4.1.1)                   
    #>    tidyselect       1.1.1    2021-04-30 [1] CRAN (R 4.1.0)                   
    #>    tidyverse      * 1.3.1    2021-04-15 [1] CRAN (R 4.1.1)                   
    #>    tzdb             0.1.2    2021-07-20 [1] CRAN (R 4.1.1)                   
    #>    utf8             1.2.1    2021-03-12 [1] CRAN (R 4.1.0)                   
    #>    V8               3.4.2    2021-05-01 [1] CRAN (R 4.1.0)                   
    #>    vctrs            0.3.8    2021-04-29 [1] CRAN (R 4.1.0)                   
    #>    viridisLite      0.4.0    2021-04-13 [1] CRAN (R 4.1.0)                   
    #>    webshot          0.5.2    2019-11-22 [1] CRAN (R 4.1.0)                   
    #>    withr            2.4.2    2021-04-18 [1] CRAN (R 4.1.0)                   
    #>    xfun             0.26     2021-09-14 [1] CRAN (R 4.1.1)                   
    #>    xml2             1.3.2    2020-04-23 [1] CRAN (R 4.1.0)                   
    #> 
    #> [1] C:/Users/Administrator/Documents/R/win-library/4.1
    #> [2] C:/Program Files/R/R-4.1.0/library
    #> 
    #>  D -- DLL MD5 mismatch, broken installation.
    ```
