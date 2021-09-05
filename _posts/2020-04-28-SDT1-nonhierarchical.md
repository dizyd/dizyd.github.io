---
layout: single
title: "Signal Detection Model with STAN - Part 1"
date: 2020-04-28
tags:
  - cognitive modeling
  - STAN
  - R
  
# status: process
# published: true
# status: publish
# published: true
permalink: /posts/2020/04/SDT-1/
classes: wide
---





## About this blog post

This blog post is supposed to be the first in a series of blog post comparing the *signal detection theory * model (SDT) with a *two-high-threshold* model (2HTM) of recognition. I wrote this blog post for three reasons. First, I wanted to learn more about the modeling of recognition and memory data. Second, I wanted to learn more about STAN, since I mostly use JAGS in my own research. Finally, I also wanted to practice writing, since I take forever when writing my own articles. So the reasons for this blog are rather selfish. However, if anyone ever finds this blog post and finds it helpful, that would be even better!


In this blog post you are going to read, I will use a *non-hierarchical SDT* model to investigate the data from a recogntion experiment. In following blog posts, I will extend this model to account for differences between individuals as well as differences between stimulus sets. Then I will model the same data with a 2HTM and finally compare both models with each other. 

## Setup 

At the beginning, I load the packages I need for this analysis. However, before we start with the actual analysis and modeling, I first want to give a short introduction into signal detection theory.


```r
library(tidyverse) # contains ggplot, dplyr etc
library(rstan)
library(bayesplot)
#library(rstanarm)
library(patchwork)
library(truncnorm)
library(kableExtra)
library(knitr)

# Set bayesplot theme
bayesplot_theme_set(theme_bw())
```


## Signal detection model

Signal detection theory (SDT) may be applied to any area of psychology in which two different types of stimuli must be discriminated. It was first applied in studies of perception, where subjects had to discriminated between signals (stimuli) and noise (no stimuli). However, SDT is also often used to describe memory recognition task, where subjects have to discriminate between old (signal) and new items (noise). 

The idea behind SDT is that signal and noise trials can be represented as values along a uni-dimensional continuous strength dimension, where signal trials are assumed to have a greater strength than noise trials. According to SDT, people produce "old" or "new" decisions by comparing the strength of the current trial to a fixed threshold. If the strength exceeds this threshold, the response is "old", otherwise the response is "new". The strength of signal and noise trials is assumed to be normally distributed with different means (but the mean of the noise distributions is assumed to be equal to 0), but the same variance (which is fixed to 1), which can be expressed as:

$$
\begin{aligned}
noise &\sim N(0,1)  \\[.5em]
signal &\sim N(d,1)
\end{aligned}
$$

where **d** is the **discriminability** or **sensitivity** parameter, which goes from $-\infty$ to $+ \infty $. This parameter **d** corresponds to the distance between means of the noise and the signal distribution in standard deviation units. A value of 0 indicates an inability to distinguish signals from noise, whereas larger values indicate a correspondingly greater ability to distinguish signals from noise. Negative values are also possible, but are harder to interpret. They are most often thought of as a sampling error or response confusion (i.e., responding "old" when intending to respond "new", and vice versa, for instance by confusing the corresponding buttons). 

Another parameter is **c**, the **bias** parameter. Positive values of **c** correspond to a bias towards saying *new* and negative values correspond to a bias towards saying *old*. 

These two parameters can then be directly translated into hit (HR) and false-alarm rates (FR) via:

$$
\begin{aligned}
HR &= \phi(0.5 \times d - c) \\[.5em]
FR &= \phi(- 0.5 \times d - c) 
\end{aligned}
$$

where, $\phi$ is the cumulative density function of the standard normal distribution. HR and FR  map naturally to the data pattern we observe in a recognition memory experiment, were participants see either an old (signal trial) or a new item (noise trial), and then have to respond by pressing the corresponding button:


|Response |Signal Trial |Noise Trial |
|:--------|:------------|:-----------|
|Old      |Old          |New         |
|New      |Old          |New         |

which corresponds to:


|Response |Signal Trial |Noise Trial       |
|:--------|:------------|:-----------------|
|Old      |Hit          |False alarm       |
|New      |Miss         |correct rejection |

This allows us to take the observed number of hits and false alarms in our experiment, and translate them into psychological meaningful parameters *d* and *c*

## The Experiment 

The experiment was conducted as part of a pre-test to select stimuli for a subsequent multiple-cue judgment experiment. The experiment consisted of five blocks, with two phases each. In the learning phase, participants saw 12 different pictures of either plants, houses, aliens, bugs, or plates, (a different stimulus set in each block) with features differing on five cues. The same 12 pictures were presented 4 times for 5 seconds to each participant. After the learning phase, participants were presented again with all 12 old pictures as well as 20 new pictures in the testing phase. In each trial of the testing phase, participants had to indicate if the picture was an old picture or a new one.  These two phases were repeated for each of the five stimulus sets. 

## The Data set 

The data I will use contains the following variables:

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


### SDT - Non Hierarchical - One Stimulus Set 

I will first build the non-hierarchical version of the SDT model as described in Chapter 11 on pages 158-159 in the fantastic book of Lee and Wagenmakers (2014), analyzing the data from only one stimulus set. 



```r
temp <- hits %>% filter(stimulus == "plants")
```

#### The Model 

Next, I will define the SDT model in STAN.  

##### The Data 

In the `data` block, I define the data we are using in the model, which is:

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

##### The Parameters 

In the `parameters` block, I define the two main parameters we have in our model and which we are interested in, *d* and *c*. These two parameters are then transformed in to the *hit rate* and the *false-alarm rate* in the `transformed parameters` block. In the `model` block, I then define the binomial likelihood function connecting our parameters and our data. This can be written as: 


$$
\begin{aligned}
HR &= \phi(0.5 \times d - c) \\[.5em]
FR &= \phi(- 0.5 \times d - c) 
\\[1.5em]
h & \sim binomial(h,s)\\[.5em]
fa &\sim binomial(fa,n)
\end{aligned}
$$




In the `generated quantities` block I also include variables for later posterior predictive analysis, capturing the predictions of `h` and `fa` based on the current parameter values of each step of the MCMC-chains. 

##### The Priors 

The priors for both *d* and *c* are normal distributions with $\mu = 0$ and $\sigma = 1$

$$
\begin{aligned}
d &\sim Normal(0,1)\\[.5em]
c &\sim Normal(0,1)
\end{aligned}
$$


which corresponds to a uniform distribution after transforming them into hit and false-alarm rates.


<img src="/figs/2020-04-28-SDT1-nonhierarchical/unnamed-chunk-4-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />

##### The STAN-Model 

The model code then looks like this:


```stan
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
} generated quantities {

  vector<lower=0> [p] h_pred;
  vector<lower=0> [p] fa_pred;
 
  for (i in 1:p){
    h_pred[i]  = binomial_rng(s[i], hit_rate[i]);
    fa_pred[i] = binomial_rng(n[i], fa_rate[i]);
  }
}
```

#### Run the Model 

Next, we sample from the model using 10.000 iterations, with  a rather small warm-up of 2000 iterations and thinning = 4. 


```r
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

fit1 <-sampling(
        object     = StanModel_oneStim_nonHier, # Stan program
        data       = stan_data, # named list of data
        chains     = 2,         # number of Markov chains
        warmup     = 2000,      # number of warmup iterations per chain
        iter       = 10000,     # total number of iterations per chain
        cores      = 2,         # number of cores (could use one per chain)
        refresh    = 0,         # no progress shown
        thin       = 4
      )

# save results 
posterior_SDT_oS_nH <- rstan::extract(fit1, permuted = FALSE)
summary_oS_nH       <- summary(fit1) %>% as.data.frame()
```

#### Inspect MCMC, Rhat, ESS

First, we can look at some MCMC-Traces for some of the parameters and persons. 


```r
 mcmc_trace(posterior_SDT_oS_nH,
            pars = vars("d[1]":"d[3]",
                        "c[1]":"c[3]",
                        "hit_rate[1]":"hit_rate[3]",
                        "fa_rate[1]" :"fa_rate[3]"),
            facet_args = list(nrow = 4, labeller = label_parsed))
```

<img src="/figs/2020-04-28-SDT1-nonhierarchical/unnamed-chunk-7-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />

So far so good, this looks exactly as you would like it, some nice hairy caterpillars. We can also plot the distributions of $\hat{R}$  and the *effective sample size*:


```r

p1 <- ggplot(summary_oS_nH,aes(x =summary.n_eff))+
        geom_histogram(bins = 40, color = "black", fill = "skyblue2")+
        theme_bw() +
        labs(x = "Effective Sample Size",
             y = "Count")

p2 <- ggplot(summary_oS_nH,aes(x =summary.Rhat))+
        geom_histogram(bins = 40, color = "black", fill = "tomato2")+
        theme_bw() +
        labs(x = "Rhat",
             y = "Count")

p1+p2 #patchwork package
```

<img src="/figs/2020-04-28-SDT1-nonhierarchical/unnamed-chunk-8-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />

This looks also fine. The effective sample sizes are always > 400, which is often recommended, and are near the total sample size of  4000. Also $\hat{R}$ is always very close to 1.

#### Posterior Summaries & Densities

Next, we can look at the summary statistics and plots of the posterior density distributions. So lets make a convenient tidy data.frame for plotting and for the posterior summary statistics. 


```r
desc_oS_nH_tidy <- summary_oS_nH %>% 
                      mutate(rn     = rownames(.),
                              param = str_split(rn,"\\[", simplify = TRUE) %>% unlist() %>%  .[,1],
                              ID    = str_extract(rn,"\\d+") %>% as.numeric()) %>% 
                      select(param,ID,
                             mean    = summary.mean,
                             se_mean = summary.se_mean,
                             HDI025  = summary.2.5.,
                             median  = summary.50.,
                             HDI975  = summary.97.5.) 


temp_d = rstan::extract(fit1,pars="d") %>%
                      as.data.frame() %>% 
                      unlist()

temp_c = rstan::extract(fit1,pars="c") %>%
                      as.data.frame() %>% 
                      unlist()

temp_h_pred = rstan::extract(fit1,pars=" h_pred") %>%
                      as.data.frame() %>% 
                      unlist()

temp_fa_pred = rstan::extract(fit1,pars="fa_pred") %>%
                      as.data.frame() %>% 
                      unlist()


mcmc_SDT_oS_nH <-  bind_cols("d" = temp_d, "c"=temp_c,
                             "h_pred"=temp_h_pred,"fa_pred"=temp_fa_pred) %>% 
                      mutate(ID     = rep(1:40,each = 4000),
                             dprime = rep(temp$dprime, each = 4000)) %>% 
                      group_by(ID) %>% 
                      mutate(mPP_d  = median(d),
                             mPP_c  = median(c))
```


##### d 

Since *d* is the parameter we are most interested in, lets start with this one. Below you see a forest plot showing the median, 50%, and 96% credible intervals of the posterior distributions, as well as uni-variate marginal posterior distributions for some participants, showing the median posterior estimate (red dashed line) as well as the analytically calculated *d* (black dashed line).


```r
mcmc_intervals(posterior_SDT_oS_nH,
               pars = vars(starts_with("d")),
               point_est = "median",
               prob = 0.5,
               prob_outer = 0.96)
```

<img src="/figs/2020-04-28-SDT1-nonhierarchical/unnamed-chunk-10-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />



```r
mcmc_SDT_oS_nH %>%
  filter(ID %in% 1:6) %>% 
  ggplot(., aes(x = d))+
    geom_density(color = "black",fill = "skyblue2",alpha=0.5) +
    geom_vline(aes(xintercept = mPP_d), color = "red", lty = "dashed",lwd = 1) +
    geom_vline(aes(xintercept = dprime), color = "black", lty = "dashed") +
    facet_wrap(.~ID,scales="free") + 
    theme_bw()
```

<img src="/figs/2020-04-28-SDT1-nonhierarchical/unnamed-chunk-11-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />

We can see that the *d* values are rather small and very close to 0 for most people. This indicates that participants had a hard time differentiating old from new stimuli. I had hoped for larger values, as it is necessary that people are able to discriminate between stimuli and their features rather well for the multiple-cue judgment experiment I wanted to conduct with these stimuli. Also, from the plots it is evident that the median of the posterior distribution is very close to the analytically calculated *d'* value.  


##### c

We can also look at the *c* values. Since we have more noise trials (new stimuli) than signal trials (old stimuli) in our testing phase (20 vs. 12), and participant were told this information, I expected to find slightly positive values of *c*. However, as apparent from the summary statistics and the plots, participants had more negative values of *c*, indicating a bias for the "old"-response.


```r
mcmc_intervals(posterior_SDT_oS_nH,
               pars = vars(starts_with("c")),
               point_est = "median",
               prob = 0.5,
               prob_outer = 0.96)
```

<img src="/figs/2020-04-28-SDT1-nonhierarchical/unnamed-chunk-12-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />


```r

mcmc_SDT_oS_nH %>%
  filter(ID %in% 1:6) %>% 
  ggplot(., aes(x = c))+
    geom_density(color = "black",fill = "skyblue2",alpha=0.5) +
    geom_vline(aes(xintercept = mPP_c), color = "red", lty = "dashed",lwd = 1) +
    facet_wrap(.~ID,scales="free") + 
    theme_bw()
```

<img src="/figs/2020-04-28-SDT1-nonhierarchical/unnamed-chunk-13-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />

### The posterior predictive values

We can also look at the posterior predictive values of `h` and `fa`.  For this, I first combine and then plot the actual observed values from our original data.frame with the MCMC estimates. 


```r
temp     <- hits  %>%  filter(stimulus == "plants") %>% ungroup() %>% select(.,IDn,h,fa)
postpred <- mcmc_SDT_oS_nH %>% 
              select(., ID, h_pred, fa_pred) %>% 
              left_join(.,temp,by = c("ID" = "IDn")) 

postpred[1:5,]
#> # A tibble: 5 x 5
#> # Groups:   ID [1]
#>      ID h_pred fa_pred     h    fa
#>   <dbl>  <dbl>   <dbl> <dbl> <dbl>
#> 1     1      8      12     7    12
#> 2     1     11      15     7    12
#> 3     1      4      16     7    12
#> 4     1      3      17     7    12
#> 5     1      9      10     7    12
```



```r
pp1 <- ggplot(filter(postpred,ID %in% 1:4),aes(x = h_pred)) +
        geom_histogram(bins=10,color = "black",fill = "skyblue2",alpha=0.5) +
        geom_vline(aes(xintercept =  h),color = "black",lwd = 1.5, lty = "dashed")+
        facet_wrap(.~ID,scales="free") + 
        theme_bw() 

pp2 <- ggplot(filter(postpred,ID %in% 1:4),aes(x = fa_pred)) +
        geom_histogram(bins=10,color = "black",fill = "tomato2",alpha=0.5) +
        geom_vline(aes(xintercept =  fa),color = "black",lwd = 1.5, lty = "dashed")+
        facet_wrap(.~ID,scales="free") + 
        theme_bw() 

pp1 + pp2 #patchwork package
```

<img src="/figs/2020-04-28-SDT1-nonhierarchical/unnamed-chunk-15-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />

This looks also fine so far. The posterior distribution is symmetrically distributed around the empirical values. In addition, let me also calculate how often the 95% credible interval of the predicted values contains the true values:


```r
temp1 <- hits  %>%  filter(stimulus == "plants") %>% ungroup() %>% select(.,ID=IDn,h,fa)

temp2 <- desc_oS_nH_tidy %>% 
          select(param,ID,HDI025,HDI975)  %>% 
          filter(param == "h_pred" | param == "fa_pred") %>% 
          pivot_wider(.,id_cols    = ID,
                        names_from = param,
                                          values_from = c(HDI025,HDI975))

left_join(temp1,temp2,by = "ID")  %>% 
  summarize(pp_h  = mean(h > HDI025_h_pred & h < HDI975_h_pred),
            pp_fa = mean(fa > HDI025_fa_pred & fa < HDI975_fa_pred)) %>% 
  as.data.frame() # better output in .md
#>    pp_h pp_fa
#> 1 0.925  0.95
```

As evident from the plots and the 95% credible interval checks of the posterior predictives, our model is able to recover our data well.


---

### References 

- Lee, M. D., & Wagenmakers, E. J. (2014). *Bayesian cognitive modeling: A practical course.* Cambridge university press.

- Snodgrass, J. G., & Corwin, J. (1988). Perceptual Identification Thresholds for 150 Fragmented Pictures from the Snodgrass and Vanderwart Picture Set. *Perceptual and Motor Skills*, 67(1), 3–36. https://doi.org/10.2466/pms.1988.67.1.3

- Stanislaw, H., & Todorov, N. (1999). Calculation of signal detection theory measures. *Behavior Research Methods, Instruments, & Computers*, 31(1), 137–149. https://doi.org/10.3758/BF03207704



***

*Last knitted on 2021-09-05.*[^si] 

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
    #>  date     2021-09-05                  
    #> 
    #> - Packages -------------------------------------------------------------------
    #>  ! package      * version  date       lib source        
    #>    assertthat     0.2.1    2019-03-21 [1] CRAN (R 4.1.0)
    #>    backports      1.2.1    2020-12-09 [1] CRAN (R 4.1.0)
    #>    bayesplot    * 1.8.1    2021-06-14 [1] CRAN (R 4.1.0)
    #>    broom          0.7.7    2021-06-13 [1] CRAN (R 4.1.0)
    #>    callr          3.7.0    2021-04-20 [1] CRAN (R 4.1.0)
    #>    cellranger     1.1.0    2016-07-27 [1] CRAN (R 4.1.0)
    #>    cli            3.0.1    2021-07-17 [1] CRAN (R 4.1.0)
    #>    codetools      0.2-18   2020-11-04 [2] CRAN (R 4.1.0)
    #>    colorspace     2.0-1    2021-05-04 [1] CRAN (R 4.1.0)
    #>    crayon         1.4.1    2021-02-08 [1] CRAN (R 4.1.0)
    #>    curl           4.3.1    2021-04-30 [1] CRAN (R 4.1.0)
    #>    DBI            1.1.1    2021-01-15 [1] CRAN (R 4.1.0)
    #>    dbplyr         2.1.1    2021-04-06 [1] CRAN (R 4.1.0)
    #>    digest         0.6.27   2020-10-24 [1] CRAN (R 4.1.0)
    #>    dplyr        * 1.0.6    2021-05-05 [1] CRAN (R 4.1.0)
    #>    ellipsis       0.3.2    2021-04-29 [1] CRAN (R 4.1.0)
    #>    evaluate       0.14     2019-05-28 [1] CRAN (R 4.1.0)
    #>    fansi          0.5.0    2021-05-25 [1] CRAN (R 4.1.0)
    #>    farver         2.1.0    2021-02-28 [1] CRAN (R 4.1.0)
    #>    forcats      * 0.5.1    2021-01-27 [1] CRAN (R 4.1.0)
    #>    fs             1.5.0    2020-07-31 [1] CRAN (R 4.1.0)
    #>    generics       0.1.0    2020-10-31 [1] CRAN (R 4.1.0)
    #>    ggplot2      * 3.3.3    2020-12-30 [1] CRAN (R 4.1.0)
    #>    ggridges       0.5.3    2021-01-08 [1] CRAN (R 4.1.0)
    #>    glue           1.4.2    2020-08-27 [1] CRAN (R 4.1.0)
    #>    gridExtra      2.3      2017-09-09 [1] CRAN (R 4.1.0)
    #>    gtable         0.3.0    2019-03-25 [1] CRAN (R 4.1.0)
    #>    haven          2.4.1    2021-04-23 [1] CRAN (R 4.1.0)
    #>    here           1.0.1    2020-12-13 [1] CRAN (R 4.1.1)
    #>    highr          0.9      2021-04-16 [1] CRAN (R 4.1.0)
    #>    hms            1.1.0    2021-05-17 [1] CRAN (R 4.1.0)
    #>    htmltools      0.5.1.1  2021-01-22 [1] CRAN (R 4.1.0)
    #>    httr           1.4.2    2020-07-20 [1] CRAN (R 4.1.0)
    #>    inline         0.3.19   2021-05-31 [1] CRAN (R 4.1.0)
    #>    jsonlite       1.7.2    2020-12-09 [1] CRAN (R 4.1.0)
    #>    kableExtra   * 1.3.4    2021-02-20 [1] CRAN (R 4.1.0)
    #>    knitr        * 1.33     2021-04-24 [1] CRAN (R 4.1.0)
    #>    labeling       0.4.2    2020-10-20 [1] CRAN (R 4.1.0)
    #>    lifecycle      1.0.0    2021-02-15 [1] CRAN (R 4.1.0)
    #>    loo            2.4.1    2020-12-09 [1] CRAN (R 4.1.0)
    #>    lubridate      1.7.10   2021-02-26 [1] CRAN (R 4.1.0)
    #>    magrittr       2.0.1    2020-11-17 [1] CRAN (R 4.1.0)
    #>    matrixStats    0.59.0   2021-06-01 [1] CRAN (R 4.1.0)
    #>    modelr         0.1.8    2020-05-19 [1] CRAN (R 4.1.0)
    #>    munsell        0.5.0    2018-06-12 [1] CRAN (R 4.1.0)
    #>    patchwork    * 1.1.1    2020-12-17 [1] CRAN (R 4.1.0)
    #>    pillar         1.6.2    2021-07-29 [1] CRAN (R 4.1.0)
    #>    pkgbuild       1.2.0    2020-12-15 [1] CRAN (R 4.1.0)
    #>    pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.1.0)
    #>    plyr           1.8.6    2020-03-03 [1] CRAN (R 4.1.0)
    #>    prettyunits    1.1.1    2020-01-24 [1] CRAN (R 4.1.0)
    #>    processx       3.5.2    2021-04-30 [1] CRAN (R 4.1.0)
    #>    ps             1.6.0    2021-02-28 [1] CRAN (R 4.1.0)
    #>    purrr        * 0.3.4    2020-04-17 [1] CRAN (R 4.1.0)
    #>    R6             2.5.0    2020-10-28 [1] CRAN (R 4.1.0)
    #>    ragg           1.1.3    2021-06-09 [1] CRAN (R 4.1.1)
    #>    Rcpp           1.0.6    2021-01-15 [1] CRAN (R 4.1.0)
    #>  D RcppParallel   5.1.4    2021-05-04 [1] CRAN (R 4.1.0)
    #>    readr        * 1.4.0    2020-10-05 [1] CRAN (R 4.1.0)
    #>    readxl         1.3.1    2019-03-13 [1] CRAN (R 4.1.0)
    #>    reprex         2.0.0    2021-04-02 [1] CRAN (R 4.1.0)
    #>    reshape2       1.4.4    2020-04-09 [1] CRAN (R 4.1.0)
    #>    rlang          0.4.11   2021-04-30 [1] CRAN (R 4.1.0)
    #>    rmarkdown      2.10     2021-08-06 [1] CRAN (R 4.1.0)
    #>    rprojroot      2.0.2    2020-11-15 [1] CRAN (R 4.1.0)
    #>    rstan        * 2.21.2   2020-07-27 [1] CRAN (R 4.1.0)
    #>    rstudioapi     0.13     2020-11-12 [1] CRAN (R 4.1.0)
    #>    rvest          1.0.0    2021-03-09 [1] CRAN (R 4.1.0)
    #>    scales         1.1.1    2020-05-11 [1] CRAN (R 4.1.0)
    #>    sessioninfo    1.1.1    2018-11-05 [1] CRAN (R 4.1.0)
    #>    StanHeaders  * 2.21.0-7 2020-12-17 [1] CRAN (R 4.1.0)
    #>    stringi        1.6.2    2021-05-17 [1] CRAN (R 4.1.0)
    #>    stringr      * 1.4.0    2019-02-10 [1] CRAN (R 4.1.0)
    #>    svglite        2.0.0    2021-02-20 [1] CRAN (R 4.1.0)
    #>    systemfonts    1.0.2    2021-05-11 [1] CRAN (R 4.1.0)
    #>    textshaping    0.3.5    2021-06-09 [1] CRAN (R 4.1.1)
    #>    tibble       * 3.1.2    2021-05-16 [1] CRAN (R 4.1.0)
    #>    tidyr        * 1.1.3    2021-03-03 [1] CRAN (R 4.1.0)
    #>    tidyselect     1.1.1    2021-04-30 [1] CRAN (R 4.1.0)
    #>    tidyverse    * 1.3.1    2021-04-15 [1] CRAN (R 4.1.0)
    #>    truncnorm    * 1.0-8    2018-02-27 [1] CRAN (R 4.1.0)
    #>    utf8           1.2.1    2021-03-12 [1] CRAN (R 4.1.0)
    #>    V8             3.4.2    2021-05-01 [1] CRAN (R 4.1.0)
    #>    vctrs          0.3.8    2021-04-29 [1] CRAN (R 4.1.0)
    #>    viridisLite    0.4.0    2021-04-13 [1] CRAN (R 4.1.0)
    #>    webshot        0.5.2    2019-11-22 [1] CRAN (R 4.1.0)
    #>    withr          2.4.2    2021-04-18 [1] CRAN (R 4.1.0)
    #>    xfun           0.23     2021-05-15 [1] CRAN (R 4.1.0)
    #>    xml2           1.3.2    2020-04-23 [1] CRAN (R 4.1.0)
    #> 
    #> [1] D:/OneDrive/Documents/R/win-library/4.1
    #> [2] C:/Program Files/R/R-4.1.0/library
    #> 
    #>  D -- DLL MD5 mismatch, broken installation.
    ```
