---
layout: single
title: "Signal Detection Model with STAN - Part 1"
excerpt: "Modeling recognition data with a signal detection model using Stan"
date: 2020-04-28
tags:
  - cognitive modeling
  - Stan
  - R
  
# status: process
# published: true
# status: publish
# published: true
permalink: /posts/2020/04/SDT-1/
classes: wide
---



This blog post is supposed to be the first in a series of blog post comparing the *signal detection theory * model (SDT) with a *two-high-threshold* model (2HTM) of recognition.


I wrote this blog post for three reasons. First, I wanted to learn more about the modeling of recognition and memory data. Second, I wanted to learn more about Stan, since I mostly use JAGS in my own research. Finally, I also wanted to practice writing, since I take forever when writing my own articles. So the reasons for this blog are rather selfish. However, if anyone ever finds this blog post and finds it helpful, that would be even better!

## About this blog post

In this blog post you are going to read, I will use a *non-hierarchical SDT* model to investigate the data from a recogntion experiment. In following blog posts, I will extend this model to account for differences between individuals as well as differences between stimulus sets. Then I will model the same data with a 2HTM and finally compare both models with each other. 

## Setup 

At the beginning, I load the packages I need for this analysis. However, before we start with the actual analysis and modeling, I first want to give a short introduction into signal detection theory.


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


## Signal detection model 

Signal detection theory (SDT) may be applied to any area of psychology in which two different types of stimuli must be discriminated. It was first applied in studies of perception, where subjects had to discriminated between signals (stimuli) and noise (no stimuli). However, SDT is also often used to describe memory recognition task, where subjects have to discriminate between old (signal) and new items (noise). 

The idea behind SDT is that signal and noise trials can be represented as values along a uni-dimensional continuous strength dimension, where signal trials are assumed to have a greater strength than noise trials. According to SDT, people produce "old" or "new" decisions by comparing the strength of the current trial to a fixed threshold. If the strength exceeds this threshold, the response is "old", otherwise the response is "new". The strength of signal and noise trials is assumed to be normally distributed with different means (but the mean of the noise distributions is assumed to be equal to 0), but the same variance (which is fixed to 1), which can be expressed as:

$$
\begin{aligned}
noise &\sim N(0,1)  \\[.5em]
signal &\sim N(d,1)
\end{aligned}
$$

where $d$ is the **discriminability** or **sensitivity** parameter, which goes from $-\infty$ to $+ \infty $. This parameter $d$ corresponds to the diStance between means of the noise and the signal distribution in Standard deviation units. A value of 0 indicates an inability to distinguish signals from noise, whereas larger values indicate a correspondingly greater ability to distinguish signals from noise. Negative values are also possible, but are harder to interpret. They are most often thought of as a sampling error or response confusion (i.e., responding "old" when intending to respond "new", and vice versa, for inStance by confusing the corresponding buttons). 

Another parameter is $c$, the **bias** parameter. Positive values of $c$ correspond to a bias towards saying *new* and negative values correspond to a bias towards saying *old*. 

These two parameters can then be directly translated into hit (HR) and false-alarm rates (FR) via:

$$
\begin{aligned}
HR &= \phi(0.5 \times d - c) \\[.5em]
FR &= \phi(- 0.5 \times d - c) 
\end{aligned}
$$

where, $\phi$ is the cumulative density function of the Standard normal distribution. HR and FR  map naturally to the data pattern we observe in a recognition memory experiment, were participants see either an old (signal trial) or a new item (noise trial), and then have to respond by pressing the corresponding button:


|Response |Signal Trial |Noise Trial |
|:--------|:------------|:-----------|
|Old      |Old          |New         |
|New      |Old          |New         |

which corresponds to:


|Response |Signal Trial |Noise Trial       |
|:--------|:------------|:-----------------|
|Old      |Hit          |False alarm       |
|New      |Miss         |correct rejection |

This allows us to take the observed number of hits and false alarms in our experiment, and translate them into psychological meaningful parameters $d$ and $c$

## The Experiment         

The experiment was conducted as part of a pre-test to select stimuli for a subsequent multiple-cue judgment experiment. The experiment consisted of five blocks, with two phases each. In the learning phase, participants saw 12 different pictures of either plants, houses, aliens, bugs, or plates, (a different stimulus set in each block) with features differing on five cues. The same 12 pictures were presented 4 times for 5 seconds to each participant. After the learning phase, participants were presented again with all 12 old pictures as well as 20 new pictures in the testing phase. In each trial of the testing phase, participants had to indicate if the picture was an old picture or a new one.  These two phases were repeated for each of the five stimulus sets. 

## The Data set           

The data set contains the following variables:

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

Based on these variables, we can calculate the number of hits, false alarms, false negatives, and misses, as well as their corresponding rates for each person, in each block. In addition, I calculated $d'$  as $d' = z(HR) - z(FR)$ (Snodgrass &  Corwin, 1988; Stanislaw & Todorov,1999), where HR again is the hit rate and FR is the false-alarm rate.


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
                    fa_rate  = sum(false_positive)/n_new,
                    .groups  = "keep") %>% 
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


| IDn|stimulus | block| n_old| n_new|  h| fa| hit_rate| fa_rate| z_h_rate| z_fa_rate| dprime|
|---:|:--------|-----:|-----:|-----:|--:|--:|--------:|-------:|--------:|---------:|------:|
|   1|aliens   |     5|    12|    20|  7| 11|     0.58|    0.55|     0.21|      0.13|   0.08|
|   1|bugs     |     2|    12|    20|  7| 13|     0.58|    0.65|     0.21|      0.39|  -0.17|
|   1|houses   |     4|    12|    20|  7| 14|     0.58|    0.70|     0.21|      0.52|  -0.31|
|   1|plants   |     3|    12|    20|  7| 12|     0.58|    0.60|     0.21|      0.25|  -0.04|
|   1|plates   |     1|    12|    20| 11| 11|     0.92|    0.55|     1.38|      0.13|   1.26|

If we have a quick look at the hit-rate of each person in each of the five stimulus sets, we see that the performance (this is the hit-rate) is rather low. 


```r
ggplot(hits,aes(x = hit_rate)) +
  geom_bar(color = "black", fill = "lightgrey") + 
  facet_wrap(.~stimulus) +
  xlim(0,1) +
  labs(x = "Hit-Rate")
```

<img src="/figs/2020-04-28-SDT1-nonhierarchical/plot hitrates-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />

For the start, let us focus on one of the stimulus sets in our modelling attempts:


```r
temp <- hits %>% filter(stimulus == "plants")
```


## SDT - Non Hierarchical - One Stimulus Set 

I will first build the non-hierarchical version of the SDT model as described in Chapter 11 on pages 158-159 in the fantastic book of Lee and Wagenmakers (2014). I will first present the complete Stan code of the model and then will go through it, step by step.

### The Stan-Model 

This is how our SDT model looks in Stan:


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
Stan_data <- list(
  s  = temp$n_old,
  n  = temp$n_new,
  h  = temp$h,
  fa = temp$fa,
  p  = length(unique(temp$ID))
)

Stan_data
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

In the `parameters` block, I define the parameters there are in our model and which we are interested in, which is one $d$ and one $c$ parameter for each participant $p$. These  parameters are then transformed in to the individual *hit* and  *false-alarm* rates in the `transformed parameters` block. 


### The Model      

In the `model` block, I then specify the model structure, this is, how the parameters relate to each other, the prior distributions of the parameters, as well as the binomial likelihood function connecting our parameters and our data. This can be written as: 


$$
\begin{aligned}
HR_p &= \phi(0.5 \times d_p - c_p) \\[.5em]
FR_p &= \phi(- 0.5 \times d_p - c_p) 
\\[1.5em]
h_p & \sim binomial(HR_p,s_p)\\[.5em]
fa_p &\sim binomial(FR_p,n_p)
\end{aligned}
$$



In the `generated quantities` block I also include variables for later posterior predictive analysis, capturing the predictions of `h` and `fa` based on the current parameter values of each step of the MCMC-chains. 

### The Priors     

The priors for the individaul $d_p$ and $c_p$ parameters are normal distributions with $\mu = 0$ and $\sigma = 1$

$$
\begin{aligned}
d_p &\sim Normal(0,1)\\[.5em]
c_p &\sim Normal(0,1)
\end{aligned}
$$


which corresponds to a uniform distribution after transforming them into hit and false-alarm rates.


<img src="/figs/2020-04-28-SDT1-nonhierarchical/unnamed-chunk-5-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />



## Run the Model 

Now that I have defined the model, the data and the priors, we are ready to go.  I use the newish `CmdStanR` interface to Stan to fit the model. I will draw 10.000 posterior samples, with a rather small warm-up of 2000 iterations and thinning = 4. 


```r
options(mc.cores = 2)

mod  <- cmdstan_model("Stan Models/StanModel_oneStim_nonHier.Stan")
#> Error in initialize(...): Assertion on 'stan_file' failed: File does not exist: 'Stan Models/StanModel_oneStim_nonHier.Stan'.

fit1 <- mod$sample(
          data           = Stan_data, # named list of data
          chains         = 2,         # number of Markov chains
          iter_warmup    = 2000,      # number of warmup iterations per chain
          iter_sampling  = 10000,     # total number of iterations per chain
          refresh        = 0,         # no progress shown
          thin           = 4,
          adapt_delta    = 0.9)
#> Error in eval(expr, envir, enclos): Objekt 'mod' nicht gefunden
```

Luckily, this model took only around 6s to run and not several hours our days, as more complex Bayesian models tend to do. Lets save the model and the posterior draws for later use. 


```r

fit1$save_object(file = "Data/fit_StanModel_nonHier.RDS")
#> Error in eval(expr, envir, enclos): Objekt 'fit1' nicht gefunden
```


### Inspect MCMC traces, Rhat, ESS

Before we can dive right in in our posterior distributions and parameter estimates, we should check the convergence of the MCMC chains. First, we can look at some MCMC-Traces for some of the parameters and persons (in my own research, I do this with every single parameter, even when it takes some time). 


```r

as_draws_df(fit1$draws()) %>% 
 mcmc_trace(posterior_SDT_oS_nH,
            pars = vars("d[1]":"d[3]",
                        "c[1]":"c[3]",
                        "hit_rate[1]":"hit_rate[3]",
                        "fa_rate[1]" :"fa_rate[3]"),
            facet_args = list(nrow = 4, labeller = label_parsed))
#> Error in as_draws_df(fit1$draws()): Objekt 'fit1' nicht gefunden
```

So far so good, this looks exactly as you would like it, some nice hairy caterpillars. Next, we should have a look at the distributions of $\hat{Rs}$  and the *effective sample size (ESS)*:


```r

summary_oS_nH  <- fit1$summary() %>% as.data.frame()
#> Error in as.data.frame(.): Objekt 'fit1' nicht gefunden


p1 <- ggplot(summary_oS_nH,aes(x = ess_bulk))+
        geom_histogram(bins = 40, color = "black", fill = "skyblue2")+
        xlim(0,6000)+
        labs(x = "Effective Sample Size",
             y = "Count")
#> Error in ggplot(summary_oS_nH, aes(x = ess_bulk)): Objekt 'summary_oS_nH' nicht gefunden

p2 <- ggplot(summary_oS_nH,aes(x = rhat))+
        geom_histogram(bins = 40, color = "black", fill = "tomato2")+
        xlim(0.99,2) + 
        labs(x = "Rhat",
             y = "Count")
#> Error in ggplot(summary_oS_nH, aes(x = rhat)): Objekt 'summary_oS_nH' nicht gefunden

p1+p2 #patchwork package
```

<img src="/figs/2020-04-28-SDT1-nonhierarchical/unnamed-chunk-7-1.png" title="center" alt="center" width="80%" style="display: block; margin: auto;" />

This looks also fine. The effective sample sizes are always > 400, which is often recommended, and are near the total sample size of  4000. Also $\hat{R}$ is always very close to 1.

### Posterior Summaries & Densities

Now we come to the interesting part and what we are interested in: the posterior distributions of our parameters. Because plots are fun, lets start with making some plots of the posterior distributions of the $d$ and $c$ parameters of our participants. Here I use the `gather_draw()` function from the `tidybayes` package to prepare the data to make the plot. This function makes it very easy to extract a tidy data.frame when we have parameter names like `d[12]`, where `d` is the name of our parameter and `[12]` indicates the ID of the participant. 



```r
df_post <- fit1 %>% gather_draws(d[ID],c[ID]) 
#> Error in tidy_draws(model): Objekt 'fit1' nicht gefunden

head(df_post)
#> Error in head(df_post): Objekt 'df_post' nicht gefunden
```

Equipped with this data.frame of posterior draws (`.value`) per person (`ID`) for both parameters (`.variable`) we can plot the posterior distributions:


```r
df_post %>% 
  ggplot(aes(y = factor(ID), x = .value)) +
    stat_halfeye() +
    facet_wrap(.~.variable) +
    labs(x = "Value",
         y = "ID")
#> Error in ggplot(., aes(y = factor(ID), x = .value)): Objekt 'df_post' nicht gefunden
```

#### Parameter: $d_p$ 

Since $d$ is the parameter we are most interested in, lets start with this one. Below you see a forest plot showing the median, 50%, and 95% credible intervals of the posterior distributions, as well as uni-variate marginal posterior distributions. We can see that the individual $d_p$ values are rather small and very close to 0 for most people. This indicates that participants had a hard time differentiating old from new stimuli. I had hoped for larger values, as it is necessary that people are able to discriminate between stimuli and their features rather well for the multiple-cue judgment experiment I wanted to conduct with these stimuli.


#### Parameter $c_p$

We can also look at the $c$ values. Since we have more noise trials (new stimuli) than signal trials (old stimuli) in our testing phase (20 vs. 12), and participant were told this information, I expected to find slightly positive values of $c$. However, as apparent from the plots, participants had more negative values of $c$, indicating a bias for the "old"-response. Looking at the plot, it is also clear that there are two participants who had smaller $c$ values then the other participants, which are rather similar to each other. 

### The posterior predictive values

Finally,let's have a look at the posterior predictive values of `h` and `fa`. For this, I first combine and then plot the actual observed values from our original data.frame with the MCMC estimates. So lets first create a tidy data.frame only containing the empirical variables we need.


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

Next, we extract the posterior draws for our predicted values. 


```r
pp_pred <- fit1 %>% spread_draws(fa_pred[ID],h_pred[ID])
#> Error in tidy_draws(model): Objekt 'fit1' nicht gefunden

head(pp_pred)
#> Error in head(pp_pred): Objekt 'pp_pred' nicht gefunden
```

Now I can simply join both data.frames together:



```r
pp_df <- pp_pred %>% left_join(.,pp_emp,by = c("ID")) 
#> Error in left_join(., pp_emp, by = c("ID")): Objekt 'pp_pred' nicht gefunden

head(pp_df)
#> Error in head(pp_df): Objekt 'pp_df' nicht gefunden
```



This data.frame now contains the empirically observed number of hits and false-alarms, as well as the corresponding posterior predicted draws for each participant. We can now plot everything together, to get an idea of how good our (in-sample) posterior predictions capture the (empirical) reality. To safe some space, I will only do the plots for four participants. 



```r

pp1 <- ggplot(filter(pp_df,ID %in% 1:4),aes(x = h_pred)) +
        geom_histogram(bins=10,color = "black",fill = "skyblue2",alpha=0.5) +
        geom_vline(aes(xintercept =  h),color = "black",lwd = 1.5, lty = "dashed")+
        facet_wrap(.~ID,scales="free") 
#> Error in filter(pp_df, ID %in% 1:4): Objekt 'pp_df' nicht gefunden

pp2 <- ggplot(filter(pp_df,ID %in% 1:4),aes(x = fa_pred)) +
        geom_histogram(bins=10,color = "black",fill = "tomato2",alpha=0.5) +
        geom_vline(aes(xintercept =  fa),color = "black",lwd = 1.5, lty = "dashed")+
        facet_wrap(.~ID,scales="free") 
#> Error in filter(pp_df, ID %in% 1:4): Objekt 'pp_df' nicht gefunden

pp1 + pp2 #patchwork package
#> Error in eval(expr, envir, enclos): Objekt 'pp1' nicht gefunden
```

This looks also fine so far. The posterior distribution is symmetrically distributed around the empirical values.  In addition, let me also calculate how often the 95% credible interval of the predicted values contains the true values:


```r
fit1 %>%
  spread_draws(fa_pred[ID],h_pred[ID]) %>%
  mean_hdci() %>% 
  left_join(.,pp_emp,by = c("ID"))  %>% 
  summarize(pp_h  = mean(h  > h_pred.lower  & h  < h_pred.upper),
            pp_fa = mean(fa > fa_pred.lower & fa < fa_pred.upper)) %>% 
  as.data.frame() # better output in .md
#> Error in tidy_draws(model): Objekt 'fit1' nicht gefunden
```

Finally, we can also make a scatterplot plotting the observed against the predicted (i.e., median of the posterior predictive distributions) hit and false-alarm rates for each participant.


```r
ph <- fit1 %>%
        spread_draws(h_pred[ID]) %>%
        mean_hdci() %>% 
        left_join(.,pp_emp,by = c("ID")) %>% 
        ggplot(aes(x = h,h_pred)) +
          geom_smooth(method="lm") +
          geom_point(size=2,shape=21,color="black",fill="grey") +
          labs(x = "Observed",
               y = "Predicted",
               title = "Hit-Rate")
#> Error in tidy_draws(model): Objekt 'fit1' nicht gefunden


pfa <- fit1 %>%
        spread_draws(fa_pred[ID]) %>%
        mean_hdci() %>% 
        left_join(.,pp_emp,by = c("ID")) %>% 
        ggplot(aes(x = fa,fa_pred)) +
          geom_smooth(method="lm") +
          geom_point(size=2,shape=21,color="black",fill="grey") +
          labs(x = "Observed",
               y = "Predicted",
               title = "False-Alarm Rate")
#> Error in tidy_draws(model): Objekt 'fit1' nicht gefunden

ph+pfa
#> Error in eval(expr, envir, enclos): Objekt 'ph' nicht gefunden
```


As evident from the plots and the 95% credible interval checks of the posterior predictives, our model is able to recover our data rather well. 


In the following blog posts I now want to check if the hierarchical SDT model or other model families (2HTM) are better able to recover our data.



---

### References 

- Lee, M. D., & Wagenmakers, E. J. (2014). *Bayesian cognitive modeling: A practical course.* Cambridge university press.

- Snodgrass, J. G., & Corwin, J. (1988). Perceptual Identification Thresholds for 150 Fragmented Pictures from the Snodgrass and Vanderwart Picture Set. *Perceptual and Motor Skills*, 67(1), 3–36. https://doi.org/10.2466/pms.1988.67.1.3

- Stanislaw, H., & Todorov, N. (1999). Calculation of signal detection theory measures. *Behavior Research Methods, Instruments, & Computers*, 31(1), 137–149. https://doi.org/10.3758/BF03207704



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
    #>  package        * version date       lib source                           
    #>  abind            1.4-5   2016-07-21 [1] CRAN (R 4.1.0)                   
    #>  arrayhelpers     1.1-0   2020-02-04 [1] CRAN (R 4.1.0)                   
    #>  assertthat       0.2.1   2019-03-21 [1] CRAN (R 4.1.0)                   
    #>  backports        1.2.1   2020-12-09 [1] CRAN (R 4.1.0)                   
    #>  bayesplot      * 1.8.1   2021-06-14 [1] CRAN (R 4.1.0)                   
    #>  broom            0.7.10  2021-10-31 [1] CRAN (R 4.1.1)                   
    #>  cellranger       1.1.0   2016-07-27 [1] CRAN (R 4.1.0)                   
    #>  checkmate        2.0.0   2020-02-06 [1] CRAN (R 4.1.0)                   
    #>  cli              3.0.1   2021-07-17 [1] CRAN (R 4.1.1)                   
    #>  cmdstanr       * 0.4.0   2021-09-20 [1] local                            
    #>  coda             0.19-4  2020-09-30 [1] CRAN (R 4.1.0)                   
    #>  colorspace       2.0-1   2021-05-04 [1] CRAN (R 4.1.0)                   
    #>  crayon           1.4.2   2021-10-29 [1] CRAN (R 4.1.1)                   
    #>  DBI              1.1.1   2021-01-15 [1] CRAN (R 4.1.0)                   
    #>  dbplyr           2.1.1   2021-04-06 [1] CRAN (R 4.1.0)                   
    #>  digest           0.6.27  2020-10-24 [1] CRAN (R 4.1.0)                   
    #>  distributional   0.2.2   2021-02-02 [1] CRAN (R 4.1.0)                   
    #>  dplyr          * 1.0.6   2021-05-05 [1] CRAN (R 4.1.0)                   
    #>  ellipsis         0.3.2   2021-04-29 [1] CRAN (R 4.1.0)                   
    #>  evaluate         0.14    2019-05-28 [1] CRAN (R 4.1.0)                   
    #>  fansi            0.5.0   2021-05-25 [1] CRAN (R 4.1.0)                   
    #>  farver           2.1.0   2021-02-28 [1] CRAN (R 4.1.0)                   
    #>  fastmap          1.1.0   2021-01-25 [1] CRAN (R 4.1.0)                   
    #>  forcats        * 0.5.1   2021-01-27 [1] CRAN (R 4.1.0)                   
    #>  fs               1.5.0   2020-07-31 [1] CRAN (R 4.1.0)                   
    #>  generics         0.1.1   2021-10-25 [1] CRAN (R 4.1.0)                   
    #>  ggdist           3.0.0   2021-07-19 [1] CRAN (R 4.1.1)                   
    #>  ggplot2        * 3.3.5   2021-06-25 [1] CRAN (R 4.1.1)                   
    #>  ggridges         0.5.3   2021-01-08 [1] CRAN (R 4.1.0)                   
    #>  glue             1.4.2   2020-08-27 [1] CRAN (R 4.1.0)                   
    #>  gtable           0.3.0   2019-03-25 [1] CRAN (R 4.1.0)                   
    #>  haven            2.4.3   2021-08-04 [1] CRAN (R 4.1.1)                   
    #>  here             1.0.1   2020-12-13 [1] CRAN (R 4.1.1)                   
    #>  highr            0.9     2021-04-16 [1] CRAN (R 4.1.0)                   
    #>  hms              1.1.1   2021-09-26 [1] CRAN (R 4.1.1)                   
    #>  htmltools        0.5.2   2021-08-25 [1] CRAN (R 4.1.1)                   
    #>  httr             1.4.2   2020-07-20 [1] CRAN (R 4.1.0)                   
    #>  jsonlite         1.7.2   2020-12-09 [1] CRAN (R 4.1.0)                   
    #>  kableExtra     * 1.3.4   2021-02-20 [1] CRAN (R 4.1.0)                   
    #>  knitr          * 1.36    2021-09-29 [1] CRAN (R 4.1.1)                   
    #>  labeling         0.4.2   2020-10-20 [1] CRAN (R 4.1.0)                   
    #>  latex2exp      * 0.5.0   2021-03-18 [1] CRAN (R 4.1.0)                   
    #>  lattice          0.20-44 2021-05-02 [2] CRAN (R 4.1.0)                   
    #>  lifecycle        1.0.1   2021-09-24 [1] CRAN (R 4.1.1)                   
    #>  lubridate        1.7.10  2021-02-26 [1] CRAN (R 4.1.0)                   
    #>  magrittr         2.0.1   2020-11-17 [1] CRAN (R 4.1.0)                   
    #>  modelr           0.1.8   2020-05-19 [1] CRAN (R 4.1.0)                   
    #>  munsell          0.5.0   2018-06-12 [1] CRAN (R 4.1.0)                   
    #>  patchwork      * 1.1.1   2020-12-17 [1] CRAN (R 4.1.0)                   
    #>  pillar           1.6.4   2021-10-18 [1] CRAN (R 4.1.0)                   
    #>  pkgconfig        2.0.3   2019-09-22 [1] CRAN (R 4.1.0)                   
    #>  plyr             1.8.6   2020-03-03 [1] CRAN (R 4.1.0)                   
    #>  posterior      * 1.1.0   2021-09-09 [1] CRAN (R 4.1.1)                   
    #>  purrr          * 0.3.4   2020-04-17 [1] CRAN (R 4.1.0)                   
    #>  R6               2.5.1   2021-08-19 [1] CRAN (R 4.1.1)                   
    #>  ragg             1.2.0   2021-10-30 [1] CRAN (R 4.1.1)                   
    #>  Rcpp             1.0.7   2021-07-07 [1] CRAN (R 4.1.1)                   
    #>  readr          * 2.0.2   2021-09-27 [1] CRAN (R 4.1.1)                   
    #>  readxl           1.3.1   2019-03-13 [1] CRAN (R 4.1.0)                   
    #>  reprex           2.0.1   2021-08-05 [1] CRAN (R 4.1.1)                   
    #>  rlang            0.4.11  2021-04-30 [1] CRAN (R 4.1.0)                   
    #>  rmarkdown        2.11    2021-09-14 [1] CRAN (R 4.1.1)                   
    #>  rprojroot        2.0.2   2020-11-15 [1] CRAN (R 4.1.0)                   
    #>  rstudioapi       0.13    2020-11-12 [1] CRAN (R 4.1.0)                   
    #>  rvest            1.0.2   2021-10-16 [1] CRAN (R 4.1.1)                   
    #>  scales           1.1.1   2020-05-11 [1] CRAN (R 4.1.0)                   
    #>  sessioninfo      1.1.1   2018-11-05 [1] CRAN (R 4.1.0)                   
    #>  stringi          1.6.1   2021-05-10 [1] CRAN (R 4.1.0)                   
    #>  stringr        * 1.4.0   2019-02-10 [1] CRAN (R 4.1.0)                   
    #>  svglite          2.0.0   2021-02-20 [1] CRAN (R 4.1.0)                   
    #>  svUnit           1.0.6   2021-04-19 [1] CRAN (R 4.1.0)                   
    #>  systemfonts      1.0.3   2021-10-13 [1] CRAN (R 4.1.1)                   
    #>  tensorA          0.36.2  2020-11-19 [1] CRAN (R 4.1.0)                   
    #>  textshaping      0.3.6   2021-10-13 [1] CRAN (R 4.1.1)                   
    #>  tibble         * 3.1.2   2021-05-16 [1] CRAN (R 4.1.0)                   
    #>  tidybayes      * 3.0.1   2021-10-27 [1] Github (mjskay/tidybayes@031b63d)
    #>  tidyr          * 1.1.4   2021-09-27 [1] CRAN (R 4.1.1)                   
    #>  tidyselect       1.1.1   2021-04-30 [1] CRAN (R 4.1.0)                   
    #>  tidyverse      * 1.3.1   2021-04-15 [1] CRAN (R 4.1.1)                   
    #>  tzdb             0.1.2   2021-07-20 [1] CRAN (R 4.1.1)                   
    #>  utf8             1.2.1   2021-03-12 [1] CRAN (R 4.1.0)                   
    #>  vctrs            0.3.8   2021-04-29 [1] CRAN (R 4.1.0)                   
    #>  viridisLite      0.4.0   2021-04-13 [1] CRAN (R 4.1.0)                   
    #>  webshot          0.5.2   2019-11-22 [1] CRAN (R 4.1.0)                   
    #>  withr            2.4.2   2021-04-18 [1] CRAN (R 4.1.0)                   
    #>  xfun             0.26    2021-09-14 [1] CRAN (R 4.1.1)                   
    #>  xml2             1.3.2   2020-04-23 [1] CRAN (R 4.1.0)                   
    #> 
    #> [1] C:/Users/Administrator/Documents/R/win-library/4.1
    #> [2] C:/Program Files/R/R-4.1.0/library
    ```
