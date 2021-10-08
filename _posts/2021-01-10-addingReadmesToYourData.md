---
layout: single
title: "Adding a Readme to your data files"
date: 2021-01-04
tags:
  - R
  - best practice
permalink: /posts/2021/01/creatingreadme/
classes: wide
---



Because of a recent experience, I was reminded that it is absolutley annoying when researchers upload their data to an open repository (e.g. OSF or Github) without adding any form of description or wiki. Thus, I decided to a little `R` - function for me and my colleagues to help with this^[You can source the function directly via `devtools::source_url("https://github.com/dizyd/functions/blob/master/make_readme_fun.R?raw=TRUE")`]. 

Lets start by creating a data.frame containing our very cool and very interesting data. 


```r
library(tidyverse)

df = data.frame("ID"   = 1:50,
                "cond" = sample(c("A","B"),50,replace=T),
                "rt"   = rnorm(50,500,40))
```

This data.frame now contains the reaction time data of 50 participant in one of two conditions:


```r
head(df)
#>   ID cond       rt
#> 1  1    A 496.7167
#> 2  2    B 481.2317
#> 3  3    A 475.6879
#> 4  4    B 548.9158
#> 5  5    B 541.5480
#> 6  6    A 509.4509
```

The next thing we need is a vector of the same length as the number of variables we have in our data.frame, which contains the description of the corresponding variables. For instance, in our example this might look like this:


```r
desc = c("unique numeric participant ID [1-50]",   # description for variable ID
         "condition [A:congruent, B:incongruent]", # description for variable condition
         "average reaction time in ms")            # description for the variable rt
```


Next, we need our function. I will first show you the complete function-code and then go through it step by step. So this is what our final function will look like:



```r
make_df_readme     <- function(df,desc,info = NULL,file = "readme.txt",add_examples=TRUE,digits=2){  
  
  
  temp0 <- data.frame("Variable"    = names(df),
                      "Type"        = sapply(df, class),
                      "Description" = desc)
  
  row.names(temp0) <- NULL
  
  if(add_examples){
    
    temp_info <- df[sample(1:nrow(df),2),] %>%
                    mutate_if(is.numeric,round,digits) %>% 
                    t() %>%
                    as.data.frame() %>%
                    apply(., 1, paste, collapse=",") %>% 
                    unlist()
    
    names(temp_info) <- NULL
    temp0 <- temp0 %>% add_column("Example" = temp_info,.before = "Description")
  }
  
  # Start writing to the file
  sink(file)
  if(length(info)){
    cat(info,"\n","\n","data.frame (",nrow(df),",",ncol(df),")","\n","\n",sep = "")
  }
  
  temp1 <- temp0 %>% knitr::kable(format = "markdown") 
  paste0(temp1,"\n") %>% cat(sep = "")
  
  # Stop writing to the file
  sink()
  
}
```

In the first part of the function we start by creating an inital data.frame containing the names of our variables, the corresponding description and the variable type. 


```r
  temp0 <- data.frame("Variable"    = names(df),
                      "Type"        = sapply(df, class),
                      "Description" = desc)
  
  row.names(temp0) <- NULL
```



```
#>   Variable      Type                            Description
#> 1       ID   integer   unique numeric participant ID [1-50]
#> 2     cond character condition [A:congruent, B:incongruent]
#> 3       rt   numeric            average reaction time in ms
```

The next step is to add two example entries of each variable to this `temp0` data.frame **if** the argument `add_examples == T`. The following few lines of codes will sample two entries from the initial data.frame (`sample()`), round numeric variables to `digits` decimal places (`mutate_if`) ,transpose it (`t()`), paste the two entries together (`apply()`) and the return everything as vector (`unlist()`).  Afterwards we add this new vector to the data.frame as a new column called `Description`


```r
    temp_info <- df[sample(1:nrow(df),2),] %>%
                  mutate_if(is.numeric,round,2) %>% 
                  t() %>%
                  apply(., 1, paste, collapse=",") %>% 
                  unlist()
    
    names(temp_info) <- NULL
    temp0 <- temp0 %>% add_column("Example" = temp_info,.before = "Description")
  
```

So now we have:


```
#>   Variable      Type       Example                            Description
#> 1       ID   integer         16,41   unique numeric participant ID [1-50]
#> 2     cond character           B,A condition [A:congruent, B:incongruent]
#> 3       rt   numeric 491.78,499.02            average reaction time in ms
```

The next line will add an info text, provided trough the `info = ` argument, as well as information about the dimensions of the data.frame to the top of the final `readme.txt`. The statement `length(info)` will return be `FALSE`, and thus nothing added, if the argument is left empty (or `= NULL`). 


The last part prints our data.frame `temp0` in the format of a `markdown` table by using the `kable()` function. I used the `markdown` format since it looks nice as a plain `.txt` file or when copy & pasted in a Github Readme.md or to the OSF wiki part. 

Everything between the  `sink()` statements is then saved as `readme.txt` in your working directory (you can change the name or the path by changing it using the `file = ` argument).


Now lets put everything together. Running the following command (of coruse, after first running the function code above) 


```r
make_df_readme(df,desc,info="This is data for the second experiment reported in Fancy (2021) and ...")
```

produces the `readme.txt` file which looks like this:


```r
This is data for the second experiment reported in Fancy (2021) and ...

data.frame (50,3)

|Variable |Type      |Example             |Description                            |
|:--------|:---------|:-------------------|:--------------------------------------|
|ID       |integer   |12,15               |unique numeric participant ID [1-50]   |
|cond     |character |congruent,congruent |condition [A:congruent, B:incongruent] |
|rt       |numeric   |511.44,486.54       |average reaction time in ms            |

```

IFf you copy & paste the table in your OSF wiki or Github Readme the table would look even better: 

|Variable |Type      |Example             |Description                            |
|:--------|:---------|:-------------------|:--------------------------------------|
|ID       |integer   |12,15               |unique numeric participant ID [1-50]   |
|cond     |character |congruent,congruent |condition [A:congruent, B:incongruent] |
|rt       |numeric   |511.44,486.54       |average reaction time in ms            |