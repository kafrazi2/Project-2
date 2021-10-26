ST 558 Project 2
================
Kaylee Frazier and Rebecca Voelker
10/31/2021

# Description

This repo includes analysis on the [online news popularity data set](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity). It subsets the data into six documents subsetted from the data's channel name. In this repo, we summarize the data and then try to predict the number of shares using predictive modeling. 

# Packages

This is the list of the packages used.
- [`tidyverse`](https://www.tidyverse.org/): useful features for data
    science
- [`caret`](https://cran.r-project.org/web/packages/caret/vignettes/caret.html): set of functions that help to streamline the process for creating predictive models
- [`knitr`](https://cran.r-project.org/web/packages/knitr/index.html):
    a markdown friendly way to display tables
    
# Links 

These are links to the generated analyses.
- [Lifestyle articles is available here](LifestyleAnalysis.html).
- [Entertainment articles is available here](EntertainmentAnalysis.html).
- [Bus articles is available here](BusAnalysis.html).
- [Socmed articles is available here](SocmedAnalysis.html).
- [Tech articles is available here](TechAnalysis.html).
- [World articles is available here](WorldAnalysis.html).

# Code

``` r
getwd()
```

    ## [1] "C:/Users/Rebecca/OneDrive/Desktop/ST 558/Repos/Project-2"

``` r
## Read in Raw Data Using Relative Path
library(readr)
rawData <- read_csv(file = "./project2_rawdata.csv") 
rawData
```

    ## # A tibble: 39,644 x 61
    ##    url        timedelta n_tokens_title n_tokens_content n_unique_tokens
    ##    <chr>          <dbl>          <dbl>            <dbl>           <dbl>
    ##  1 http://ma~       731             12              219           0.664
    ##  2 http://ma~       731              9              255           0.605
    ##  3 http://ma~       731              9              211           0.575
    ##  4 http://ma~       731              9              531           0.504
    ##  5 http://ma~       731             13             1072           0.416
    ##  6 http://ma~       731             10              370           0.560
    ##  7 http://ma~       731              8              960           0.418
    ##  8 http://ma~       731             12              989           0.434
    ##  9 http://ma~       731             11               97           0.670
    ## 10 http://ma~       731             10              231           0.636
    ## # ... with 39,634 more rows, and 56 more variables:
    ## #   n_non_stop_words <dbl>, n_non_stop_unique_tokens <dbl>,
    ## #   num_hrefs <dbl>, num_self_hrefs <dbl>, num_imgs <dbl>,
    ## #   num_videos <dbl>, average_token_length <dbl>, num_keywords <dbl>,
    ## #   data_channel_is_lifestyle <dbl>,
    ## #   data_channel_is_entertainment <dbl>, data_channel_is_bus <dbl>,
    ## #   data_channel_is_socmed <dbl>, data_channel_is_tech <dbl>, ...

``` r
## Subset Data for Entertainment Data Channel
library(dplyr)
entertainmentData <- rawData %>% filter( data_channel_is_entertainment == 1)

## Create Training and Test Data Sets
library(caret)
set.seed(500)
trainIndex <- createDataPartition(entertainmentData$shares, p = 0.7, list = FALSE)

trainData <- entertainmentData[trainIndex,]
testData <- entertainmentData[-trainIndex,]

## Printed Out Training and Test Data Sets - Can Check if you are getting matching first ~5 Observations (Using Seed = 500)
trainData
```

    ## # A tibble: 4,941 x 61
    ##    url        timedelta n_tokens_title n_tokens_content n_unique_tokens
    ##    <chr>          <dbl>          <dbl>            <dbl>           <dbl>
    ##  1 http://ma~       731              9              531           0.504
    ##  2 http://ma~       731             14              194           0.765
    ##  3 http://ma~       731             12              161           0.669
    ##  4 http://ma~       731             12              177           0.741
    ##  5 http://ma~       731              5              356           0.618
    ##  6 http://ma~       730             11              281           0.611
    ##  7 http://ma~       730             10              909           0.450
    ##  8 http://ma~       729              6              241           0.660
    ##  9 http://ma~       729              7              376           0.569
    ## 10 http://ma~       729             12              855           0.439
    ## # ... with 4,931 more rows, and 56 more variables:
    ## #   n_non_stop_words <dbl>, n_non_stop_unique_tokens <dbl>,
    ## #   num_hrefs <dbl>, num_self_hrefs <dbl>, num_imgs <dbl>,
    ## #   num_videos <dbl>, average_token_length <dbl>, num_keywords <dbl>,
    ## #   data_channel_is_lifestyle <dbl>,
    ## #   data_channel_is_entertainment <dbl>, data_channel_is_bus <dbl>,
    ## #   data_channel_is_socmed <dbl>, data_channel_is_tech <dbl>, ...

``` r
testData
```

    ## # A tibble: 2,116 x 61
    ##    url        timedelta n_tokens_title n_tokens_content n_unique_tokens
    ##    <chr>          <dbl>          <dbl>            <dbl>           <dbl>
    ##  1 http://ma~       731             12              219           0.664
    ##  2 http://ma~       731             11              454           0.566
    ##  3 http://ma~       729             10              413           0.606
    ##  4 http://ma~       729              9               81           0.787
    ##  5 http://ma~       729             11              308           0.633
    ##  6 http://ma~       729              8              345           0.604
    ##  7 http://ma~       729             11              289           0.649
    ##  8 http://ma~       729             11              272           0.524
    ##  9 http://ma~       729             11              277           0.450
    ## 10 http://ma~       728             13              204           0.646
    ## # ... with 2,106 more rows, and 56 more variables:
    ## #   n_non_stop_words <dbl>, n_non_stop_unique_tokens <dbl>,
    ## #   num_hrefs <dbl>, num_self_hrefs <dbl>, num_imgs <dbl>,
    ## #   num_videos <dbl>, average_token_length <dbl>, num_keywords <dbl>,
    ## #   data_channel_is_lifestyle <dbl>,
    ## #   data_channel_is_entertainment <dbl>, data_channel_is_bus <dbl>,
    ## #   data_channel_is_socmed <dbl>, data_channel_is_tech <dbl>, ...
