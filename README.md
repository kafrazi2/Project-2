ST 558 Project 2
================
Kaylee Frazier and Rebecca Voelker
10/31/2021

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
entertainmentData
```

    ## # A tibble: 7,057 x 61
    ##    url        timedelta n_tokens_title n_tokens_content n_unique_tokens
    ##    <chr>          <dbl>          <dbl>            <dbl>           <dbl>
    ##  1 http://ma~       731             12              219           0.664
    ##  2 http://ma~       731              9              531           0.504
    ##  3 http://ma~       731             14              194           0.765
    ##  4 http://ma~       731             12              161           0.669
    ##  5 http://ma~       731             11              454           0.566
    ##  6 http://ma~       731             12              177           0.741
    ##  7 http://ma~       731              5              356           0.618
    ##  8 http://ma~       730             11              281           0.611
    ##  9 http://ma~       730             10              909           0.450
    ## 10 http://ma~       729             10              413           0.606
    ## # ... with 7,047 more rows, and 56 more variables:
    ## #   n_non_stop_words <dbl>, n_non_stop_unique_tokens <dbl>,
    ## #   num_hrefs <dbl>, num_self_hrefs <dbl>, num_imgs <dbl>,
    ## #   num_videos <dbl>, average_token_length <dbl>, num_keywords <dbl>,
    ## #   data_channel_is_lifestyle <dbl>,
    ## #   data_channel_is_entertainment <dbl>, data_channel_is_bus <dbl>,
    ## #   data_channel_is_socmed <dbl>, data_channel_is_tech <dbl>, ...
