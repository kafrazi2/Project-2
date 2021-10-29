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
- [`knitr`](https://cran.r-project.org/web/packages/knitr/index.html): a markdown friendly way to display tables
- [`ggplot2`](https://ggplot2.tidyverse.org/): a package for making graphs and visualizations
- [`randomForest`](https://www.rdocumentation.org/packages/randomForest/versions/4.6-14/topics/randomForest): helps create random forest models
- [`readr`](https://readr.tidyverse.org/): a fast and easy way to read in rectangular data
- [`dplyr`](https://dplyr.tidyverse.org/): aids with data manipulation
- [`rmarkdown`](https://www.rdocumentation.org/packages/rmarkdown/versions/1.7): adds enhancements to R Markdown
- [`shiny`](https://shiny.rstudio.com/): makes it easy to create interactive webpages from R
    
# Links 

These are links to the generated analyses.
- [Lifestyle articles is available here](Lifestyle.html).
- [Entertainment articles is available here](Entertainment.html).
- [Business articles is available here](https://kafrazi2.github.io/Project-2/Business.html).
- [Social Media articles is available here](Social-Media.html).
- [Tech articles is available here](Tech.html).
- [World articles is available here](World.html).

# Code

```{r}
#get unique names
channelIDs <- unique(rawDataNew$data_channel)
#create file names
output_file <- paste0(channelIDs, ".html")
#create a list for each channel with just the channel name parameter
params = lapply(channelIDs, FUN = function(x){list(data_channel = x)})

#put into a data frame
reports <- tibble(output_file, params)
```

```{r}
#read in library
library(rmarkdown)
#need to use x[[1]] to get at elements since tibble doesn't simplify
apply(reports, MARGIN = 1, 
            FUN = function(x){
                render(input = "ST558_Project2.Rmd", output_file = x[[1]], params = x[[2]])
                })
```

