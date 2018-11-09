hw06-aidanh14
================
Aidan Hughes
November 7, 2018

Overview
========

Taken from the [homework HTML file](http://stat545.com/Classroom/assignments/hw05/hw05.html), here are the two topics I chose to complete for this assignment:

-   Write one (or more) functions that do something useful to pieces of the Gapminder or Singer data.

-   Create a nested data frame and map a function over the list column holding the nested data. Use list extraction or other functions to pull interesting information out of these results and work your way back to a simple data frame you can visualize and explore.

Before getting started, let load the necessary packages.

``` r
suppressPackageStartupMessages(library("gapminder"))
suppressPackageStartupMessages(library("tidyverse"))
suppressPackageStartupMessages(library("scales"))
```

Part 1: Writing a function
==========================

Let's write a function that takes Gapminder data on a single continent and plots a linear a regression of population vs. life expectancy for a ranom set of countries. This will be useful in **Part 2: Working With Nested Frames**.

As always we should walk before we run, so we'll get the functionality working on a sample dataset first.

``` r
size <- 10

america_gap <- gapminder %>%
  filter(continent == "Americas") %>%
  select(-continent)

# randomly choose sample indexes for countries in the dataset
some_countries <- america_gap$country %>%
  unique() %>% {
    rand_nums <- sample(1:length(.), size = min(size, length(.)))
    .[rand_nums] }

some_countries
```

    ##  [1] Trinidad and Tobago Uruguay             Canada             
    ##  [4] Venezuela           United States       Jamaica            
    ##  [7] El Salvador         Brazil              Colombia           
    ## [10] Haiti              
    ## 142 Levels: Afghanistan Albania Algeria Angola Argentina ... Zimbabwe

``` r
america_gap %>%
  group_by(country) %>%
  filter(country %in% some_countries) %>%
  ggplot(aes(x = pop, y = lifeExp)) +
  geom_point(aes(pop, lifeExp, colour = country)) +
  geom_smooth(aes(pop, lifeExp, colour = country), method = "lm", se = FALSE) +
  scale_x_log10()
```

![](hw06-aidanh14_files/figure-markdown_github/unnamed-chunk-1-1.png)

Now that we've got the code working, let's store it as a function and try it on a different dataset.

``` r
gap_linreg <- function (continent_df, num_countries) {
  
  try(continent_df <- unnest(continent_df))
  
  # randomly choose sample indexes for countries in the dataset
  some_countries <- continent_df$country %>%
    unique() %>% {
      rand_nums <- sample(1:length(.), size = min(num_countries, length(.)))
      .[rand_nums] }
  
  continent_df %>%
    group_by(country) %>%
    filter(country %in% some_countries) %>%
    ggplot(aes(x = pop, y = lifeExp)) +
    geom_point(aes(pop, lifeExp, colour = country)) +
    geom_smooth(aes(pop, lifeExp, colour = country), method = "lm", se = FALSE) +
    scale_x_log10(labels = comma_format()) +
    theme_minimal() +
    labs(x = "Population (Logarithm)",
         y = "Life Expectancy",
         title = "Population vs. Life Expectancy",
         colour = "Country") +
    theme(plot.title = element_text(hjust = 0.5))
  
  
}

gapminder %>%
  filter(continent == "Europe") %>%
  gap_linreg(num_countries = 5)
```

![](hw06-aidanh14_files/figure-markdown_github/unnamed-chunk-2-1.png)

Part 2: Working with Nested Dataframes
======================================

Nesting the Gapminder dataset by continent makes it easy to apply the function `gap_linreg()` from the last

``` r
cont_nests <- gapminder %>%
  group_by(continent) %>%
  nest()
  
map(cont_nests[["data"]], gap_linreg, 4)
```

    ## [[1]]

![](hw06-aidanh14_files/figure-markdown_github/unnamed-chunk-3-1.png)

    ## 
    ## [[2]]

![](hw06-aidanh14_files/figure-markdown_github/unnamed-chunk-3-2.png)

    ## 
    ## [[3]]

![](hw06-aidanh14_files/figure-markdown_github/unnamed-chunk-3-3.png)

    ## 
    ## [[4]]

![](hw06-aidanh14_files/figure-markdown_github/unnamed-chunk-3-4.png)

    ## 
    ## [[5]]

![](hw06-aidanh14_files/figure-markdown_github/unnamed-chunk-3-5.png)

``` r
gap_linreg(cont_nests[[1,"data"]], 4)
```

![](hw06-aidanh14_files/figure-markdown_github/unnamed-chunk-3-6.png)
