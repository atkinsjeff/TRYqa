
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TRYqa

<!-- badges: start -->
<!-- badges: end -->

The goal of TRYqa is to …

## Installation

You can install the development version of TRYqa from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("atkinsjeff/TRYqa")
```

## Example

This is a basic example of importing data via TRYqa:

``` r
library(TRYqa)
## basic example code
data.path <- ("./inst/extdata/try_sample_data.txt") # you can also replace this with yr own data

df <- read.try(data.path)
```

The other functions are in beta mode:

``` r
# creating community weighted means from sample data
# running ?sum.traits() will give you some further details on what is going on here
trait.list <- c(3117, 185, 4, 45, 56, 14, 151, 55)
species.list <- unique(df$SpeciesName)
weights <- c(0.1, 0.2, 0.3, 0.4, 0.1)

sum.traits(df, cwm = FALSE, species = species.list[1:5], traits = trait.list, weights = weights)
```

There are some graphing functions but who knows if they will work on
your computer:

``` r
# by species
plot.species(df, trait = 14)

# then by traits
plot.traits(df, trait.list = trait.list, species.list[1:5],, group = TRUE)
```
