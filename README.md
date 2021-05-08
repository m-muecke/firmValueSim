
<!-- README.md is generated from README.Rmd. Please edit that file -->

# firmValueSim

<!-- badges: start -->

[![R-CMD-check](https://github.com/maximilian-muecke/firmValueSim/workflows/R-CMD-check/badge.svg)](https://github.com/maximilian-muecke/firmValueSim/actions)
[![Codecov test
coverage](https://codecov.io/gh/maximilian-muecke/firmValueSim/branch/master/graph/badge.svg)](https://codecov.io/gh/maximilian-muecke/firmValueSim?branch=master)
<!-- badges: end -->

## Overview

To minimize the downside of the assumptions made in an intrinsic
valuation approach, the possibility exists to model the risk via a Monte
Carlo simulation. Each variable in the model that is derived through
assumption that can be stochastically modeled. The main purpose of a
Monte Carlo in the context of a valuation is to achieve a risk
management component, by integrating the expected value of multiple
parameter outcomes. The two main approaches of risk management in
valuation are either via a tree-based or simulation approach. The
advantages of using simulations instead of decision-trees is that the
flexibility of not only choosing a binary input approach, but instead
choosing an underlying distribution. The first-step in the simulation is
assigning a distribution of the variable, either through historical
data, most likely outcome or market consensus. After the distributions
are assigned, a single value of each parameter distribution gets sampled
and the valuation is done as per usual.

## Installation

``` r
devtools::install_github("maximilian-muecke/firmValueSim")
```

## Usage

``` r
library(firmValueSim)
library(ggplot2)
library(quantmod)
#> Loading required package: xts
#> Loading required package: zoo
#> 
#> Attaching package: 'zoo'
#> The following objects are masked from 'package:base':
#> 
#>     as.Date, as.Date.numeric
#> Loading required package: TTR
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo

# simulate stock prices via the Gordon Growth model
prices <- ddm_sim(
  1.5, r = 0.07, g_mu = 0.03, g_sigma = 0.01, n_sim = 10000, seed = 12345
)
# visualize distribution of prices
df <- data.frame(prices = prices)
ggplot(df, aes(x = prices)) +
  geom_histogram(binwidth = 5)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r
# or fetch yahoo finance data with quantmod
financials <- getQuote(
  "DAI.DE",
  what = yahooQF(c("Market Capitalization", "Dividend/Share", "Earnings/Share"))
)
# calculate plowback ration = 1 - (dividend per share / EPS)
b <- plowback_ratio(financials[, "Dividend/Share"], financials[, "Earnings/Share"])
# calculate growth rate: g = ROE x plowback ratio
g <- 0.1252 * b
# simulate stock prices via the Gordon Growth model
prices <- ddm_sim(
  financials[, "Dividend/Share"], r = 0.12, g_mu = g, g_sigma = 0.15*g,
  n_sim = 10000, seed = 12345
)
# visualize distribution of prices
df <- data.frame(prices = prices)
ggplot(df, aes(x = prices)) +
  geom_histogram(binwidth = 5)
```

<img src="man/figures/README-unnamed-chunk-3-2.png" width="100%" />

## References

-   Abrams, J. B. (2001). Quantitative business valuation. New York:
    McGraw-Hill.
