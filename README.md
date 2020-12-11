
<!-- README.md is generated from README.Rmd. Please edit that file -->

# firmValueSim

<!-- badges: start -->

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
and the discounted cash flow valuation is done either via a FCFF or FCFE
as per usual.
