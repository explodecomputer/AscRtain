<!-- badges: start -->

  [![Travis build status](https://travis-ci.org/explodecomputer/AscRtain.svg?branch=master)](https://travis-ci.org/explodecomputer/AscRtain)
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
  [![codecov](https://codecov.io/github/explodecomputer/AscRtain/branch/master/graphs/badge.svg)](https://codecov.io/github/explodecomputer/AscRtain)
  <!-- badges: end -->

# AscRtain

A set of simulation and plotting functions for examining bias that might arise due to sampling ascertainment, in particular, when hypothesised exposure and outcome both influence sample inclusion.

This was motivated by two examples

1. Do the massive sample sizes forecast in GWAS make it more problematic now to detect small effects, due to sample ascertainment?
2. How liable is the inference of causal factors for COVID-19 severity to false positives due to the highly ascertained nature of data collection?

<img src="https://drive.google.com/uc?id=15heO4ms7ra9g2yl0OYxVU5UwetU-Tnmu"/>

The V or Y-structure DAGs representing the sampling bias illustrate how colliders can induce biased associations between hypothesised exposures and outcomes.

## Installation

```
devtools::install_github("explodecomputer/AscRtain")
```
