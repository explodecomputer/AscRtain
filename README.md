<!-- badges: start -->

  [![Travis build status](https://travis-ci.org/explodecomputer/collidR.svg?branch=master)](https://travis-ci.org/explodecomputer/collidR)
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
  [![codecov](https://codecov.io/github/explodecomputer/collidR/branch/master/graphs/badge.svg)](https://codecov.io/github/explodecomputer/collidR)
  <!-- badges: end -->

**Under development**

# CollidR

A set of simulation and plotting functions for examining different collider structures on estimates of associations between two variables.

This was motivated by two examples

1. Do the massive sample sizes forecast in GWAS make it more problematic now to detect small effects, due to sample ascertainment?
2. How liable is the inference of causal factors for COVID-19 severity to false positives due to the highly ascertained nature of data collection?

The Y-structure DAGs representing the sampling bias illustrate how colliders can induce problematic associations between hypothesised exposures and outcomes.

<img src="https://drive.google.com/uc?id=15heO4ms7ra9g2yl0OYxVU5UwetU-Tnmu"/>



## Installation

```
devtools::install_github("explodecomputer/collidR")
```

## Documentation

https://explodecomputer.github.io/collidR/
