
<!-- README.md is generated from README.Rmd. Please edit that file -->
tensorr: sparse tensors in R
============================

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tensorr)](https://cran.r-project.org/package=tensorr) [![Build Status](https://travis-ci.org/zamorarr/tensorr.svg?branch=master)](https://travis-ci.org/zamorarr/tensorr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/zamorarr/tensorr?branch=master&svg=true)](https://ci.appveyor.com/project/zamorarr/tensorr) [![Coverage Status](https://img.shields.io/codecov/c/github/zamorarr/tensorr/master.svg)](https://codecov.io/github/zamorarr/tensorr?branch=master)

`tensorr` provides methods to manipulate and store sparse tensors. Tensors are multi-dimensional generalizations of matrices (two dimensional) and vectors (one dimensional).

It has three main goals:

-   Provide an effecient format to store sparse tensors in R.
-   Provide standard tensor operations such as multiplication and unfolding.
-   Provide standard tensor decomposition techniques such as CP and Tucker.

Installation
------------

The development version of **tensorr** is available on github.

``` r
devtools::install_github("zamorarr/tensorr")
```

Usage
-----

See the [introduction vignette](https://zamorarr.github.io/tensorr/articles/introduction.html) for a comprehensive overview. To create a sparse tensor you have to provide the non-zero values, subscripts to the non-zero values, and the overall dimensions of the tensor.

``` r
library(tensorr)

subs <- list(c(1,1,1), c(1,1,2))
vals <- c(10, 20)
dims <- c(2,2,2)
x <- sptensor(subs, vals, dims)
x
#> <A 2x2x2 sparse tensor with 2 non-zero entries>
#> subs: <1,1,1> <1,1,2>
#> vals: 10 20
```

Tensor References
-----------------

Many of the dense and sparse implementation ideas were adpated from:

-   B. W. Bader and T. G. Kolda. Algorithm 862: MATLAB tensor classes for fast algorithm prototyping, ACM Transactions on Mathematical Software 32(4):635-653, December 2006.
-   B. W. Bader and T. G. Kolda. Efficient MATLAB computations with sparse and factored tensors, SIAM Journal on Scientific Computing 30(1):205-231, December 2007.
-   [scikit-tensor](https://github.com/mnick/scikit-tensor)

For a review on tensors, see:

-   T. G. Kolda and B. W. Bader, Tensor Decompositions and Applications, SIAM Review 51(3):455-500, September 2009
