
<!-- README.md is generated from README.Rmd. Please edit that file -->
tensorr: sparse tensors in R
============================

[![Build Status](https://travis-ci.org/zamorarr/tensorr.svg?branch=master)](https://travis-ci.org/zamorarr/tensorr) [![Coverage Status](https://img.shields.io/codecov/c/github/zamorarr/tensorr/master.svg)](https://codecov.io/github/zamorarr/tensorr?branch=master)

`tensorr` provides methods to manipulate and store sparse tensors. Tensors are multi-dimensional generalizations of matrices (two dimensional) and vectors (one dimensional).

It has three main goals:

-   Provide an effecient format to store sparse tensors in R.
-   Provide standard tensor operations such as multiplication and unfolding.
-   Provide standard tensor decomposition techniques such as CP and Tucker.

References
----------

Many of the dense and sparse implementation ideas were adpated from:

-   B. W. Bader and T. G. Kolda. Algorithm 862: MATLAB tensor classes for fast algorithm prototyping, ACM Transactions on Mathematical Software 32(4):635-653, December 2006.
-   B. W. Bader and T. G. Kolda. Efficient MATLAB computations with sparse and factored tensors, SIAM Journal on Scientific Computing 30(1):205-231, December 2007.
-   [scikit-tensor](https://github.com/mnick/scikit-tensor)

For a review on tensors, see:

-   T. G. Kolda and B. W. Bader, Tensor Decompositions and Applications, SIAM Review 51(3):455-500, September 2009
