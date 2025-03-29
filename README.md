
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mitools

<!-- badges: start -->
<!-- badges: end -->

mitools is an R package designed to help users create sets of variables
based on a mutual information approach. In this context, a set is a
collection of distinct elements (e.g., variables) that can also be
treated as a single entity. Mutual information, a concept from
probability theory, quantifies the dependence between two variables by
expressing how much information about one variable can be gained from
observing the other.

## Installation

You can install mitools like with the following code snippet:

``` r
devtools::install_github('nicolasleenaerts/mitools')
```

## Pairing variables

You can create sets of variables with the *pairmi* function. It takes a
dataframe of variables and pairs them until a certain depth (i.e., a
maximum number of elements). For each set of variables, the mutual
information between the variables is calculated, after which a
G-statistic is calculated and evalauted for its significance under a
chi-squared distribution with a prespecified alpha. Alternatively, the
user can set a certain threshold of mutual information to determine the
significance of the sets.

``` r
# Loading the package, which automatically also downloads the example data (misimdata)
library(mitools) 

# Pairing variables
results = pairmi(misimdata[,2:6])

# View data with sets added
View(results$expanded.data)
```

| x1  | x2  | x3  | x4  | x5  | x1_x5 |
|:---:|:---:|:---:|:---:|:---:|:-----:|
|  1  |  1  |  1  |  1  |  1  |   1   |
|  1  |  0  |  0  |  1  |  1  |   1   |
|  1  |  0  |  1  |  0  |  1  |   1   |
|  1  |  1  |  1  |  1  |  1  |   1   |
|  1  |  1  |  0  |  1  |  1  |   1   |

Table 1. Expanded Data

``` r
# View information on the sets
View(results$pairs)
```

| depth | pair  |    mi    | relative.mi |     p     |
|:-----:|:-----:|:--------:|:-----------:|:---------:|
|   2   | x1_x5 | 0.003869 |      0      | 0.0121356 |

Table 2. Information on sets
