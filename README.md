# portkey
An R package for implementing the Portkey two-coin Bernoulli factory of Vats et. al. (2020). The main function is `portkey` which is meant to be used within an MCMC algorithm to determine whether the move should be accepted or rejected.


# Installation
This R package is not on CRAN and is hosted on GitHub only

To download this development repo,  through the the `devtools` package:

```{r}
# install.packages("devtools")
library(devtools)
devtools::install_github("dvats/portkey")
```

