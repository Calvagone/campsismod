
<!-- badges: start -->

[![R-CMD-check](https://github.com/Calvagone/campsismod/workflows/R-CMD-check/badge.svg)](https://github.com/Calvagone/campsismod/actions)
[![codecov](https://codecov.io/gh/Calvagone/campsismod/branch/main/graph/badge.svg?token=7DHBRQD7AG)](https://codecov.io/gh/Calvagone/campsismod)
<!-- badges: end -->

## Installation

Install the latest stable release using `devtools`:

``` r
devtools::install_github("Calvagone/campsismod")
```

## Basic examples

### Load example from model library

Load 2-compartment PK model from built-in model library:

``` r
library(campsismod)
model <- model_library$advan4_trans4
```

### Write CAMPSIS model

``` r
model %>% write(file="path_to_model_folder")
```

``` r
list.files("path_to_model_folder")
```

    ## [1] "model.campsis" "omega.csv"     "sigma.csv"     "theta.csv"

### Read and show CAMPSIS model

``` r
model <- read.campsis(file="path_to_model_folder")
show(model)
```

    ## [MAIN]
    ## KA=THETA_KA*exp(ETA_KA)
    ## CL=THETA_CL*exp(ETA_CL)
    ## V2=THETA_V2*exp(ETA_V2)
    ## V3=THETA_V3*exp(ETA_V3)
    ## Q=THETA_Q*exp(ETA_Q)
    ## S2=V2
    ## 
    ## [ODE]
    ## d/dt(A_DEPOT)=-KA*A_DEPOT
    ## d/dt(A_CENTRAL)=KA*A_DEPOT + Q*A_PERIPHERAL/V3 + (-CL/V2 - Q/V2)*A_CENTRAL
    ## d/dt(A_PERIPHERAL)=-Q*A_PERIPHERAL/V3 + Q*A_CENTRAL/V2
    ## d/dt(A_OUTPUT)=CL*A_CENTRAL/V2
    ## F=A_CENTRAL/S2
    ## 
    ## [ERROR]
    ## CP=F
    ## OBS_CP=CP*(EPS_PROP + 1)
    ## Y=OBS_CP
    ## 
    ## 
    ## THETA's:
    ##   name index value   fix
    ## 1   KA     1     1 FALSE
    ## 2   CL     2     5 FALSE
    ## 3   V2     3    80 FALSE
    ## 4   V3     4    20 FALSE
    ## 5    Q     5     4 FALSE
    ## OMEGA's:
    ##   name index index2 value   fix type same
    ## 1   KA     1      1 0.025 FALSE  var   NA
    ## 2   CL     2      2 0.025 FALSE  var   NA
    ## 3   V2     3      3 0.025 FALSE  var   NA
    ## 4   V3     4      4 0.025 FALSE  var   NA
    ## 5    Q     5      5 0.025 FALSE  var   NA
    ## SIGMA's:
    ##   name index index2 value   fix type
    ## 1 PROP     1      1 0.025 FALSE  var
    ## No variance-covariance matrix
    ## 
    ## Compartments:
    ## A_DEPOT (CMT=1)
    ## A_CENTRAL (CMT=2)
    ## A_PERIPHERAL (CMT=3)
    ## A_OUTPUT (CMT=4)

### Simulate with RxODE or mrgsolve

``` r
library(campsis)

dataset <- Dataset(5) %>%
  add(Bolus(time=0, amount=1000, ii=12, addl=2)) %>%
  add(Observations(times=0:36))

rxode <- model %>% simulate(dataset=dataset, dest="RxODE", seed=1)
mrgsolve <- model %>% simulate(dataset=dataset, dest="mrgsolve", seed=1)
```

``` r
spaghettiPlot(rxode, "CP")
```

![RxODE simulation results](vignettes/resources/results_rxode.png)

``` r
spaghettiPlot(mrgsolve, "CP")
```

![mrgsolve simulation results](vignettes/resources/results_mrgsolve.png)
