
<!-- badges: start -->

[![R-CMD-check](https://github.com/Calvagone/campsismod/workflows/R-CMD-check/badge.svg)](https://github.com/Calvagone/campsismod/actions)
[![codecov](https://codecov.io/gh/Calvagone/campsismod/branch/main/graph/badge.svg?token=7DHBRQD7AG)](https://codecov.io/gh/Calvagone/campsismod)
<!-- badges: end -->

# campsismod

Generic implementation of a drug model in pharmacometrics.

## Installation

Install the latest stable release with the authentication token you have
received:

``` r
devtools::install_github("Calvagone/campsismod", ref="main", auth_token="AUTH_TOKEN", force=TRUE)
```

## Some examples

### Load example from model library

First import the `campsismod` package:

``` r
library(campsismod)
```

Load 2-compartment MAIN model from built-in model library and show
content:

``` r
model <- getNONMEMModelTemplate(advan=4, trans=4)
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

Show all methods than can be called on the parameters object:

``` r
methods(class=class(model@parameters))
```

    ##  [1] add         clean       contains    disable     fixOmega    getByIndex 
    ##  [7] getByName   getNames    indexOf     length      maxIndex    minIndex   
    ## [13] replace     select      show        sort        standardise write      
    ## see '?methods' for accessing help and source code

Retrieve parameter by index:

``` r
theta <- model@parameters %>% getByIndex(Theta(index=1))
show(theta)
```

    ##   name index value   fix
    ## 1   KA     1     1 FALSE

``` r
omega <- model@parameters %>% getByIndex(Omega(index=2, index2=2))
show(omega)
```

    ##   name index index2 value   fix type same
    ## 1   CL     2      2 0.025 FALSE  var   NA

Retrieve parameter by name:

``` r
theta <- model@parameters %>% getByName("THETA_KA")
show(theta)
```

    ##   name index value   fix
    ## 1   KA     1     1 FALSE

Select parameters by type:

``` r
thetas <- model@parameters %>% select("theta")
show(thetas)
```

    ## THETA's:
    ##   name index value   fix
    ## 1   KA     1     1 FALSE
    ## 2   CL     2     5 FALSE
    ## 3   V2     3    80 FALSE
    ## 4   V3     4    20 FALSE
    ## 5    Q     5     4 FALSE
    ## OMEGA's:
    ## # A tibble: 0 x 0
    ## SIGMA's:
    ## # A tibble: 0 x 0
    ## No variance-covariance matrix

### Write PMX model

``` r
model %>% write(file="readme_tmp")
```

    ## [1] TRUE

``` r
list.files("readme_tmp")
```

    ## [1] "model.pmx" "omega.csv" "sigma.csv" "theta.csv"

### Read PMX model

``` r
model <- read.pmxmod(file="readme_tmp")
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

### Export PMX model to RxODE

``` r
rxmod <- model %>% export(dest="RxODE")
rxmod@theta
```

    ## THETA_KA THETA_CL THETA_V2 THETA_V3  THETA_Q 
    ##        1        5       80       20        4

``` r
rxmod@omega
```

    ##        ETA_KA ETA_CL ETA_V2 ETA_V3 ETA_Q
    ## ETA_KA  0.025  0.000  0.000  0.000 0.000
    ## ETA_CL  0.000  0.025  0.000  0.000 0.000
    ## ETA_V2  0.000  0.000  0.025  0.000 0.000
    ## ETA_V3  0.000  0.000  0.000  0.025 0.000
    ## ETA_Q   0.000  0.000  0.000  0.000 0.025

``` r
rxmod@sigma
```

    ##          EPS_PROP
    ## EPS_PROP    0.025
