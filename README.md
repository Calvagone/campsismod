
# pmxmod

Generic implementation of a drug model in pharmacometrics.

## Installation

Install the current development version:

``` r
remotes::install_github("Calvagone/pmxmod@dev")
```

## Some examples

### Load example from model library

First import the `pmxmod` package:

``` r
library(pmxmod)
```

Load 2-compartment PK model from built-in model library and show
content:

``` r
model <- getNONMEMModelTemplate(advan=4, trans=4)
show(model)
```

    ## An object of class "pmx_model"
    ## Slot "model":
    ## [PK]
    ## KA=THETA_1*exp(ETA_1)
    ## CL=THETA_2*exp(ETA_2)
    ## V2=THETA_3*exp(ETA_3)
    ## V3=THETA_4*exp(ETA_4)
    ## Q=THETA_5*exp(ETA_5)
    ## S2=V2
    ## [1] ""
    ## [DES]
    ## d/dt(A_DEPOT)=-KA*A_DEPOT
    ## d/dt(A_CENTRAL)=KA*A_DEPOT + Q*A_PERIPHERAL/V3 + (-CL/V2 - Q/V2)*A_CENTRAL
    ## d/dt(A_PERIPHERAL)=-Q*A_PERIPHERAL/V3 + Q*A_CENTRAL/V2
    ## d/dt(A_OUTPUT)=CL*A_CENTRAL/V2
    ## F=A_CENTRAL/S2
    ## [1] ""
    ## [ERROR]
    ## CP=F
    ## OBS_CP=CP*(EPS_1 + 1)
    ## Y=OBS_CP
    ## [1] ""
    ## 
    ## Slot "parameters":
    ## [1] "THETA's:"
    ##   name index value   fix
    ## 1 <NA>     1     1 FALSE
    ## 2 <NA>     2     5 FALSE
    ## 3 <NA>     3    80 FALSE
    ## 4 <NA>     4    20 FALSE
    ## 5 <NA>     5     4 FALSE
    ## [1] "OMEGA's:"
    ##   name index index2 value   fix type
    ## 1 <NA>     1      1 0.025 FALSE  var
    ## 2 <NA>     2      2 0.025 FALSE  var
    ## 3 <NA>     3      3 0.025 FALSE  var
    ## 4 <NA>     4      4 0.025 FALSE  var
    ## 5 <NA>     5      5 0.025 FALSE  var
    ## [1] "SIGMA's:"
    ##   name index index2 value   fix type
    ## 1 <NA>     1      1 0.025 FALSE  var

Show all methods than can be called on the parameters object:

``` r
methods(class=class(model@parameters))
```

    ##  [1] add         clean       contains    disable     fixOmega    getByIndex 
    ##  [7] getByName   getNames    indexOf     length      maxIndex    replace    
    ## [13] select      show        sort        standardise write      
    ## see '?methods' for accessing help and source code

Retrieve parameter by index:

``` r
theta <- model@parameters %>% getByIndex(Theta(index=1))
show(theta)
```

    ##   name index value   fix
    ## 1 <NA>     1     1 FALSE

``` r
omega <- model@parameters %>% getByIndex(Omega(index=2, index2=2))
show(omega)
```

    ##   name index index2 value   fix type
    ## 1 <NA>     2      2 0.025 FALSE  var

Retrieve parameter by name:

``` r
sigma <- model@parameters %>% getByName("SIGMA_1_1")
show(sigma)
```

    ##   name index index2 value   fix type
    ## 1 <NA>     1      1 0.025 FALSE  var

Select parameter by type:

``` r
thetas <- model@parameters %>% select("theta")
show(thetas)
```

    ## [1] "THETA's:"
    ##   name index value   fix
    ## 1 <NA>     1     1 FALSE
    ## 2 <NA>     2     5 FALSE
    ## 3 <NA>     3    80 FALSE
    ## 4 <NA>     4    20 FALSE
    ## 5 <NA>     5     4 FALSE
    ## [1] "OMEGA's:"
    ## # A tibble: 0 x 0
    ## [1] "SIGMA's:"
    ## # A tibble: 0 x 0

### Write PMX model

``` r
model %>% write(file="readme_tmp")
```

    ## [1] TRUE

``` r
list.files("readme_tmp")
```

    ## [1] "model.mod" "omega.csv" "sigma.csv" "theta.csv"

### Read PMX model

``` r
model <- read.pmxmod(file="readme_tmp")
show(model)
```

    ## An object of class "pmx_model"
    ## Slot "model":
    ## [PK]
    ## KA=THETA_1*exp(ETA_1)
    ## CL=THETA_2*exp(ETA_2)
    ## V2=THETA_3*exp(ETA_3)
    ## V3=THETA_4*exp(ETA_4)
    ## Q=THETA_5*exp(ETA_5)
    ## S2=V2
    ## [1] ""
    ## [DES]
    ## d/dt(A_DEPOT)=-KA*A_DEPOT
    ## d/dt(A_CENTRAL)=KA*A_DEPOT + Q*A_PERIPHERAL/V3 + (-CL/V2 - Q/V2)*A_CENTRAL
    ## d/dt(A_PERIPHERAL)=-Q*A_PERIPHERAL/V3 + Q*A_CENTRAL/V2
    ## d/dt(A_OUTPUT)=CL*A_CENTRAL/V2
    ## F=A_CENTRAL/S2
    ## [1] ""
    ## [ERROR]
    ## CP=F
    ## OBS_CP=CP*(EPS_1 + 1)
    ## Y=OBS_CP
    ## [1] ""
    ## 
    ## Slot "parameters":
    ## [1] "THETA's:"
    ##   name index value   fix
    ## 1 <NA>     1     1 FALSE
    ## 2 <NA>     2     5 FALSE
    ## 3 <NA>     3    80 FALSE
    ## 4 <NA>     4    20 FALSE
    ## 5 <NA>     5     4 FALSE
    ## [1] "OMEGA's:"
    ##   name index index2 value   fix type
    ## 1 <NA>     1      1 0.025 FALSE  var
    ## 2 <NA>     2      2 0.025 FALSE  var
    ## 3 <NA>     3      3 0.025 FALSE  var
    ## 4 <NA>     4      4 0.025 FALSE  var
    ## 5 <NA>     5      5 0.025 FALSE  var
    ## [1] "SIGMA's:"
    ##   name index index2 value   fix type
    ## 1 <NA>     1      1 0.025 FALSE  var

### Export PMX model to RxODE

``` r
rxmod <- model %>% export(dest="RxODE")
rxmod@theta
```

    ## THETA_1 THETA_2 THETA_3 THETA_4 THETA_5 
    ##       1       5      80      20       4

``` r
rxmod@omega
```

    ##       ETA_1 ETA_2 ETA_3 ETA_4 ETA_5
    ## ETA_1 0.025 0.000 0.000 0.000 0.000
    ## ETA_2 0.000 0.025 0.000 0.000 0.000
    ## ETA_3 0.000 0.000 0.025 0.000 0.000
    ## ETA_4 0.000 0.000 0.000 0.025 0.000
    ## ETA_5 0.000 0.000 0.000 0.000 0.025

``` r
rxmod@sigma
```

    ##       EPS_1
    ## EPS_1 0.025
