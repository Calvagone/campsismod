# campsismod 1.2.1
* Export from replicated Campsis model too slow, revise implementation #99
* OMEGA/SIGMA matrix export too slow, revise implementation #100

# campsismod 1.2.0

* Add min/max columns to THETA's #24
* Method 'addSuffix' does not preserve the variance-covariance matrix #81
* Method 'add' does not merge variance-covariance matrices #82
* Method 'addRSE' to quickly add relative standard error on parameter #83
* Implement generic method 'move' #84
* Generate parameter uncertainty in campsismod rather than in campsis #85
* Error when printing the model in the console #88
* Allow manual import of sampled parameters for model replication #89
* Get rid of plyr package #90
* Quality: increase code coverage #91
* Method 'addRSE' can't be used to replace an existing value #92
* Issue when the model is replicated only once #93
* Implement show method on replication setting object #95
* Check OMEGA and SIGMA matrix for positive definiteness #96
* Min/max values not loaded from Campsis model files #97

# campsismod 1.1.2

* Code coverage does not appear on Codecov #77
* Accept dest='rxode2' in addition to 'RxODE' #78
* Update pkgdown documentation #79

# campsismod 1.1.1

* Function 'replaceAll' not replacing occurrences in compartment properties #74

# campsismod 1.1.0

* Revise model suite #66
* Add label, unit and comment fields to parameters #65

# campsismod 1.0.0

* New method 'addSuffix' to combine several models #61
* Code review: extra parameters to mrgsolve model #62

# campsismod 0.9.1

* Regenerate model suite #59
* Read model equations from character vector #58

# campsismod 0.9.0

* New suite of models (> 100 model templates of all kinds: PK, PD, TMDD, etc.) #54
* Standard error and relative standard error on parameters #53
* Improved code coverage #55

# campsismod 0.8.1

* Harmonise simulation time in RxODE and mrgsolve #22
* Easy insertion of equations into the model at specific position #49

# campsismod 0.8.0

* Added a `NEWS.md` file to track changes to the package.
* Initial release of campsismod on CRAN
