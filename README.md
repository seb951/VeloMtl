## Overview  
* This is an interactive Shiny dashboard using biking statistics in Montreal, Canada. 


## Installation  
* you can install it as a regular R package.   
* Data are actually pull from [here](https://donnees.montreal.ca/ville-de-montreal/velos-comptage), but a local parsed copy already exists in the repo.

``` r
devtools::install_github(seb951/VeloMtl)
```

## Basic Usage  
``` r
library(golem)
golem::run_dev()
```


## Deployment  
* You can deploy from your local machine to a server (e.g. shinyapps.io, assuming you have an account). However, this app is automatically deployed from its [github repository](https://github.com/seb951/VeloMtl) on a `PUSH` to `main`.

``` r
golem::add_shinyappsio_file()
rsconnect::deployApp()
```

## Further Information   
* sebastien.renaut@gmail.com



 
