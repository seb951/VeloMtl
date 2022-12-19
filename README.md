## Overview  
* This is an interactive Shiny dashboard using biking statistics in Montreal, Canada. 


## Installation  
* you can install it as a regulat R package.   
* Data are actually pull from [here] (https://donnees.montreal.ca/ville-de-montreal/velos-comptage), but a local parsed copy already exists in the repo.
``` r
devtools::install_github(seb951/VeloMtl)
```

## Basic Usage  
``` r
library(golem)
golem::run_dev()
```


## Deployment  
``` r
golem::add_shinyappsio_file()
rsconnect::deployApp()
```

## Further Information   
* sebastien.renaut@gmail.com



 
