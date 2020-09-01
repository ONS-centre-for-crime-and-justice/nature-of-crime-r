# Crime Tables Package

crime-tables-r is a Reproducible Analytical Pipeline (RAP) built in R for producing the Nature of Crime tables from the Crime Survey for England and Wales.

crime-tables-r uses gptables and the official guidance on good practice spreadsheets. It advocates a strong adherence to the guidance by restricting the range of operations possible.

crime-tables-r is developed and maintained by the Centre for Crime and Justice in the Office for National Statistics, UK.

For more information on this code please contact crimestatistics@ons.gov.uk

## Installation and use:


For development, clone the package from the ONS GitLab using:

`git clone http://np2rvlapxx507/Crime_Stats/crime-tables-r.git`

In R studio open the file `crime-tables-r.proj`. 

## Installation 

When opened in the R project `crime-tables-r.proj` the autotable package (in the directory autotable) needs to be set as the working directory to use build tools, using 

`setwd("./autotable")`

The package can be installed using the package `devtools` by running:

`devtools::install()`

> **_NOTE:_** RStudio exposes similar functionality in the Build menu and in the Build pane via the Install and Restart button.

You will need to build the documentation using `devtools:document()`.

# Reticulate

At the moment, the pipeline has examples of writing outputs to Excel using the R package xlsxwriter and the Python package gptables which is called from R using the reticulate package. Reticulate is the preferred option for the future.

## Dev

The pipeline uses a package called `autotable` which contains the functions used in the pipeline. This follows a standard R package layout. For more details read [R Packages](https://r-pkgs.org/index.html) 

## Testing

The pipeline can be tested using the package `testthat`. You can run the tests using: 

`devtools::test()`. 