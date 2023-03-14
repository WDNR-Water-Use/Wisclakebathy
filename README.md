# Wisclakebathy

This R-package was designed to help scientists at the Wisconsin DNR process and
analyze lake bathymetry data for Wisconsin lakes.

For examples of how to obtain lake bathymetry information and use these
functions to process and analyze lake bathymetry information, see the vignette
[WI_Lake_Bathy_Ex](https://github.com/WDNR-Water-Use/Wisclakebathy/blob/main/vignettes/WI_Lake_Bathy_Ex.Rmd).

For descriptions of the assumptions and behavior of individual functions, see
the [User
Manual](https://github.com/WDNR-Water-Use/Wisclakebathy/blob/main/Wisclakebathy_1.2.0.pdf).

## Installing WiscLakeBathy
To install from source and/or explore this R package:

  1. On Windows, it can help to install RTools first, from:
     https://cran.r-project.org/bin/windows/Rtools

  2. Make sure all currently installed packages are updated to the latest
     version. In RStudio: Packages > Update > Select All > Install Updates, or:
  ```
  update.packages(ask = FALSE)
  ```
  
  3. Install and load `devtools`.
  ```
  install.packages("devtools")
  library(devtools)
  ```  

  4. If you don't need to explore or update the source files, and just need the
     package, use `devtools` to install the package.  
  ```
  devtools::install_github("WDNR-Water-Use/Wisclakebathy", build_vignettes=T)
  ```
  
  5. If you do want to explore or update the source files, fork this repository 
     from github, clone or download ZIP, then open the `Wisclakebathy.Rprj` file 
     in R. Install from within this directory using: 
  ```
  devtools::install(build_vignettes=T)
  ```

  6. Note: to run the .Rmd files, you will likely want to install `extrafont`. 
     Note that `font_import()` will take several minutes to complete.
  ```
  install.packages("extrafont")
  library(extrafont)
  font_import()
  ```  
