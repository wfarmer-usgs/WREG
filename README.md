# WREG (Beta Release)

The WREG program can be used to develop a regional estimation equation for streamflow characteristics that can be applied at an ungaged basin, or to improve the corresponding estimate at continuous-record streamflow gages with short records. The regional estimation equation results from a multiple-linear regression that relates observable basin characteristics, such as drainage area, to streamflow characteristics. WREG allows use of three approaches to estimating regression parameters: ordinary-least-squares (OLS), weighted-least-squares (WLS), and generalized-least-squares (GLS). All three approaches are based on the minimization of the sum of squares of differences between the gage values and the line or surface defined by the regression. WREG can also be used to test region of influence (RoI) regression models.


## Installation of R and RStudio

This section should only need to be done once per computer.

The following link walks you through an installation of R and RStudio:

[Installation Instructions](https://owi.usgs.gov/R/training-curriculum/intro-curriculum/Before/)

If you follow those instructions exactly, you should have the USGS R repository (GRAN) added to your R profile. If that step doesn't ring a bell, paste the following into your R console:

```r
rprofile_path = file.path(Sys.getenv("HOME"), ".Rprofile")
write('\noptions(repos=c(getOption(\'repos\'),
    CRAN=\'https://cloud.r-project.org\',
    USGS=\'https://owi.usgs.gov/R\'))\n',
      rprofile_path, 
      append =  TRUE)

cat('Your Rprofile has been updated to include GRAN.
    Please restart R for changes to take effect.')
```

*RESTART RSTUDIO!*

Useful links:

* [Download R Windows](https://cran.r-project.org/bin/windows/base/)
* [Download R Mac](https://cran.r-project.org/bin/macosx/)
* [Download RStudio](https://www.rstudio.com/products/rstudio/download/)

## Installation of WREG

This section should also only have to be done once. It assumes the USGS R repository (GRAN) was added to your R profile as described above.

```r
install.packages("WREG")
```

Regularly, it is a good idea to update *ALL* your packages in R. If using RStudio, this is quite easy, there's an Update button in the "Packages" tab. This checks CRAN and GRAN for updates. It is a good idea to click this update regularly.

## Version Notes
Version 1: Implemented in Matlab with standalone executable.

Version 2: Implemented in R, with GUI interface and standalone executable.

Version 3.00: Removed GUI, thus depricating the standalone executable.

  Fixed bugs in importPeakFQ function and added function to import from 
  large, multi-site EXPs and PRTs.


Disclaimer
----------
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."




Windows Tests: [![Build status](https://ci.appveyor.com/api/projects/status/j83w5jf94nkb419s/branch/noGUI?svg=true)](https://ci.appveyor.com/project/wfarmer-usgs/wreg/branch/noGUI)

Linux Tests: [![Build Status](https://travis-ci.org/wfarmer-usgs/WREG.svg?branch=noGUI)](https://travis-ci.org/wfarmer-usgs/WREG)

Code Coverage: [![Coverage Status](https://coveralls.io/repos/github/wfarmer-usgs/WREG/badge.svg?branch=noGUI)](https://coveralls.io/github/wfarmer-usgs/WREG?branch=noGUI)