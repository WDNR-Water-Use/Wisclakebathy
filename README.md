# Wisclakebathy

This R-package was designed to help scientists at the Wisconsin DNR process and
analyze lake bathymetry data for Wisconsin lakes. For many Wisconsin lakes, pdf
maps of lake bathymetry are available [on the WDNR
website](https://dnr.wi.gov/lakes/maps/) and it is possible to use the approach
of [Rounds et al. (2021)](https://conservancy.umn.edu/handle/11299/216182) to
digitize the proportional area associated with depth contour intervals. This
package is designed to convert these proportional areas to areas in acres,
estimate lake volume at each contour interval, and perform simple analyses on
how depth and volume are related at any given lake.

For an example of how to use these functions, see the vignette
["WI_Hypsography_Ex"](https://github.com/WDNR-Water-Use/Wisclakebathy/blob/main/vignettes/WI_Hypsography_Ex.Rmd).

For descriptions of the assumptions and behavior of individual functions, see
the [User Manual](https://github.com/WDNR-Water-Use/Wisclakebathy/blob/main/Wisclakebathy_1.1.0.pdf).
