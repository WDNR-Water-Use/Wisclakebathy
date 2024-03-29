% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/depth_change_given_volume_loss.R
\name{depth_change_given_volume_loss}
\alias{depth_change_given_volume_loss}
\title{Calculate change in lake depth with X percent loss of lake volume}
\usage{
depth_change_given_volume_loss(df, pcnt_loss = 10)
}
\arguments{
\item{df}{data frame with information about "WBIC", "lake", depth_feet",
and "volume_acre_ft". Input data frame may have other columns, but
must have these.}

\item{pcnt_loss}{Specify what percentage loss of lake volume from maximum
lake volume to evaluate. Defaults to 10 (percent).}
}
\value{
data frame with the following columns:
\item{WBIC}{Wisconsin Water Body Identification Code (WBIC) of lake}
\item{lake}{name of lake as included in the input data frame. Typically
            something like "easthorsehead", just to help with quick
            identification)}
\item{max_vol}{maximum lake volume (acre-ft)}
\item{max_depth}{lake depth corresponding to maximum lake volume (ft)}
\item{lower_vol}{volume assuming X pcnt loss (as input, default is 10 pcnt
                 loss) (acre-ft)}
\item{lower_depth}{lake depth corresponding to lowered lake volume (ft)}
\item{depth_change}{change in lake depth corresponding to X pcnt loss (as
                    input, default is 10 pcnt loss) in lake volume (ft)}
}
\description{
Given a data frame with information about "WBIC", "depth_feet", and
"volume_acre_ft" and what percentage volume loss to assume, calculates the
estimated change in lake depth from the maximum lake depth/volume. Assumes a
linear change in depth and volume between specified contour intervals in
order to approximate lower lake depth.
}
