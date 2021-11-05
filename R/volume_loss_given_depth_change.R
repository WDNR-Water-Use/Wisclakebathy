#' Calculate percent loss of lake volume with Xft change in lake depth
#'
#' Given a data frame with information about "WBIC", "depth_feet", and
#' "volume_acre_ft" and what reduction in max depth (ft) to assume, calculates
#' the estimated change in lake volume (%) from the maximum lake depth/volume.
#' Assumes a linear change in depth and volume between specified contour
#' intervals in order to approximate lower lake volume.
#'
#'
#' @param df data frame with information about "WBIC", "lakename", depth_feet",
#'           and "volume_acre_ft". Input data frame may have other columns, but
#'           must have these.
#' @param depth_change_ft Specify what reduction in lake depth from maximum
#'                        lake depth to evaluate. Defaults to 1 (ft).
#'
#' @return data frame with the following columns:
#' \item{WBIC}{Wisconsin Water Body Identification Code (WBIC) of lake}
#' \item{lakename}{name of lake as included in the input data frame. Typically
#'                 something like "easthorsehead", just to help with quick
#'                 identification)}
#' \item{max_depth}{maximum lake depth (ft)}
#' \item{max_vol}{lake volume corresponding to macimum lake depth (acre-ft)}
#' \item{lower_depth}{lake depth assuming Xft reduction (as input, default is
#'                    1ft reduction) (ft)}
#' \item{lower_vol}{volume corresponding to lowered depth (acre-ft)}
#' \item{vol_pcnt_loss}{percentage change in lake volume relative to maximum
#'                      lake volume given a Xft reduction in lake depth (as
#'                      input, default is 1ft reduction)}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats approx
#' @importFrom dplyr bind_rows
#'
#' @export

volume_loss_given_depth_change <- function(df, depth_change_ft = 1) {

  # Check that expected columns exist
  expected_cols <- c("WBIC", "lakename", "depth_feet", "volume_acre_ft")
  if (sum(!expected_cols %in% colnames(df)) > 0) {
    stop(sprintf("Unexpected names in df. Expected: (%s). Read: (%s).",
                 str_c(expected_cols, collapse = ", "),
                 str_c(colnames(df), collapse = ", ")))
  }

  # Calculate percent loss in lake volume given Xft change in max depth
  reduction <- list()
  i <- 1
  for (WBIC in unique(df$WBIC)) {
    this_lake     <- df %>% filter(.data$WBIC == !!WBIC)
    lakename      <- unique(this_lake$lakename)
    max_depth     <- max(this_lake$depth_feet)
    max_vol       <- this_lake$volume_acre_ft[this_lake$depth_feet == max_depth]
    lower_depth   <- max_depth - depth_change_ft
    lower_vol     <- approx(this_lake$depth_feet,
                            this_lake$volume_acre_ft,
                            lower_depth)$y
    vol_pcnt_loss <- 100*(max_vol - lower_vol)/max_vol

    reduction[[i]] <- data.frame(WBIC = WBIC,
                                 lakename = lakename,
                                 max_depth = max_depth,
                                 max_vol = max_vol,
                                 lower_depth = lower_depth,
                                 lower_vol = lower_vol,
                                 vol_pcnt_loss = vol_pcnt_loss)
    i <- i + 1
  }

  reduction <- bind_rows(reduction)

  return(reduction)
}
