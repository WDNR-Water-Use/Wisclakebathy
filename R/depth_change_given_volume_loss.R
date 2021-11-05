#' Calculate change in lake depth with X percent loss of lake volume
#'
#' Given a data frame with information about "WBIC", "depth_feet", and
#' "volume_acre_ft" and what percentage volume loss to assume, calculates the
#' estimated change in lake depth from the maximum lake depth/volume. Assumes a
#' linear change in depth and volume between specified contour intervals in
#' order to approximate lower lake depth.
#'
#' @param df data frame with information about "WBIC", "lakename", depth_feet",
#'           and "volume_acre_ft". Input data frame may have other columns, but
#'           must have these.
#' @param pcnt_loss Specify what percentage loss of lake volume from maximum
#'                  lake volume to evaluate. Defaults to 10 (percent).
#'
#' @return data frame with the following columns:
#' \item{WBIC}{Wisconsin Water Body Identification Code (WBIC) of lake}
#' \item{lakename}{name of lake as included in the input data frame. Typically
#'                 something like "easthorsehead", just to help with quick
#'                 identification)}
#' \item{max_vol}{maximum lake volume (acre-ft)}
#' \item{max_depth}{lake depth corresponding to maximum lake volume (ft)}
#' \item{lower_vol}{volume assuming X pcnt loss (as input, default is 10 pcnt
#'                  loss) (acre-ft)}
#' \item{lower_depth}{lake depth corresponding to lowered lake volume (ft)}
#' \item{depth_change}{change in lake depth corresponding to X pcnt loss (as
#'                     input, default is 10 pcnt loss) in lake volume (ft)}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats approx
#' @importFrom dplyr bind_rows
#'
#' @export

depth_change_given_volume_loss <- function(df, pcnt_loss = 10) {

  # Check that expected columns exist
  expected_cols <- c("WBIC", "lakename", "depth_feet", "volume_acre_ft")
  if (sum(!expected_cols %in% colnames(df)) > 0) {
    stop(sprintf("Unexpected names in df. Expected: (%s). Read: (%s).",
                 str_c(expected_cols, collapse = ", "),
                 str_c(colnames(df), collapse = ", ")))
  }

  # Calculate X pcnt loss in lake volume and corresponding lake depth
  reduction <- list()
  i <- 1
  for (WBIC in unique(df$WBIC)) {
    this_lake     <- df %>% filter(.data$WBIC == !!WBIC)
    lakename      <- unique(this_lake$lakename)
    max_vol       <- max(this_lake$volume_acre_ft)
    max_depth     <- this_lake$depth_feet[this_lake$volume_acre_ft == max_vol]
    lower_vol     <- ((100-pcnt_loss)/100)*max_vol
    lower_depth   <- approx(this_lake$volume_acre_ft,
                            this_lake$depth_feet,
                            lower_vol)$y
    depth_change <- abs(max_depth - lower_depth)

    reduction[[i]] <- data.frame(WBIC = WBIC,
                                 lakename = lakename,
                                 max_vol = max_vol,
                                 max_depth = max_depth,
                                 lower_vol = lower_vol,
                                 lower_depth = lower_depth,
                                 depth_change = depth_change)
    i <- i + 1
  }

  reduction <- bind_rows(reduction)

  return(reduction)
}
