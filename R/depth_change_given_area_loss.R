#' Calculate change in lake depth with X percent loss of lake area
#'
#' Given a data frame with information about "WBIC", "depth_feet", and
#' "area_acres" and what percentage area loss to assume, calculates the
#' estimated change in lake depth from the maximum lake depth/area Assumes a
#' linear change in depth and area between specified contour intervals in
#' order to approximate lower lake depth.
#'
#' @param df data frame with information about "WBIC", "lake", depth_feet",
#'           and "area_acres". Input data frame may have other columns, but
#'           must have these.
#' @param pcnt_loss Specify what percentage loss of lake area from maximum
#'                  lake area to evaluate. Defaults to 10 (percent).
#'
#' @return data frame with the following columns:
#' \item{WBIC}{Wisconsin Water Body Identification Code (WBIC) of lake}
#' \item{lake}{name of lake as included in the input data frame. Typically
#'             something like "easthorsehead", just to help with quick
#'             identification)}
#' \item{max_area}{maximum lake area (acres)}
#' \item{max_depth}{lake depth corresponding to maximum lake area (ft)}
#' \item{lower_area}{area assuming X pcnt loss (as input, default is 10 pcnt
#'                  loss) (acres)}
#' \item{lower_depth}{lake depth corresponding to lowered lake area (ft)}
#' \item{depth_change}{change in lake depth corresponding to X pcnt loss (as
#'                     input, default is 10 pcnt loss) in lake area (ft)}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats approx
#' @importFrom dplyr bind_rows
#'
#' @export

depth_change_given_area_loss <- function(df, pcnt_loss = 10) {

  # Check that expected columns exist
  expected_cols <- c("WBIC", "lake", "depth_feet", "area_acres")
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
    lake          <- unique(this_lake$lake)
    max_area      <- max(this_lake$area_acres)
    max_depth     <- this_lake$depth_feet[this_lake$area_acres == max_area]
    lower_area    <- ((100-pcnt_loss)/100)*max_area
    lower_depth   <- approx(this_lake$area_acres,
                            this_lake$depth_feet,
                            lower_area)$y
    depth_change <- abs(max_depth - lower_depth)

    reduction[[i]] <- data.frame(WBIC = WBIC,
                                 lake = lake,
                                 max_area = max_area,
                                 max_depth = max_depth,
                                 lower_area = lower_area,
                                 lower_depth = lower_depth,
                                 depth_change = depth_change)
    i <- i + 1
  }

  reduction <- bind_rows(reduction)

  return(reduction)
}
