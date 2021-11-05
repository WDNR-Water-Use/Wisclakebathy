#' Calculate lake volumes (acre-ft) from lake areas (acres) and lake depths (ft)
#'
#' Given a data frame with information about "WBIC", "depth_feet", and
#' "area_acres" and which type of volume estimate to use ("trapezoidal" or
#' "conical"), calculates the lake volume in acre-ft associated with each lake
#' depth. Note that this approach assumes that information about "depth_feet" is
#' arranged such that the maximum depth value corresponds with the lake surface
#' and the maximum lake volume value, while a depth value of 0 corresponds with
#' the lake bottom and a lake volume of 0.
#'
#'
#' @param df data frame with information about "WBIC", "depth_feet", and
#'           "area_acres". Input data frame may have other columns, but must
#'           have these.
#' @param method Specify which volume estimation approach to use. Options
#'               include "trapezoidal" or "conical". Defaults to "trapezoidal".
#'
#' @return the input data frame with an additional column for "volume_acre_ft"
#'         appended, which represents the lake volume at the given lake depth
#'         (acre-ft).
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter arrange mutate select desc lead bind_rows all_of
#' @importFrom stringr str_c
#'
#' @export

calculate_volumes <- function(df,
                              method = "trapezoidal") {

  # Check that expected columns exist, with expected conventions for depth
  expected_cols <- c("WBIC", "depth_feet", "area_acres")
  if (sum(!expected_cols %in% colnames(df)) > 0) {
    stop(sprintf("Unexpected names in df. Expected: (%s). Read: (%s).",
                 str_c(expected_cols, collapse = ", "),
                 str_c(colnames(df), collapse = ", ")))
  }


  volumes <- list()
  i <- 1
  if (method == "trapezoidal") {
    for (WBIC in unique(df$WBIC)) {
      this_lake <- df %>%
                   filter(.data$WBIC == !!WBIC) %>%
                   arrange(desc(.data$depth_feet)) %>%
                   mutate(incr_depth = abs(.data$depth_feet -
                                             lead(.data$depth_feet)),
                          avg_area = 0.5*(.data$area_acres +
                                            lead(.data$area_acres)),
                          incr_vol = ifelse(.data$depth_feet == 0,
                                            0,
                                            .data$incr_depth*.data$avg_area)) %>%
                   arrange(.data$depth_feet) %>%
                   mutate(volume_acre_ft = cumsum(.data$incr_vol)) %>%
                   arrange(desc(.data$depth_feet))
      volumes[[i]] <- this_lake
      i <- i + 1
    }
  } else if (method == "conical") {
    for (WBIC in unique(df$WBIC)) {
      this_lake <- df %>%
                   filter(.data$WBIC == !!WBIC) %>%
                   arrange(desc(.data$depth_feet)) %>%
                   mutate(d = abs(.data$depth_feet - lead(.data$depth_feet)),
                          A1 = .data$area_acres,
                          A2 = lead(.data$area_acres),
                          incr_vol = ifelse(.data$depth_feet == 0,
                                            0,
                                            (.data$d/3)*(.data$A1 +
                                                           sqrt(.data$A1*.data$A2) +
                                                           .data$A2))) %>%
                   arrange(.data$depth_feet) %>%
                   mutate(volume_acre_ft = cumsum(.data$incr_vol)) %>%
                   arrange(desc(.data$depth_feet))
      volumes[[i]] <- this_lake
      i <- i + 1
    }
  }
  volumes <- bind_rows(volumes)
  volumes <- volumes %>%
             select(all_of(c(colnames(df), "volume_acre_ft")))
  return(volumes)
}
