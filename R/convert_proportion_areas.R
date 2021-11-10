#' Convert proportional area hypsography to areas in acres
#'
#' Given the path of a directory where csv files live, reads in information
#' about depth (ft) vs. proportional area relationship, maximum areas, and
#' converts to depth (ft) vs. area (acres) relationships.
#'
#' Expects csv files with depth vs. proportional area information to adhere to
#' the following conventions:
#' * All csv files are stored in the same directory (specified by "path")
#' * Filenames are "singlelakename_WBIC.csv", e.g. "easthorsehead_1523000.csv"
#' * Columns are (in order): "depth_ft", "proportion_area"
#' * A proportion area of "1" corresponds with a depth_ft of "0" and represents
#'  the maximum area as measured at the top of the lake.
#' * Every csv file following this convention should have a corresponding entry
#'   in the maximum lake area csv file.
#'
#' Expects that information about maximum lake areas (in acres) adheres to the
#' following conventions:
#'  * Information is in a single csv file (specified by "max_area_file") stored
#'    in the same directory as includes the individual csv files.
#'  * Columns are (in order): "WBIC" and "Area_acres"
#'  * Every lake with a proportional area csv file should have an entry here
#'
#' Note that the returned data frame lists the lake area (acres) corresponding
#' to lake depths such that 0ft is the lake bottom, 5ft is 5ft above the the
#' lake bottom, and the maximum depth value represents the surface of the lake.
#' If the parameter "flip" is set to FALSE, the returned data frame instead
#' retains the conventions in the proportional area csv files, where lake areas
#' correspond to lake depth contours such that 0ft is the lake surface, 5ft is
#' 5ft below the lake surface, and the maximum depth value represents the bottom
#' of the lake.
#'
#' @param path Path of directory containing all csv files with depth and
#'             proportional area info (extracted from bathymetry maps using
#'             imageJ), e.g. "data-raw/Wisconsin_Hypsography". See Details about
#'             expected format of these csv files.
#' @param max_area_file Name of csv file with information about maximum areas of
#'                      each lake, e.g. "WI_areas.csv". See Details about
#'                      expected format of this csv file.
#' @param flip Indicates whether should flip depths so that the maximum depth
#'             corresponds to the lake surface and a depth of zero corresponds
#'             to the lake bottom (TRUE) or keep as-is, with a depth of zero
#'             corresponding to the lake surface (FALSE). Defaults to TRUE to
#'             flip.
#'
#' @return a data frame with the following columns:
#' \item{WBIC}{Wisconsin Water Body Identification Code (WBIC) of lake}
#' \item{lake}{name of lake as included in csv filename (will be a single
#'             word, e.g. "easthorsehead", just to help with quick
#'             identification)}
#' \item{depth_ft}{lake depth (ft)}
#' \item{area_acres}{lake area at this lake depth (acres)}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr select bind_rows
#' @importFrom stringr str_c str_extract str_replace str_count
#' @importFrom utils read.csv
#'
#' @export

convert_proportion_areas <- function(path,
                                     max_area_file,
                                     flip = TRUE) {

  # Get filenames to read ------------------------------------------------------
  pattern   <- ".*[:digit:].*.csv" #assumes files are named "lakename_WBIC.csv"
  filenames <- list.files(path = path, pattern = pattern)

  # Load information about maximum area ----------------------------------------
  max_areas <- read.csv(sprintf("%s/%s", path, max_area_file))

  # Check that csv file is in expected format
  expected_cols <- c("WBIC", "Area_acres")
  if (sum(!expected_cols %in% colnames(max_areas)) > 0) {
    stop(sprintf("Unexpected names in %s. Expected: (%s). Read: (%s).",
                 max_area_file,
                 str_c(expected_cols, collapse = ", "),
                 str_c(colnames(max_areas), collapse = ", ")))
  }

  # Read in bathymetry, convert proportional area to area in acres -------------
  areas <- list()
  i <- 1
  for (filename in filenames) {
    # Read in proportional areas
    df <- read.csv(sprintf("%s/%s", path, filename))

    # Check that csv file is in expected format
    expected_cols <- c("depth_feet", "proportion_area")
    if (sum(!expected_cols %in% colnames(df)) > 0) {
      stop(sprintf("Unexpected names in %s. Expected: (%s). Read: (%s).",
                   filename,
                   str_c(expected_cols, collapse = ", "),
                   str_c(colnames(df), collapse = ", ")))
    } else if (str_count(filename, "\\_") != 1) {
      stop(sprintf("Unexpected format of csv file: Expected: singlelakename_WBIC.csv (exactly one underscore), but filename is: %s", filename))
    }

    # Unique lake identifying information
    WBIC <- str_extract(filename, "\\_.*") %>%
            str_replace("\\_", "") %>%
            str_replace("\\.csv", "") %>%
            as.numeric()
    lakename <- str_extract(filename, ".*\\_") %>%
                str_replace("\\_", "")

    # Convert to areas in acres
    max_area      <- max_areas$Area_acres[max_areas$WBIC == WBIC]
    if (length(max_area) > 1) {
      # Too many entries
      warning(sprintf("Multiple entries: %s has multiple entries in %s, cannot calculate areas in acres", filename, max_area_file))
    } else if (length(max_area) == 0) {
      # Not enough entries
      warning(sprintf("Missing entry: %s is missing entry in %s, cannot calculate areas in acres", filename, max_area_file))
    } else {
      # Just right - proceed!
      df$area_acres <- df$proportion_area*max_area
      df$WBIC       <- WBIC
      df$lake       <- lakename

      # If desired, flip depths so that:
      # maximum value indicates lake surface, 0 indicates lake bottom
      if (flip) {
        max_depth  <- max(df$depth_feet)
        df$depth_feet <- max_depth - df$depth_feet
      }

      # Save results
      areas[[i]] <- df %>%
                    select("WBIC", "lake", "depth_feet", "area_acres")
      i <- i + 1
    }
  }
  areas <- bind_rows(areas)

  return(areas)
}
