#' Convert depth vs. volume information obtained via DataThief into standard format
#'
#' Given the path of a directory where txt files from DataThief output live,
#' reads in information about depth (ft) vs. volume (acre-ft) relationship and
#' converts to standard format for other analyses.
#'
#' Expects txt files with depth vs. volume information to adhere to the
#' following conventions:
#' * All txt files are stored in the same directory (specified by "path")
#' * Filenames are "singlelakename_WBIC.csv", e.g. "easthorsehead_1523000.txt"
#' * Each file starts with a header from DataThief that should be deleted
#' * Columns are unnamed but represent (in order): "volume_acre_ft", "depth_feet"
#' * A depth/volume value of 0 represents the top of the lake, maximum values
#'   represent the bottom of hte lake.
#' * Every txt file following this convention should have a corresponding entry
#'   in the maximums txt file.
#'
#' Expects that information about maximum lake depth (ft) and lake volume
#' (acre-ft) adheres to the following conventions:
#'  * Information is in a single csv file (specified by "max_file") stored
#'    in the same directory as includes the individual txt files.
#'  * Every lake with a DataThief txt file should have an entry here
#'  * Columns include: "WBIC", "Depth_feet", and "Volume_acre_ft" (may include
#'    others as well, but unused here).
#'  * Missing values are denoted by NA (capitalized).
#'  * If there is a value for "Depth_feet", but "Volume_acre_ft" is NA,
#'    will use maximum depth plus the DataThief information to estimate what
#'    maximum volume is likely to be (and vice versa, if has maximum volume but
#'    maximum depth is NA).
#'
#' Note that the returned data frame lists the lake volume (acre-ft)
#' corresponding to lake depths such that 0ft is the lake bottom, 5ft is 5ft
#' above the the lake bottom, and the maximum depth value represents the surface
#' of the lake. If the parameter "flip" is set to FALSE, the returned data frame
#' instead retains the conventions in the DataThief txt files, where 0 values
#' for depth and volume represent the lake surface and maximum values represent
#' the bottom of the lake.
#'
#' @param path Path of directory containing all txt files with depth and
#'             volume info (extracted from bathymetry maps using
#'             DataThief), e.g. "data-raw/CS_DataThief". See details in
#'             description about expected format of these txt files.
#' @param max_file Name of csv file with information about maximum areas of
#'                 each lake, e.g. "WI_maxes.csv". See details in description
#'                 about expected format of this csv file.
#' @param flip Indicates whether should flip depths so that the maximum
#'             depth/volume corresponds to the lake surface and a depth/volume
#'             of zero corresponds to the lake bottom (TRUE) or keep as-is, with
#'             a depth of zero corresponding to the lake surface (FALSE).
#'             Defaults to TRUE to flip.
#' @param depth_interval optional parameter specifies what depth contour
#'                       interval to extrapolate volume information at. Defaults
#'                       to 3ft.
#'
#' @return a data frame with the following columns:
#' \item{WBIC}{Wisconsin Water Body Identification Code (WBIC) of lake}
#' \item{lake}{name of lake as included in csv filename (will be a single
#'             word, e.g. "easthorsehead", just to help with quick
#'             identification)}
#' \item{depth_feet}{lake depth (ft)}
#' \item{volume_acre_ft}{lake volume at this lake depth (acre-ft)}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr select bind_rows
#' @importFrom stringr str_c str_extract str_replace str_count
#' @importFrom utils read.csv
#' @importFrom Hmisc approxExtrap
#'
#' @export

convert_data_thief_volumes <- function(path,
                                       max_file,
                                       flip = TRUE,
                                       depth_interval = 3) {

  # Get filenames to read ------------------------------------------------------
  pattern   <- ".*_.*.txt" #assumes files are named "singlelakename_WBIC.txt"
  filenames <- list.files(path = path, pattern = pattern)

  # Load information about maximum area ----------------------------------------
  maxes <- read.csv(sprintf("%s/%s", path, max_file))

  # Check that csv file is in expected format
  expected_cols <- c("WBIC", "Volume_acre_ft", "Depth_feet")
  if (sum(!expected_cols %in% colnames(maxes)) > 0) {
    stop(sprintf("Unexpected names in %s. Expected: (%s). Read: (%s).",
                 max_file,
                 str_c(expected_cols, collapse = ", "),
                 str_c(colnames(maxes), collapse = ", ")))
  }

  # Read in bathymetry, convert proportional area to area in acres -------------
  volumes <- list()
  i <- 1
  for (filename in filenames) {
    # Read in proportional areas
    datathief <- read.csv(sprintf("%s/%s", path, filename),
                          skip = 1,
                          header = FALSE,
                          col.names = c("volume_acre_ft", "depth_feet"))

    # Unique lake identifying information
    WBIC <- str_extract(filename, "\\_.*") %>%
            str_replace("\\_", "") %>%
            str_replace("\\.txt", "") %>%
            as.numeric()
    lakename <- str_extract(filename, ".*\\_") %>%
                str_replace("\\_", "")

    # Convert to areas in acres
    max_vol   <- maxes$Volume_acre_ft[maxes$WBIC == WBIC]
    max_depth <- maxes$Depth_feet[maxes$WBIC == WBIC]
    if (length(max_vol) > 1 | length(max_depth) > 1) {
      # Too many entries
      warning(sprintf("Multiple entries: %s has multiple entries in %s",
                      filename, max_file))
    } else if (length(max_vol) == 0 | length(max_depth) == 0 |
               (is.na(max_vol) & is.na(max_depth))) {
      # Not enough entries
      warning(sprintf("Missing entry: %s is missing entry in %s",
                      filename, max_file))
    } else {
      # If missing max volume, guess from max_depth and vice versa
      if (is.na(max_vol) & !is.na(max_depth)) {
        max_vol <- approxExtrap(datathief$depth_feet,
                                datathief$volume_acre_ft,
                                max_depth,
                                ties = mean)$y
      } else if (!is.na(max_vol) & is.na(max_depth)) {
        max_depth <- approxExtrap(datathief$volume_acre_ft,
                                  datathief$depth_feet,
                                  max_vol,
                                  ties = mean)$y
      }
      # Just right - proceed!
      # QC data
      datathief <- datathief %>%
                   filter(.data$volume_acre_ft >= 0,
                          .data$depth_feet >= 0,
                          .data$volume_acre_ft <= max_vol,
                          .data$depth_feet <= max_depth) %>%
                   mutate(volume_acre_ft = round(.data$volume_acre_ft),
                          depth_feet = round(.data$depth_feet, 1))

      # Add in zero and max values
      add_df    <- data.frame(volume_acre_ft = c(0,max_vol),
                              depth_feet = c(0,max_depth))
      datathief <- rbind(datathief, add_df)

      # Interpolate at X ft contour intervals (default = 5ft)
      df <- data.frame(depth_feet = seq(0, max_depth, depth_interval))
      df$volume_acre_ft  <- approx(datathief$depth_feet,
                                   datathief$volume_acre_ft,
                                   df$depth_feet,
                                   ties = mean)$y
      df$volume_acre_ft[df$depth_feet == 0] <- 0 # Force 0 to 0
      if (!max_depth %in% df$depth_feet) {
        add_df <- data.frame(depth_feet = max_depth,
                             volume_acre_ft = max_vol)
        df <- rbind(df, add_df)
      }

      # If desired, flip depths and volumes so that:
      # maximum value indicates lake surface, 0 indicates lake bottom
      if (flip) {
        df$depth_feet     <- max_depth - df$depth_feet
        df$volume_acre_ft <- max_vol - df$volume_acre_ft
      }

      # Add identifying info for lake
      df$WBIC <- WBIC
      df$lake <- lakename

      # Save results
      volumes[[i]] <- df %>%
                    select("WBIC", "lake", "depth_feet", "volume_acre_ft")
      i <- i + 1
    }
  }
  volumes <- bind_rows(volumes)

  return(volumes)
}
