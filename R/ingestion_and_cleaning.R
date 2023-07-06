#' This file containts ingestion and formatting scripts

#' Process single PCL transects.
#'
#' \code{read.try} imports raw TRY data into the R environment.
#'
#' This function imports raw TRY data
#'
#' \code{read.try} brings in the data
#'
#'
#' @param data.path the path to the raw TRY data to be inputed
#'
#' @return creates a data frame containing the columns "DatasetID", "SpeciesName",
#' "ObservationID", "ObsDataID", "TraitID", "DataID", "ValueKindName", "Replicates",
#' "StdValue", "StdUnit", "ErrorRisk"
#'
#' @keywords data input csv read
#' @export
#'
#'
#'
#' @examples
#'
#' # Run process complete PCL transect without storing to disk
#' data.path <- system.file("extdata", "try_sample_data.txt", package = "TRYqa")
#'
#' read.try(data.path)
#'
read.try <- function(data.path){
  # make note to automatically remove duplicates with OrigObsDataID
  # read in using vroom
  dataset <- vroom::vroom(data.path, delim = "\t", col_names = TRUE,
                          col_select = c(3, 5, 8, 9, 10, 12, 17, 20, 21, 22, 25), show_col_types = FALSE)

  # rename the columns
  colnames(dataset) <- c("DatasetID",
                         "SpeciesName",
                         "ObservationID",
                         "ObsDataID",
                         "TraitID",
                         "DataID",
                         "ValueKindName",
                         "Replicates",
                         "StdValue",
                         "StdUnit",
                         "ErrorRisk")

  # bring in additional information from TraitID
  df.traits <- utils::read.csv("./inst/extdata/TraitID_lookup_table.csv")

  # merge columns to give full names to column based on TraitID
  dataset <- merge(dataset, df.traits)

  return(dataset)
}


