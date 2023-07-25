#' This file containts ingestion and formatting scripts

#' Read in TRY data
#'
#' \code{read.try} imports raw TRY data into the R environment.
#'
#' This function imports raw TRY data
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

  df.traits <- read.csv(file.path("inst", "extdata","TraitID_lookup_table.csv"))

  # merge columns to give full names to column based on TraitID
  dataset <- merge(dataset, df.traits)

  return(dataset)
}





#' Create trait summaries
#'
#' \code{sum.traits} creates summary tables of traits.
#'
#' This trait summarizes things
#'
#' @param df a data frame of TRY data that has been read into the workspace with
#' the `read.try()` function
#' @param cwm a TRUE FALSE
#' @param weights a vector of weights for community weighted means. must be same length as species list
#' @param species a vectpr of species you'd like to look at
#' @param traits a vector of traits to look at
#'
#' @return creates a data frame contain the mean, median and standard deviation
#' by species for each trait included
#'
#' @keywords summary trait
#' @export
#'
#'
#'
sum.traits <- function(df, cwm, weights, species, traits){

  # variables
  species.list <- NULL
  trait.list <- NULL
  SpeciesName <- NULL
  TraitID <- NULL
  StdValue <- NULL
  MeanValWTD <- NULL

  # If missing values for community weights
  if(missing(cwm)){
    cwm == FALSE
  }

  # if missing species list
  if(missing(species)){
    message("You forgot a species list so this is going to give you every species in the data frame. You sure?")
    species.list = unique(df$SpeciesName)
  } else {
    species.list = species
  }

  # if missing trait list
  if(missing(traits)){
    message("You forgot a trait list so this is going to give you every trait in the data frame. You sure?")
    trait.list = unique(df$TraitID)
  } else {
    trait.list = traits
  }

  # if missing a vector of weights
  if(missing(weights)){
    weights = 1
  }

  # This section applies no weights for community means
  if(cwm == FALSE){
  # this gives you ever trait and species

    df %>%
      dplyr::filter(SpeciesName %in% species.list)%>%
      dplyr::filter(TraitID %in% trait.list) %>%
      dplyr::group_by(SpeciesName, TraitID) %>%
      dplyr::summarise(MeanVal = mean(StdValue, na.rm = TRUE),
                     MedVal = stats::median(StdValue, na.rm = TRUE),
                     SDVal = stats::sd(StdValue, na.rm = TRUE)) %>%
      data.frame()-> df.sum
  } else if (cwm == TRUE){

    df %>%
      dplyr::filter(SpeciesName %in% species.list)%>%
      dplyr::filter(TraitID %in% trait.list) %>%
      dplyr::group_by(SpeciesName, TraitID) %>%
      dplyr::summarise(MeanVal = mean(StdValue, na.rm = TRUE),
                       MedVal = stats::median(StdValue, na.rm = TRUE),
                       SDVal = stats::sd(StdValue, na.rm = TRUE)) -> df.sum


    # add the weights in
    cwm <- data.frame(species.list, weights)

    # change names
    cwm <- stats::setNames(cwm, c("SpeciesName", "Weight"))

    # combine with df.sum
    df.sum <- merge(df.sum, cwm, by = c("SpeciesName"))
    df.sum$MeanValWTD <- df.sum$MeanVal * df.sum$Weight

    df.sum %>%
      dplyr::group_by(TraitID) %>%
      dplyr::summarize(WeightedMean = sum(MeanValWTD, na.rm = TRUE)) %>%
      data.frame() -> df.sum
  }


  return(df.sum)
}

