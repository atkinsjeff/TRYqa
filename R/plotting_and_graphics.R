# Contains plotting and formatting functions

### FIRST PLOT FUNCTION:  Community distribution



#' Read in TRY data
#'
#' \code{theme_try} provides a plotting theme for TRY data
#'
#' A plotting theme to be added to a `ggplot2` graph and base theme used in the
#' `TRYqa` package plotting functions
#'
#' @keywords plot theme
#' @export
#'
#'
theme_try <- function() {
  ggplot2::theme(
    # add border 1)
    panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 0.5),
    # color background 2)
    #panel.background = element_rect(fill = "white"),
    # modify grid 3)
    panel.grid.major.x = element_line(colour = "#333333", linetype = 3, size = 0.5),
    panel.grid.minor.x = element_line(colour = "darkgrey", linetype = 3, size = 0.5),
    panel.grid.major.y = element_line(colour = "#333333", linetype = 3, size = 0.5),
    panel.grid.minor.y = element_line(colour = "darkgrey", linetype = 3, size = 0.5),
    # modify text, axis and colour 4) and 5)
    axis.text = element_text(colour = "black", family = grDevices::windowsFont("TT Times New Roman")),
    axis.title = element_text(colour = "black", family = grDevices::windowsFont("TT Times New Roman")),
    axis.ticks = element_line(colour = "black"),
    axis.ticks.length=unit(-0.1, "cm"),
    # legend at the bottom 6)
    legend.position = "bottom",
    strip.text.x = element_text(size = 10, color = "black",  grDevices::windowsFont("TT Times New Roman")),
    strip.text.y = element_text(size = 10, color = "black",  grDevices::windowsFont("TT Times New Roman")),
    strip.background = element_blank()
  )
}



#' Plot species level trait data
#'
#' \code{plot.species} plots species data
#'
#' This function plots species data
#'
#' @param df a data frame of TRY data that has been read into the workspace with
#' the `read.try()` function
#'
#' @param trait the specific trait to plot by species
#'
#' @return a plot of species data
#'
#' @keywords plot graphics
#' @export
#'
#'
plot.species <- function(df, trait){

  # define variables
  TraitID <- NULL
  SpeciesName <- NULL
  StdValue <- NULL


  df %>%
    dplyr::filter(TraitID == trait) %>%
    data.frame() -> df.trait

  # make label
  trait.label <- unique(df.trait$Trait)
  axis.units <- paste("Measurement Units [", unique(df.trait$StdUnit),"]", sep = "")

  # add median line
  trait.median <- stats::median(df.trait$StdValue)
  # flipped boxplot

  ggplot2::ggplot(df.trait, ggplot2::aes(x = SpeciesName, y = StdValue))+
    ggplot2::geom_boxplot()+
    ggplot2::geom_jitter(width = 0.15, alpha = 0.5)+
    theme_try()+
    #scale_y_log10()+
    ggplot2::labs(x = "Species", y = axis.units)+
    ggplot2::theme(legend.position="none")+
    ggplot2::ggtitle(trait.label)+
    ggplot2::geom_hline(yintercept = trait.median, color = "red", linewidth = 1)+
    ggplot2::coord_flip()
}


#' Plot trait data by species
#'
#' \code{plot.species} plots species data
#'
#' This function plots species data
#'
#' @param df a data frame of TRY data that has been read into the workspace with
#' the `read.try()` function
#'
#' @param species.list the specific trait to plot by species
#' @param trait.list vector of traits to look at
#' @param group i don't remember what it does
#' @return a plot of species data
#'
#' @keywords plot graphics
#' @export
#'
#'
plot.traits <- function(df, species.list, trait.list, group){

  # define variables
  TraitID <- NULL
  SpeciesName <- NULL
  StdValue <- NULL

  ######## Need to update to include the OVERALL trait mean density on top of
  ######## species groups

  #creates empty part if no filename.
  if (missing(group)) {
    group = TRUE
  }

  #creates empty part if no filename.
  if (missing(species.list)) {
    species.list = unique(df$SpeciesName)
  }

  # filter to trait.list
  df %>%
    dplyr::filter(TraitID %in% trait.list) %>%
    data.frame() -> df.traits

  # filter to species.list
  df.traits %>%
    dplyr::filter(SpeciesName %in% species.list) %>%
    data.frame() -> df.traits

  print("You have selected the following traits:")
  print(unique(df.traits$Trait))


  # make label
  #trait.label <- unique(df.trait$Trait)
  #axis.units <- paste("Measurement Units [", unique(df.traits$StdUnit),"]", sep = "")

  # add median line
  #trait.median <- median(df.trait$StdValue)
  # now we make a panel plot
  # ggplot2::ggplot(df.traits, aes(x = StdValue, fill = SpeciesName))+
  #     ggplot2::geom_histogram()+
  #     ggplot2::xlab("")+
  #     #ggplot2::ggtitle(species)+
  #     ggplot2::facet_wrap(TraitID ~ ., scales = "free")
  if(group == TRUE){
    ggplot2::ggplot(df.traits, ggplot2::aes(x = StdValue, fill = SpeciesName))+
      ggplot2::geom_density(alpha = 0.666, color = "black")+
      ggplot2::xlab("")+
      try_theme()+
      ggplot2::labs(x = "Standard Measurement", y = "No. of Measurements")+
      ggplot2::facet_wrap(Trait ~ ., scales = "free")
  } else {
    ggplot2::ggplot(df.traits, ggplot2::aes(x = StdValue))+
      ggplot2::geom_density(alpha = 0.666, color = "black")+
      ggplot2::xlab("")+
      try_theme()+
      ggplot2::labs(x =  "Standard Measurement", y = "No. of Measurements")+
      ggplot2::facet_wrap(Trait ~ ., scales = "free")

  }
}
