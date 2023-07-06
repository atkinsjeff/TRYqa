# analysis scripts
data.path <- system.file("./inst/extdata/try_sample_data.txt")
# import data
df <- read.try("./inst/extdata/try_sample_data.txt")




# # make summary
# df.sum <- sum.traits(df)
trait.list <- c(3117, 185, 4, 45, 56, 14, 151, 55)
species.list <- unique(df$SpeciesName)
weights <- c(0.1, 0.2, 0.3, 0.4, 0.1)

species.list.short <- species.list[1:5]


sum.traits(df, cwm = FALSE, species = species.list[1:5], traits = trait.list, weights = weights)

sum.traits(df, cwm = TRUE,  weights = weights)


x11()
plot.species(df, trait = 14)

#### Need to 1) make a summary table of means, medians, sd,.....
sum.traits <- function(df){

  #### 3) make community weighted means based on an input weights. (0 to 1 for each species)
  # if(missing(weights)){
  # next()
  #}

  # make table
  df %>%
    dplyr::group_by(SpeciesName, TraitID) %>%
    dplyr::summarise(MeanVal = mean(StdValue, na.rm = TRUE),
                     MedVal = median(StdValue, na.rm = TRUE),
                     SDVal = sd(StdValue, na.rm = TRUE)) %>%
    data.frame()-> df.sum
  return(df.sum)
}

#### then 2) make a covariance table by trait and make it plot!
cov.traits <- function(df, type, trait.list){

  # type of math
  if (type == 1) {

    # need to input data from df.sums, traits in a list format
    df %>%
      dplyr::select(SpeciesName, TraitID, MeanVal) %>%
      dplyr::filter(TraitID %in% trait.list) %>%
      data.frame() -> df2


  } else {
    df %>%
      dplyr::select(SpeciesName, TraitID, MedVal) %>%
      dplyr::filter(TraitID %in% trait.list) %>%
      data.frame() -> df2
  }
  # need to input data from df.sums, traits in a list format

  return(df2)

}




# making community weight means
species <- species.list.short
weights <- c(0.1, 0.2, 0.3, 0.4, 0.1)

cwm <- data.frame(species, weights)

df %>%
  dplyr::filter(SpeciesName %in% species.list.short)%>%
  dplyr::filter(TraitID %in% trait.list) %>%
  dplyr::group_by(SpeciesName, TraitID) %>%
  dplyr::summarise(MeanVal = mean(StdValue, na.rm = TRUE),
                   MedVal = median(StdValue, na.rm = TRUE),
                   SDVal = sd(StdValue, na.rm = TRUE)) %>%
  data.frame()-> df.sum

df.cwm <- merge(df.sum, cwm)
df.cwm$MeanValWTD <- df.cwm$MeanVal * df.cwm$weights

df.cwm %>%
  dplyr::group_by(TraitID) %>%
  dplyr::summarize(WeightedMean = sum(MeanValWTD, na.rm = TRUE)) %>%
  data.frame() -> cw.means
# need to make weighted variance





### FUNCTIONS PAGE
require(tidyverse)
require(ggplot2)

##### TO DO
# 1 - fix the correlation matrix. probably needs an input trait list and will
#     likely kick out with any NA's so make sure to add na.omit() in the function
# 2 - figure out some what to do covariance
# 3 - community weighted means
# 4 - community weighted variance
# 5 - function to fit distributions and give stats on fits.
# 6 - make sure all functions work with rtry import as well





### READ IN DATA
# read in try data
# need to add in a short option that only gives the TraitID, SpeciesName and STDValue, right now
# it also gives the data
read.TRY <- function(data.path){
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
  df.traits <- read.csv("./inst/extdata/TraitID_lookup_table.csv")

  # merge columns to give full names to column based on TraitID
  dataset <- merge(dataset, df.traits)

  return(dataset)
}



### FIRST PLOT FUNCTION:  Community distribution


##### CUSTOM PLOT THEME
try_theme <- function() {
  theme(
    # add border 1)
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    # color background 2)
    #panel.background = element_rect(fill = "white"),
    # modify grid 3)
    panel.grid.major.x = element_line(colour = "#333333", linetype = 3, size = 0.5),
    panel.grid.minor.x = element_line(colour = "darkgrey", linetype = 3, size = 0.5),
    panel.grid.major.y = element_line(colour = "#333333", linetype = 3, size = 0.5),
    panel.grid.minor.y = element_line(colour = "darkgrey", linetype = 3, size = 0.5),
    # modify text, axis and colour 4) and 5)
    axis.text = element_text(colour = "black", family = "Times New Roman"),
    axis.title = element_text(colour = "black", family = "Times New Roman"),
    axis.ticks = element_line(colour = "black"),
    axis.ticks.length=unit(-0.1, "cm"),
    # legend at the bottom 6)
    legend.position = "bottom",
    strip.text.x = element_text(size=10, color="black",  family = "Times New Roman"),
    strip.text.y = element_text(size=10, color="black",  family = "Times New Roman"),
    strip.background = element_blank()
  )
}




#### SPECIES LEVEL TRAIT DISTRIBUTION
plot.species <- function(df, trait){

  df %>%
    dplyr::filter(TraitID == trait) %>%
    data.frame() -> df.trait

  # make label
  trait.label <- unique(df.trait$Trait)
  axis.units <- paste("Measurement Units [", unique(df.trait$StdUnit),"]", sep = "")

  # add median line
  trait.median <- median(df.trait$StdValue)
  # flipped boxplot
  x11()
  ggplot(df.trait, aes(x = SpeciesName, y = StdValue))+
    geom_boxplot()+
    geom_jitter(width=0.15, alpha=0.5)+
    theme_bw()+
    #scale_y_log10()+
    labs(x = "Species", y = axis.units)+
    theme(legend.position="none")+
    ggtitle(trait.label)+
    geom_hline(yintercept = trait.median, color = "red", size = 1)+
    coord_flip()
}


#### SPECIES LEVEL TRAIT DISTRIBUTION
plot.traits <- function(df, species.list, trait.list, group){

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
    ggplot2::ggplot(df.traits, aes(x = StdValue, fill = SpeciesName))+
      ggplot2::geom_density(alpha = 0.666, color = "black")+
      ggplot2::xlab("")+
      try_theme()+
      labs(x = "Standard Measurement", y = "No. of Measurements")+
      ggplot2::facet_wrap(Trait ~ ., scales = "free")
  } else {
    ggplot2::ggplot(df.traits, aes(x = StdValue))+
      ggplot2::geom_density(alpha = 0.666, color = "black")+
      ggplot2::xlab("")+
      try_theme()+
      labs(x =  "Standard Measurement", y = "No. of Measurements")+
      ggplot2::facet_wrap(Trait ~ ., scales = "free")

  }
}
#### Need to 1) make a summary table of means, medians, sd,.....
sum.traits <- function(df){

  #### 3) make community weighted means based on an input weights. (0 to 1 for each species)
  # if(missing(weights)){
  # next()
  #}

  # make table
  df %>%
    dplyr::group_by(SpeciesName, TraitID) %>%
    dplyr::summarise(MeanVal = mean(StdValue, na.rm = TRUE),
                     MedVal = median(StdValue, na.rm = TRUE),
                     SDVal = sd(StdValue, na.rm = TRUE)) %>%
    data.frame()-> df.sum
  return(df.sum)
}

#### then 2) make a covariance table by trait and make it plot!
cov.traits <- function(df, type, trait.list){

  # type of math
  if (type == 1) {

    # need to input data from df.sums, traits in a list format
    df %>%
      dplyr::select(SpeciesName, TraitID, MeanVal) %>%
      dplyr::filter(TraitID %in% trait.list) %>%
      data.frame() -> df2


  } else {
    df %>%
      dplyr::select(SpeciesName, TraitID, MedVal) %>%
      dplyr::filter(TraitID %in% trait.list) %>%
      data.frame() -> df2
  }
  # need to input data from df.sums, traits in a list format

  return(df2)

}

trait.list <- c(3117, 185, 4, 45, 56, 1809, 12, 146, 151)
cov.traits(df.sum, type = 2, trait.list)

x11()
plot(df.sum)

cov(df.sum)

#
m <- spread(df.sum[ , c(1:3)], key = TraitID, value = MeanVal, drop = TRUE)

m.cor <- cor(m[2:24])

require(corrplot)

x11()
corrplot(m.cor)









#### TEST RUN
# define data.path
data.path <- "./inst/extdata/12258.txt"

# bring in data
df <- read.TRY(data.path)
x <- rtry::rtry_import(data.path)


x11()
plot.species(df, trait = 14)


# # make summary
# df.sum <- sum.traits(df)
trait.list <- c(3117, 185, 4, 45, 56, 14, 151, 55)
species.list <- unique(df$SpeciesName)

species.list.short <- species.list[1:5]

# making community weight means
species <- species.list.short
weights <- c(0.1, 0.2, 0.3, 0.4, 0.1)

cwm <- data.frame(species, weights)

df %>%
  dplyr::filter(SpeciesName %in% species.list.short)%>%
  dplyr::filter(TraitID %in% trait.list) %>%
  dplyr::group_by(SpeciesName, TraitID) %>%
  dplyr::summarise(MeanVal = mean(StdValue, na.rm = TRUE),
                   MedVal = median(StdValue, na.rm = TRUE),
                   SDVal = sd(StdValue, na.rm = TRUE)) %>%
  data.frame()-> df.sum

df.cwm <- merge(df.sum, cwm)
df.cwm$MeanValWTD <- df.cwm$MeanVal * df.cwm$weights

df.cwm %>%
  dplyr::group_by(TraitID) %>%
  dplyr::summarize(WeightedMean = sum(MeanValWTD, na.rm = TRUE)) %>%
  data.frame() -> cw.means
# need to make weighted variance




x11()
plot.traits(df, trait.list = trait.list, species.list = species.list.short, group = TRUE)














####
require(corrplot)
require(PerformanceAnalytics)

# COVARIANCE
data <- data.frame(Appearance = c(8, 8, 8, 9, 7, 9, NA, 7, 8, 9),
                   Thickness = c(8, 8, 7, 7, NA, 8, 9, 8, 7, 9),
                   Spredability= c(7, 9, 9, 9, 8, 8, 7, 8, NA, 7))

data <- na.omit(data)
m <- cor(data)
corrplot(m)

x11()
chart.Correlation(m, histogram=TRUE, pch=19)


# correlation matrix sandbox
df.sums <- sum.traits(df)


# long form correlation matrix
df %>%
  filter(TraitID %in% trait.list) %>%
  data.frame() -> df2

df2$TraitID <- as.factor(df2$TraitID)
df2 %>%
  #filter(SpeciesName == "Acer rubrum") %>%
  filter(TraitID %in% trait.list) %>%
  select(SpeciesName, TraitID, StdValue) %>%
  data.frame() -> df3

df3 %>%
  #na.omit() %>%
  pivot_wider(names_from =TraitID, values_from = StdValue, values_fn = ~mean(.x, na.rm = TRUE)) %>%
  data.frame()-> df4
str(df)

df5 <- na.omit(df4[2:8])
m <- cor(df5)
corrplot(m)

x11()
chart.Correlation(m, histogram=TRUE, pch=19)

x11()
corrplot(m)


df2 %>%
  filter(SpeciesName == "Acer rubrum") %>%
  #rownames_to_column( "Value") %>%
  filter(TraitID %in% trait.list) %>%
  select(TraitID, StdValue) %>%
  data.frame() -> df3

df3 %>%
  #na.omit() %>%
  pivot_wider(names_from =TraitID, values_from = StdValue, values_fill = NULL) %>%
  data.frame()-> df4
str(df)









####### USDA Taxon Species Lookup Code


taxa <- read.csv("./inst/extdata/plantlst.txt")

















