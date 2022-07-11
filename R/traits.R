# try pipeline
require(data.table)
require(tidyverse)

# bring in traits
trait.list <- data.table::fread("./inst/extdata/TryTraits.txt", skip = 3, select = c("TraitID", "Trait"))


# Trait Families
vec.carbon <- c(3117,185,4,45,56,1809,12,146,151)
vec.water <- c(45,4,56,1080,3468,719)
vec.nutrients <- c(4,56,1080,1975,14,15)
vec.c.l.n.w <- c(3117,185,45,1809,12,146,151,4,56,1080,3468,719,1975,14,15)

# filter traits 
traits.carbon <- subset(trait.list, TraitID %in% vec.carbon)
traits.water <- subset(trait.list, TraitID %in% vec.water)
traits.nutrients <- subset(trait.list, TraitID %in% vec.nutrients)
traits.c.l.n.w <- subset(trait.list, TraitID %in% vec.c.l.n.w)


