# ####
require(data.table)
require(magrittr)

# # server.r
# options(shiny.maxRequestSize= 1000*1024^2)
# options(shiny.deprecation.messages=FALSE)
# 
# # quiet <- function(x) {
# #     sink(tempfile())
# #     on.exit(sink())
# #     invisible(force(x))
# # }
# 
# #quiet(
# 
# # shinyServer(function(input,output){
# #     # this cleans the data a bit
# #     rfa.packages <- utils::read.csv("https://raw.githubusercontent.com/atkinsjeff/ForestAnalysisInR/main/inst/extdata/bibliometrics/list_of_packages.csv")
# #     rfa.packages$Applications <- as.factor(rfa.packages$Applications)
# #     rfa.packages$Link <- paste0("<a href='",rfa.packages$Link,"' target='_blank'>",rfa.packages$Link,"</a>")
# #     
# #     # adding
# #     # choose columns to display
# #     tab <- reactive({
# #         rfa.packages %>%
# #             dplyr::filter(Applications == input$var1)
# #     })
# #     output$table <- DT::renderDataTable({
# #         tab()}, escape = FALSE, server = TRUE)
# # })
# 
# # # brings in trait data
#  trait.list <- data.table::fread("https://raw.githubusercontent.com/atkinsjeff/TRYqa/main/inst/extdata/TryTraits.txt?token=GHSAT0AAAAAABQPL6KL7FY5CTNXK7LNJL5KYWMJNJA", skip = 3, select = c("TraitID", "Trait"))
# # 
# # 
# # bring in traits
# #trait.list <- data.table::fread("./inst/extdata/TryTraits.txt", skip = 3, select = c("TraitID", "Trait"))
# 
# 
# # Trait Families
# vec.carbon <- c(3117,185,4,45,56,1809,12,146,151)
# vec.water <- c(45,4,56,1080,3468,719)
# vec.nutrients <- c(4,56,1080,1975,14,15)
# vec.c.l.n.w <- c(3117,185,45,1809,12,146,151,4,56,1080,3468,719,1975,14,15)
# 
# # filter traits 
# traits.carbon <- as.data.frame(subset(trait.list, TraitID %in% vec.carbon))
# traits.water <- subset(trait.list, TraitID %in% vec.water)
# traits.nutrients <- subset(trait.list, TraitID %in% vec.nutrients)
# traits.c.l.n.w <- subset(trait.list, TraitID %in% vec.c.l.n.w)
# 
# traits.carbon$TraitFamily <- "Carbon Acquisition"
# traits.water$TraitFamily <- "Water Acquisition"
# traits.nutrients$TraitFamily <- "Nutrient Acquisition"
# traits.c.l.n.w$TraitFamily <- "Carbon, Light, Nutrient & Water Acquisition"
# 
# traits <- merge(traits.carbon, traits.water, traits.nutrients, traits.c.l.n.w)
# # 
traits <- data.frame(TraitFamily = c("Carbon Acquisition", "Water Acquisition","Nutrient Acquisition", "Carbon, Light, Nutrient & Water Acquisition"),
                        TraitIDs =  c("3117,185,4,45,56,1809,12,146,151", "45,4,56,1080,3468,719", "4,56,1080,1975,14,15",
                                      "3117,185,45,1809,12,146,151,4,56,1080,3468,719,1975,14,15"))
row.names(traits) <- traits[,1]
traits$TraitFamily <- NULL

 write.csv(traits, "./inst/extdata/TraitFamilies.csv", row.names = FALSE)

# Define server logic to summarize and view selected dataset ----
shinyServer(function(input, output) {
    
    
    # # bring in traits
    traits <- read.csv("https://raw.githubusercontent.com/atkinsjeff/TRYqa/main/inst/extdata/TraitFamilies.csv")
    
    # row.names(traits) <- traits[,1]
    # traits$TraitFamily <- NULL
    # # 
    # # Trait Families
    # vec.carbon <- c(3117,185,4,45,56,1809,12,146,151)
    # vec.water <- c(45,4,56,1080,3468,719)
    # vec.nutrients <- c(4,56,1080,1975,14,15)
    # vec.c.l.n.w <- c(3117,185,45,1809,12,146,151,4,56,1080,3468,719,1975,14,15)
    
    # # filter traits 
    # traits.carbon <- as.data.frame(subset(trait.list, TraitID %in% vec.carbon))
    # traits.water <- as.data.frame(subset(trait.list, TraitID %in% vec.water))
    # traits.nutrients <- as.data.frame(subset(trait.list, TraitID %in% vec.nutrients))
    # traits.c.l.n.w <- as.data.frame(subset(trait.list, TraitID %in% vec.c.l.n.w))
    # bring in traits

    
    # Trait Families

   
    
    # choose columns to display
    # tab <- reactive({
    #     traits %>%
    #         dplyr::filter(TraitFamily == input$traitset)
    # })
    # 

        
    # choose columns to display
        result <- reactive({
            traits %>%
                dplyr::filter(row.names(traits) == input$traitset) %>%
                select(TraitIDs)
        })
        output$result <- renderPrint(result()) 

        output$summary <- renderPrint({
            summary(cars)
        })
        
        output$table <- DT::renderDataTable({
            DT::datatable(cars)
        })
    }
)



