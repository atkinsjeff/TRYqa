####
require(data.table)
require(magrittr)

# server.r
options(shiny.maxRequestSize= 1000*1024^2)
options(shiny.deprecation.messages=FALSE)

quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
}

#quiet(

# shinyServer(function(input,output){
#     # this cleans the data a bit
#     rfa.packages <- utils::read.csv("https://raw.githubusercontent.com/atkinsjeff/ForestAnalysisInR/main/inst/extdata/bibliometrics/list_of_packages.csv")
#     rfa.packages$Applications <- as.factor(rfa.packages$Applications)
#     rfa.packages$Link <- paste0("<a href='",rfa.packages$Link,"' target='_blank'>",rfa.packages$Link,"</a>")
#     
#     # adding
#     # choose columns to display
#     tab <- reactive({
#         rfa.packages %>%
#             dplyr::filter(Applications == input$var1)
#     })
#     output$table <- DT::renderDataTable({
#         tab()}, escape = FALSE, server = TRUE)
# })
#)
# brings in trait data
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

# Define server logic to summarize and view selected dataset ----
shinyServer(function(input, output, traits.carbon, traits.water, traits.nutrients, traits.c.l.n.w) {
    
    
    
        output$traitset <- renderText({
            input$traitset
        })
        
        output$summary <- renderPrint({
            summary(cars)
        })
        
        output$table <- DT::renderDataTable({
            DT::datatable(cars)
        })
    }
)








# val<-reactiveValues()
# val$txt<-""
# 
# observeEvent(input$var,{
#     new<-paste("You have selected", input$var)
#     val$txt<-paste( val$txt,new,sep='\n')
# })
# output$text <- renderText({
#     str1 <- val$txt
# })