
# Libraries
#options(rgl.useNULL=TRUE)
#options(shiny.deprecation.messages=FALSE)

################################################################################

appDir <- file.path(path.package("ForestAnalysisInR", quiet=TRUE),"app")



###
shinyUI(fluidPage(
        
    navbarPage("TRYqa:  A Query and Analysis tool for the TRY Database",
               tabPanel("Trait Q&A",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("traitset", "Choose a trait set:",
                                            choices = c("Carbon Acquisition", 
                                                        "Nutrient Acquisition", 
                                                        "Water Acquisition", 
                                                        "Carbon, Light, Nutrient & Water Acquisition"), selected = "Carbon Acquisition",
                                            ),
                                
                                # # Input: Specify the number of observations to view ----
                                # numericInput("obs", "Number of observations to view:", 10),
                                
                                # Include clarifying text ----
                                helpText("Note: Output will be formatted for the TRY database."),
                            ),
                            mainPanel(
                                textOutput("result")
                            )
                        )
               ),
               
               tabPanel("Species Q&A",
                        textAreaInput("species", "Species list (seperated by commas)", rows = 6)
                        #verbatimTextOutput("summary")
               ),
               
               tabPanel("Analysis",
                        verbatimTextOutput("summary")
                                   
                        )
                )
       
    )
)


        
