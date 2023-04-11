MBUI <- tabPanel("MB",
                 h1("Mallows-Binomial (MB)"),
                 p("Description Text...."),
                 
                 tags$hr(),
                 
                 fluidRow(
                   column(
                     2,
                     radioButtons(
                       label = htmlOutput("EstimationWarningText"),
                       inputId = "MBEstimationMethod",
                       choices = c(
                         "Exact"= "exact",
                         "Approximate" = "approximate"
                       ),
                       selected = "approximate"
                     )
                     
                   ),
                   column(
                     2,
                     radioButtons(
                       label = htmlOutput("CIWarningText"),
                       inputId = "CI_Included",
                       choices = c(
                         "Yes" = "yes",
                         "No" = "no"
                       ),
                       selected = "no"
                     )
                   )
                 ),
                 actionButton("plot","Plot",width = "150px", height= "100px", style="color: white; background-color: green"),
                 shinycssloaders::withSpinner(
                   plotlyOutput("MallowsBinomial")
                 ),
                 downloadButton('downloadMB','Download Plot')
                 
           
                 
                 
                 
                 

                 
)
