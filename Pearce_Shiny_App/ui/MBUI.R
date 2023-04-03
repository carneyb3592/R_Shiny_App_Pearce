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
                   ),
                   column(
                     2,
                     checkboxGroupInput(
                       label = "Result Type",
                       inputId = "ResultType",
                       choices = c(
                         "MB Ranks" = "mb_ranks",
                         "MB vs Mean Ratings" = "mb_vs_mean",
                         "MB Parameters" = "mb_parameters"
                       )
                     )
                   ) 
                 ),
                 actionButton("plot","Plot",width = "150px", height= "100px", style="color: white; background-color: green"),
                 shinycssloaders::withSpinner(
                   plotOutput("MallowsBinomial")
                 )
           
                 
                 
                 
                 

                 
)
