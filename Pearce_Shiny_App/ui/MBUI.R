MBUI <- tabPanel("MB",
                 h1("Mallows-Binomial (MB)"),
                 p("Description Text...."),
                 
                 tags$hr(),
                 
                 fluidRow(
                   column(
                     2,
                     radioButtons(
                       label = "Estimation Method",
                       inputId = "MBEstimationMethod",
                       choices = c(
                         "Exact"= "exact",
                         "Approximate" = "approximate"
                       ),
                       selected = "exact"
                     )
                     
                   ),
                   column(
                     2,
                     radioButtons(
                       label = "Include C.I.?",
                       inputId = "CI_Included",
                       choices = c(
                         "Yes" = "yes",
                         "No" = "no"
                       ),
                       selected = "yes"
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
