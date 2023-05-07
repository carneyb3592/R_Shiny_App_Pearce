MBUI <- tabPanel("MB",
                 h1("Mallows-Binomial"),
                 p("This page allows for fitting a Mallows-Binomial (MB) model to rankings and ratings preference
                 data. Select how you would like to fit the model below using the radio buttons below, and then click 
                 'Plot' to begin estimation. After model fitting, results are visualized via three plots."),

                 tags$hr(),
                verticalLayout(
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
                        selected = "approximate"
                      )
                      
                    ),
                    column(
                      4,
                      htmlOutput("EstimationWarningText")
                    )
                  
                ),
                 fluidRow(
                   column(
                     2,
                     radioButtons(
                       label = "Include C.I.?",
                       inputId = "CI_Included",
                       choices = c(
                         "Yes" = "yes",
                         "No" = "no"
                       ),
                       selected = "no"
                     )
                   ),
                   column(
                     4,
                     htmlOutput("CIWarningText")
                   )
                  )
                 ),
                 actionButton("plot","Plot",width = "150px", height= "100px", style="color: white; background-color: green"),
                 shinycssloaders::withSpinner(
                   plotlyOutput("MallowsBinomialQuality")
                 ),
                 downloadButton('downloadMB','Download Plot'),
                 shinycssloaders::withSpinner(
                   plotlyOutput("MallowsBinomialRank")
                 ),
                 downloadButton('downloadMBRank','Download Plot'),
                 shinycssloaders::withSpinner(
                   plotOutput("MallowsBinomialMean")
                 ),
                 downloadButton('downloadMBMean','Download Plot')


)
