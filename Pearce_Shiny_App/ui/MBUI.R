MBUI <- tabPanel("MB",
                 h1("Mallows-Binomial"),
                 p("On this page, we fit a Mallows-Binomial (MB) model to rankings and ratings data. Select how you would like to fit the model using the radio buttons
                 below, and then click 'Plot'. After model fitting, you may visualize model results via three plots."),

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
                   plotlyOutput("MallowsBinomialQuality")
                 ),
                 downloadButton('downloadMB','Download Plot'),
                 shinycssloaders::withSpinner(
                   plotlyOutput("MallowsBinomialRank")
                 ),
                 downloadButton('downloadMBRank','Download Plot'),
                 shinycssloaders::withSpinner(
                   plotlyOutput("MallowsBinomialMean")
                 ),
                 downloadButton('downloadMBMean','Download Plot'),
                 downloadButton('downloadReport','Generate Report')


)
