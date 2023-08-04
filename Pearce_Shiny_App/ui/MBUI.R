MBUI <- tabPanel("Mallows Binomial",
                 h1("Mallows-Binomial"),
                 uiOutput("DataName3"),
                 p("The Mallows-Binomial tab allows for fitting the Mallows-Binomial model to rankings and ratings. 
                 Mallows-Binomial is the first joint statistical model for rankings and ratings to not rely on 
                 data conversion. For additional details of the model, please refer to the 'Home' tab of this tool."),
                 br(),
                 p("Select how you would like to fit the model using the radio buttons below, and then click 
                 'Plot' to begin estimation. After model fitting, results are visualized in three plots."),
                 
                 tags$hr(),
                 verticalLayout(
                   fluidRow(
                     column(
                       3,
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
                       3,
                       radioButtons(
                         label = "Include Confidence Intervals?",
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
                   ),
                   fluidRow(
                     column(
                       2,
                       numericInput(
                         "confidencelevel",
                         "Confidence Level",
                         value = 0.50,
                         min=0.01,max=0.99
                       )
                     ),
                     column(
                       4,
                       numericInput(
                         "bootstrapsample",
                         "Number of Bootstrap Samples",
                         value = 30,
                         min=1,max=100,step=1
                       )
                     )
                   )
                 ),
                 actionButton("plot","Plot",width = "150px", height= "100px", style="color: white; background-color: green"),
                 
                 sidebarLayout(position = "left",
                               sidebarPanel(
                                 h3("Estimated Proposal Quality"),
                                 p("The first plot displays estimates of proposal quality on the unit interval,
                                 based on the Mallows-Binomial model. Here, estimated proposal quality is identical 
                                   to the 'integrated score' of the proposal, as defined in",
                                   a(href="https://researchintegrityjournal.biomedcentral.com/articles/10.1186/s41073-023-00131-7","Gallo et al. (2023)."),
                                   "Hover over the plot for additional details.")
                               ),
                               mainPanel(
                                 shinycssloaders::withSpinner(
                                   plotlyOutput("MallowsBinomialQuality")
                                 ),
                                 downloadButton('downloadMB','Download Plot'),
                                 downloadButton('donwloadMBQualData','Download Data')
                                 
                                 
                               )
                 ),
                 sidebarLayout(position = "left",
                               sidebarPanel(
                                 h3("Estimated Proposal Rank"),
                                 p("The second plot displays estimates of proposals' overall ranks, 
                                based on the Mallows-Binomial model. Non-integer values occur when one or more proposals are
                                tied for the same rank, which may occur when, for example, two proposals receive the same mean 
                                rating and identical sets of ranks. Hover over the plot for additional details.")
                               ),
                               mainPanel(
                                 shinycssloaders::withSpinner(
                                   plotlyOutput("MallowsBinomialRank")
                                 ),
                                 downloadButton('downloadMBRank','Download Plot'),
                                 downloadButton('donwloadMBRankData','Download Data')
                                 
                               )
                 ),
                 sidebarLayout(position = "left",
                               sidebarPanel(
                                 h3("Comparison of Estimated Ranks based on Rankings and Ratings vs. Only Ratings"),
                                 p("The third plot allows for direct comparison of estimated ranks by proposal
                                between the joint Mallows-Binomial model for rankings and ratings (x-axis), and a method based 
                                on ordering proposals by their mean rating (y-axis). The red dotted line on the main
                                diagonal represents equivalent results between models, and the gray dotted lines indicate a
                                difference of one rank place.")
                               ),
                               mainPanel(
                                 shinycssloaders::withSpinner(
                                   plotOutput("MallowsBinomialMean")
                                 ),
                                 downloadButton('downloadMBMean','Download Plot')
                               )
                 )
)
