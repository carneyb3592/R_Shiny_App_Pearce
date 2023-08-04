dataUI <- tabPanel("Data",
                   useShinyjs(),
                   tabsetPanel(
                     tabPanel("Upload",
                              br(),
                              p("The data tab allows you to select, display, and visualize rankings and ratings. 
                                Get started by either selecting an example dataset or uploading your own data (instructions below)."),
                              
                              h2("Example Data Sets"),
                              p("The following four data sets were described and analyzed in",a(href="https://researchintegrityjournal.biomedcentral.com/articles/10.1186/s41073-023-00131-7","Gallo et al. (2023)."),"Please select from the following options."),
                              br(),
                              fluidRow(
                                column(
                                  3,
                                  selectInput(
                                    "toyfile",
                                    "Choose Toy Dataset:",
                                    choices = c("Toy Data 1"="ToyData1","Toy Data 2"="ToyData2","Toy Data 3"="ToyData3","AIBS")
                                  )
                                ),
                                column(
                                  8,
                                  uiOutput("ToyDataDescription")
                                )
                              ),
                              
                              tags$hr(),
                              h2("Upload Your Own Data"),
                              br(),
                              p("You may upload your own rankings and ratings for analysis. Rankings, ratings, and reviewer assignments (if applicable) must be
                                  uploaded separately using the instructions below. After you have selected your data files and options below, press the green 'Upload Data' button at the bottom of the screen."),
                              h3("Rankings"),
                              
                              p("Rankings should be uploaded as a .csv file, in the form of a two-dimensional array with one row per reviewer and one column per proposal.
                                There are two upload types for rankings,",em("Orderings"),"or",em("Ranks."),"If uploading Orderings, each row should contain the corresponding 
                                reviewer's ordering of proposals from best to worst. If the reviewer only provided a partial ordering, values of NA should be used to complete 
                                the row. If uploading Ranks, the (i,j)th entry should contain the rank assigned by reviewer i to proposal j, where NA may be used to indicate 
                                that a proposal was not ranked."),
                              p("Example Data Files:",a(href="orderings.csv", "orderings.csv", download=NA, target="_blank",.noWS = c('after')),",",a(href="ranks.csv", "ranks.csv", download=NA, target="_blank",.noWS = c('after'))),
                              
                              fileInput(inputId = "RankingsFile", "Choose Rankings File:",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv",
                                          ".RData")
                              ),
                              fluidRow(
                                column(
                                  2,
                                  radioButtons(
                                    label = "Type of Data",
                                    inputId = "RankingsFileDataType",
                                    choices = c(
                                      "Orderings" = "orderings",
                                      "Ranks" = "ranks"
                                    ),
                                    selected = "orderings"
                                  )
                                  
                                ),
                                column(
                                  2,
                                  radioButtons(
                                    label = "Separator",
                                    inputId = "RankingsFileDataSeperator",
                                    choices = c(
                                      "Comma" = ",",
                                      "Semicolon" = ";",
                                      "Tab" = "\t"
                                    ),
                                    selected = ","
                                  )
                                )
                                
                              ),
                              h3("Ratings"),
                              
                              p("Ratings should be uploaded as a .csv file, in the form of a two-dimensional array with one row per reviewer and 
                                one column per proposal. The (i,j)th entry should contain the rating assigned by reviewer i to proposal j. Ratings 
                                must be integer-valued, where ",strong("0 = best rating")," and ",strong("M = worst rating.")," The maximum score 
                                M must be inputted manually below."),
                              p("Example Data File:",a(href="ratings.csv", "ratings.csv", download=NA, target="_blank",.noWS = c('after'))),
                              
                              fileInput("RatingsFile", "Choose Ratings File:",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                              ),
                              fluidRow(
                                column(
                                  2,
                                  textInput(
                                    inputId = "RatingsMValue",
                                    label = "Maximum score (M)",
                                    value=4
                                  )
                                  
                                ),
                                column(
                                  2,
                                  radioButtons(
                                    label = "Seperator",
                                    inputId = "RatingsFileDataSeperator",
                                    choices = c(
                                      "Comma" = ",",
                                      "Semicolon" = ";",
                                      "Tab" = "\t"
                                    ),
                                    selected = ","
                                  )
                                )
                                
                              ),
                              h3("Reviewer Assignments"),
                              p("If applicable, reviewer assignments should be uploaded as a .csv file, in the form of a two-dimensional array 
                                with one row per reviewer and one column per proposal. The (i,j) entry should contain a boolean (TRUE or FALSE) 
                                indicating if reviewer i was assigned and/or able to review proposal j. For example, FALSE may be used to indicate
                                a reviewer's conflict of interest with a proposal. If no file is uploaded, we assume all reviewers were able to 
                                review all proposals (i.e., all entries are TRUE)."),
                              p("Example Data File:",a(href="assignments.csv", "assignments.csv", download=NA, target="_blank",.noWS = c('after'))),
                              
                              fileInput("ReviewersAssignments", "Choose File:",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                              ),
                              fluidRow(
                                column(
                                  2,
                                  radioButtons(
                                    label = "Seperator",
                                    inputId = "AssignmentsFileDataSeperator",
                                    choices = c(
                                      "Comma" = ",",
                                      "Semicolon" = ";",
                                      "Tab" = "\t"
                                    ),
                                    selected = ","
                                  )
                                )
                              ),
                              actionButton("upload","Upload Data", width = "150px", height= "100px", style="color: white; background-color: green; float: right"),
                              uiOutput("unloadButton")
                     ),
                     tabPanel("Data Tables",
                              uiOutput("DataName1"),
                              p("Below, we display rankings and ratings in tabular form. Please explore the data below and 
                                confirm correct data entry."),
                              h2("Rankings"),
                              p("The rankings table should have one row per reviewer and one column per rank place. 
                                Values in the table indicate proposal numbers; i.e., the (i,j)th entry indicates the proposal 
                                assigned by reviewer i to jth place."),
                              tags$hr(),
                              DT::dataTableOutput("dataTableRank"),
                              h2("Ratings"),
                              p("The ratings table should have one row per reviewer and one column per proposal. Values in the table 
                                indicate ratings; i.e., the (i,j)th entry indicates the rating assigned by reviewer i to proposal j."),
                              tags$hr(),
                              DT::dataTableOutput("dataTableRate")
                     ),
                     tabPanel("Exploratory Data Analysis",
                              uiOutput("DataName2"),
                              p("We provide four plots to visually summarize rankings and ratings. You may hover over the data in each plot to 
                                view additional information."),
                              sidebarLayout(position = "left",
                                            sidebarPanel(
                                              h3("Ratings by Proposal"),
                                              p("The first plot displays side-by-side boxplots of ratings assigned to each proposal. The
                                  x-axis contains proposals in numerical order and the y-axis displays ratings from best (0)
                                    to worst (M = ",textOutput("RankingsText",inline=TRUE),
                                                "). Hover over each boxplot for more information.",sep="")
                                            ),
                                            mainPanel(
                                              shinycssloaders::withSpinner(plotlyOutput("RatingsPlot")),
                                              downloadButton('downloadRatings','Download Plot')
                                              
                                            )
                              ),
                              sidebarLayout(position = "left",
                                            sidebarPanel(
                                              h3("Rankings Figure"),
                                              p("The second plot displays stacked barcharts of ranks
                                    assigned to each proposal. The x-axis contains proposals
                                    in numerical order and the y-axis displays counts.
                                    Different rank levels are indicated using colors.
                                    Hover over each bar for more information.")
                                            ),
                                            mainPanel(
                                              shinycssloaders::withSpinner(plotlyOutput("RankingsPlot")),
                                              downloadButton('downloadRankings','Download Plot')
                                            )
                              ),
                              sidebarLayout(position = "left",
                                            sidebarPanel(
                                              h3("Proposal-Level Inconsistency"),
                                              p("The third plot displays counts of internal inconsistencies between 
                                              ratings and rankings across reviewers. An inconsistency occurs when the order 
                                              of two objects in a reviewer's ranking does not match the order implied by the
                                              reviewers ratings. We do not count pairs in which the order of objects cannot 
                                              be inferred (e.g., due to missingness, ratings ties, conflicts of interest).
                                              The x-axis contains proposals in numerical order and the y-axis displays the 
                                              total number of inconsistencies associated with each proposal across reviewers.")
                                            ),
                                            mainPanel(
                                              shinycssloaders::withSpinner(plotlyOutput("InconsistenciesProposalPlot")),
                                              downloadButton('downloadInconsistenciesProposal','Download Plot')
                                              
                                            )
                              ),
                              sidebarLayout(position = "left",
                                            sidebarPanel(
                                              h3("Reviewer-Level Inconsistency"),
                                              p("The fourth plot displays counts of internal inconsistencies between 
                                              ratings and rankings by reviewer. An inconsistency occurs when the order 
                                              of two objects in a reviewer's ranking does not match the order implied by the
                                              reviewers ratings. We do not count pairs in which the order of objects cannot 
                                              be inferred (e.g., due to missingness, ratings ties, conflicts of interest).
                                              The x-axis contains reviewers in numerical order and the y-axis displays the 
                                              total number of internal inconsistencies in each reviewer's data. A value of 0 indicates a 
                                              judge whose ratings were internally consistent with his/her ranking.")
                                            ),
                                            mainPanel(
                                              shinycssloaders::withSpinner(plotlyOutput("InconsistenciesPlot")),
                                              downloadButton('downloadInconsistencies','Download Plot')
                                              
                                            )
                              )
                     )
                   )
)
