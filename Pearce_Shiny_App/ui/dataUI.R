dataUI <- tabPanel("Data",
                   useShinyjs(),
                   tabsetPanel(
                       tabPanel("Upload",
                                br(),
                                p("On this page, we select, explore, and visualize rankings and ratings data. Get started by either selecting
                                either a 'toy dataset' or uploading your own data (instructions below)."),

                                h2("Toy Datasets"),
                                p("The following three toy datasets were described and analyzed in Gallo et al. (2023). Please select from the
                                following options."),
                                br(),
                                fluidRow(
                                  column(
                                    2,
                                    selectInput(
                                      "toyfile",
                                      "Choose Toy Dataset:",
                                      choices = c("ToyData1","ToyData2","ToyData3")
                                    )
                                  ),
                                  column(
                                    4,
                                    uiOutput("ToyDataDescription")
                                  )
                                ),
                                
                                tags$hr(),
                                h2("Upload Your Own Data"),
                                br(),
                                p("Instead of using toy datasets, you may upload your own rankings and ratings for analysis. Each data type must be
                                  uploaded separately using the instructions below."),
                                h3("Rankings"),

                                p("Rankings should be uploaded as a .csv file, in the form of a two-dimensional array with one row per reviewer and one column per proposal.
                                If uploading ",em("Orderings,"),"each row should contain the corresponding reviewer's ordering of proposals from best to worst. If the
                                reviewer only provided a partial ordering, values of NA should be used to complete the row. If uploading ",em("Ranks,")," each the (i,j)
                                entry should contain the rank assigned by reviewer i to proposal j, where NA may be used to indicate that a proposal was not ranked."),

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
                                        label = "Seperator",
                                        inputId = "RankingsFileDataSeperator",
                                        choices = c(
                                          "Comma" = "comma",
                                          "Semicolon" = "semicolon",
                                          "Tab" = "tab"
                                        ),
                                        selected = "comma"
                                      )
                                    )

                                ),
                                h3("Ratings"),

                                p("Ratings should be uploaded as a .csv file, in the form of a two-dimensional array with one row per reviewer and one column per proposal.
                                The (i,j) entry should contain the rating assigned by reviewer i to proposal j. Ratings must be integer-valued, where ",
                                strong("0 = best rating")," and ",strong("M = worst rating.")," The maximum score M should be inputted manually below."),
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
                                        "Comma" = "comma",
                                        "Semicolon" = "semicolon",
                                        "Tab" = "tab"
                                      ),
                                      selected = "comma"
                                    )
                                  )

                                ),
                                h3("Reviewer Assignments"),
                                p("Description Text."),
                                fileInput("ReviewersAssignments", "Choose File:",
                                          accept = c(
                                            "text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")
                                ),
                                uiOutput("check_data"),
                                actionButton("upload","Upload Data", width = "150px", height= "100px", style="color: white; background-color: green; float: right"),
                                uiOutput("unloadButton")
                              ),
                     tabPanel("Data Tables",
                              p("Below, we view the provided rankings and ratings in tabular form. Please explore the data below and confirm correct data entry."),
                              h2("Rankings"),
                              tags$hr(),
                              DT::dataTableOutput("dataTableRank"),
                              h2("Ratings"),
                              tags$hr(),
                              DT::dataTableOutput("dataTableRate")
                            ),
                     tabPanel("Exploratory Data Analysis",
                              h2("Exploratory Data Analysis"),
                              p("We provide three plots to visually summarize rankings and ratings. You may hover over the data in each plot to view additional information."),
                              sidebarLayout(position = "left",
                                sidebarPanel(
                                  h3("Ratings by Proposal"),
                                  p("The first plot displays side-by-side boxplots of ratings assigned to each proposal. The
                                  x-axis displays proposals in numerical order and the y-asix displays ratings from best (0)
                                    to worst ( M = ",textOutput("RankingsText",inline=TRUE),"). Hover over each boxplot for more information.",sep="")
                                ),
                                mainPanel(
                                  shinycssloaders::withSpinner(plotlyOutput("RatingsPlot")),
                                  downloadButton('downloadRatings','Download Plot'),
                                  downloadButton('donwloadRatingsData','Download Estimates')
                                  
                                )
                              ),
                              sidebarLayout(position = "left",
                                sidebarPanel(
                                  h3("Rankings Figure"),
                                  p("The second plot displays stacked barcharts of the ranks
                                    assigned to each proposal. The x-axis displays proposals
                                    in numerical order and the y-axis displays counts.
                                    Different rank levels are indicated using colors.
                                    Hover over each bar for more information.")
                                ),
                                mainPanel(
                                  shinycssloaders::withSpinner(plotlyOutput("RankingsPlot")),
                                  downloadButton('downloadRankings','Download Plot'),
                                  downloadButton('donwloadRankingsData','Download Estimates')
                                )
                              ),
                              sidebarLayout(position = "left",
                                sidebarPanel(
                                  h3("Inconsistencies"),
                                  p(" The third plot displays a histogram of reviewer-level inconsistencies.
                                    Here, inconsistency is defined as the total number of unique object pairs
                                    for which their ordering in a judge's ranking does not match their implied ordering by
                                    a judge's ratings. We do not count pairs in which the order of objects cannot be inferred
                                    based on ratings or rankings (e.g., due to missing data or rating ties). A value of 0 indicates a judge
                                    whose ratings were internally consistent with his/her ranking.")
                                ),
                                mainPanel(
                                  shinycssloaders::withSpinner(plotlyOutput("InconsistenciesPlot")),
                                  downloadButton('downloadInconsistencies','Download Plot')

                                )
                              )
                   )
             )
)
