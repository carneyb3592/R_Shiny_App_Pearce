dataUI <- tabPanel("Data",
                   useShinyjs(),
                   tabsetPanel(
                       tabPanel("Upload",
                                br(),
                                p("On this page, you may select a built-in toy dataset or 
                                  upload your own. For specific instructions on the data 
                                  formats required for rankings and ratings, see the instructions below."),
                                
                                h2("Example Datasets"),
                                p("Choose one of the following..."),
                                selectInput(
                                  "toyfile",
                                  "Example file:",
                                  choices = c("ToyData","ToyData2")
                                ),
                                tags$hr(),
                                h2("Upload Your Own Data"),
                                h3("Rankings"),
                                
                                p("Rankings should be uploaded as a .csv file. If uploading ",em("Orderings,"),"the file 
                                  should contain one row per reviewer. Each row will contain the reviewer's 
                                  ordering of proposals from best to worst, with one proposal per column. 
                                  If uploading ",em("Ranks,"),"the file should contain one row per reviewer and one 
                                  column per proposal, such that the (i,j) entry contains the rank assigned 
                                  by reviewer i to proposal j. NA indicates that proposal j was not accessible to 
                                  reviewer i, while 0 indicates that a proposal was considered but not ranked."),
                                
                                fileInput(inputId = "RankingsFile", "Choose Rankings file",
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
                                p("Ratings should be uploaded as a .csv file. The file should contain one row per reviewer 
                                  and one column per proposal, such that the (i,j) entry contains the rating assigned 
                                  by reviewer i to proposal j. Ratings must be integer-valued, where ",strong("0 = best rating"),"
                                  and ",strong("M = worst rating.")," The maximum score M can be inputted manually below."),
                                fileInput("RatingsFile", "Choose Ratings File",
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
                                uiOutput("check_data"),
                                actionButton("upload","Upload Data", width = "150px", height= "100px", style="color: white; background-color: green; float: right"),
                                uiOutput("unloadButton")
                              ),
                     tabPanel("Summary",
                              h2("Rankings"),
                              tags$hr(),
                              div(style="overflow-x: scroll;height: 200px;overflow-y:scroll;",tableOutput("dataTableRank")),
                              h2("Ratings"),
                              tags$hr(), 
                              div(style="overflow-x: scroll;height: 200px;overflow-y:scroll;",tableOutput("dataTableRate"))
                            ),
                     tabPanel("EDA",
                              h2("Exploratory Data Analysis"),
                              p("The following three plots visually summarize the rankings and ratings independently, as well as calculate the internal consistency of each reviewer."),
                              sidebarLayout(position = "left",
                                sidebarPanel(
                                  h3("Ratings Figure"),
                                  p("The first plot displays boxplots of ratings assigned 
                                    to each proposal, overlaid with the original data. The 
                                    x-axis displays proposals in numerical order and the y-axis 
                                    displays ratings from best (0) to worst ( M = ",textOutput("RankingsText",inline=TRUE),").",sep="")
                                 
                                ),
                                mainPanel(
                                  shinycssloaders::withSpinner(plotlyOutput("RatingsPlot")),
                                  downloadButton('downloadRatings','Download Plot')
                                  
                                )
                              ),
                              sidebarLayout(position = "left",
                                sidebarPanel(
                                  h3("Rankings Figure"),
                                  p("The second plot displays the stacked bar charts of ranks 
                                    assigned to each proposal. The x-axis displays proposals 
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
                                  h3("Inconsistencies"),
                                  p(" The third plot displays a histogram of reviewer-level inconsistencies. 
                                    Here, inconsistency is defined as the total number of unique object pairs 
                                    in which their order in the judge's ranking is opposite that in the judge's 
                                    ratings. We do not count pairs in which the order of objects cannot be inferred
                                    based on the ratings or rankings (e.g., due to missing data or rating ties).")
                                ),
                                mainPanel(
                                  shinycssloaders::withSpinner(plotlyOutput("InconsistenciesPlot")),
                                  downloadButton('downloadInconsistencies','Download Plot')
                                  
                                )
                              )
                   )
             )
)