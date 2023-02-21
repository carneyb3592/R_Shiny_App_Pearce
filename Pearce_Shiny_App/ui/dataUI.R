dataUI <- tabPanel("Data",
                   tabsetPanel(
                       tabPanel("Upload",
                                fileInput("file1", "Choose CSV File",
                                  accept = c(
                                     "text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv",
                                     ".RData")        
                                ),
                                checkboxInput("header", "Header", TRUE),
                                tags$hr(),
                                fileInput("file2", "Choose CSV File",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")        
                                ),
                                checkboxInput("header", "Header", TRUE)
                              ),
                     tabPanel("Summary",
                              fluidRow(
                                column(width = 6, h2("Table 1"), div(style="width: 100%;overflow-x: scroll;",tableOutput("dataT"))),
                                column(width = 6, h2("Table 2"), tableOutput("contents2"))
                              )
                            ),
                     tabPanel("EDA",
                              h2("Exploratory Data Analysis"),
                              sidebarLayout(position = "left",
                                sidebarPanel(
                                  h3("Ratings Figure"),
                                  p("This is a description, a long long long long long long long long long long description")
                                ),
                                mainPanel(
                                  plotOutput("RatingsPlot")
                                )
                              ),
                              sidebarLayout(position = "left",
                                sidebarPanel(
                                  h3("Rankings Figure"),
                                  p("This is a description, a long long long long long long long long long long description")
                                ),
                                mainPanel(
                                  plotOutput("RankingsPlot")
                                )
                              ),
                              sidebarLayout(position = "left",
                                sidebarPanel(
                                  h3("Inconsistencies"),
                                  p("This is a description, a long long long long long long long long long long description")
                                ),
                                mainPanel(
                                  plotOutput("InconsistenciesPlot")
                                )
                              )
                   )
             )
)