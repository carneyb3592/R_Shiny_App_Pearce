dataUI <- tabPanel("Data",
                   tabsetPanel(
                     tabPanel("Upload",
                              fileInput("file1", "Choose CSV File",
                                accept = c(
                                   "text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")        
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
                                column(width = 6, h2("Table 1"), tableOutput("contents1")),
                                column(width = 6, h2("Table 2"), tableOutput("contents2"))
                              )
                            ),
                             
                     
                     tabPanel("EDA",
                              fluidRow(
                                column(width = 6, h2("Table 1"), plotOutput("distPlot1")),
                                column(width = 6, h2("Table 2"), plotOutput("distPlot2"))
                              ))
                   )
)