reportUI <- tabPanel("Generate Report",
                   h1("Generate Report"),
                   uiOutput("DataName4"),
                   p("This tab allows for creating a short RMarkdown report with figures from the RShiny app, 
                     according to the specifications selected on the previous tabs. Please select which plots 
                     you'd like to include and then press the 'Generate Report' button."),
                   checkboxGroupInput("reportPlots", "Select which plots to add in report:",
                                      c("EDA: Ratings By Proposal" = "RateBP",
                                        "EDA: Rankings By Proposal" = "RankBP",
                                        "EDA: Inconsistencies (Proposal-Level)" = "InconProp",
                                        "EDA: Inconsistencies (Reviewer-Level)" = "Incon",
                                        "Mallows Binomial: Quality" = "MBQual",
                                        "Mallows Binomial: Rank" = "MBRank",
                                        "Mallows Binomial: Comparison" = "MBMean"),
                                        selected = c("RateBP","RankBP","InconProp","Incon",
                                                     "MBQual","MBRank","MBMean")
                                        ),
                   downloadButton('downloadReport','Generate Report')
)