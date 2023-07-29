reportUI <- tabPanel("Generate Report",
                   h1("Generate Report!"),
                   checkboxGroupInput("reportPlots", "Select which plots to add in report:",
                                      c("Ratings By Proposal" = "RateBP",
                                        "Rankings By Proposal" = "RankBP",
                                        "Inconsistencies (Proposal-Level)" = "InconProp",
                                        "Inconsistencies (Reviewer-Level)" = "Incon",
                                        "Mallows Binomial Quality" = "MBQual",
                                        "Mallows Binomial Rank" = "MBRank",
                                        "Mallows Binomial Mean Rankings" = "MBMean"),
                                        selected = c("RateBP","RankBP","InconProp","Incon",
                                                     "MBQual","MBRank","MBMean")
                                        ),
                   downloadButton('downloadReport','Generate Report')
)