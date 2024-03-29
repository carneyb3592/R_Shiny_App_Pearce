homeUI <- tabPanel("Home",
                   icon = icon("house"),
                   fluidRow(position = "right",
                            column(8,
                                   h1("Peer Review with Rankings and Ratings"),
                                   p("Welcome to the ",em("Peer Review with Rankings and Ratings")," R Shiny app! This interactive application fits joint statistical models to peer review data in the form of rankings and ratings, as well as provides tools for data exploration and decision-making. This Shiny application is based on the free and open-access",a(href="https://cran.r-project.org/web/packages/rankrate/index.html","rankrate"),"R package. For a tutorial on using the rankrate package in R, please see the corresponding",a(href="https://pearce790.github.io/rankrate/","vignettes.")),
                                   p("Get started with the tool by clicking on the ",em("Data")," tab in the toolbar above, which allows you to upload your own data for analysis or explore one of four built-in toy datasets."),
                                   h2("Further Information"),
                                   p("Additional information on the models included in this tool can be found in the following papers:"),
                                   tags$ul(
                                     tags$li('Stephen A. Gallo, Michael Pearce, Carole J. Lee, and Elena A. Erosheva.',a(href="https://researchintegrityjournal.biomedcentral.com/articles/10.1186/s41073-023-00131-7","'A new approach to peer review assessments: Score, then rank.'"),em("Research Integrity and Peer Review"),' 8.10 (2023): 10.'),
                                     tags$li('Michael Pearce and Elena A. Erosheva.',a(href="https://www.jmlr.org/papers/v23/21-1262.html","'A unified statistical learning model for rankings and scores with application to grant panel review.'"),em("Journal of Machine Learning Research"),'23.210 (2022): 1-33.'),
                                     tags$li('Michael Pearce and Elena A. Erosheva.',a(href="https://arxiv.org/abs/2301.09755","'Modeling Preferences: A Bayesian Mixture of Finite Mixtures for Rankings and Ratings.'"),em('arXiv preprint arXiv:2301.09755'),'(2023).')
                                   ),
                                   h2("Feedback"),
                                   p('If you discover any issues when working with this tool, please fill out the following', a(href="https://forms.gle/FpEVBsLjcWAYHjqx8","Google Form,"),
                                   'write to',a(href="mailto:michaelpearce@reed.edu","michaelpearce@reed.edu,"),'or use', a(href="https://github.com/carneyb3592/R_Shiny_App_Pearce","Github.")),
                                   h2("Acknowledgement and Funding"),
                                   p("This project was supported by the National Science Foundation under Grant No. 2019901. We would like to thank Ben Carney, the principal software developer of this package."),
                            ),
                            column(4,
                                   div(img(src="rankrate_logo.png",
                                           height = 350,
                                           width = 350,
                                           style = "display:flex; margin:auto; justify-content: center; top: 10%; position: absolute"),
                                       style="height:800px")
                            )
                   )
)
