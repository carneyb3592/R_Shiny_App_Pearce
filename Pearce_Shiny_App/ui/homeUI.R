homeUI <- tabPanel("Home",
                   icon = icon("house"),
                   h1("Peer Review with Rankings and Ratings"),
                   p("Welcome to the ",em("Peer Review with Rankings and Ratings")," R Shiny app! This interactive application fits joint statistical models to peer review data in the form of rankings and ratings, as well as provides tools for data exploration and decision-making."),
                   p("Get started with the tool by clicking on the ",em("Data")," tab in the toolbar above, which allows you to upload your own data for analysis or explore one of four built-in toy datasets."),
                   h2("Further Information"),
                   p("Additional information on the models included in this tool can be found in the following papers:"),
                   tags$ul(
                     tags$li('Stephen A. Gallo, Michael Pearce, Carole J. Lee, and Elena A. Erosheva.',a(href="https://www.researchsquare.com/article/rs-2198949/v1","'A new approach to peer review assessments: Score, then rank.'"),em("Research Integrity and Peer Review"),' (2023).'),
                     tags$li('Michael Pearce and Elena A. Erosheva.',a(href="https://www.jmlr.org/papers/v23/21-1262.html","'A unified statistical learning model for rankings and scores with application to grant panel review.'"),em("Journal of Machine Learning Research"),'23.210 (2022): 1-33.'),
                     tags$li('Michael Pearce and Elena A. Erosheva.',a(href="https://arxiv.org/abs/2301.09755","'Modeling Preferences: A Bayesian Mixture of Finite Mixtures for Rankings and Ratings.'"),em('arXiv preprint arXiv:2301.09755'),'(2023).')
                   ),
                   h2("Feedback"),
                   p('If you discover any issues when working with this tool, please write to',a(href="mailto:mpp790@uw.edu","mpp790@uw.edu"),'or use', a(href="https://github.com/carneyb3592/R_Shiny_App_Pearce","Github.")),
                   h2("Acknowledgement and Funding"),
                   p("This project was supported by the National Science Foundation under Grant No. 2019901. We would like to thank Ben Carney, the principal software developer of this package.")

)
