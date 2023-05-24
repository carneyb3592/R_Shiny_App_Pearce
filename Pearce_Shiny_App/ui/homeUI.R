homeUI <- tabPanel("Home",
                   icon = icon("house"),
                   fluidRow(position = "right",
                     column(8,
                       h1("Peer Review with Rankings and Ratings"),
                       p("Welcome to the ",em("Peer Review with Rankings and Ratings")," R Shiny app! This interactive application fits joint statistical models to peer review data in the form of rankings and ratings, as well as provides tools for data exploration and decision-making."),
                       p("Get started with the tool by clicking on the ",em("Data")," tab in the toolbar above, which allows you to use either your own data or a built-in toy dataset."),
                       h2("Further Information"),
                       p("Additional information on the models included in this tool can be found in the following papers:"),
                       tags$ul(
                         tags$li('Gallo, Stephen A., et al. "A new approach to peer review assessments: Score, then rank." (2023).'),
                         tags$li('Pearce, Michael, and Elena A. Erosheva. "A unified statistical learning model for rankings and scores with application to grant panel review." ',em("Journal of Machine Learning Research"),'23.210 (2022): 1-33.'),
                         tags$li('Pearce, Michael, and Elena A. Erosheva. "Modeling Preferences: A Bayesian Mixture of Finite Mixtures for Rankings and Ratings." ',em('arXiv preprint arXiv:2301.09755'),'(2023).')
                       ),
                       h2("Feedback"),
                       p('If you discover any issues when working with this tool, please write to mpp790@uw.edu or use', a(href="https://github.com/carneyb3592/R_Shiny_App_Pearce","Github.")),
                       h2("Funding"),
                       p("This project was supported by the National Science Foundation under Grant No. 2019901.")
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
