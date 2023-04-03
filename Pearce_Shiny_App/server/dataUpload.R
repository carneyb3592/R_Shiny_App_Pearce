csvdata_status <- reactiveValues()
csvdata_status$uploaded <- 0
csvdata_status$unloaded <- 0


check_data_message <- eventReactive(input$upload,{
  csvdata_status$uploaded <- 1
  csvdata_status$unloaded <- 0
  return("<p>Succesful upload!</p>")
})

output$check_data <- renderUI({
  HTML(check_data_message())
})

observeEvent(input$unload, {
  useShinyjs()
  reset("RankingsFile")
  reset("RatingsFile")
  reactive_data$Rankings = NULL
  reactive_data$Ratings = NULL
  reactive_data$M = NULL
  csvdata_status$unloaded <- 1
  csvdata_status$uploaded <- 0
})


output$unloadButton <- renderUI({
  if (csvdata_status$uploaded > 0) {
    actionButton("unload","Unload Data", width = "150px", height= "100px", style="color: white; background-color: green; float: right")
  }
})