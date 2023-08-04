### Global Values ##############################################################

csvdata_status <- reactiveValues()
csvdata_status$uploaded <- 0
csvdata_status$unloaded <- 0


reactive_data <- reactiveValues()
reactive_data$Rankings = NULL
reactive_data$Ratings = NULL
reactive_data$M = NULL
################################################################################

### Set Data to Toy data #######################################################
observeEvent(c(input$toyfile, csvdata_status$unloaded == 1),{
  if(csvdata_status$uploaded == 0){
    
    tmp_env <- new.env()
    fileName <- paste(input$toyfile,".RData",sep = "")
    myfile <- file.path("server",fileName)
    Data <- load(myfile, tmp_env)
    if(length(Data)==1)
      Data <- tmp_env[[Data]]
    else
      Data <- NULL
    reactive_data$Ratings <- Data$ratings
    reactive_data$Rankings <- Data$rankings
    reactive_data$M <- Data$M
  }
})
################################################################################

### Set Data to Uploaded Data ##################################################

observeEvent(input$upload,{
  sessionEnvir <- sys.frame()
  if (!is.null(input$RankingsFile) && !is.null(input$RatingsFile)){
    ratings = as.matrix(read.csv(input$RatingsFile$datapath,header=FALSE,
                                 sep=input$RatingsFileDataSeperator))
    rankings = as.matrix(read.csv(input$RankingsFile$datapath,header=FALSE,
                                  sep=input$RankingsFileDataSeperator))
    if(input$RankingsFileDataType == "ranks"){rankings = to_rankings(rankings)}
    if(!is.null(input$ReviewersAssignments)){
      assignments = as.matrix(read.csv(input$ReviewersAssignments$datapath,header=FALSE,
                                       sep=input$AssignmentsFileDataSeperator))
      attr(rankings,"assignments") <- assignments
      print(rankings)
    }
    colnames(rankings) <- NULL 
    colnames(ratings) <- NULL
    
    M <- as.numeric(input$RatingsMValue)
    
    
    reactive_data$Ratings <- ratings
    reactive_data$Rankings <- rankings
    reactive_data$M <- M
    check_data_message()
    
  } else {
    shinyalert(title="Error!",text="Please upload all necessary files",type="error")
    
  }
})

check_data_message <- eventReactive(input$upload,{
  csvdata_status$uploaded <- 1
  csvdata_status$unloaded <- 0
  shinyalert(title="Successful Upload!",type="success")
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