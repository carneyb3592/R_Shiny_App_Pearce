output$contents1 <- renderTable({
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  inFile <- input$file1
  
  if (is.null(inFile))
    return(NULL)
  
  read.csv(inFile$datapath, header = input$header)
})

output$contents2 <- renderTable({
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  inFile <- input$file2
  
  if (is.null(inFile))
    return(NULL)
  
  read.csv(inFile$datapath, header = input$header)
})

output$distPlot1 <- renderPlot({
  
  # generate bins based on input$bins from ui.R
  inFile <- input$file1
  if (is.null(inFile))
    return(NULL)
  ex_data <- read.csv(inFile$datapath, header = input$header)
  
  ggplot(data=ex_data, mapping= aes(x=Year, y=Value)) + geom_line()
  
})
output$distPlot2 <- renderPlot({
  
  # generate bins based on input$bins from ui.R
  inFile <- input$file2
  if (is.null(inFile))
    return(NULL)
  ex_data <- read.csv(inFile$datapath, header = input$header)
  
  ggplot(data=ex_data, mapping= aes(x=Year, y=Value)) + geom_line()
  
})