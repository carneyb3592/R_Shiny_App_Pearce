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

dataTableRankings <- reactive({
  req(input$file)
  sessionEnvir <- sys.frame()
  if (!is.null(input$RankingsFile)){
    tmp_env <- new.env()
    Data <- load(input$RankingsFile$datapath, tmp_env)
    if(length(Data)==1)
      Data <- tmp_env[[Data]]
    else
      Data <- NULL
  }
  else if(!is.null(input$file)) {
    tmp_env <- new.env()
    fileName <- paste(input$file,".RData",sep = "")
    myfile <- file.path("server",fileName)
    Data <- load(myfile, tmp_env)
    if(length(Data)==1)
      Data <- tmp_env[[Data]]
    else
      Data <- NULL
    
  }
  Data
  
})
dataTableRatings <- reactive({
  req(input$file)
  sessionEnvir <- sys.frame()
  if (!is.null(input$RatingsFile)){
    tmp_env <- new.env()
    Data <- load(input$RatingsFile$datapath, tmp_env)
    if(length(Data)==1)
      Data <- tmp_env[[Data]]
    else
      Data <- NULL
  }
  else if(!is.null(input$file)) {
    tmp_env <- new.env()
    fileName <- paste(input$file,".RData",sep = "")
    myfile <- file.path("server",fileName)
    Data <- load(myfile, tmp_env)
    if(length(Data)==1)
      Data <- tmp_env[[Data]]
    else
      Data <- NULL
    
  }
  Data
  
})

Ratings <- reactive({
  req(input$file)
  sessionEnvir <- sys.frame()
  if (!is.null(input$RankingsFile)){
    tmp_env <- new.env()
    Data <- load(input$RankingsFile$datapath, tmp_env)
    if(length(Data)==1)
      Data <- tmp_env[[Data]]
    else
      Data <- NULL
  }
  else if(!is.null(input$file)) {
    tmp_env <- new.env()
    fileName <- paste(input$file,".RData",sep = "")
    myfile <- file.path("server",fileName)
    Data <- load(myfile, tmp_env)
    if(length(Data)==1)
      Data <- tmp_env[[Data]]
    else
      Data <- NULL
    
  }
  ratings <- Data$ratings
  rankings <- Data$rankings
  M <- Data$M #this is the "max" score, which the user should input too!
  
  ## Simple EDA
  I <- nrow(ratings)
  J <- ncol(ratings)
  R <- ncol(rankings)
  
  ratings_long <- melt(ratings)
  names(ratings_long) <- c("Judge","Proposal","Rating")
  ratings_long$Judge <- factor(ratings_long$Judge)
  ratings_long$Proposal <- factor(ratings_long$Proposal)
  
  g <- ggplot(ratings_long,aes(Proposal,Rating))+
    theme_bw(base_size=15)+geom_boxplot()+ylim(c(0,M))+
    ggtitle("Ratings by Proposal",paste0("0 = best; ",M," = worst"))+
    theme(panel.grid.major.x = element_blank(),panel.grid.minor.y = element_blank())
  g
})

Rankings <- reactive({
  req(input$file)
  sessionEnvir <- sys.frame()
  if (!is.null(input$RankingsFile)){
    tmp_env <- new.env()
    Data <- load(input$RankingsFile$datapath, tmp_env)
    if(length(Data)==1)
      Data <- tmp_env[[Data]]
    else
      Data <- NULL
  }
  else if(!is.null(input$file)) {
    tmp_env <- new.env()
    fileName <- paste(input$file,".RData",sep = "")
    myfile <- file.path("server",fileName)
    Data <- load(myfile, tmp_env)
    if(length(Data)==1)
      Data <- tmp_env[[Data]]
    else
      Data <- NULL
    
  }
  ratings <- Data$ratings
  rankings <- Data$rankings
  M <- Data$M #this is the "max" score, which the user should input too!
  
  ## Simple EDA
  I <- nrow(ratings)
  J <- ncol(ratings)
  R <- ncol(rankings)
  rankings_long <- melt(rankings)
  names(rankings_long) <- c("Judge","Place","Proposal")
  rankings_long$Judge <- factor(rankings_long$Judge)
  rankings_long$Place <- factor(rankings_long$Place,levels=R:1,labels=toOrdinal(R:1))
  colfunc<-colorRampPalette(c("lightgray","black"))
  
  g <- ggplot(rankings_long,aes(Proposal,fill=Place))+
    theme_bw(base_size=15)+geom_bar()+
    ggtitle("Rankings by Proposal")+
    guides(fill = guide_legend(reverse = TRUE))+
    scale_fill_manual(values=colfunc(max(rankings)))+ylab("Count")+
    theme(panel.grid = element_blank(),legend.position = "bottom")
  g
})


Inconsistencies <- reactive({
  req(input$file)
  sessionEnvir <- sys.frame()
  if (!is.null(input$RankingsFile)){
    tmp_env <- new.env()
    Data <- load(input$RankingsFile$datapath, tmp_env)
    if(length(Data)==1)
      Data <- tmp_env[[Data]]
    else
      Data <- NULL
  }
  else if(!is.null(input$file)) {
    tmp_env <- new.env()
    fileName <- paste(input$file,".RData",sep = "")
    myfile <- file.path("server",fileName)
    Data <- load(myfile, tmp_env)
    if(length(Data)==1)
      Data <- tmp_env[[Data]]
    else
      Data <- NULL
    
  }
  ratings <- Data$ratings
  rankings <- Data$rankings
  M <- Data$M #this is the "max" score, which the user should input too!
  
  ## Simple EDA
  I <- nrow(ratings)
  J <- ncol(ratings)
  R <- ncol(rankings)
  
  consistency <- data.frame(Judge=1:nrow(ratings),Kendall=NA)
  for(i in 1:nrow(ratings)){
    kendall <- 0
    for(j1 in 1:(J-1)){for(j2 in (j1+1):J){
      if(ratings[i,j1] < ratings[i,j2]){
        if(which(rankings[i,]==j1)>which(rankings[i,]==j2)){kendall <- kendall + 1}
      }else if(ratings[i,j1] > ratings[i,j2]){
        if(which(rankings[i,]==j1) < which(rankings[i,]==j2)){kendall <- kendall + 1}
      }
    }}
    consistency[i,2] <- kendall
  }
  g <- ggplot(consistency,aes(x=Kendall))+
    theme_bw(base_size=15)+geom_histogram()+
    xlim(c(-.1,max(consistency$Kendall)+.1))+
    xlab("Number of Inconsistent Item Pairs")+ylab("Count")+
    ggtitle("Inconsistency Between Ratings and Rankings")+
    theme(panel.grid.major.x = element_blank(),panel.grid.minor.y = element_blank())
  g
})
output$dataTableRank <- renderTable(
  dataTableRankings()
)
output$dataTableRate <- renderTable(
  dataTableRatings()
)
output$RatingsPlot <- renderPlot(
  Ratings()
)

output$RankingsPlot <- renderPlot(
  Rankings()
)

output$InconsistenciesPlot <- renderPlot(
  Inconsistencies()
)


output$RankingsText <- renderText(
 input$RatingsMValue
)
