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

dataTable <- reactive({
  req(input$file1)
  sessionEnvir <- sys.frame()
  if (!is.null(input$file1)){
    tmp_env <- new.env()
    p <- load(input$file1$datapath, tmp_env)
    if(length(p)==1)
      p <- tmp_env[[p]]
    else
      p <- NULL
    
  }
  p
})

Ratings <- reactive({
  req(input$file1)
  sessionEnvir <- sys.frame()
  if (!is.null(input$file1)){
    tmp_env <- new.env()
    ToyData <- load(input$file1$datapath, tmp_env)
    if(length(ToyData)==1)
      ToyData <- tmp_env[[ToyData]]
    else
      ToyData <- NULL
      
  }
  ratings <- ToyData$ratings
  rankings <- ToyData$rankings
  M <- ToyData$M #this is the "max" score, which the user should input too!
  
  ## Simple EDA
  I <- nrow(ratings)
  J <- ncol(ratings)
  R <- ncol(rankings)
  
  ratings_long <- melt(ratings)
  names(ratings_long) <- c("Judge","Proposal","Rating")
  ratings_long$Judge <- factor(ratings_long$Judge)
  ratings_long$Proposal <- factor(ratings_long$Proposal)
  
  p <- ggplot(ratings_long,aes(Proposal,Rating))+
    theme_bw(base_size=15)+geom_boxplot()+ylim(c(0,M))+
    ggtitle("Ratings by Proposal",paste0("0 = best; ",M," = worst"))+
    theme(panel.grid.major.x = element_blank(),panel.grid.minor.y = element_blank())
  p
})

Rankings <- reactive({
  req(input$file1)
  sessionEnvir <- sys.frame()
  if (!is.null(input$file1)){
    tmp_env <- new.env()
    ToyData <- load(input$file1$datapath, tmp_env)
    if(length(ToyData)==1)
      ToyData <- tmp_env[[ToyData]]
    else
      ToyData <- NULL
  }
  ratings <- ToyData$ratings
  rankings <- ToyData$rankings
  M <- ToyData$M #this is the "max" score, which the user should input too!
  
  ## Simple EDA
  I <- nrow(ratings)
  J <- ncol(ratings)
  R <- ncol(rankings)
  rankings_long <- melt(rankings)
  names(rankings_long) <- c("Judge","Place","Proposal")
  rankings_long$Judge <- factor(rankings_long$Judge)
  rankings_long$Place <- factor(rankings_long$Place,levels=R:1,labels=toOrdinal(R:1))
  colfunc<-colorRampPalette(c("lightgray","black"))
  
  p <- ggplot(rankings_long,aes(Proposal,fill=Place))+
    theme_bw(base_size=15)+geom_bar()+
    ggtitle("Rankings by Proposal")+
    guides(fill = guide_legend(reverse = TRUE))+
    scale_fill_manual(values=colfunc(max(rankings)))+ylab("Count")+
    theme(panel.grid = element_blank(),legend.position = "bottom")
  p
})


Inconsistencies <- reactive({
  req(input$file1)
  sessionEnvir <- sys.frame()
  if (!is.null(input$file1)){
    tmp_env <- new.env()
    ToyData <- load(input$file1$datapath, tmp_env)
    if(length(ToyData)==1)
      ToyData <- tmp_env[[ToyData]]
    else
      ToyData <- NULL
  }
  ratings <- ToyData$ratings
  rankings <- ToyData$rankings
  M <- ToyData$M #this is the "max" score, which the user should input too!
  
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
  p <- ggplot(consistency,aes(x=Kendall))+
    theme_bw(base_size=15)+geom_histogram()+
    xlim(c(-.1,max(consistency$Kendall)+.1))+
    xlab("Number of Inconsistent Item Pairs")+ylab("Count")+
    ggtitle("Inconsistency Between Ratings and Rankings")+
    theme(panel.grid.major.x = element_blank(),panel.grid.minor.y = element_blank())
  p
})
output$dataT <- renderTable(
  dataTable()
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
