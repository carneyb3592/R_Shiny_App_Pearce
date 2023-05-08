reactive_data <- reactiveValues()
reactive_data$Rankings = NULL
reactive_data$Ratings = NULL
reactive_data$M = NULL

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


observeEvent(input$upload,{
  sessionEnvir <- sys.frame()
  if (!is.null(input$RankingsFile) && !is.null(input$RatingsFile)){
    ratings = as.matrix(read.csv(input$RatingsFile$datapath))
    rankings = as.matrix(read.csv(input$RankingsFile$datapath))
    colnames(rankings) <- sub("^X", "", colnames(rankings))
    colnames(ratings) <- sub("^X", "", colnames(ratings))
    
    M <- as.numeric(input$RatingsMValue)
    
    
    reactive_data$Ratings <- ratings
    reactive_data$Rankings <- rankings
    reactive_data$M <- M
  }
})

rankings_table_input <- reactive({
  data <- as.matrix(reactive_data$Rankings)
  colnames(data) <- paste0(toOrdinal(1:ncol(data))," Place")
  rownames(data) <- paste0("Judge ",seq(1:nrow(data)))
  data
})

output$dataTableRank <- DT::renderDataTable(
  rankings_table_input(),
  options = list(
    scrollX = TRUE,
    pageLength = 10,
    server = TRUE,
    scrollCollapse = TRUE,
    dom = "tipr"
  )
)

ratings_table_input <- reactive({
  data <- as.matrix(reactive_data$Ratings)
  colnames(data) <- paste0("Proposal ",seq(1:ncol(data)))
  rownames(data) <- paste0("Judge ",seq(1:nrow(data)))
  data
})

output$dataTableRate <- DT::renderDataTable(
  ratings_table_input(),
  options = list(
    scrollX = TRUE,
    pageLength = 10,
    server = TRUE,
    scrollCollapse = TRUE,
    dom = "tipr"
  )
)

ratings_plot_input <- reactive({
  rankings <- reactive_data$Rankings
  ratings <- reactive_data$Ratings
  M <- reactive_data$M
  
  #this is the "max" score, which the user should input too!
  ## Simple EDA
  I <- nrow(ratings)
  J <- ncol(ratings)
  R <- ncol(rankings)
  colnames(rankings) <- sub("^X", "", colnames(rankings))
  
  ratings_long <- melt(ratings)
  names(ratings_long) <- c("Judge","Proposal","Rating")
  ratings_long$Judge <- factor(ratings_long$Judge)
  ratings_long$Proposal <- factor(ratings_long$Proposal)
  
  ggplot(ratings_long,aes(Proposal,Rating))+
    theme_bw(base_size=15)+geom_boxplot()+ylim(c(0,M))+
    ggtitle("Ratings by Proposal",paste0("0 = best; ",M," = worst"))+
    theme(panel.grid.major.x = element_blank(),panel.grid.minor.y = element_blank())
})

output$RatingsPlot <- renderPlotly({
  g <- ratings_plot_input()
  p <- ggplotly(g)
  for (i in 1:length(p$x$data)) {
    #print(p$x$data[[i]]$x)
    #print(p$x$data[[i]]$y)
    #print(p$x$data[[i]]$text)
    print(p$x$data[[i]])
    
    #print(p$x$data[[i]])$hoverinfo 
    #p$x$data[[i]]$hoverinfo = "text"
    
  }
  p
})

rankings_plot_input <- reactive({
  rankings <- reactive_data$Rankings
  ratings <- reactive_data$Ratings
  M <- reactive_data$M
  #this is the "max" score, which the user should input too!
  
  ## Simple EDA
  I <- nrow(ratings)
  J <- ncol(ratings)
  R <- ncol(rankings)
  rankings_long <- melt(rankings)
  names(rankings_long) <- c("Judge","Place","Proposal")
  rankings_long$Judge <- factor(rankings_long$Judge)
  rankings_long$Place <- factor(rankings_long$Place,
                                levels = paste0(ncol(rankings):1),
                                labels = toOrdinal(ncol(rankings):1))
  colfunc<-colorRampPalette(c("#aa076b","#61045f"))
  ggplot(rankings_long,aes(Proposal,fill=Place)) +
    theme_bw(base_size=15)+geom_bar()+
    ggtitle("Rankings by Proposal")+
    scale_fill_manual(values=colfunc(max(rankings,na.rm=T)))+ylab("Count")+
    theme(panel.grid = element_blank(),legend.position = "bottom") + scale_x_continuous(breaks = 1:J)
})

output$RankingsPlot <- renderPlotly({
  g <- rankings_plot_input()
  p <- ggplotly(g)
  
  for (i in 1:length(p$x$data)) {
    #print(p$x$data[[i]]$x)
    #print(p$x$data[[i]]$y)
    #print(p$x$data[[i]]$text)
    #print(p$x$data[[i]])
    
    p$x$data[[i]]$text <- paste0(
      p$x$data[[i]]$y, " judges put proposal ",
      p$x$data[[i]]$x, "<br /> in ", p$x$data[[i]]$name,
      " Place"
      
    )
  }
  p %>% config(displayModeBar = F) %>% reverse_legend_labels()
})


reverse_legend_labels <- function(plotly_plot) {
  n_labels <- length(plotly_plot$x$data)
  plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
  plotly_plot
}


inconsistencies_plot_input <- reactive({
  rankings <- reactive_data$Rankings
  ratings <- reactive_data$Ratings
  M <- reactive_data$M
  
  
  ## Simple EDA
  I <- nrow(ratings)
  J <- ncol(ratings)
  R <- ncol(rankings)
  
  consistency <- data.frame(Judge=1:nrow(ratings),Kendall=NA)
  for(i in 1:nrow(ratings)){
    kendall <- 0
    for(j1 in 1:(J-1)){for(j2 in (j1+1):J){
      if(any(is.na(ratings[i,c(j1,j2)]))==FALSE){
        if(ratings[i,j1] < ratings[i,j2]){
          if(j2 %in% rankings[i,] & !(j1 %in% rankings[i,])){kendall <- kendall+1
          }else if(j1%in%rankings[i,] & j2%in%rankings[i,]){
            if(which(rankings[i,]==j1)>which(rankings[i,]==j2)){kendall <- kendall + 1}}
        }else if(ratings[i,j1] > ratings[i,j2]){
          if(j1 %in% rankings[i,] & !(j2 %in% rankings[i,])){kendall <- kendall+1
          }else if(j2%in%rankings[i,] & j1%in%rankings[i,]){
            if(which(rankings[i,]==j2)>which(rankings[i,]==j1)){kendall <- kendall + 1}}
        }
      }
      
    }}
    consistency[i,2] <- kendall
  }
  
  ggplot(consistency,aes(x=Kendall))+
    theme_bw(base_size=15)+geom_histogram(binwidth = 0.5)+
    scale_x_continuous(limits=c(-.6,max(consistency$Kendall)+.6),
                       breaks=function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))+
    xlab("Number of Inconsistent Item Pairs")+ylab("Count")+
    ggtitle("Inconsistency Between Ratings and Rankings")+
    theme(panel.grid.major.x = element_blank(),panel.grid.minor.y = element_blank())
})

output$InconsistenciesPlot <- renderPlotly({
  g <- inconsistencies_plot_input()
  p <- ggplotly(g)
  p %>% config(displayModeBar = F)
})

output$RankingsText <- renderText(
  reactive_data$M
)

##DOWNLOAD FUNCTIONS########################################################

output$downloadRatings <- downloadHandler(
  filename = function() {
    paste('ratingsplot.png', sep='')
  },
  content = function(file){
    ggsave(file,
      plot = ratings_plot_input(),
      device = "png",
      width = 1920,
      height = 1080,
      units = "px") 
  }
)
output$downloadRankings <- downloadHandler(
  filename = function() {
    paste('rankingsplot.png', sep='')
  },
  content = function(file){
    ggsave(file,
           plot = rankings_plot_input(),
           device = "png",
           width = 1920,
           height = 1080,
           units = "px") 
  }
)

output$downloadInconsistencies <- downloadHandler(
  filename = function() {
    paste('inconsistenciesplot.png', sep='')
  },
  content = function(file){
    ggsave(file,
           plot = inconsistencies_plot_input(),
           device = "png",
           width = 1920,
           height = 1080,
           units = "px")
  }
)


output$ToyDataDescription <- reactive({
  selectedData <- input$toyfile
  text <- switch (input$toyfile,
    ToyData1 = "Toy Data 1 contains 3 proposals and 16 judges. 
    The dataset demonstrates tie-breaking equally rated proposals using rankings.",
    ToyData2 = "Toy Data 2 contains 8 proposals and 16 judges, 
    who each only provided top-3 rankings. The dataset demonstrates improved decision-making 
    even with partial rankings.",
    ToyData3 = "Toy Data 3 contains 3 proposals and 16 judges.
    The dataset demonstrates the ability of Mallows-Binomial to analyze data when judges
    provide internally inconsistent sets of ratings and rankings."
  )
})