### Global Values ##############################################################
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
################################################################################

### Rankings Table Output ######################################################
rankings_table_input <- reactive({
  data <- as.matrix(reactive_data$Rankings)
  colnames(data) <- paste0(toOrdinal(1:ncol(data))," Place")
  rownames(data) <- paste0("Reviewer ",seq(1:nrow(data)))
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

################################################################################

#### Ratings Data Output ########################################################
ratings_table_input <- reactive({
  data <- as.matrix(reactive_data$Ratings)
  colnames(data) <- paste0("Proposal ",seq(1:ncol(data)))
  rownames(data) <- paste0("Reviewer ",seq(1:nrow(data)))
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

################################################################################


### Ratings Plot ###############################################################
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
  names(ratings_long) <- c("Reviewer","Proposal","Rating")
  ratings_long$Reviewer <- factor(ratings_long$Reviewer)
  ratings_long$Proposal <- factor(ratings_long$Proposal)
  
  ggplot(ratings_long,aes(Proposal,Rating))+
    theme_bw(base_size=15)+geom_boxplot()+ylim(c(0,M))+
    ggtitle("Ratings by Proposal",paste0("0 = best; ",M," = worst"))+
    theme(panel.grid.major.x = element_blank(),panel.grid.minor.y = element_blank())
})

output$RatingsPlot <- renderPlotly({
  g <- ratings_plot_input()
  p <- ggplotly(g)
  p
})

################################################################################

### Rankings Plot ##############################################################
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
  names(rankings_long) <- c("Reviewer","Place","Proposal")
  rankings_long$Reviewer <- factor(rankings_long$Reviewer)
  rankings_long$Place <- factor(rankings_long$Place,
                                levels = paste0(ncol(rankings):1),
                                labels = toOrdinal(ncol(rankings):1))
  colfunc<-colorRampPalette(c("#e5f5e0","#a1d99b","#31a354"))
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
    p$x$data[[i]]$text <- paste0(
      p$x$data[[i]]$y, " reviewers put proposal ",
      p$x$data[[i]]$x, "<br /> in ", p$x$data[[i]]$name,
      " place."
      
    )
  }
  p %>% config(displayModeBar = F) %>% reverse_legend_labels()
})


reverse_legend_labels <- function(plotly_plot) {
  n_labels <- length(plotly_plot$x$data)
  plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
  plotly_plot
}

output$RankingsText <- renderText({
  if (csvdata_status$uploaded == 1){
    return(reactive_data$M)
  } else {
    return(max(reactive_data$Ratings,na.rm = TRUE))
  }
})
################################################################################

### Inconsistencies Plot #######################################################
inconsistencies_plot_input <- reactive({
  rankings <- reactive_data$Rankings
  ratings <- reactive_data$Ratings
  M <- reactive_data$M
  
  
  ## Simple EDA
  I <- nrow(ratings)
  J <- ncol(ratings)
  R <- ncol(rankings)
  
  consistency <- data.frame(Reviewer=1:nrow(ratings),Kendall=NA)
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

################################################################################



###DOWNLOAD FUNCTIONS###########################################################

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
################################################################################


### Data Description ###########################################################
output$ToyDataDescription <- reactive({
  selectedData <- input$toyfile
  text <- switch (input$toyfile,
    ToyData1 = "Toy Data 1 contains 3 proposals and 16 reviewers. 
    The data set demonstrates tie-breaking equally rated proposals using rankings.",
    ToyData2 = "Toy Data 2 contains 8 proposals and 16 reviewers, 
    who each only provided top-3 rankings. The data set demonstrates improved decision-making 
    even with partial rankings.",
    ToyData3 = "Toy Data 3 contains 3 proposals and 16 reviewers.
    The data set demonstrates the ability of Mallows-Binomial to analyze data when reviewers
    provide internally inconsistent sets of ratings and rankings.",
    AIBS = "AIBS contains real grant panel review data from the American Institute of Biological
    Sciences (AIBS), in which 12 reviewers assessed 28 proposals using top-6 partial rankings and 
    a 41-point rating scale. The data set demonstrates the ability of Mallows-Binomial to estimate 
    overall preferences with uncertainty, even with partial rankings, missing data, and conflicts of interest."
  )
})