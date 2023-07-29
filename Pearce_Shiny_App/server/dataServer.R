### Global Values ##############################################################
reactive_data <- reactiveValues()

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
  for(i in 1:ncol(rankings)){if(all(is.na(rankings[,i]))){R <- i-1;break()}}
  rankings_long <- na.exclude(melt(rankings))
  names(rankings_long) <- c("Reviewer","Place","Proposal")
  rankings_long$Reviewer <- factor(rankings_long$Reviewer)
  rankings_long$Proposal <- factor(rankings_long$Proposal,levels=1:J)
  rankings_long$Place <- factor(rankings_long$Place,
                                levels = paste0(R:1),
                                labels = toOrdinal(R:1))
  colfunc<-colorRampPalette(c("#def2f1","#3AAFA9"))
  ggplot(rankings_long,aes(Proposal,fill=Place)) +
    theme_bw(base_size=15)+geom_bar()+
    ggtitle("Rankings by Proposal",paste0(input$toyfile))+
    scale_fill_manual(values=colfunc(R))+ylab("Count")+scale_x_discrete(drop=F)+
    theme(panel.grid = element_blank(),legend.position = "bottom")
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
  p %>% config(displayModeBar = T) %>% reverse_legend_labels() %>%
    layout(legend = list(orientation = "h",xanchor = "center",x = 0.5))
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
    return(reactive_data$M)
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
  
  which_rankings <- which(!apply(!is.na(rankings),1,function(pi){all(pi==FALSE)}))
  consistency <- data.frame(Reviewer=which_rankings,Kendall=NA)
  for(ind in seq_along(which_rankings)){
    i <- which_rankings[ind]
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
    consistency[ind,2] <- kendall
  }
  consistency$Reviewer <- factor(consistency$Reviewer,1:I)
  
  ggplot(consistency,aes(x=Reviewer,y=Kendall))+
    theme_bw(base_size=15)+geom_bar(stat="identity")+
    xlab("Reviewer")+ylab("Count")+
    ggtitle("Reviewer-Level Inconsistency")+
    theme(panel.grid.major.x = element_blank(),panel.grid.minor.y = element_blank())
})

output$InconsistenciesPlot <- renderPlotly({
  g <- inconsistencies_plot_input()
  p <- ggplotly(g)
  p %>% config(displayModeBar = F)
})

### Inconsistencies Plot 2 #######################################################
inconsistenciesproposal_plot_input <- reactive({
  rankings <- reactive_data$Rankings
  ratings <- reactive_data$Ratings
  M <- reactive_data$M
  
  I <- nrow(ratings)
  J <- ncol(ratings)
  R <- ncol(rankings)
  
  consistency_proposals <- data.frame(Proposal=1:J,Kendall=NA)
  ranks <- matrix(unlist(lapply(1:J,function(j){
    unlist(apply(rankings,1,function(pi){ifelse(j%in%pi,which(pi==j),J+1)}))})),ncol=J)
  
  for(j in 1:J){
    kendall <- 0
    for(j2 in setdiff(1:J,j)){
      true_ranks <- which(ranks[,j] != ranks[,j2])
      kendall <- kendall + sum((ranks[true_ranks,j] < ranks[true_ranks,j2] & ratings[true_ranks,j] > ratings[true_ranks,j2]) | (ranks[true_ranks,j] > ranks[true_ranks,j2] & ratings[true_ranks,j] < ratings[true_ranks,j2]),na.rm=T)
    }
    consistency_proposals[j,2] <- kendall
  }
  consistency_proposals$Proposal <- as.factor(consistency_proposals$Proposal)
  
  ggplot(consistency_proposals,aes(x=Proposal,y=Kendall))+
    theme_bw(base_size=15)+geom_bar(stat="identity")+
    ylim(c(0,max(consistency_proposals$Kendall,1)))+
    xlab("Proposal")+ylab("Count")+
    ggtitle("Proposal-Level Inconsistency")+
    theme(panel.grid.major.x = element_blank(),panel.grid.minor.y = element_blank())
})

output$InconsistenciesProposalPlot <- renderPlotly({
  g <- inconsistenciesproposal_plot_input()
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

output$downloadInconsistenciesProposal <- downloadHandler(
  filename = function() {
    paste('inconsistenciesproposalplot.png', sep='')
  },
  content = function(file){
    ggsave(file,
           plot = inconsistenciesproposal_plot_input(),
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