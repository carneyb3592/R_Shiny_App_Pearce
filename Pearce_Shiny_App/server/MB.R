### Global Values ##############################################################

reactive_data$ci <- NULL
reactive_data$point_estimate <- NULL
reactive_data$results_p <- NULL
reactive_data$ratings_long <- NULL
reactive_data$I <- NULL
reactive_data$J <- NULL
reactive_data$R <- NULL
reactive_data$results_rankCI <- NULL

################################################################################


### Code to Run Model ##########################################################
run_model <- reactive({
  rankings <- reactive_data$Rankings
  ratings <- reactive_data$Ratings
  M <- reactive_data$M
  
  I <- nrow(ratings)
  J <- ncol(ratings)
  R <- ncol(rankings)
  reactive_data$I <- I
  reactive_data$J <- J
  reactive_data$R <- R
  
  ratings_long <- melt(ratings)
  names(ratings_long) <- c("Reviewer","Proposal","Rating")
  ratings_long$Reviewer <- factor(ratings_long$Reviewer)
  ratings_long$Proposal <- factor(ratings_long$Proposal)
  reactive_data$ratings_long <- ratings_long
  
  if(input$MBEstimationMethod == "exact"){
    reactive_data$point_estimate <- fit_mb(rankings=rankings,ratings=ratings,M=M,method="ASTAR")
    if(is.matrix(reactive_data$point_estimate$p)){
      reactive_data$point_estimate$p <- apply(reactive_data$point_estimate$p,2,mean)
    }
    if(is.matrix(reactive_data$point_estimate$theta)){
      reactive_data$point_estimate$theta <- apply(reactive_data$point_estimate$theta,2,mean)
    }
    if(is.matrix(reactive_data$point_estimate$pi0)){
      reactive_data$point_estimate$rank <- apply(apply(reactive_data$point_estimate$pi0,1,order),1,mean)
    }else{
      reactive_data$point_estimate$rank <- order(reactive_data$point_estimate$pi0)
    }
    if(input$CI_Included == "yes") {
      ci <- ci_mb(rankings=rankings,ratings=ratings,M=M,nsamples=input$bootstrapsample,interval=input$confidencelevel,all=TRUE,method="ASTAR")
      reactive_data$ci <- ci
      reactive_data$results_p <- data.frame(Proposal=1:J,
                                            PointEstimate=reactive_data$point_estimate$p,
                                            LowerLimit=reactive_data$ci$ci[1,1:J],
                                            UpperLimit=reactive_data$ci$ci[2,1:J])
    } else { 
      reactive_data$results_p <- data.frame(Proposal=1:J,
                                            PointEstimate=reactive_data$point_estimate$p)
    }
  } else {
    reactive_data$point_estimate <- fit_mb(rankings=rankings,ratings=ratings,M=M,method="Greedy")
    if(is.matrix(reactive_data$point_estimate$p)){
      reactive_data$point_estimate$p <- apply(reactive_data$point_estimate$p,2,mean)
    }
    if(is.matrix(reactive_data$point_estimate$theta)){
      reactive_data$point_estimate$theta <- apply(reactive_data$point_estimate$theta,2,mean)
    }
    if(is.matrix(reactive_data$point_estimate$pi0)){
      reactive_data$point_estimate$rank <- apply(apply(reactive_data$point_estimate$pi0,1,order),1,mean)
    }else{
      reactive_data$point_estimate$rank <- order(reactive_data$point_estimate$pi0)
    }
    if(input$CI_Included == "yes") {
      ci <- ci_mb(rankings=rankings,ratings=ratings,M=M,nsamples=input$bootstrapsample,interval=input$confidencelevel,all=TRUE,method="Greedy")
      reactive_data$ci <- ci
      reactive_data$results_p <- data.frame(Proposal=1:J,
                                            PointEstimate=reactive_data$point_estimate$p,
                                            LowerLimit=reactive_data$ci$ci[1,1:J],
                                            UpperLimit=reactive_data$ci$ci[2,1:J])
    } else { 
      reactive_data$results_p <- data.frame(Proposal=1:J,
                                            PointEstimate=reactive_data$point_estimate$p)
    }
  }
  
  
})
################################################################################

## Code to Generate First MB Plot (Integrated Scores) ##########################
mb_quality_plot_input <- reactive({
  run_model()
  
  I <- reactive_data$I
  J <- reactive_data$J
  R <- reactive_data$R
  
  ratings_long <- reactive_data$ratings_long
  
  if(input$CI_Included == "yes") {
    ci <- reactive_data$ci
    results_p <- reactive_data$results_p
    g <- ggplot(results_p,aes(Proposal,y=PointEstimate,ymin=LowerLimit,ymax=UpperLimit))+
      theme_bw(base_size=15)+geom_errorbar()+geom_point(size=3)+
      scale_x_continuous(limits=c(.4,max(results_p$Proposal)+.6),
                         breaks=function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))+
      ylim(c(0,1))+ylab("Integrated Score")+
      theme(panel.grid.major.x = element_blank(),panel.grid.minor.y = element_blank())
  } else {
    results_p <- reactive_data$results_p
    g <- ggplot(results_p,aes(Proposal,y=PointEstimate))+
      theme_bw(base_size=15)+geom_point(size=3)+
      scale_x_continuous(limits=c(.4,max(results_p$Proposal)+.6),
                         breaks=function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))+
      ylim(c(0,1))+ylab("Integrated Score")+
      theme(panel.grid.major.x = element_blank(),panel.grid.minor.y = element_blank())
  }
  g
})
MB_Quality <- eventReactive(input$plot,{
  mb_quality_plot_input()
  
})
output$MallowsBinomialQuality <- renderPlotly({
  p <- ggplotly(MB_Quality())
  if(input$CI_Included == "yes") {
    for (i in 1:length(p$x$data)) {
      
      p$x$data[[i]]$text <- paste0(
        "Proposal ", p$x$data[[i]]$x,
        "<br />Integrated Score: ", round(p$x$data[[i]]$y, digits = 3),
        "<br />95% CI Lower Limit: ",round(p$x$data[[i]]$y-p$x$data[[i]]$error_y$arrayminus, digits = 3),
        "<br />95% CI Upper Limit: ",round(p$x$data[[i]]$y+p$x$data[[i]]$error_y$array, digits = 3)
      )
    }
  } else {
    for (i in 1:length(p$x$data)) {
      p$x$data[[i]]$text <- paste0(
        "Proposal ", p$x$data[[i]]$x,
        "<br />Integrated Score: ", round(p$x$data[[i]]$y, digits = 3)
      )
    }
    
  }
  
  p %>% layout(
    spikedistance = -1,
    yaxis = list(
      showspikes = TRUE,
      spikemode = "across",
      showline=TRUE,
      spikedash = 'solid',
      spikethickness=1
    )
  )
})
################################################################################


## Code to Generate Second MB Plot (Estimated Ranks) ###########################
mb_rank_plot_input <- reactive({
  
  if(input$CI_Included == "yes") {
    ci <- reactive_data$ci
    point_estimate <- reactive_data$point_estimate
    I <- reactive_data$I
    J <- reactive_data$J
    R <- reactive_data$R
    results_rankCI <- data.frame(Proposal=1:J,RankEstimate = point_estimate$rank,
                                 LowerLimit=ci$ci_ranks[1,1:J],
                                 UpperLimit=ci$ci_ranks[2,1:J])
    reactive_data$results_rankCI <- results_rankCI
    
    g <- ggplot(results_rankCI,aes(Proposal,y=RankEstimate,ymin=LowerLimit,ymax=UpperLimit))+
      theme_bw(base_size=15)+geom_errorbar()+geom_point(size=3)+
      scale_x_continuous(limits=c(.4,max(results_rankCI$Proposal)+.6),
                         breaks=function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))+
      ylab("Estimated Rank")+
      theme(panel.grid.major.x = element_blank(),panel.grid.minor.y = element_blank())
  } else {
    point_estimate <- reactive_data$point_estimate
    I <- reactive_data$I
    J <- reactive_data$J
    R <- reactive_data$R
    results_rankCI <- data.frame(Proposal=1:J,RankEstimate = point_estimate$rank)
    reactive_data$results_rankCI <- results_rankCI
    g <- ggplot(results_rankCI,aes(Proposal,y=RankEstimate))+
      theme_bw(base_size=15)+geom_point(size=3)+
      scale_x_continuous(limits=c(.4,max(results_rankCI$Proposal)+.6),
                         breaks=function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))+
      ylab("Estimated Rank")+
      theme(panel.grid.major.x = element_blank(),panel.grid.minor.y = element_blank())
  }
  g
})
MB_Rank <- eventReactive(input$plot,{
  mb_rank_plot_input()
})
output$MallowsBinomialRank <- renderPlotly({
  p <- ggplotly(MB_Rank())
  if(input$CI_Included == "yes") {
    for (i in 1:length(p$x$data)) {
      p$x$data[[i]]$text <- paste0(
        "Proposal ", p$x$data[[i]]$x,
        "<br />Rank Estimate: ", round(p$x$data[[i]]$y, digits = 3),
        "<br />95% CI Lower Limit: ",round(p$x$data[[i]]$y-p$x$data[[i]]$error_y$arrayminus, digits = 3),
        "<br />95% CI Upper Limit: ",round(p$x$data[[i]]$y+p$x$data[[i]]$error_y$array, digits = 3)
      )
    }
  } else {
    for (i in 1:length(p$x$data)) {
      p$x$data[[i]]$text <- paste0(
        "Proposal ", p$x$data[[i]]$x,
        "<br />Rank Estimate: ", round(p$x$data[[i]]$y, digits = 3)
      )
    }
    
  }
  p
})

################################################################################

## Code to Generate Third MB Plot (MB vs. MR Ranks) ############################
mb_mean_plot_input <- reactive({
  
  point_estimate <- reactive_data$point_estimate
  I <- reactive_data$I
  J <- reactive_data$J
  R <- reactive_data$R
  ratings <- reactive_data$Ratings
  
  
  results_comparison <- data.frame(Proposal=paste0("Pr.",1:J),
                                   MeanRatings=rank(apply(ratings,2,function(x){mean(x,na.rm=TRUE)}),ties.method="average"),
                                   MallowsBinomial=point_estimate$rank)
  ggplot(results_comparison,aes(MallowsBinomial,MeanRatings,label=Proposal))+
    theme_bw(base_size=15)+geom_abline(slope=1,intercept=0,lty=2,color="gray")+
    geom_point(size=2)+
    geom_label_repel(min.segment.length = 0,force=J,box.padding = .5,point.padding = 0)+
    labs(x="Rank based on Mallows-Binomial",y="Ranks based on Mean Ratings",
         title="Proposals by Rank based on Mallows-Binomial vs. Mean Ratings")+
    xlim(c(0,J+1))+ylim(c(0,J+1))+
    theme(panel.grid.minor = element_blank())
  
})
MB_Mean <- eventReactive(input$plot,{
  mb_mean_plot_input()
  
})
output$MallowsBinomialMean <- renderPlot({
  MB_Mean()
})
################################################################################

### Warnings Text ##############################################################
output$EstimationWarningText <- renderText({
  if(input$MBEstimationMethod == "exact"){
    HTML("<p style='color:red;'> (Warning: Running exact estimation may be slow.)</p>")
  } else {
    HTML("")
  }
})
output$CIWarningText <- renderText({
  if(input$CI_Included == "yes"){
    HTML("<p style='color:red;'> (Warning: Bootstrapped confidence intervals may be slow.)</p>")
  } else {
    HTML("")
  }
})

################################################################################

### Download Functions #########################################################
output$downloadMB <- downloadHandler(
  filename = function() {
    paste('MBQuality.png', sep='')
  },
  content = function(file){
    ggsave(filename = file,
           plot = mb_quality_plot_input(),
           device = "png",
           width = 1920,
           height = 1080,
           units = "px")
  }
)

output$downloadMBRank <- downloadHandler(
  filename = function() {
    paste('MBRank.png', sep='')
  },
  content = function(file){
    ggsave(filename = file,
           plot = mb_rank_plot_input(),
           device = "png",
           width = 1920,
           height = 1080,
           units = "px")
  }
)

output$downloadMBMean <- downloadHandler(
  filename = function() {
    paste('MBMean.png', sep='')
  },
  content = function(file){
    ggsave(filename = file,
           plot = mb_mean_plot_input(),
           device = "png",
           width = 1920,
           height = 1080,
           units = "px")
  }
)

output$donwloadMBQualData <- downloadHandler(
  filename = function() {
    paste('MBQualData.csv', sep='')
  },
  content = function(file){
    write.csv(reactive_data$results_p,file)
  }
)

output$donwloadMBRankData <- downloadHandler(
  filename = function() {
    paste('MBRankData.csv', sep='')
  },
  content = function(file){
    write.csv(reactive_data$results_rankCI,file)
  }
)



################################################################################