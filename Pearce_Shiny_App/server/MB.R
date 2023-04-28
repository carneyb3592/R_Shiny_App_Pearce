reactive_data$ci <- NULL
reactive_data$point_estimate <- NULL
reactive_data$results_p <- NULL
reactive_data$ratings_long <- NULL
reactive_data$I <- NULL
reactive_data$J <- NULL
reactive_data$R <- NULL

## Code to Run Model
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
  names(ratings_long) <- c("Judge","Proposal","Rating")
  ratings_long$Judge <- factor(ratings_long$Judge)
  ratings_long$Proposal <- factor(ratings_long$Proposal)
  reactive_data$ratings_long <- ratings_long
  
  if(input$MBEstimationMethod == "exact"){
    reactive_data$point_estimate <- fit_mb(Pi=rankings,X=ratings,M=M,method="ASTAR")
    if(input$CI_Included == "yes") {
      ci <- ci_mb(Pi=rankings,X=ratings,M=M,nsamples=20,interval=0.95,all=TRUE,method="ASTAR")
      reactive_data$ci <- ci
      reactive_data$results_p <- data.frame(Proposal=1:J,
                                            PointEstimate=reactive_data$point_estimate$p,
                                            LowerLimit=unlist(ci$ci[1,1:J]),
                                            UpperLimit=unlist(ci$ci[2,1:J]))
    } else { 
      reactive_data$results_p <- data.frame(Proposal=1:J,
                                            PointEstimate=reactive_data$point_estimate$p)
    }
  } else {
    reactive_data$point_estimate <- fit_mb(Pi=rankings,X=ratings,M=M,method="Greedy")
    if(input$CI_Included == "yes") {
      ci <- ci_mb(Pi=rankings,X=ratings,M=M,nsamples=20,interval=0.95,all=TRUE,method="Greedy")
      reactive_data$ci <- ci
      reactive_data$results_p <- data.frame(Proposal=1:J,
                                            PointEstimate=reactive_data$point_estimate$p,
                                            LowerLimit=unlist(ci$ci[1,1:J]),
                                            UpperLimit=unlist(ci$ci[2,1:J]))
    } else { 
      reactive_data$results_p <- data.frame(Proposal=1:J,
                                            PointEstimate=reactive_data$point_estimate$p)
    }
  }

  
})

## Code to Generate First MB Plot (Integrated Scores)
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
      ggtitle("Point Estimates and 95% CI of Quality")+
      theme(panel.grid.major.x = element_blank(),panel.grid.minor.y = element_blank())
  } else {
    results_p <- reactive_data$results_p
    g <- ggplot(results_p,aes(Proposal,y=PointEstimate))+
      theme_bw(base_size=15)+geom_point(size=3)+
      scale_x_continuous(limits=c(.4,max(results_p$Proposal)+.6),
                         breaks=function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))+
      ylim(c(0,1))+ylab("Integrated Score")+
      ggtitle("Point Estimates of Quality")+
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

## Code to Generate Second MB Plot (Estimated Ranks)
mb_rank_plot_input <- reactive({

  if(input$CI_Included == "yes") {
    ci <- reactive_data$ci
    point_estimate <- reactive_data$point_estimate
    I <- reactive_data$I
    J <- reactive_data$J
    R <- reactive_data$R
    results_rank <- t(apply(ci$boostrap_ptheta[,1:J],1,rank))
    results_rankCI <- data.frame(Proposal=1:J,RankEstimate = rank(point_estimate$pi0),
                                 LowerLimit=apply(results_rank,2,function(ranks){quantile(ranks,0.025)}),
                                 UpperLimit=apply(results_rank,2,function(ranks){quantile(ranks,0.975)}))
    
    g <- ggplot(results_rankCI,aes(Proposal,y=RankEstimate,ymin=LowerLimit,ymax=UpperLimit))+
      theme_bw(base_size=15)+geom_errorbar()+geom_point(size=3)+
      ylab("Estimated Rank")+
      theme(panel.grid.major.x = element_blank(),panel.grid.minor.y = element_blank())
  } else {
    point_estimate <- reactive_data$point_estimate
    I <- reactive_data$I
    J <- reactive_data$J
    R <- reactive_data$R
    results_rankCI <- data.frame(Proposal=1:J,RankEstimate = rank(point_estimate$pi0))
    
    g <- ggplot(results_rankCI,aes(Proposal,y=RankEstimate))+
      theme_bw(base_size=15)+geom_point(size=3)+
      ylab("Estimated Rank")+
      theme(panel.grid.major.x = element_blank(),panel.grid.minor.y = element_blank())
  }
  g
})
MB_Rank <- eventReactive(input$plot,{
  mb_rank_plot_input()
})
output$MallowsBinomialRank <- renderPlotly({
  ggplotly(MB_Rank())
})

## Code to Generate Third MB Plot (MB vs. MR Ranks)
mb_mean_plot_input <- reactive({
  
  point_estimate <- reactive_data$point_estimate
  I <- reactive_data$I
  J <- reactive_data$J
  R <- reactive_data$R
  ratings <- reactive_data$Ratings
  
  
  results_comparison <- data.frame(Proposal=1:J,
                                   MeanRatings=rank(apply(ratings,2,mean),ties.method="average"),
                                   MallowsBinomial=rank(point_estimate$pi0))
  ggplot(results_comparison,aes(MallowsBinomial,MeanRatings,label=Proposal))+
    theme_bw(base_size=15)+geom_abline(slope=1,intercept=0,lty=2,color="gray")+
    geom_point(size=3)+
    geom_label_repel()+
    labs(x="Mallows-Binomial Ranks",y="Ranks based on Mean Ratings",
         title="Proposals by rank based on Mallows-Binomial or Mean Ratings")+
    xlim(c(0.5,J+.5))+ylim(c(0.5,J+.5))+
    theme(panel.grid.minor = element_blank())
  
})
MB_Mean <- eventReactive(input$plot,{
  mb_mean_plot_input()
  
})
output$MallowsBinomialMean <- renderPlotly({
  ggplotly(MB_Mean())
})


output$EstimationWarningText <- renderText({
  if(input$MBEstimationMethod == "exact"){
    HTML("Estimation Method <p style='color:red;'> (Warning, running an exact estimation may take a while)</p>")
  } else {
    HTML("Estimation Method")
  }
})
output$CIWarningText <- renderText({
  if(input$CI_Included == "yes"){
    HTML("Include C.I.? <p style='color:red;'> (Warning, including a confidence interval will significantly add to the time.)</p>")
  } else {
    HTML("Include C.I.?")
  }
})

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

output$downloadReport <- downloadHandler(
  filename = function() {
    paste('MBReport.pdf', sep='')
  },
  content = function(file) {
    tempReport <- file.path(tempdir(),"report.Rmd")
    file.copy("report.Rmd",tempReport,overwrite = TRUE)
    params <- list(plot1=mb_quality_plot_input(),
                   plot2=mb_mean_plot_input(),
                   plot3=mb_rank_plot_input())
    rmarkdown::render(tempReport,output_file = file,
                      output_format = "pdf_document",
                      params = params,
                      envir = new.env(parent=globalenv()),
                      
    )
  }
)

