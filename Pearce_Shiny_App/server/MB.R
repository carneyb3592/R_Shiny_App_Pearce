MB <- eventReactive(input$plot,{
  rankings <- reactive_data$Rankings
  ratings <- reactive_data$Ratings
  M <- reactive_data$M
  #this is the "max" score, which the user should input too!
  ## Simple EDA
  I <- nrow(ratings)
  J <- ncol(ratings)
  R <- ncol(rankings)
  
  ratings_long <- melt(ratings)
  names(ratings_long) <- c("Judge","Proposal","Rating")
  ratings_long$Judge <- factor(ratings_long$Judge)
  ratings_long$Proposal <- factor(ratings_long$Proposal)
  
  if(input$MBEstimationMethod == "exact"){
    point_estimate <- fit_mb(Pi=rankings,X=ratings,M=M,method="ASTAR")
    ci <- ci_mb(Pi=rankings,X=ratings,M=M,nsamples=20,interval=0.95,all=TRUE,method="ASTAR")
    results_p <- data.frame(Proposal=1:J,PointEstimate=point_estimate$p,
                            LowerLimit=unlist(ci$ci[1,1:J]),UpperLimit=unlist(ci$ci[2,1:J]))
    
    g <- ggplot(results_p,aes(Proposal,y=PointEstimate,ymin=LowerLimit,ymax=UpperLimit))+
      theme_bw(base_size=15)+geom_errorbar()+geom_point(size=3)+
      ylim(c(0,1))+ylab("Estimated Quality")+
      ggtitle("Point Estimates and 95% CI of Quality","Exact Estimation Algorithm")+
      theme(panel.grid.major.x = element_blank(),panel.grid.minor.y = element_blank())
  } else {
    point_estimate <- fit_mb(Pi=rankings,X=ratings,M=M,method="Greedy")
    ci <- ci_mb(Pi=rankings,X=ratings,M=M,nsamples=20,interval=0.95,all=TRUE,method="Greedy")
    results_p <- data.frame(Proposal=1:J,PointEstimate=point_estimate$p,
                            LowerLimit=unlist(ci$ci[1,1:J]),UpperLimit=unlist(ci$ci[2,1:J]))
    
    g <- ggplot(results_p,aes(Proposal,y=PointEstimate,ymin=LowerLimit,ymax=UpperLimit))+
      theme_bw(base_size=15)+geom_errorbar()+geom_point(size=3)+
      ylim(c(0,1))+ylab("Estimated Quality")+
      ggtitle("Point Estimates and 95% CI of Quality","Approximate Estimation Algorithm")+
      theme(panel.grid.major.x = element_blank(),panel.grid.minor.y = element_blank())
  }
  
  g
})


output$MallowsBinomial <- renderPlot(
  MB()
)

output$EstimationWarningText <- renderText({
  if(input$MBEstimationMethod == "exact"){
    HTML("Estimation Method <p style='color:red;'> (Warning, running an exact estimation may take a while)</p>")
  } else {
    HTML("Estimation Method")
  }
})

output$CIWarningText <- renderText({
  if(input$CI_Included == "yes"){
    HTML("Include C.I.? <p style='color:red;'> (Warning, including a confidence interval will significanty add to the time.)</p>")
  } else {
    HTML("Include C.I.?")
  }
})