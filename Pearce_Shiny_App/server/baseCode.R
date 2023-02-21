library(rankrate)
library(ggplot2)
library(reshape2)
library(toOrdinal)

# set.seed(1)
# data <- rankrate::rmb(I=20,p=seq(.1,.9,length=5),theta=2,M=10)
# ratings <- data$X
# rankings <- data$Pi
# ToyData <- list(rankings=rankings,ratings=ratings,M=10)
# save(ToyData,file="ToyData.RData")
load("ToyData.RData")
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
  
print(p)
rankings_long <- melt(rankings)
names(rankings_long) <- c("Judge","Place","Proposal")
rankings_long$Judge <- factor(rankings_long$Judge)
rankings_long$Place <- factor(rankings_long$Place,levels=R:1,labels=toOrdinal(R:1))
colfunc<-colorRampPalette(c("lightgray","black"))
  
p2 <- ggplot(rankings_long,aes(Proposal,fill=Place))+
  theme_bw(base_size=15)+geom_bar()+
  ggtitle("Rankings by Proposal")+
  guides(fill = guide_legend(reverse = TRUE))+
  scale_fill_manual(values=colfunc(max(rankings)))+ylab("Count")+
  theme(panel.grid = element_blank(),legend.position = "bottom")

print(p2)

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
p3 <- ggplot(consistency,aes(x=Kendall))+
  theme_bw(base_size=15)+geom_histogram()+
  xlim(c(-.1,max(consistency$Kendall)+.1))+
  xlab("Number of Inconsistent Item Pairs")+ylab("Count")+
  ggtitle("Inconsistency Between Ratings and Rankings")+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor.y = element_blank())
print(p3)
  
  ## Model Fitting: Exact
point_estimate <- fit_mb(Pi=rankings,X=ratings,M=M,method="ASTAR")
ci <- ci_mb(Pi=rankings,X=ratings,M=M,nsamples=20,interval=0.95,all=TRUE,method="ASTAR")
results_p <- data.frame(Proposal=1:J,PointEstimate=point_estimate$p,
                          LowerLimit=unlist(ci$ci[1,1:J]),UpperLimit=unlist(ci$ci[2,1:J]))

p4 <- ggplot(results_p,aes(Proposal,y=PointEstimate,ymin=LowerLimit,ymax=UpperLimit))+
  theme_bw(base_size=15)+geom_errorbar()+geom_point(size=3)+
  ylim(c(0,1))+ylab("Estimated Quality")+
  ggtitle("Point Estimates and 95% CI of Quality","Exact Estimation Algorithm")+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor.y = element_blank())
print(p4)  
  ## Model Fitting: Approximate
point_estimate <- fit_mb(Pi=rankings,X=ratings,M=M,method="Greedy")
ci <- ci_mb(Pi=rankings,X=ratings,M=M,nsamples=20,interval=0.95,all=TRUE,method="Greedy")
results_p <- data.frame(Proposal=1:J,PointEstimate=point_estimate$p,
                        LowerLimit=unlist(ci$ci[1,1:J]),UpperLimit=unlist(ci$ci[2,1:J]))


p5 <- ggplot(results_p,aes(Proposal,y=PointEstimate,ymin=LowerLimit,ymax=UpperLimit))+
  theme_bw(base_size=15)+geom_errorbar()+geom_point(size=3)+
  ylim(c(0,1))+ylab("Estimated Quality")+
  ggtitle("Point Estimates and 95% CI of Quality","Approximate Estimation Algorithm")+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor.y = element_blank())
print(p5)
