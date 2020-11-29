
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

unique(test$dt)
test <- quizStuMod
test$dt <- normalize(test$dt)
test$dt <- round(test$dt,digits=1)
test <- test[!(test$ft < -5), ]
test <- test[!(test$ft > 3), ]

ggplot(test, aes(x = numCorr, y =tot , col = factor(run))) +
  geom_point() +
  stat_smooth(method = "lm", se=F)

#plot scatter graph
 g = ggplot(data=df, aes(x=tot, y=numAns))
  g + geom_point(  aes(color=run, size=run )) +
  xlab("Percentage of Questions Completed") +
  ylab("Number of Ansers")


  # Plot the density by runs
  ggplot(df, aes(x=tot, color=run)) +
    geom_density() +
    xlab("Percentage of Questions Completed") +
    ylab("Density")
  
  
  #Create the percentage of students completeing more than 75% of the course questions 
  totRunPerc <- data.frame (Run = c("1","2","3","4","5","6","7"),
                    Percentage = c( sum(quizStatClean1$tot > 0.75) / length(quizStatClean1$tot),  
                                    sum(quizStatClean2$tot > 0.75) / length(quizStatClean2$tot),  
                                    sum(quizStatClean3$tot > 0.75) / length(quizStatClean3$tot),  
                                    sum(quizStatClean4$tot > 0.75) / length(quizStatClean4$tot),  
                                    sum(quizStatClean5$tot > 0.75) / length(quizStatClean5$tot),  
                                    sum(quizStatClean6$tot > 0.75) / length(quizStatClean6$tot),  
                                    sum(quizStatClean7$tot > 0.75) / length(quizStatClean7$tot))
  
  
 

 
 
 

  # Histogram plot
  ggplot(df, aes(x=tot, color=run, fill=run)) + 
    geom_histogram(aes(x=tot), alpha=0.3, position="identity") +
                  xlab("Ratio of Questions Completed") +
                  ylab("Number of Students")
                  
 
  dfToNum <- function(data){
    df <- data
    df <- select(df, -c(id))
    df <- as.data.frame(sapply(df, as.numeric))
    df <- data.frame(data$id, test )
    return(df)
  }
  
  str(test)
  pairs(test)
 # install.packages("corrplot")
  #library(corrplot)
  M<-cor(test)
  corrplot(M, method="circle")
 

 # install.packages("PerformanceAnalytics")
  #library("PerformanceAnalytics")

  chart.Correlation(test, histogram=TRUE, pch=19)
  
  
 
  test$dt <- scale(test$dt)
  test$ft <- scale(test$ft)
  test$st <- scale(test$st)
  
  chart.Correlation(test, histogram=TRUE, pch=19)
  
  
  

  
  
  