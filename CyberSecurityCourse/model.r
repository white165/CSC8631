


#prepping the data for the scatter plot 

df1 <- data.frame(run="1", tot=quizStatClean1$tot, numAns=quizStatClean1$numAns)
df2 <- data.frame(run="2", tot=quizStatClean2$tot, numAns=quizStatClean2$numAns)
df3 <- data.frame(run="3", tot=quizStatClean3$tot, numAns=quizStatClean3$numAns)
df4 <- data.frame(run="4", tot=quizStatClean4$tot, numAns=quizStatClean4$numAns)
df5 <- data.frame(run="5", tot=quizStatClean5$tot, numAns=quizStatClean5$numAns)
df6 <- data.frame(run="6", tot=quizStatClean6$tot, numAns=quizStatClean6$numAns)
df7 <- data.frame(run="7", tot=quizStatClean7$tot, numAns=quizStatClean7$numAns)
df <- rbind( df1,  df2, df3, df4, df5, df6, df7)

#removing the anomalies 
df <- df[!(df$numAns > 75), ]


#plot scatter graph
 g = ggplot(data=df, aes(x=tot, y=numAns))
  g + geom_point(  aes(color=run )) +
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
                  
 
  
  
  
 
 
 
 

