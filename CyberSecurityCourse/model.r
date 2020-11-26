


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



  


  
  
  
  



  
  
  # Change density plot fill colors by groups
  ggplot(df, aes(x=tot, fill=run)) +
    geom_density()
  # Use semi-transparent fill
  p<-ggplot(df, aes(x=tot, fill=run)) +
    geom_density(alpha=0.3)
  p
  # Add mean lines
  p+ geom_vline(aes(xintercept=mean(tot)),
                color=run, linetype="dashed")
 p
 
 
 
 
 
 
 
 
 
 

