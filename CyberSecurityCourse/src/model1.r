
#plot a scatter plot with linear line of correlation between runs
plot1 <-ggplot(quizStuMod, aes(x = tot, y = numAns, col = factor(run))) +
  geom_point() +
  stat_smooth(method = "lm", se=F) +
  xlab("Ratio of Questions Completed") +
  ylab("Number of Question Attempts")

plot1

ggsave(file.path('graphs', 'plot1.pdf'))

# Plot the density by runs
plot2 <- ggplot(quizStuMod, aes(x=tot, color=factor(run))) +
  geom_density() +
  xlab("Ratio of Questions Completed") +
  ylab("Density")

plot2

ggsave(file.path('graphs', 'plot2.pdf'))

  
  #Create the percentage of students completeing more than 75% of the course questions 
  totRunPerc <- data.frame (Run = c("1","2","3","4","5","6","7"),
                    Percentage = c( sum(quizStatClean1$tot > 0.75) / length(quizStatClean1$tot),  
                                    sum(quizStatClean2$tot > 0.75) / length(quizStatClean2$tot),  
                                    sum(quizStatClean3$tot > 0.75) / length(quizStatClean3$tot),  
                                    sum(quizStatClean4$tot > 0.75) / length(quizStatClean4$tot),  
                                    sum(quizStatClean5$tot > 0.75) / length(quizStatClean5$tot),  
                                    sum(quizStatClean6$tot > 0.75) / length(quizStatClean6$tot),  
                                    sum(quizStatClean7$tot > 0.75) / length(quizStatClean7$tot))
  
  
 

 
 

  
  

  
  
  