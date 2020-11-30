


 

#Plot the scatter plot matrix 
plot3 <- ggplot(quizQueMod, aes(x = tot, y = numStu, col = factor(run))) +
    geom_point() +
    stat_smooth(method = "lm", se=F) +
    xlab("Ratio of Questions Completed") +
    ylab("Ratio of Students")

plot3

ggsave(file.path('graphs', 'plot3.pdf'))
 
 
 
# excluding run 1 from the model
quizQueMod1 <- rbind(df2,df3, df4, df5, df6, df7)
 
#scatter plot with linear regression
plot4 <- ggplot(quizQueMod1, aes(x = tot, y = numStu, col = factor(wn))) +
    geom_point() +
    stat_smooth(method = "lm", se=F) +
    xlab("Ratio of Questions Completed") +
    ylab("Ratio of Students")
 
plot4

ggsave(file.path('graphs', 'plot4.pdf'))




#removing week one
quizQueMod2 <-quizQueMod1[(!quizQueMod1$wn == 1), ]

# Box plot
plot5 <- ggplot(quizQueMod2, aes(y=numAns, x=numCorr, color=factor(qq), fill=factor(qq))) + 
    geom_boxplot(alpha=0.6) +
    xlab("Ratio of Questions Completed") +
    ylab("Number of Attemtps")

plot5

ggsave(file.path('graphs', 'plot5.pdf'))

 