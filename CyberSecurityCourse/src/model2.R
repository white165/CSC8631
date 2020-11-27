

#prepping the data for the scatter plot 

df1 <- data.frame(run=1, quizCleanQQ1)
df2 <- data.frame(run=2, quizCleanQQ2)
df3 <- data.frame(run=3, quizCleanQQ3)
df4 <- data.frame(run=4, quizCleanQQ4)
df5 <- data.frame(run=5, quizCleanQQ5)
df6 <- data.frame(run=6, quizCleanQQ6)
df7 <- data.frame(run=7, quizCleanQQ7)
quizQuestionStat <- rbind( df1,  df2, df3, df4, df5, df6, df7)

quizQuestionStat <- data.frame(quizQuestionStat, StuAns = (quizQuestionStat$numStu / quizQuestionStat$numAns))



 pairs(select(quizQuestionStat, -c(qq)))

 

 
 
 
 
 ggplot(quizQuestionStat, aes(x = tot, y = StuAns, col = factor(wn))) +
   geom_point() +
   stat_smooth(method = "lm", se=F)
 
 
 
 

 
 ggplot(quizQuestionStat, aes(x = tot, y = numAns, color = factor(wn) )) +
   geom_point() +
   stat_smooth(method = "lm")
 
 
 
 
 
 # Histogram plot
 ggplot(quizQuestionStat, aes(x=tot, color=factor(run), fill=factor(run))) + 
   geom_boxplot(alpha=0.6)
 
 
 