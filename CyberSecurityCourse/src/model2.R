





quizQueMod <- data.frame(quizQueMod, StuAns = (quizQueMod$numStu / quizQueMod$numAns))



 pairs(quizQueMod)

 

 
 
 
 
 
 
 ggplot(quizQueMod, aes(x = tot, y = numStu, col = factor(run))) +
   geom_point() +
   stat_smooth(method = "lm", se=F)
 
 
 #When we look at run one, the distribution is completely different, and actually nor bad retaining 50% of the students completing the module. 
 #However, this is not the case for the other 6 runs. 
 #To get a better understanding run 1 was removed from the df, the new df is called quizQueMod1
 
 #Now when plotting this against the week number theres a clear, almost clusterable distribusion as the weeks matures. In fact, these number are quite predictable. 
 
 ggplot(quizQueMod1, aes(x = tot, y = numStu, col = factor(wn))) +
    geom_point() +
    stat_smooth(method = "lm", se=F)
 
 # Histogram plot
 ggplot(quizQueMod1, aes(y=numStu, color=factor(wn), fill=factor(wn))) + 
    geom_boxplot(alpha=0.6)
 
 
 
 #So now this is quite predictable, but what happens in week two to make people stop engaging in the quiz quesitons 
 
 quizQueMod2 <-quizQueMod1[(!quizQueMod1$wn == 1), ]
 
 # Box plot
 ggplot(quizQueMod2, aes(y=numStu, x=numCorr, color=factor(qq), fill=factor(qq))) + 
    geom_boxplot(alpha=0.6)
 
 
 


 quizQueMod1 <-quizQueMod[(quizQueMod$wn == 2), ]
 
 
 ggplot(quizQueMod1, aes(x = tot, y = numAns, col = factor(sn))) +
    geom_point() +
    stat_smooth(method = "lm", se=F)
 
 
 
 
 
 
 

 
 
 