









 pairs(quizQueMod)

 

 
 
 
 
 
 
 ggplot(quizQueMod, aes(x = tot, y = numStu, col = factor(run))) +
   geom_point() +
   stat_smooth(method = "lm", se=F)
 
 

 ggplot(quizQueMod1, aes(x = tot, y = numStu, col = factor(wn))) +
    geom_point() +
    stat_smooth(method = "lm", se=F)
 
 # Histogram plot
 ggplot(quizQueMod1, aes(y=numStu, color=factor(wn), fill=factor(wn))) + 
    geom_histogram(alpha=0.6)
 
 
 
 #So now this is quite predictable, but what happens in week two to make people stop engaging in the quiz quesitons 
 
 quizQueMod2 <-quizQueMod1[(!quizQueMod1$wn == 1), ]
 
 # Box plot
 ggplot(quizQueMod1, aes(y=numAns, x=numCorr, color=factor(qq), fill=factor(qq))) + 
    geom_boxplot(alpha=0.6)
 
 
 


 #quizQueMod1 <-quizQueMod1[(quizQueMod$wn == 2), ]
 
 
 ggplot(quizQueMod1, aes(x = tot, y = numAns, col = factor(sn))) +
    geom_point() +
    stat_smooth(method = "lm", se=F)
 
 
 
 
 
 
 

 
 
 