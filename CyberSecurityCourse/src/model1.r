
#plot a scatter plot with linear line of correlation between runs
plot1 <-ggplot(quizStuMod, aes(x = tot, y = numAns, col = factor(run))) +
  geom_point() +
  stat_smooth(method = "lm", se=F) +
  labs(x = "Ratio of Questions Completed", y = "Number of Question Attempts", color = "run" ) +
  theme_classic()

plot1

ggsave(file.path('graphs', 'plot1.pdf'))

# Plot the density by runs
plot2 <- ggplot(quizStuMod, aes(x=tot, color=factor(run))) +
  geom_density() +
  labs(x = "Ratio of Questions Completed", y = "Density", color = "run" ) +
  theme_classic()


plot2

ggsave(file.path('graphs', 'plot2.pdf'))

  

  
  
  

 
 

  
  

  
  
  