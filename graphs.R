### Summary Plots
library(tidyr)

gameSheet %>% 
  ggplot(aes(x=turnovers ,y=rebounds ,colour=winner)) + geom_point() + 
  
# Histograms/bars



BetaSheet %>%
  ggplot(aes(x= position)) + geom_bar(aes(fill=archetype)) + scale_fill_economist() +
  ggtitle("Archetype per Position", subtitle = "Refer to Archetype Key") +
  xlab("Position") +
  scale_x_discrete(breaks=c(1,2,3,4,5),lables=c("Point Guard", "Shooting Guard", "Short Forward", "Power Forward", "Center")) +
  ylab(" ") +
  labs(fill= "Archetype Color Code")



ggplot() + geom_histogram(data = gameSheet[win_team], aes(points), fill = "red", alpha = 0.2) + 
  geom_histogram(data = gameSheet[!win_team], aes(points), fill = "blue", alpha = 0.2) 


pred_win <- glm(winner ~ pg + sg + sf + pf + c, gameSheet, family = binomial)
predict(pred_win, type = "response")

ggpairs(gameSheet[,c(3:16)]) 
