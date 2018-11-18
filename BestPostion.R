### Points needed to win (given points scored, probability of a win)
model <- glm(winner~ points, gameSheet, family=binomial)
predict(model, type="response") 

### 
model <- glm(winner~ threes, gameSheet, family=binomial)
predict(model, type="response")

###
glm.fit <- glm(winner~rebounds, gameSheet, family = "binomial")

### teams that won, lineup
win_team <- gameSheet$winner==1
ggplot() + geom_histogram(data = gameSheet[win_team,], aes(points), fill = "blue", alpha = 0.4) + 
  geom_histogram(data = gameSheet[!win_team,], aes(points), fill = "red", alpha = 0.4) 

#point guards, win/loss
ggplot(gameSheet, aes(winner, fill=pg)) + geom_histogram(binwidth = .5) +
  ggtitle("Archetypes of Winning/Losing Pointguards") + xlab("Game result") + ylab("Archetype Used")
#shooting guard, win/loss
ggplot(gameSheet, aes(winner, fill=sg)) + geom_histogram(binwidth = .5) +
  ggtitle("Archetypes of Winning/Losing Shootinguards") + xlab("Game result") + ylab("Archetype Used")
#Small forward
ggplot(gameSheet, aes(winner, fill=sf)) + geom_histogram(binwidth = .5) +
  ggtitle("Archetypes of Winning/Losing Small Forward") + xlab("Game result") + ylab("Archetype Used") +
  scale_color_economist()
#Power forward
lables <- c('0' = 'Loss', '1' = 'Win')

ggplot(gameSheet, aes(winner, fill=pf)) + geom_histogram(binwidth = .5) +
  ggtitle("Archetypes of Power Forwards") + ylab("y") +
  scale_x_continuous("Game result (Loss = 0, Win = 1)", breaks = c(0,1)) +
  scale_fill_economist() +
  labs(fill= "Archetype Color Code")

ggplot(gameSheet, aes(sf, fill=assists)) + geom_histogram(binwidth = .5) +
  ggtitle("Archetypes of Small Forwards") + ylab("y") +
  scale_x_discrete("Game result (Loss = 0, Win = 1)") +
  scale_fill_economist() +
  labs(fill= "Archetype Color Code")

# Center
ggplot(gameSheet, aes(winner, fill=c)) + geom_histogram(binwidth = .5) +
  ggtitle("Archytypes of Winning/Losing Center") + xlab("Game result") + ylab("Archtype Used")

# Point guard by Archtype (value added looking at assists)
BetaSheet$points_responsible <- BetaSheet$points + BetaSheet$assists
ggboxplot(BetaSheet[every_pg,], x="archetype", y="points", ylab="Points (Raw)", xlab= "Archtype", title = "Point Guard Effectiveness",  ggtheme = theme_economist())
ggboxplot(BetaSheet[every_pg,],x="archetype",y="points_responsible", ylab="Points Created", xlab= "Archtype", title = "Point Guard Effectiveness (Created)",  ggtheme = theme_economist())

# Power Forward (Arch=2 is dominant)
ggboxplot(BetaSheet[every_pf,], x="archetype", y="points")
ggboxplot(BetaSheet[every_pf,],x="archetype",y="points_responsible")

# Shooting Guard
ggboxplot(BetaSheet[every_sg,], x="archetype", y="points")
ggboxplot(BetaSheet[every_sg,],x="archetype",y="three_ptrs")
ggboxplot(BetaSheet[every_sg,],x="archetype",y="threes")
# check how many threes made v win/loss
# pair point guards with shooting guards

# SHOWS: for point guards Archetype 2 is dominant (over 4)
BetaSheet[every_pg,] %>%
  ggplot(aes(x=points, y=assists, colour=archetype)) + geom_point() + geom_smooth(method = lm, se=FALSE)

# Shows: For powerforward: archetype 2 is best, then 5 (only 1 pt for 3)
BetaSheet[every_pf,] %>%
  ggplot(aes(x=points, y=assists, size=three_ptrs, colour=archetype)) + geom_point() + 
  geom_smooth(method = lm, se=FALSE) + scale_fill_economist() + ggtitle("Power Forward Archetype Data") +
  labs(colour= "Archetype", size= "# of Three Pointers")
  
  

# Shows: for small forward, archetype 1 is dominant. 2/3/4 are comparable, ignore 5
BetaSheet[every_sf,] %>% ggplot(aes(x=steals, y=points_responsible, colour=archetype)) + geom_point(aes(size = three_ptrs)) + 
  labs(colour= "Archetype", size= "# of Three Pointers") + ggtitle("Short Forward Archetype Data")+
  scale_x_continuous("# of Steals") + ylab("Points Created")

BetaSheet %>%
  ggplot(aes(x= factor(position,labels =c("Point Guard", "Shooting Guard", "Short Forward", "Power Forward", "Center")) )) + 
  geom_bar(aes(fill=archetype)) + scale_fill_economist() +
  ggtitle("Archetype per Position", subtitle = "Refer to Archetype Key") +
  #scale_x_discrete(breaks=c(1,2,3,4,5),lables=c("Point Guard", "Shooting Guard", "Short Forward", "Power Forward", "Center")) +
  ylab(" ") + xlab("Position") +
  labs(fill= "Archetype Color Code")


BetaSheet[every_c,] %>%
  group_by(archetype) %>%
  summary(mean(points))
