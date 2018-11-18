### DATA CLEANING ###
# Import 
library(readxl)
library(dplyr)
BetaSheet <- read_excel("BetaSheet.xlsx")
# Sort Beta Sheet
BetaSheet <- BetaSheet %>% group_by(game_id, team)
View(BetaSheet)
BetaSheet$position <- factor(BetaSheet$position)
BetaSheet$archetype <- factor(BetaSheet$archetype)

# Making Team Sheet
gameSheet <- data.frame(matrix(155,100)) 
# Total Team Points
teamScores <- BetaSheet %>% group_by(game_id, team) %>% summarize(points = sum(points))
# Toal Team Rebounds
teamRebounds <- BetaSheet %>% group_by(game_id, team) %>% summarize(rebounds = sum(rebounds))
# Total Team Assists
teamAssists <- BetaSheet %>% group_by(game_id, team) %>% summarize(assists = sum(assists))
# Total Team Blocks
teamBlocks <- BetaSheet %>% group_by(game_id, team) %>% summarize(blocks = sum(blocks))
# Total Team Turnovers
teamTurnovers <- BetaSheet %>% group_by(game_id, team) %>% summarize(turnovers = sum(turnovers))
# Total Team Steals
teamSteals <- BetaSheet %>% group_by(game_id, team) %>% summarize(steals = sum(steals))
# Threes #curry
teamThrees <- BetaSheet %>% group_by(game_id, team) %>% summarize(threes = sum(three_ptrs))

### Total Team ### 
gameSheet <- left_join(teamScores, teamRebounds, by = c("game_id", "team"))
gameSheet <- left_join(gameSheet, teamAssists, by = c("game_id", "team"))
gameSheet <- left_join(gameSheet, teamBlocks, by = c("game_id", "team"))
gameSheet <- left_join(gameSheet, teamTurnovers, by = c("game_id", "team"))
gameSheet <- left_join(gameSheet, teamSteals, by = c("game_id", "team"))
gameSheet <- left_join(gameSheet, teamThrees, by = c("game_id", "team"))

# HomeWin 0/1
gameSheet[,"winner"] <- c(1,0,0,1,1,0,1,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,0,1,1,0,0,1,0,1)
gameSheet[,"pt_dif"] <- c(2,-2,-6,6,5,-5,5,-5, 25,-25,-22,22,6,-6,-10,10,-3,3, 5,-5,-9,9,-4,4,11,-11,-27,27, -1, 1)

### Subsetting Archtypes
gameSheet[,"pg"] <- NA
gameSheet[,"sg"] <- NA
gameSheet[,"sf"] <- NA
gameSheet[,"pf"] <- NA
gameSheet[,"c"] <- NA

every_pg <- BetaSheet$position == 1
gameSheet$pg <- BetaSheet$archetype[every_pg]
every_sg <- BetaSheet$position == 2
gameSheet$sg <- BetaSheet$archetype[every_sg]
every_sf <- BetaSheet$position == 3
gameSheet$sf <- BetaSheet$archetype[every_sf]
every_pf <- BetaSheet$position == 4
gameSheet$pf <- BetaSheet$archetype[every_pf]
every_c <- BetaSheet$position == 5
gameSheet$c <- BetaSheet$archetype[every_c]

as.factor(gameSheet$pg)
as.factor(gameSheet$sg)
as.factor(gameSheet$sf)
as.factor(gameSheet$pf)
as.factor(gameSheet$c)








