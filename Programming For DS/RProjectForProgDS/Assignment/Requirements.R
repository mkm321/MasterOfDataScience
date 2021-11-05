# Loading JSON files
library("rjson")
## Loading matchResults
matchResults <- fromJSON(file = "matchResults.json")
# matchResults
# class(matchResults)
# matchResults[[2]][[2]]$runs ## how to fetch 2nd match's away team's individual runs.
# c <- matchResults[[1]][1]
# names(c)
# matchResults[[1]][1]$Newcastle$playerID
# for(i in 1:length(matchResults)) {
#   nc[i] <- class(matchResults[[1]])[["Newcastle"]]
# }

## fetch team names
m <- c()
# names(matchResults[[1]])
count <- 1
for(i in 1:length(matchResults)){
  team_names <- names(matchResults[[i]])
  m[count] <- team_names[1]
  count <- count + 1
  m[count] <- team_names[2]
  count <- count + 1
}
m <- unique(m)
m

# ## loading team Members
# teamMembers <- fromJSON(file = "teamMembers.json")
# teamMembers
# class(teamMembers)
# teamMembers$Sydney
#
# ## loading playerNames
#
# playerNames <- read.csv("playerNames.csv")
# head(playerNames)
# class(playerNames)
# playerNames[playerNames$ID == teamMembers$Sydney[1],]$playerNames



player_names[which(player_names$ID %in% parramatta_player_ids),]
player_names %>% filter(ID %in% parramatta_player_ids)
player_names[player_names$ID %in% parramatta_player_ids,]
