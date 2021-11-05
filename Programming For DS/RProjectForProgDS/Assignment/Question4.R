## importing libraries
library("rjson")
library(tidyverse)

## loading match data from matchResults.json file
match_results <- fromJSON(file = "matchResults.json")
## loading team members details from teamMembers.json file
team_members <- fromJSON(file = "teamMembers.json")
## loading player names from playerNames.csv file
player_names <- read.csv("playerNames.csv")

## A function to get team marches
## input required :
## match_data : a list of all the matches
## team_name: a team name to find total matches for.
get_team_matches <- function(match_data, team_name){
  indexes <- c()
  for(i in 1:length(match_data)){
    team_names <- names(match_data[[i]])
    if(team_name %in% team_names){
      indexes <- c(indexes, i)
    }
  }
  return(match_data[indexes])
}

## A function get player names from given set of player Ids
## Input required:
## playerIds: a vector of player Ids
## Output:
## returns a vector of player names.
get_player_names_by_id <- function(player_names_list, playerIds){
  playerNames <- c()
  for(playerId in playerIds){
    name <- player_names_list[player_names_list$ID == playerId,]$playerNames
    playerNames <- c(playerNames, name)
  }
  return(playerNames)
}

## A function to calculate player details of a given team, such as their total wickets
## taken or total matches played.
## Input Required:
## match_data: a list of all the matches
## team_name: a team name to fetch player details for.
## Output:
## returns a data frame containing player's total wickets taken and matches played
## for the given team.
calculate_player_details <- function(match_data, team_name, team_details, player_nams){
  player_ids <- team_details[[team_name]]
  player_names <- (player_nams %>% filter(ID %in% player_ids))$playerNames
  
  # an empty data frame for player details with wickets and matches fields.
  player_details <- data.frame(playerNames = player_names,
                                          wickets = rep(0, length(player_names)),
                                          matches = rep(0, length(player_names)))
  ## iterating over match data to fetch player details that played for given team
  ## in a match season.
  for(i in 1:length(match_data)){
    players <- match_data[[i]][[team_name]]$playerID
    wickets <- match_data[[i]][[team_name]]$wickets
    players <- get_player_names_by_id(player_nams, players)
    
    ## iterating over the players played for the given team in a particular match
    ## and fetching their total wickets taken, plus incrementing their total matches
    ## and updating them into original data frame of player_details. 
    for(i in 1:length(players)){
      total_wickets <- player_details[player_details$playerNames %in% players[i],]$wickets +wickets[i]
      player_details[player_details$playerNames %in% players[i],]$wickets <- total_wickets
      
      total_matches <- player_details[player_details$playerNames %in% players[i],]$matches + 1
      player_details[player_details$playerNames %in% players[i],]$matches <- total_matches
    }
  }
  
  ## adding a new column averageWickets for average wickets per match by players.
  player_details <- player_details %>% 
                    mutate(averageWickets = wickets / matches)
  ## removing the rows which have null averageWickets(if a player didn't played any match)
  player_details <- na.omit(player_details)
  return(player_details)
}

## A function to fetch bowlers average wicket taken per match
fetch_bowlers_average_wickets <- function(match_data, team_name, team_members, player_names){
  ## fetching all the matches played by provided team.
  team_matches <- get_team_matches(match_data, team_name)
  ## calculate player details(wickets, matches played, average wickets per match)
  ## of the given team.
  player_details <- calculate_player_details(team_matches, team_name, 
                                             team_members, player_names)
  ## sorting the bowlers by highest average wickets to lowest average wickets.
  player_details <- player_details %>% arrange(desc(averageWickets), desc(matches))
  
  ## subsetting the original data frame to get players 
  ## and their average wickets per match only.
  players_average_wickets <- subset(player_details, select = c(playerNames, averageWickets))
  return(players_average_wickets)
}

result <- fetch_bowlers_average_wickets(match_results, "Parramatta", team_members, player_names)
result
View(result)

################################################################################
###################### Code Testing! ###########################################
# 
# ipl_match_results <- fromJSON(file = "IPLMatchResults.json")
# # loading team members details from teamMembers.json file
# ipl_team_members <- fromJSON(file = "IPLTeamMembers.json")
# # loading player names from playerNames.csv file
# ipl_player_names <- read.csv("IPLPlayerNames.csv")
# 
# testing_results4 <- fetch_bowlers_average_wickets(ipl_match_results, "Punjab",
#                                                   ipl_team_members, ipl_player_names)
# View(testing_results4)
