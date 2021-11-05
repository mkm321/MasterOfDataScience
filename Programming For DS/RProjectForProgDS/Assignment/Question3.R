## importing rjson libraries to load json data
library("rjson")
source("Utility.R")

## A function to get home matches of the given team.
## input required :
## match_data : a list of all the matches
## team_name: a team name to find home matches for.
fetch_home_matches <- function(match_data, team_name){
  count <- 0
  for(i in 1:length(match_data)){
    team_names <- names(match_data[[i]])
    if(team_names[1] == team_name){
      count <- count + 1
    }
  }
  return(count)
}

## A function to get total matches of the given team.
## input required :
## match_data : a list of all the matches
## team_name: a team name to find total matches for.
fetch_total_matches <- function(match_data, team_name){
  count <- 0
  for(i in 1:length(match_data)){
    team_names <- names(match_data[[i]])
    if(team_name %in% team_names){
      count <- count + 1
    }
  }
  return(count)
}

## A function that provides the number of matches played as a home or both from 
## a given json file.
fetch_matches_played <- function(file_name){
  ## importing match data from the given file.
  match_results <- fromJSON(file = file_name)
  ## get team names from the match data
  team_names <- fetch_team_names(match_results)
  
  ## an empty data frame that will contain number of home and total matches of the team.
  matches_played <- data.frame(home_matches = rep(0, length(team_names)),
                               total_matches = rep(0, length(team_names)))
  rownames(matches_played) <- team_names
  
  ## iterating through list of team names and finding their home and total matches
  ## from the given data file.
  for(team_name in team_names){
    home_matches <- fetch_home_matches(match_results, team_name)
    total_matches <- fetch_total_matches(match_results, team_name)
    
    matches_played[team_name,]$home_matches <- home_matches
    matches_played[team_name,]$total_matches <- total_matches
  }
  
  return(matches_played)
  
}

team_matches <- fetch_matches_played("matchResults.json")
team_matches
View(team_matches)

################################################################################
###################### Code Testing! ###########################################

testing_results3 <- fetch_matches_played("IPLMatchResults.json")
View(testing_results3)

