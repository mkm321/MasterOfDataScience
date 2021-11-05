## importing libraries
library("rjson")
library(tidyverse)
source("Utility.R")

## A function to calculate and update team scores for a particular match.
## input required: team_score :  a data frame of points table
## match_details: a data frame of match details like team names and their total runs.
## adds a score of 3 if a team(home/away) wins
## adds a score of 1 if they tie the total runs scored for that match.
calculate_and_update_scores <- function(team_scores, match_details){
  home_team_scores <- team_scores[which(team_scores$teams == 
                                        match_details$home_team_name),]$scores
  
  away_team_scores <- team_scores[which(team_scores$teams == 
                                        match_details$away_team_name),]$scores
  
  if(match_details$home_team_runs > match_details$away_team_runs){
    home_team_scores <-  home_team_scores+ 3
  }else if(match_details$home_team_runs == match_details$away_team_runs){
    home_team_scores <- home_team_scores + 1
    away_team_scores <- away_team_scores + 1
  }else{
    away_team_scores <- away_team_scores + 3
  }
  
  team_scores[which(team_scores$teams == match_details$home_team_name),]$scores = home_team_scores
  team_scores[which(team_scores$teams == match_details$away_team_name),]$scores = away_team_scores
  
  
  return(team_scores)
}

## A function that will return the points table for the given match data upto
## the specified number of matches.
compute_points_table <- function(match_data, number_of_matches){
  team_names <- fetch_team_names(match_data) # fetching team names
  
  ## creating an empty data frame to store the team wise scores.
  team_scores <- data.frame(teams = team_names,
                   scores = rep(0, length(team_names)))
  
  ## iterating through specified number of the matches to find out 
  ## about the points table.
  for(i in 1:number_of_matches){
    home_team_runs <- fetch_home_team_total_runs(match_data, i)
    away_team_runs <- fetch_away_team_total_runs(match_data, i)
    
    ## a data frame which has match details like team names and their total runs
    match_details <- data.frame(home_team_name = names(match_data[[i]][1]),
                                away_team_name = names(match_data[[i]][2]),
                                home_team_runs = home_team_runs,
                                away_team_runs = away_team_runs)
    
    team_scores <- calculate_and_update_scores(team_scores, match_details)
  }
  ## sort the teams based on high to low scores.
  team_scores <- team_scores %>% arrange(desc(scores))
  return(team_scores)
}

## importing match results data from the matchResults.json file.
match_results <- fromJSON(file = "matchResults.json")

## calculating points table
result1 <- compute_points_table(match_results, 3)
head(result1)
View(result1)

result2 <- compute_points_table(match_results, 28)
head(result2)
View(result2)

################################################################################
###################### Code Testing! ###########################################

ipl_match_results <- fromJSON(file = "IPLMatchResults.json")

testing_results1 <- compute_points_table(ipl_match_results, 3)
View(testing_results1)

testing_results2 <- compute_points_table(ipl_match_results, 6)
View(testing_results2)
