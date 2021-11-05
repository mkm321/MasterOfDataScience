## A function to calculate home team total scores
## this function requires the match number and match data as an input and returns the total run
## scored by the home team for that match.
fetch_home_team_total_runs <- function(match_data, match_number) {
  home_team <- match_data[[match_number]][[1]]
  home_team_runs <- home_team$runs
  return(sum(home_team_runs))
}

## A function to calculate away team total scores
## this function requires the match number and match data as an input and returns the total run
## scored by the away team for that match.
fetch_away_team_total_runs <- function(match_data, match_number){
  away_team <- match_data[[match_number]][[2]]
  away_team_runs <- away_team$runs
  return(sum(away_team_runs))
}

## A function to get team names from the provided match data.
## this function requires a list of match data and will return all the teams
## in the data.
fetch_team_names <- function(match_data){
  teams <- c() # created an empty vector
  
  # iterating through all the matches
  for(i in 1:length(match_data)){
    team_names <- names(match_data[[i]]) # team name for a particular match
    # adding the team names to the teams vector.
    teams <- c(teams,team_names[1], team_names[2])
  }
  
  # removing any duplicate team name and returning it.
  return(unique(teams))
}

