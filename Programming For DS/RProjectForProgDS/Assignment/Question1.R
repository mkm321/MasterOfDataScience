library("rjson")
library("pander")
source("Utility.R") # source Utility.R file so there function be available


## A function to calculate total runs from each time.                                                                                                      
fetch_team_wise_total_scores <- function(match_data){
  ## creating an empty data frame to store the final results.
  total_runs <- data.frame(matrix(ncol=5,nrow=length(match_data), 
                                  dimnames=list(NULL, c("Match", 
                                                        "Home_Team", 
                                                        "Away_Team",
                                                        "Home_Team_Runs",
                                                        "Away_Team_Runs"))))
  
  ## Iterating through each list and fetching home and away team scores per match
  for(i in 1:length(match_data)){
    home_team_total <- fetch_home_team_total_runs(match_data, i)
    class(home_team_total)
    away_team_total <- fetch_away_team_total_runs(match_data, i)
    
    ## Appending the home and away team total for a match in the final table.
    total_runs[i,]$Match = i
    total_runs[i,]$Home_Team = names(match_data[[i]][1])
    total_runs[i,]$Away_Team = names(match_data[[i]][2])
    total_runs[i,]$Home_Team_Runs = home_team_total
    total_runs[i,]$Away_Team_Runs = away_team_total
  }
  
  return(total_runs)
}
## importing match results data from the matchResults.json file.
match_results <- fromJSON(file = "matchResults.json")
results <- fetch_team_wise_total_scores(match_results)
pander(results)

################################################################################
###################### Code Testing! ###########################################

# ## Manual verification
# ## First match Home team total runs
# runs <- sum(match_results[[1]][[1]]$runs)
# ## condition if the actual and observed results are equal.
# if(results[1,]$home_team_total_runs == runs){
#   print("Test Case Passed.")
# } else {
#   print("Test Case Failed.")
# }
# 
# ## testing with new data
# ipl_match_results <- fromJSON(file = "IPLMatchResults.json")
# 
# testing_results <- fetch_team_wise_total_scores(ipl_match_results)
# View(testing_results)
