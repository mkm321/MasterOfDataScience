library("DBI")
library(tidyverse)
#################################################################################
#################################################################################
#################################################################################
## Complete the task below, source the script and submit the code hash number.
 


## Write the function described in the quiz question and make sure to name
## the function dataJoin.
## NOTE that the function will be called by the script with its own input,
## so do not provide the data or call the function when generating the hash value.

# insert function called dataJoin here.

dataJoin <- function(dfx, dfy){
  innerJoin <- dfx %>%
    inner_join(dfy, by = c("black" = "orange"))
  return(innerJoin)
}
 
#################################################################################
###################### Do not edit the code below this line! ####################
 dfx<-list(structure(list(white = c(4L, 1L, 2L, 5L, 6L, 3L), green = c(2L, 
4L, 3L, 6L, 5L, 1L), blue = c(5L, 4L, 3L, 1L, 2L, 6L), black = c(5L, 
4L, 2L, 3L, 1L, 6L), red = c(4L, 1L, 2L, 6L, 3L, 5L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(4L, 1L, 6L, 5L, 2L, 3L), green = c(4L, 
3L, 5L, 2L, 1L, 6L), blue = c(1L, 2L, 6L, 5L, 4L, 3L), black = c(6L, 
5L, 1L, 4L, 3L, 2L), red = c(2L, 3L, 5L, 6L, 1L, 4L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(2L, 4L, 6L, 5L, 3L, 1L), green = c(3L, 
1L, 6L, 2L, 4L, 5L), blue = c(3L, 4L, 6L, 2L, 1L, 5L), black = c(2L, 
4L, 6L, 3L, 5L, 1L), red = c(2L, 3L, 5L, 6L, 1L, 4L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(4L, 5L, 1L, 2L, 6L, 3L), green = c(1L, 
6L, 3L, 2L, 4L, 5L), blue = c(2L, 4L, 6L, 5L, 1L, 3L), black = c(2L, 
3L, 5L, 1L, 6L, 4L), red = c(5L, 6L, 3L, 1L, 4L, 2L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(1L, 3L, 2L, 4L, 6L, 5L), green = c(6L, 
5L, 2L, 4L, 1L, 3L), blue = c(5L, 2L, 4L, 1L, 3L, 6L), black = c(4L, 
3L, 2L, 1L, 5L, 6L), red = c(4L, 5L, 2L, 1L, 6L, 3L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(2L, 3L, 6L, 1L, 5L, 4L), green = c(1L, 
4L, 3L, 5L, 2L, 6L), blue = c(6L, 3L, 4L, 1L, 2L, 5L), black = c(6L, 
2L, 5L, 3L, 1L, 4L), red = c(4L, 1L, 2L, 6L, 3L, 5L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(2L, 1L, 6L, 3L, 4L, 5L), green = c(5L, 
6L, 2L, 1L, 4L, 3L), blue = c(2L, 6L, 4L, 5L, 1L, 3L), black = c(2L, 
4L, 6L, 3L, 5L, 1L), red = c(2L, 5L, 1L, 6L, 3L, 4L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(3L, 2L, 5L, 4L, 1L, 6L), green = c(4L, 
3L, 6L, 2L, 1L, 5L), blue = c(5L, 4L, 2L, 1L, 3L, 6L), black = c(2L, 
6L, 1L, 5L, 3L, 4L), red = c(5L, 1L, 3L, 2L, 4L, 6L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(5L, 6L, 4L, 1L, 2L, 3L), green = c(4L, 
6L, 5L, 1L, 2L, 3L), blue = c(6L, 1L, 3L, 4L, 2L, 5L), black = c(2L, 
6L, 3L, 4L, 5L, 1L), red = c(4L, 2L, 3L, 5L, 1L, 6L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(3L, 4L, 1L, 2L, 5L, 6L), green = c(5L, 
3L, 6L, 1L, 4L, 2L), blue = c(2L, 5L, 1L, 6L, 4L, 3L), black = c(4L, 
5L, 2L, 6L, 3L, 1L), red = c(3L, 1L, 6L, 5L, 4L, 2L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(3L, 1L, 5L, 6L, 4L, 2L), green = c(3L, 
2L, 6L, 1L, 4L, 5L), blue = c(1L, 6L, 4L, 2L, 3L, 5L), black = c(2L, 
5L, 6L, 1L, 4L, 3L), red = c(5L, 1L, 2L, 3L, 6L, 4L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(5L, 4L, 3L, 2L, 6L, 1L), green = c(2L, 
1L, 5L, 3L, 6L, 4L), blue = c(1L, 4L, 6L, 5L, 2L, 3L), black = c(5L, 
1L, 6L, 3L, 2L, 4L), red = c(3L, 1L, 2L, 5L, 4L, 6L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(5L, 1L, 6L, 3L, 4L, 2L), green = c(5L, 
3L, 1L, 6L, 4L, 2L), blue = c(2L, 3L, 6L, 1L, 4L, 5L), black = c(6L, 
2L, 5L, 3L, 1L, 4L), red = c(6L, 3L, 1L, 2L, 4L, 5L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(5L, 1L, 3L, 2L, 4L, 6L), green = c(2L, 
4L, 3L, 6L, 5L, 1L), blue = c(4L, 1L, 3L, 2L, 5L, 6L), black = c(6L, 
3L, 2L, 5L, 1L, 4L), red = c(2L, 1L, 6L, 4L, 5L, 3L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(3L, 4L, 5L, 1L, 2L, 6L), green = c(1L, 
3L, 4L, 6L, 2L, 5L), blue = c(1L, 3L, 6L, 5L, 4L, 2L), black = c(2L, 
6L, 5L, 3L, 4L, 1L), red = c(2L, 1L, 3L, 5L, 4L, 6L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(6L, 2L, 3L, 4L, 1L, 5L), green = c(6L, 
1L, 3L, 2L, 4L, 5L), blue = c(4L, 6L, 1L, 5L, 2L, 3L), black = c(3L, 
6L, 1L, 5L, 4L, 2L), red = c(6L, 1L, 5L, 2L, 3L, 4L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(1L, 4L, 3L, 2L, 5L, 6L), green = c(2L, 
6L, 3L, 5L, 1L, 4L), blue = c(2L, 1L, 5L, 3L, 4L, 6L), black = c(4L, 
1L, 5L, 3L, 2L, 6L), red = c(5L, 2L, 1L, 3L, 6L, 4L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(2L, 5L, 1L, 3L, 4L, 6L), green = c(1L, 
3L, 5L, 6L, 4L, 2L), blue = c(6L, 1L, 4L, 3L, 5L, 2L), black = c(2L, 
3L, 4L, 5L, 6L, 1L), red = c(4L, 5L, 2L, 3L, 1L, 6L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(4L, 5L, 2L, 3L, 1L, 6L), green = c(1L, 
3L, 2L, 5L, 6L, 4L), blue = c(1L, 4L, 2L, 3L, 5L, 6L), black = c(3L, 
2L, 1L, 6L, 5L, 4L), red = c(6L, 3L, 4L, 1L, 2L, 5L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(4L, 1L, 2L, 6L, 5L, 3L), green = c(1L, 
2L, 6L, 3L, 5L, 4L), blue = c(2L, 4L, 1L, 3L, 6L, 5L), black = c(3L, 
4L, 5L, 1L, 6L, 2L), red = c(4L, 5L, 6L, 1L, 3L, 2L)), class = "data.frame", row.names = c(NA, 
-6L))); dfy<-list(structure(list(yellow = c(5L, 6L, 2L, 3L, 1L, 4L), orange = c(3L, 
6L, 5L, 4L, 1L, 2L), purple = c(1L, 3L, 2L, 4L, 5L, 6L), pink = c(2L, 
5L, 1L, 3L, 6L, 4L), cyan = c(5L, 6L, 4L, 2L, 3L, 1L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(yellow = c(1L, 4L, 3L, 6L, 2L, 5L), orange = c(4L, 
1L, 3L, 5L, 2L, 6L), purple = c(2L, 1L, 5L, 4L, 6L, 3L), pink = c(2L, 
4L, 1L, 3L, 6L, 5L), cyan = c(6L, 2L, 1L, 5L, 3L, 4L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(yellow = c(6L, 5L, 2L, 1L, 3L, 4L), orange = c(1L, 
3L, 2L, 6L, 4L, 5L), purple = c(5L, 2L, 4L, 3L, 6L, 1L), pink = c(6L, 
5L, 4L, 1L, 3L, 2L), cyan = c(5L, 4L, 2L, 3L, 1L, 6L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(yellow = c(2L, 5L, 3L, 6L, 1L, 4L), orange = c(6L, 
4L, 5L, 3L, 2L, 1L), purple = c(5L, 1L, 2L, 3L, 4L, 6L), pink = c(5L, 
2L, 4L, 6L, 1L, 3L), cyan = c(5L, 1L, 3L, 6L, 4L, 2L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(yellow = c(1L, 3L, 4L, 6L, 2L, 5L), orange = c(4L, 
2L, 6L, 1L, 3L, 5L), purple = c(4L, 2L, 6L, 3L, 1L, 5L), pink = c(6L, 
5L, 1L, 3L, 4L, 2L), cyan = c(3L, 4L, 6L, 5L, 2L, 1L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(yellow = c(3L, 5L, 1L, 4L, 2L, 6L), orange = c(1L, 
6L, 5L, 4L, 2L, 3L), purple = c(6L, 1L, 2L, 5L, 4L, 3L), pink = c(2L, 
3L, 1L, 6L, 5L, 4L), cyan = c(5L, 3L, 2L, 4L, 1L, 6L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(yellow = c(5L, 6L, 3L, 2L, 4L, 1L), orange = c(4L, 
6L, 3L, 2L, 1L, 5L), purple = c(5L, 4L, 3L, 2L, 6L, 1L), pink = c(5L, 
1L, 3L, 6L, 2L, 4L), cyan = c(4L, 3L, 5L, 6L, 1L, 2L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(yellow = c(3L, 1L, 2L, 6L, 5L, 4L), orange = c(3L, 
5L, 1L, 4L, 2L, 6L), purple = c(1L, 6L, 5L, 3L, 2L, 4L), pink = c(3L, 
6L, 1L, 2L, 4L, 5L), cyan = c(6L, 4L, 2L, 1L, 3L, 5L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(yellow = c(3L, 5L, 2L, 1L, 4L, 6L), orange = c(1L, 
4L, 3L, 5L, 6L, 2L), purple = c(6L, 3L, 2L, 4L, 5L, 1L), pink = c(4L, 
1L, 3L, 2L, 6L, 5L), cyan = c(6L, 2L, 5L, 3L, 1L, 4L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(yellow = c(1L, 5L, 6L, 2L, 4L, 3L), orange = c(3L, 
6L, 4L, 2L, 1L, 5L), purple = c(2L, 3L, 5L, 1L, 6L, 4L), pink = c(3L, 
6L, 1L, 2L, 5L, 4L), cyan = c(1L, 3L, 2L, 4L, 5L, 6L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(yellow = c(2L, 5L, 6L, 4L, 3L, 1L), orange = c(6L, 
2L, 1L, 3L, 5L, 4L), purple = c(3L, 6L, 5L, 4L, 2L, 1L), pink = c(2L, 
5L, 6L, 3L, 1L, 4L), cyan = c(5L, 2L, 3L, 4L, 1L, 6L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(yellow = c(4L, 6L, 2L, 1L, 3L, 5L), orange = c(5L, 
2L, 6L, 4L, 1L, 3L), purple = c(1L, 2L, 6L, 5L, 4L, 3L), pink = c(6L, 
5L, 4L, 3L, 1L, 2L), cyan = c(2L, 5L, 3L, 1L, 4L, 6L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(yellow = c(6L, 3L, 5L, 1L, 4L, 2L), orange = c(2L, 
5L, 3L, 4L, 1L, 6L), purple = c(5L, 1L, 6L, 2L, 3L, 4L), pink = c(6L, 
4L, 5L, 3L, 1L, 2L), cyan = c(2L, 3L, 4L, 1L, 5L, 6L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(yellow = c(3L, 6L, 1L, 4L, 5L, 2L), orange = c(4L, 
3L, 6L, 1L, 5L, 2L), purple = c(4L, 2L, 6L, 3L, 1L, 5L), pink = c(5L, 
4L, 1L, 2L, 6L, 3L), cyan = c(1L, 2L, 3L, 6L, 4L, 5L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(yellow = c(3L, 4L, 2L, 6L, 5L, 1L), orange = c(1L, 
3L, 4L, 6L, 2L, 5L), purple = c(5L, 1L, 6L, 4L, 2L, 3L), pink = c(4L, 
6L, 3L, 2L, 5L, 1L), cyan = c(1L, 2L, 5L, 4L, 6L, 3L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(yellow = c(5L, 2L, 1L, 4L, 6L, 3L), orange = c(5L, 
4L, 2L, 6L, 3L, 1L), purple = c(2L, 1L, 3L, 6L, 4L, 5L), pink = c(1L, 
5L, 2L, 3L, 6L, 4L), cyan = c(5L, 4L, 6L, 2L, 3L, 1L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(yellow = c(5L, 6L, 1L, 3L, 4L, 2L), orange = c(2L, 
1L, 3L, 6L, 4L, 5L), purple = c(5L, 3L, 1L, 6L, 4L, 2L), pink = c(2L, 
6L, 3L, 4L, 5L, 1L), cyan = c(3L, 5L, 6L, 4L, 2L, 1L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(yellow = c(3L, 2L, 5L, 4L, 6L, 1L), orange = c(1L, 
2L, 4L, 3L, 6L, 5L), purple = c(4L, 5L, 2L, 1L, 3L, 6L), pink = c(6L, 
3L, 2L, 5L, 4L, 1L), cyan = c(5L, 1L, 6L, 2L, 4L, 3L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(yellow = c(5L, 3L, 2L, 6L, 4L, 1L), orange = c(3L, 
6L, 5L, 2L, 4L, 1L), purple = c(2L, 4L, 5L, 3L, 1L, 6L), pink = c(6L, 
2L, 3L, 1L, 5L, 4L), cyan = c(6L, 4L, 3L, 1L, 5L, 2L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(yellow = c(2L, 4L, 6L, 3L, 5L, 1L), orange = c(3L, 
6L, 4L, 2L, 1L, 5L), purple = c(1L, 4L, 3L, 2L, 6L, 5L), pink = c(2L, 
6L, 5L, 4L, 3L, 1L), cyan = c(2L, 3L, 1L, 6L, 5L, 4L)), class = "data.frame", row.names = c(NA, 
-6L))); options(scipen = 999);mid<-unlist(mapply(dataJoin, dfx, dfy));
nid<-as.integer(sapply(strsplit(paste(mid, collapse = ""),"")[[1]], charToRaw));mnid=c(113);lnid<-length(nid);for(a in 2:lnid){mnid<-c(mnid,(mnid[a-1]*113) %% 1000)};hash<-sum(abs(diff(mnid*nid)))%%1e5; 
cat("Provide the followning code hash value as your quiz question answer:", hash, "
")
cat("Test input:","
")
print(dfx[[1]])
print(dfy[[1]])
print("Test output")
print(dataJoin(dfx[[1]],dfy[[1]]))

