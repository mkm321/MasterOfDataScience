library("ggplot2")
#################################################################################
#################################################################################
#################################################################################
## Complete the task below, source the script and submit the code hash number.
 


## Write the function described in the quiz question and make sure to name
## the function simplePlot.
## NOTE that the function will be called by the script with its own input,
## so do not provide the data or call the function when generating the hash value.

# insert function called simplePlot here.

simplePlot <- function(df){
  g <- ggplot(data = df) +
    geom_point(mapping = aes(x = black, y = red))
  return(g)
}
 
#################################################################################
###################### Do not edit the code below this line! ####################
 dfx<-list(structure(list(white = c(6L, 2L, 4L, 3L, 1L, 5L), green = c(2L, 
4L, 3L, 5L, 1L, 6L), blue = c(6L, 3L, 2L, 4L, 1L, 5L), black = c(2L, 
1L, 6L, 5L, 3L, 4L), red = c(5L, 2L, 4L, 1L, 3L, 6L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(6L, 1L, 2L, 5L, 3L, 4L), green = c(5L, 
3L, 2L, 6L, 4L, 1L), blue = c(6L, 5L, 3L, 4L, 1L, 2L), black = c(1L, 
5L, 3L, 2L, 6L, 4L), red = c(3L, 4L, 5L, 6L, 2L, 1L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(5L, 2L, 3L, 4L, 6L, 1L), green = c(2L, 
4L, 5L, 6L, 1L, 3L), blue = c(4L, 2L, 6L, 3L, 1L, 5L), black = c(1L, 
5L, 6L, 4L, 3L, 2L), red = c(2L, 4L, 1L, 5L, 6L, 3L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(3L, 5L, 6L, 4L, 2L, 1L), green = c(6L, 
1L, 3L, 2L, 5L, 4L), blue = c(6L, 5L, 1L, 4L, 2L, 3L), black = c(5L, 
1L, 3L, 4L, 2L, 6L), red = c(4L, 1L, 5L, 3L, 2L, 6L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(3L, 5L, 1L, 4L, 2L, 6L), green = c(4L, 
6L, 2L, 3L, 1L, 5L), blue = c(6L, 4L, 1L, 2L, 3L, 5L), black = c(2L, 
4L, 6L, 1L, 3L, 5L), red = c(4L, 1L, 6L, 3L, 2L, 5L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(4L, 3L, 6L, 5L, 2L, 1L), green = c(1L, 
3L, 4L, 5L, 2L, 6L), blue = c(1L, 2L, 4L, 5L, 3L, 6L), black = c(4L, 
3L, 2L, 6L, 5L, 1L), red = c(3L, 4L, 1L, 6L, 2L, 5L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(5L, 3L, 4L, 1L, 2L, 6L), green = c(1L, 
5L, 3L, 2L, 4L, 6L), blue = c(3L, 6L, 2L, 5L, 4L, 1L), black = c(6L, 
1L, 3L, 5L, 4L, 2L), red = c(1L, 5L, 3L, 4L, 6L, 2L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(5L, 6L, 1L, 4L, 2L, 3L), green = c(4L, 
5L, 3L, 6L, 2L, 1L), blue = c(6L, 1L, 5L, 2L, 4L, 3L), black = c(3L, 
5L, 6L, 1L, 2L, 4L), red = c(5L, 6L, 3L, 2L, 4L, 1L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(6L, 1L, 5L, 2L, 3L, 4L), green = c(4L, 
2L, 5L, 3L, 6L, 1L), blue = c(1L, 6L, 3L, 4L, 5L, 2L), black = c(6L, 
2L, 4L, 3L, 5L, 1L), red = c(2L, 4L, 3L, 5L, 6L, 1L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(2L, 3L, 4L, 1L, 5L, 6L), green = c(3L, 
6L, 5L, 1L, 4L, 2L), blue = c(2L, 1L, 3L, 4L, 5L, 6L), black = c(4L, 
5L, 6L, 2L, 3L, 1L), red = c(5L, 4L, 1L, 6L, 2L, 3L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(5L, 4L, 1L, 3L, 6L, 2L), green = c(5L, 
1L, 3L, 2L, 6L, 4L), blue = c(4L, 1L, 5L, 2L, 6L, 3L), black = c(5L, 
2L, 4L, 1L, 6L, 3L), red = c(1L, 6L, 3L, 4L, 2L, 5L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(4L, 6L, 1L, 3L, 5L, 2L), green = c(1L, 
5L, 6L, 3L, 4L, 2L), blue = c(4L, 6L, 2L, 5L, 3L, 1L), black = c(1L, 
5L, 2L, 3L, 4L, 6L), red = c(4L, 6L, 2L, 1L, 5L, 3L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(1L, 5L, 6L, 2L, 4L, 3L), green = c(4L, 
2L, 3L, 5L, 6L, 1L), blue = c(5L, 1L, 6L, 4L, 3L, 2L), black = c(1L, 
4L, 3L, 2L, 6L, 5L), red = c(3L, 5L, 6L, 2L, 4L, 1L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(2L, 4L, 3L, 5L, 6L, 1L), green = c(4L, 
1L, 5L, 6L, 2L, 3L), blue = c(3L, 2L, 1L, 6L, 5L, 4L), black = c(1L, 
2L, 4L, 6L, 3L, 5L), red = c(3L, 2L, 1L, 4L, 6L, 5L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(1L, 3L, 6L, 2L, 5L, 4L), green = c(5L, 
2L, 4L, 6L, 1L, 3L), blue = c(3L, 2L, 5L, 1L, 4L, 6L), black = c(3L, 
4L, 6L, 5L, 2L, 1L), red = c(6L, 2L, 1L, 3L, 4L, 5L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(3L, 6L, 4L, 2L, 5L, 1L), green = c(2L, 
3L, 1L, 6L, 4L, 5L), blue = c(6L, 3L, 1L, 2L, 5L, 4L), black = c(5L, 
4L, 2L, 6L, 1L, 3L), red = c(4L, 2L, 5L, 3L, 6L, 1L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(5L, 1L, 3L, 4L, 6L, 2L), green = c(2L, 
6L, 5L, 1L, 3L, 4L), blue = c(2L, 1L, 6L, 3L, 5L, 4L), black = c(6L, 
5L, 3L, 2L, 4L, 1L), red = c(3L, 1L, 6L, 5L, 2L, 4L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(1L, 3L, 2L, 6L, 5L, 4L), green = c(2L, 
5L, 3L, 4L, 1L, 6L), blue = c(3L, 4L, 5L, 6L, 1L, 2L), black = c(5L, 
2L, 3L, 4L, 1L, 6L), red = c(5L, 2L, 6L, 1L, 3L, 4L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(2L, 4L, 1L, 6L, 3L, 5L), green = c(3L, 
1L, 5L, 6L, 4L, 2L), blue = c(6L, 2L, 3L, 5L, 1L, 4L), black = c(4L, 
6L, 2L, 3L, 5L, 1L), red = c(4L, 2L, 3L, 1L, 6L, 5L)), class = "data.frame", row.names = c(NA, 
-6L)), structure(list(white = c(4L, 1L, 5L, 2L, 6L, 3L), green = c(5L, 
1L, 3L, 4L, 2L, 6L), blue = c(6L, 3L, 1L, 4L, 2L, 5L), black = c(5L, 
2L, 6L, 1L, 3L, 4L), red = c(5L, 6L, 1L, 2L, 3L, 4L)), class = "data.frame", row.names = c(NA, 
-6L))); options(scipen = 999);extractPoints<-function(g){gl<-layer_data(g);return(c(gl$x,gl$y))};mid<-unlist(lapply(lapply(dfx,simplePlot),extractPoints));
nid<-as.integer(sapply(strsplit(paste(mid, collapse = ""),"")[[1]], charToRaw));mnid=c(113);lnid<-length(nid);for(a in 2:lnid){mnid<-c(mnid,(mnid[a-1]*113) %% 1000)};hash<-sum(abs(diff(mnid*nid)))%%1e5; 
cat("Provide the followning code hash value as your quiz question answer:", hash, "
")
cat("Test input:","
")
print(dfx[[1]])
print("Test output")
print(simplePlot(dfx[[1]]))

