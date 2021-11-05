z <- c("Linux", "OSX", "Windows")
for(i in 1:length(z)){
  if(i<length(z)){
    if(z[i] > z[i+1]){
      cat(z[i], "is greater than ", z[i+1], "\n")
    } else {
      cat(z[i+1], "is greater than ", z[i], "\n")
    }
  }
}