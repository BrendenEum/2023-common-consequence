se <- function(x, na.rm = T) {
  ste = sd(x, na.rm=na.rm)/sqrt(length(x))
  if (length(unique(x))==2) {
    if (all(unique(x)==c(0,1))) { # binomial
      ste = sqrt(length(x))*sqrt(mean(x, na.rm=na.rm)*(1-mean(x, na.rm=na.rm)))
    }
  }
  return(ste)
}