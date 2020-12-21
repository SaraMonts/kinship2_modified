# Automatically generated from all.nw using noweb

as.data.frame.pedigree <- function(x, ...) {

  dadid <- momid <- rep(0, length(x$id))
  dadid[x$findex>0] <- x$id[x$findex]
  momid[x$mindex>0] <- x$id[x$mindex]
  df <- data.frame(id=x$id, dadid=dadid, momid=momid, sex=x$sex)
  
  # Afegeixo els valors d'affected en el data frame
  if(!is.null(x$affected)) {
    for (i in 1:ncol(x$affected)) {
      aff <- x$affected[,i]
      if (i == 1) df$affected1 <- aff
      else if (i == 2) df$affected2 <- aff
      else if (i == 3) df$affected3 <- aff
      else if (i == 4) df$affected4 <- aff
    }
  }
  
  if(!is.null(x$status))
    df$status = x$status
  return(df)
}

