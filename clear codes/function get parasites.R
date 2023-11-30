get.parasites <- function(names, dataset, n.plants){
  #Returns number of parasites involved in interactions with caterpillars, which names are given as a character vector
  sub <- dataset[(n.plants+1):dim(dataset)[1], names]
  rownames(sub) <- dataset[(n.plants+1):dim(dataset)[1], 1]
  parasites.id <- unique(row.names(sub[rowSums(sub)>0,]))
  number <- length(parasites.id)
  return(number)
}