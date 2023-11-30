setwd("C:\\R\\data\\50perc")
parasite <- read.csv("tri_trophic_janda.csv", header = TRUE, sep = "\t")

uni.inter <- function(data, col = 1, comp.cases = TRUE) {
  "Function that returns sorted number of interaction for species in a specified column
  with species from another column, unexisting observations are in the last column"
  if (comp.cases == TRUE){
    data <- data[,c(col,(col+1))]
    #print(data[1:5,c(col, (col+1))])
    data <- data[complete.cases(data),]
    #print(data[1:5,c(1, 2)])
  }
  result <- matrix(0,nrow=1,ncol = length(unique(data[,1])))
  for (i in 1:length(unique(data[,1]))){
    names <- unique(data[,1])
    species <- as.character(names[i])
    uni <- length(unique(data[data[,1] == species, 2]))
    result[i] <- uni
  }
  colnames(result) <- unique(data[,1])
  return(result)
}

plant.cat.no.int <- sum(uni.inter(parasite))
cat.par.no.int <- sum(uni.inter(parasite, col=2))

half.interactions <- round((plant.cat.no.int + cat.par.no.int)*0.5)

# Reducing the dataset only to caterpillars and pants that are necessary to represent all