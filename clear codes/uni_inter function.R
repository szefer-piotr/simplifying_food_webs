setwd("C:\\R\\data\\50perc")
parasite <- read.csv("tri_trophic_janda.csv", header = TRUE, sep = "\t")
sort(table(parasite$parasitoid), decreasing = TRUE)

parasitoids.names <- unique(parasite$parasitoid)
parasitoids.names

sum(length(unique(parasite$herbivore)), )unique(parasite$plant)
names(parasite)

con.plant.cat <- lapply(split(parasite, parasite$plant), FUN = function(x) length(unique(x$herbivore)))


#FUNCTION

uni.inter <- function(data, col = 1, comp.cases = TRUE) {
  "Function that returns sorted number of interaction for species in a specified column
  with species from another column, unexisting observations are in the last column"
  if (comp.cases == TRUE){
    data <- data[,c(col,(col+1))]
    data <- data[complete.cases(data),]
  }
  result <- matrix(0,nrow=1,ncol = length(unique(data[,col])))
  for (i in 1:length(unique(data[,1]))){
    names <- unique(data[,1])
    species = as.character(names[i])
    length(unique(data[data[,1] == species, 2)) -> uni
    result[i] <- uni
  }
  colnames(result) <- unique(data[,1])
  return(result)
}

first.level.interactions <- uni.inter(parasite)
first.level.interactions <- sort(as.data.frame(first.level.interactions), decreasing = TRUE)
sum(first.level.interactions)
par(mar = c(2,2,2,2))
barplot(as.matrix(first.level.interactions), cex.names = 0.3)

data <- data[,c(col,(col+1))]
data <- data[complete.cases(data),]

second.level.interactions <- parasite[, c(2,3)]
dim(second.level.interactions)
second.level.interactions <- second.level.interactions[complete.cases(second.level.interactions), ]
second.level.interactions <- uni.inter(second.level.interactions, col = 2)
second.level.interactions <- sort(as.data.frame(second.level.interactions), decreasing = TRUE)
sum(second.level.interactions)
par(mar = c(2,2,2,2))
barplot(as.matrix(second.level.interactions), cex.names = 0.3)
