setwd("C:\\R\\how simple are tropical food webs\\tritrophic data")
equal <- read.csv("ufwEqual.csv", header = TRUE, sep = "\t")
equal[is.na(equal)] <- 0
set.seed(2)

herb.eq <- as.matrix(rowSums(equal[3:dim(equal)[2]]) + equal[,1])
herb.eq <- as.matrix(herb.eq[rowSums(herb.eq)>0,])

par.eq <- as.matrix(colSums(equal[3:dim(equal)[2]]))
par.eq <- as.matrix(par.eq[rowSums(par.eq)>0,])
sum(par.eq)

equal <- equal[3:dim(equal)[2]]

int.eq <- as.matrix(equal[which(equal > 0, arr.ind = TRUE)])
sum(int.eq)
#ind.par <- rep(1:dim(par.eq)[1], par.eq)
#length(ind.red)
#ind.red <- ind.par[-sample(length(ind.par), 1)]
#windows(200,400)
#par(mfrow = c(2,1))
#plot(table(ind))
#plot(par.eq)
#length(unique(ind.par))

rarefaction <- function(dataset, n = 100, standarise = TRUE) {
  conv.data <- rep(1:dim(dataset)[1], dataset)
  result <- matrix(0, nrow = sum(dataset), ncol = 2)
  for (i in 1:n){
    res.temp <- matrix(0, nrow = sum(dataset), ncol = 2)
    res.temp[1,] <-c(sum(conv.data), dim(dataset)[1])
    ind.dat <- conv.data
    for(j in 2:sum(dataset)){
      ind.dat <- ind.dat[-sample(length(ind.dat) , 1)]
      spec.no <- length(unique(ind.dat))
      res.temp[j, ] <- c(sum(ind.dat), spec.no)
    }
    #print(res.temp)
    result <- result + res.temp
  }
  result <- result/n
  if (standarise == TRUE){
    result[,1] <- result[,1]/result[1,1]
    result[,2] <- result[,2]/result[1,2]
  }
  return(result)
}

#test
par.out <- rarefaction(par.eq)
herb.out <- rarefaction(herb.eq)
int.out <- rarefaction(int.eq)

rarefy.plants <- function(dataset, n = 100){
  #This function reduces randomly the number of tree species
  result <- matrix(data = 0, nrow = dim(dataset)[2], ncol = 2)
  for (i in 1:n){
    n.arthropods <- sum(rowSums(dataset) > 0) #number of arthropod species
    abundance <- sum(dataset)
    temp.result <- matrix(data = 0, nrow = dim(dataset)[2], ncol = 2) # temporal result matrix
    temp.result[1,1] <- n.arthropods # assigning first row of result matrix
    temp.result[1,2] <- abundance
    temp.dataset <- as.matrix(dataset) # creating a temprary dataset for manipulations
    for (j in 2:dim(dataset)[2]){
      temp.dataset <- as.matrix(temp.dataset[, -sample(dim(temp.dataset)[2], 1)])
      temp.result[j,1] <- sum(rowSums(temp.dataset) > 0) #Number of arthropod species after reomoving one species
      temp.result[j,2] <- sum(temp.dataset)
    }
    result <- result + temp.result
  }
  result <- result/n
  return(result)
}

rarefy.arthropods <- function(dataset, n = 100){
  #UNDER CONSTRUCTION - This function reduces randomly the number of arthropod species and look at abundance
  result <- matrix(data = 0, nrow = dim(dataset)[2], ncol = 2)
  for (i in 1:n){
    n.arthropods <- sum(colSums(dataset) > 0) #number of arthropod species
    abundance <- sum(dataset)
    temp.result <- matrix(data = 0, nrow = dim(dataset)[2], ncol = 2) # temporal result matrix
    temp.result[1,1] <- n.arthropods # assigning first row of result matrix
    temp.result[1,2] <- abundance
    temp.dataset <- as.matrix(dataset) # creating a temprary dataset for manipulations
    for (j in 2:dim(dataset)[2]){
      temp.dataset <- as.matrix(temp.dataset[, -sample(dim(temp.dataset)[2], 1)])
      temp.result[j,1] <- sum(colSums(temp.dataset) > 0) #Number of arthropod species after reomoving one species
      temp.result[j,2] <- sum(temp.dataset)
    }
    result <- result + temp.result
  }
  result <- result/n
  return(result)
}

rarefy.interactions <- function(dataset, n = 100){
  #This function reduces randomly the number of interactions from interaction matrix
  result <- matrix(data = 0, nrow = sum(dataset>0), ncol = 2)
  for (i in 1:n){
    n.arthropods <- sum(rowSums(dataset) > 0) #number of arthropod species
    abundance <- sum(dataset)
    temp.result <- matrix(data = 0, nrow = sum(dataset>0), ncol = 2) # temporal result matrix
    temp.result[1,1] <- n.arthropods # assigning first row of result matrix
    temp.result[1,2] <- abundance
    temp.dataset <- as.matrix(dataset) # creating a temprary dataset for manipulations
    for (j in 2:sum(dataset>0)){
      
      #temp.dataset <- temp.dataset[, colSums(temp.dataset)>0] #Removing empty columns
      #temp.dataset <- temp.dataset[, rowSums(temp.dataset)>0] #Removing empty rows
      
      nonzero.entries <- which(temp.dataset>0, arr.ind = TRUE)
      
      #print(length(nonzero.entries))
      
      coord <- nonzero.entries[sample(dim(nonzero.entries)[1],1),]
      
      #print(dataset[row,col])
      
      temp.dataset[coord[1],coord[2]] <- 0
      temp.result[j,1] <- sum(rowSums(temp.dataset) > 0) #Number of arthropod species after reomoving one species
      temp.result[j,2] <- sum(temp.dataset)
    }
    result <- result + temp.result
  }
  result <- result/n
  return(result)
}

#RARAEFACTION FOR HOST PARASITE WEB
rar.par <- rarefy.arthropods(equal) #parasite species reduction on parasites
rar.par[,1] <- rar.par[,1]/rar.par[1,1]
rar.par[,2] <- rar.par[,2]/rar.par[1,2]
#plot(rar.par)
rar.herb <- rarefy.plants(t(equal)) #host reduction and effect on parasites abu
rar.herb[,1] <- rar.herb[,1]/rar.herb[1,1]
rar.herb[,2] <- rar.herb[,2]/rar.herb[1,2]
#plot(rev(rar.herb[,2]), rev(rar.herb[,1])) #herbivore removed and effect of parasites abundance on species diversity
#sum(rowSums(equal)>0)
rar.int <- rarefy.interactions(t(equal))
rar.int[,1] <- rar.int[,1]/rar.int[1,1]
rar.int[,2] <- rar.int[,2]/rar.int[1,2]
#plot(rar.int[,2], rar.int[,1]

# NONRANDOM PARASITES

reduce.interactions <- function(dataset){
  wp.temp <- dataset
  no.of.interaction <- sum(dataset>0)
  print(no.of.interaction)
  result <- matrix(0, nrow = no.of.interaction, ncol = 2)
  result[1,] <- c(1,1)
  for (i in 1:(no.of.interaction-1)){
    print(no.of.interaction - i)
    coord <- which(wp.temp == min(wp.temp[wp.temp>0]), arr.ind = TRUE)
    print(wp.temp[coord[1,1], coord[1,2]])
    wp.temp[coord[1,1], coord[1,2]] <- 0
    #Count the interactions
    arthropods.abundance <- sum(wp.temp)
    arthropods.abundance.prop <- arthropods.abundance/sum(dataset)
    result[i + 1, ] <- c(arthropods.abundance.prop, (no.of.interaction - i)/no.of.interaction)
  }
  return(result)
}

reduce <- function(dataset, margin){
  # This function takes data matrix ordered by rows and by columns and reuces
  # it by removing least abundant components from given margin and calculates
  # proportion of abundance left.
  ws.temp <- dataset
  abu <- sum(dataset)
  prop <- matrix(0, nrow = dim(dataset)[margin], ncol = 2)
  prop [1, ] <- c(1, dim(dataset)[margin])
  for (i in 1:(dim(dataset)[margin]-1)){
    #print(i)
    #print(dim(ws.temp)[2] - i)
    if (margin == 2) {
      ws.temp <- ws.temp[, -dim(ws.temp)[margin]]
      res <- sum(ws.temp)/abu
      prop[i + 1, ] <- c(res, dim(as.matrix(ws.temp))[margin])
    }
    else{
      ws.temp <- ws.temp[-dim(ws.temp)[margin], ]
      res <- sum(ws.temp)/abu
      prop[i + 1, ] <- c(res, dim(ws.temp)[margin])
    }
    
  }
  prop[,2] <- prop[,2]/prop[1,2]
  return(prop)
}

#Parasites host database ordered without emty rows and columns
#ordes before
equal <- equal[rowSums(equal)>0,]
equal <- equal[order(rowSums(equal), decreasing =TRUE), ]
equal <- equal[,order(colSums(equal), decreasing = TRUE)]

equal.nonrandom.par <- reduce(equal, 2)
equal.nonrandom.her <- reduce(equal, 1)
equal.nanrandom.interactions <- reduce.interactions(equal)

#Plotting nonrandom reduction of food web

#PLOTTING
windows(300,400)
par(mfrow = c(3,1))

plot(par.out, 
     type = "l", lty = 1, col = "black", 
     axes = FALSE, xlab = "", ylab = "",
     xlim = c(0,1), ylim = c(0,1))
par(new = TRUE)
plot(herb.out, 
     type = "l", lty = 2, col = "black", lwd = 2, 
     axes = FALSE, xlab = "", ylab = "",
     xlim = c(0,1), ylim = c(0,1))
par(new = TRUE)
plot(int.out, 
     type = "l", lty = 1, col = "black", lwd = 3, 
     axes = TRUE, xlab = "Proportion of units", ylab = "Proportion of quantity",
     xlim = c(0,1), ylim = c(0,1))
#legend(0.5,0.4, c("Parasites individuals", "Herbivore individuals", 
                  #"Individual interactions"), lty = c(1,2,1),
       #lwd = c(1,2,3))
text(0, 0.95, c("A"))

plot(rar.par[,2], rar.par[,1], 
     type = "l", lty = 1, col = "black", 
     axes = FALSE, xlab = "", ylab = "",
     xlim = c(0,1), ylim = c(0,1))
par(new = TRUE)
plot(rar.herb[,2], rar.herb[,1],
     type = "l", lty = 2, col = "black", lwd = 2, 
     axes = FALSE, xlab = "", ylab = "",
     xlim = c(0,1), ylim = c(0,1))
par(new = TRUE)
plot(rar.int[,2], rar.int[,1],
     type = "l", lty = 1, col = "black", lwd = 3, 
     axes = TRUE, xlab = "Proportion of parasites abundance", ylab = "Proportion of parsites species",
     xlim = c(0,1), ylim = c(0,1))
#legend(0.5, 0.4, c("Parasites removed", "Hosts removed", "Interaction removed"),
       #lty = c(1,2,1), lwd = c(1,2,3))
text(0, 0.95, c("B"))

#par(new = FALSE)
plot(equal.nonrandom.par[,1], equal.nonrandom.par[,2], type = "l",
     lty = 1, lwd = 1, axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)
plot(equal.nonrandom.her[,1], equal.nonrandom.her[,2], type = "l",
     lwd = 2, lty = 2,axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)
plot(equal.nanrandom.interactions[,1], equal.nanrandom.interactions[,2],
     type = "l", lty = 1, lwd = 3, xlab = "Proportion of abundanace", ylab = "Proportion of species/interactions")
legend(0.05,0.95, c("Parasite species", "Herbivore species",
                  "Interactions"), lty = c(1,2,1),
       lwd = c(1,2,3))
text(0, 0.95, c("C"))
