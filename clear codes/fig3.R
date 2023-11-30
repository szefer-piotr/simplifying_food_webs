# Parasites

setwd("D:\\PRACA\\R\\how simple are tropical food webs\\tritrophic data")
equal <- read.csv("ufwEqual.csv", header = TRUE, sep = "\t")
equal[is.na(equal)] <- 0
set.seed(2)

order.and.clear <- function(dataset){
  #This function order the dataset based on rows and columns
  #and removes zero rows and columns. Dataset should not contain any NAs!
  dataset <- dataset[rowSums(dataset)>0,]
  dataset <- dataset[, colSums(dataset)>0]
  dataset <- dataset[order(rowSums(dataset), decreasing = TRUE), ]
  dataset <- dataset[,order(colSums(dataset), decreasing = TRUE)]
  return(dataset)
}

herb.all <- equal[,-2]
herb.eq <- as.matrix(rowSums(equal[3:dim(equal)[2]]) + equal[,1])
herb.eq <- as.matrix(herb.eq[rowSums(herb.eq)>0,])
par.eq <- as.matrix(colSums(equal[3:dim(equal)[2]]))
par.eq <- as.matrix(par.eq[rowSums(par.eq)>0,])


sum(rowSums(equal)>0)
dim(equal)
ordCl <- order.and.clear(equal[3:dim(equal)[2]])
length(rowSums(ordCl))

rarefaction <- function(dataset, n = 100, standarise = TRUE) {
  conv.data <- rep(1:dim(dataset)[1], dataset)
  result <- matrix(0, nrow = sum(dataset), ncol = 2)
  for (i in 1:n){
    res.temp <- matrix(0, nrow = sum(dataset), ncol = 2)
    res.temp[1,] <-c(length(conv.data), dim(dataset)[1])
    ind.dat <- conv.data
    for(j in 2:sum(dataset)){
      ind.dat <- ind.dat[-sample(length(ind.dat) , 1)]
      spec.no <- length(unique(ind.dat))
      res.temp[j, ] <- c(length(ind.dat), spec.no)
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

order.and.clear <- function(dataset){
  #This function order the dataset based on rows and columns
  #and removes zero rows and columns. Dataset should not contain any NAs!
  dataset <- dataset[rowSums(dataset)>0,]
  dataset <- dataset[, colSums(dataset)>0]
  dataset <- dataset[order(rowSums(dataset), decreasing = TRUE), ]
  dataset <- dataset[,order(colSums(dataset), decreasing = TRUE)]
  return(dataset)
}

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

rarefy.interactions <- function(dataset, n = 100){
  #This function reduces randomly the number of interactions from interaction matrix
  result <- matrix(data = 0, nrow = sum(dataset>0), ncol = 2)
  for (i in 1:n){
    n.arthropods <- sum(dataset > 0) #number of arthropod species
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
      temp.result[j,1] <- sum(temp.dataset > 0) #Number of arthropod species after reomoving one species
      temp.result[j,2] <- sum(temp.dataset)
    }
    result <- result + temp.result
  }
  result <- result/n
  return(result)
}

# Parasites individual based rarefaction
par.rarefied <- rarefaction(par.eq, standarise = T)

# Parasites species randomised
par.sp.rar <- rarefy.arthropods(ordCl) # need to be rescaled
par.sp.rar[,1] <- par.sp.rar[,1]/par.sp.rar[1,1]
par.sp.rar[,2] <- par.sp.rar[,2]/par.sp.rar[1,2]

# Parasites 
equal.subset <- order.and.clear(equal[,3:dim(equal)[2]])
par.sp.nonrand <- reduce(ordCl, margin = 2)

#HOSTS
host.rarefied <- rarefaction(herb.eq, standarise = T)
host.sp.rar <- rarefy.arthropods(t(ordCl))
host.sp.rar[,1] <- host.sp.rar[,1]/host.sp.rar[1,1]
host.sp.rar[,2] <- host.sp.rar[,2]/host.sp.rar[1,2]
herb.all <- order.and.clear(herb.all)
host.sp.nonrand <- reduce(ordCl, margin = 1)

#Interactions
interactions <- equal[,3:dim(equal)[2]]
interactions <- interactions[interactions > 0]
int.rarefied <- rarefaction(as.matrix(interactions))
int.group.rar <- rarefy.interactions(ordCl) #this rarefies interactions and looks at species and not at interactions richness
int.group.rar[,1] <- int.group.rar[,1]/int.group.rar[1,1]
int.group.rar[,2] <- int.group.rar[,2]/int.group.rar[1,2]
int.nonrandom <- reduce.interactions(ordCl) #was herb.all

windows(400,400)
par(mfrow = c(2,2))
plot(par.rarefied, 
     xlim = c(0,1), ylim = c(0,1), type = "l",
     axes = FALSE, ylab = "", xlab = "",
     lwd = 1, lty = 1)  
par(new = TRUE)
plot(par.sp.rar[,1]~par.sp.rar[,2],type = "l",
     xlim = c(0,1), ylim = c(0,1),
     axes = FALSE, ylab = "", xlab = "",
     lwd = 1, lty = 2)
par(new = TRUE)
plot(par.sp.nonrand,
     xlim = c(0,1), ylim = c(0,1),type = "l",
     axes = TRUE, ylab = "Fraction of parasites species", xlab = "Fraction of parasites individuals",
     lwd = 2, lty = 1)
text(0, 0.95, "A")
abline(v=0.5, lty=2, col=rgb(150,0,0,75,maxColorValue=255))
#HOSTS plot
plot(host.rarefied, 
     xlim = c(0,1), ylim = c(0,1), type = "l",
     axes = FALSE, ylab = "", xlab = "",
     lwd = 1, lty = 1)  
par(new = TRUE)
plot(host.sp.rar[,1]~host.sp.rar[,2],type = "l",
     xlim = c(0,1), ylim = c(0,1),
     axes = FALSE, ylab = "", xlab = "",
     lwd = 1, lty = 2)
par(new = TRUE)
plot(host.sp.nonrand,
     xlim = c(0,1), ylim = c(0,1),type = "l",
     axes = TRUE, ylab = "Fraction of host species", xlab = "Fraction of host individuals",
     lwd = 2, lty = 1)
text(0, 0.95, "B")
abline(v=0.5, lty=2, col=rgb(150,0,0,75,maxColorValue=255))
#Interactions plot
plot(int.rarefied, 
     xlim = c(0,1), ylim = c(0,1), type = "l",
     axes = FALSE, ylab = "", xlab = "",
     lwd = 1, lty = 1)  
par(new = TRUE)
plot(int.group.rar[,1]~int.group.rar[,2],type = "l",
     xlim = c(0,1), ylim = c(0,1),
     axes = FALSE, ylab = "", xlab = "",
     lwd = 1, lty = 2)
par(new = TRUE)
plot(int.nonrandom,
     xlim = c(0,1), ylim = c(0,1),type = "l",
     axes = TRUE, ylab = "Fraction of qualitative interactions", xlab = "Fraction of individual interactions",
     lwd = 2, lty = 1)
text(0, 0.95, "C", cex = 2)
abline(v=0.5, lty=2, col=rgb(150,0,0,75,maxColorValue=255))
plot.new()
legend(0,0.95, c("Individual based rarefaction",
                   "Random removal",
                   "Nonrandom removal"),
       lwd = c(1,1,2), lty = c(1,2,1))

###############################################
###############   One panel?
setwd("C:\\R\\how simple are tropical food webs\\clear codes")
pdf("fig3.pdf", width=10,height=10/3)
#windows(600,200)
par(mfrow = c(1,3))
plot(par.rarefied, 
     xlim = c(0,1), ylim = c(0,1), type = "p", cex=2,
     axes = FALSE, ylab = "", xlab = "",
     col = rgb(150,0,0,75,maxColorValue = 255), pch = 16)  
par(new = TRUE)
plot(par.sp.rar[,1]~par.sp.rar[,2],type = "p",cex=2,
     xlim = c(0,1), ylim = c(0,1),
     axes = FALSE, ylab = "", xlab = "",
     col = rgb(150,0,0,75,maxColorValue = 255), pch = 17)
par(new = TRUE)
plot(par.sp.nonrand,
     xlim = c(0,1), ylim = c(0,1),type = "p",cex=2,
     axes = TRUE, ylab = "Fraction of parasites species", xlab = "Fraction of parasites individuals",
     col = rgb(0,150,0,75,maxColorValue = 255), pch = 16, cex.axis=1.2, cex.lab=1.2)
text(0, 0.95, "A", cex=2, font=2)
#HOSTS plot
plot(host.rarefied, 
     xlim = c(0,1), ylim = c(0,1), type = "p",    
     axes = FALSE, ylab = "", xlab = "",
     col = rgb(150,0,0,75,maxColorValue = 255), pch = 16,cex=2)  
par(new = TRUE)
plot(host.sp.rar[,1]~host.sp.rar[,2],type = "p",
     xlim = c(0,1), ylim = c(0,1),
     axes = FALSE, ylab = "", xlab = "",
     col = rgb(150,0,0,75,maxColorValue = 255), pch = 17,cex=2)
par(new = TRUE)
plot(host.sp.nonrand,
     xlim = c(0,1), ylim = c(0,1),type = "p",
     axes = TRUE, ylab = "Fraction of host species", xlab = "Fraction of parasites individuals",
     col = rgb(0,150,0,75,maxColorValue = 255), pch = 16,cex=2, cex.axis=1.2, cex.lab=1.2)
text(0, 0.95, "B", cex=2, font=2)
#Interactions plot
plot(int.rarefied, 
     xlim = c(0,1), ylim = c(0,1), type = "p",
     axes = FALSE, ylab = "", xlab = "",
     col = rgb(150,0,0,75,maxColorValue = 255), pch = 16,cex=2)  
par(new = TRUE)
plot(int.group.rar[,1]~int.group.rar[,2],type = "p",
     xlim = c(0,1), ylim = c(0,1),
     axes = FALSE, ylab = "", xlab = "",
     col = rgb(150,0,0,75,maxColorValue = 255), pch = 17,cex=2)
par(new = TRUE)
plot(int.nonrandom,
     xlim = c(0,1), ylim = c(0,1),type = "p",
     axes = TRUE, ylab = "Fraction of qualitative interactions", xlab = "Fraction of parasites individuals",
     col = rgb(0,150,0,75,maxColorValue = 255), pch = 16,cex=2, cex.axis=1.2, cex.lab=1.2)
text(0, 0.95, "C", cex=2, font=2)
legend(0.025,1, c("Random individuals",
                 "Random species",
                 "Ordered species"),
       pch = c(16,17,16), col = c(rgb(150,0,0,75,maxColorValue = 255),
                                  rgb(150,0,0,75,maxColorValue = 255),
                                  rgb(0,150,0,75,maxColorValue = 255)),
       pt.cex = 2, bty="n")
dev.off()
###################################################
########## Truly one panel

windows(150,150)
par(mfrow = c(1,1))
plot(par.rarefied, 
     xlim = c(0,1), ylim = c(0,1), type = "l",
     axes = FALSE, ylab = "", xlab = "",
     lwd = 2, lty = 1)  
par(new = TRUE)
plot(par.sp.rar[,1]~par.sp.rar[,2],type = "l",
     xlim = c(0,1), ylim = c(0,1),
     axes = FALSE, ylab = "", xlab = "",
     lwd = 2, lty = 1)
par(new = TRUE)
plot(par.sp.nonrand,
     xlim = c(0,1), ylim = c(0,1),type = "l",
     axes = TRUE, ylab = "Fraction of parasites species", xlab = "Fraction of parasites individuals",
     lwd = 2, lty = 1)
text(0, 0.95, "A")
#HOSTS plot
par(new = TRUE)
plot(host.rarefied, 
     xlim = c(0,1), ylim = c(0,1), type = "l",
     axes = FALSE, ylab = "", xlab = "",
     lwd = 2, lty = 2)  
par(new = TRUE)
plot(host.sp.rar[,1]~host.sp.rar[,2],type = "l",
     xlim = c(0,1), ylim = c(0,1),
     axes = FALSE, ylab = "", xlab = "",
     lwd = 2, lty = 2)
par(new = TRUE)
plot(host.sp.nonrand,
     xlim = c(0,1), ylim = c(0,1),type = "l",
     axes = TRUE, ylab = "Fraction of host species", xlab = "Fraction of host individuals",
     lwd = 2, lty = 2)
text(0, 0.95, "B")
#Interactions plot
par(new = TRUE)
plot(int.rarefied, 
     xlim = c(0,1), ylim = c(0,1), type = "l",
     axes = FALSE, ylab = "", xlab = "",
     lwd = 2, lty = 3)  
par(new = TRUE)
plot(int.group.rar[,1]~int.group.rar[,2],type = "l",
     xlim = c(0,1), ylim = c(0,1),
     axes = FALSE, ylab = "", xlab = "",
     lwd = 2, lty = 3)
par(new = TRUE)
plot(int.nonrandom,
     xlim = c(0,1), ylim = c(0,1),type = "l",
     axes = TRUE, ylab = "Fraction of qualitative interactions", xlab = "Fraction of individual interactions",
     lwd = 2, lty = 3)
text(0, 0.95, "C")
plot.new()
legend(0,0.95, c("Individual based rarefaction",
                 "Random removal",
                 "Nonrandom removal"),
       lwd = c(1,1,2), lty = c(1,2,1))