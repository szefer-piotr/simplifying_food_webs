# Fig 5

require(bipartite)
setwd("C:\\R\\how simple are tropical food webs")
guilds <- read.table("guilds.csv", header = TRUE, sep = "\t")
guilds[is.na(guilds)] <- 0
guilds.val <- guilds[,2:39] #Plants are in columns and arthropods are in rows

#Subsetting for three guilds
miners <- guilds.val[guilds$Guild == unique(guilds$Guild)[3],]
mobile <- guilds.val[guilds$Guild == unique(guilds$Guild)[4],]
semi <- guilds.val[guilds$Guild == unique(guilds$Guild)[5],]


#######################
###### FUNCTIONS ######
#######################

# Function sort dataset and removes zero rows and columns 
order.and.clear <- function(dataset){
  #This function order the dataset based on rows and columns
  #and removes zero rows and columns. Dataset should not contain any NAs!
  dataset <- dataset[rowSums(dataset)>0,]
  dataset <- dataset[, colSums(dataset)>0]
  dataset <- dataset[order(rowSums(dataset), decreasing = TRUE), ]
  dataset <- dataset[,order(colSums(dataset), decreasing = TRUE)]
  return(dataset)
}

# Rarefy with plant species
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

# Removing of arthropod species ## its taking the plant diversity into account
#############
# RAREFY ARTHROPODS IS THE SAME AS RAREFY PLANTS BUT WORKS ON TRANSFORMED MATRIX
rarefy.arthropods <- function(dataset, n = 100){
  #This function reduces randomly the number of arthropod species and looks at abundance
  result <- matrix(data = 0, nrow = dim(dataset)[2], ncol = 3)
  for (i in 1:n){
    n.arthropods <- sum(colSums(dataset) > 0) #number of arthropod species
    abundance <- sum(dataset)
    temp.result <- matrix(data = 0, nrow = dim(dataset)[2], ncol = 3) # temporal result matrix
    temp.result[1,1] <- n.arthropods # assigning first row of result matrix
    temp.result[1,2] <- abundance
    temp.result[1,3] <- networklevel(dataset, index = "H2")
    temp.dataset <- as.matrix(dataset) # creating a temprary dataset for manipulations
    for (j in 2:(dim(dataset)[2]-1)){ # no to reduce completely
      temp.dataset <- as.matrix(temp.dataset[, -sample(dim(temp.dataset)[2], 1)])
      print((i-1) + (j/sum(dataset>0)))
      temp.result[j,1] <- sum(colSums(temp.dataset) > 0) #Number of arthropod species after reomoving one species
      temp.result[j,2] <- sum(temp.dataset)
      temp.result[j,3] <- networklevel(temp.dataset, index = "H2")
    }
    result <- result + temp.result
  }
  result <- result/n
  return(result)
}

# Rarefying interactions
rarefy.interactions <- function(dataset, n = 100){
  #This function reduces randomly the number of interactions from interaction matrix
  result <- matrix(data = 0, nrow = sum(dataset>0), ncol = 3) # one col for H2
  for (i in 1:n){
    n.arthropods <- sum(rowSums(dataset) > 0) #number of arthropod species
    abundance <- sum(dataset)
    temp.result <- matrix(data = 0, nrow = sum(dataset>0), ncol = 3) # temporal result matrix
    temp.result[1,1] <- n.arthropods # assigning first row of result matrix
    temp.result[1,2] <- abundance
    temp.result[1,3] <- networklevel(dataset, index = "H2")
    temp.dataset <- as.matrix(dataset) # creating a temprary dataset for manipulations
    for (j in 2:sum(dataset>0)){
      print((i-1) + (j/sum(dataset>0)))
      #temp.dataset <- temp.dataset[, colSums(temp.dataset)>0] #Removing empty columns
      #temp.dataset <- temp.dataset[, rowSums(temp.dataset)>0] #Removing empty rows
      nonzero.entries <- which(temp.dataset>0, arr.ind = TRUE)
      coord <- nonzero.entries[sample(dim(nonzero.entries)[1],1),]
      temp.dataset[coord[1],coord[2]] <- 0
      temp.result[j,1] <- sum(rowSums(temp.dataset) > 0) #Number of arthropod species after reomoving one species
      temp.result[j,2] <- sum(temp.dataset)
      temp.result[j,3] <- networklevel(temp.dataset, index="H2")
    }
    result <- result + temp.result
  }
  result <- result/n
  return(result)
}

# Function that nonrandomly reduces interactions
reduce.interactions <- function(dataset, ...){
  wp.temp <- dataset
  no.of.interaction <- sum(dataset>0)
  print(no.of.interaction)
  result <- matrix(0, nrow = no.of.interaction, ncol = 3) # additional column for h2 res
  result[1,] <- c(1,1, networklevel(wp.temp, index="H2"))
  for (i in 1:(no.of.interaction-1)){
    print(no.of.interaction - i)
    coord <- which(wp.temp == min(wp.temp[wp.temp>0]), arr.ind = TRUE)
    print(wp.temp[coord[1,1], coord[1,2]])
    wp.temp[coord[1,1], coord[1,2]] <- 0
    #Count the interactions
    arthropods.abundance <- sum(wp.temp)
    arthropods.abundance.prop <- arthropods.abundance/sum(dataset)
    result[i + 1, ] <- c(arthropods.abundance.prop, 
                         (no.of.interaction - i)/no.of.interaction,
                         networklevel(wp.temp, index="H2"))
  }
  return(result)
}

# Function that nonrandomly reduces plants/arthropods specified as a margin of a matrix
reduce <- function(dataset, margin, ...){
  # This function takes data matrix ordered by rows and by columns and reduces
  # it by removing least abundant components from given margin and calculates
  # proportion of abundance left.
  ws.temp <- dataset
  abu <- sum(dataset)
  prop <- matrix(0, nrow = dim(dataset)[margin], ncol = 3)
  prop [1, c(1,2)] <- c(1, dim(dataset)[margin]) # rows are species
  prop[1, 3] <- networklevel(ws.temp, index="H2")
  for (i in 1:(dim(dataset)[margin]-2)){
    #print(i)
    #print(dim(ws.temp)[2] - i)
    if (margin == 2) {
      ws.temp <- ws.temp[, -dim(ws.temp)[margin]]
      res <- sum(ws.temp)/abu
      prop[i + 1, c(1,2)] <- c(res, dim(as.matrix(ws.temp))[margin])
      if (dim(as.matrix(ws.temp))[margin] > 1){
        prop[i + 1, 3] <- networklevel(ws.temp, index="H2")
      }
    }
    else{
      ws.temp <- ws.temp[-dim(ws.temp)[margin], ]
      res <- sum(ws.temp)/abu
      prop[i + 1, c(1,2)] <- c(res, dim(ws.temp)[margin])
      if (dim(as.matrix(ws.temp))[margin] > 1){
        prop[i + 1, 3] <- networklevel(ws.temp, index="H2")
      }
    }
    
  }
  prop[,2] <- prop[,2]/prop[1,2]
  return(prop)
}

######################
###### DATASETS ######

miners <- order.and.clear(miners)
mobile <- order.and.clear(mobile)
semi <- order.and.clear(semi)

# RANDOM REMOVAL OF INTERACTIONS - H2
#miners.rand <- rarefy.interactions(miners)
#write.table(miners.rand, "miners.rand.txt")
#mobile.rand <- rarefy.interactions(mobile)
#write.table(mobile.rand, "mobile.rand.txt")
#semi.rand <- rarefy.interactions(semi)
#write.table(semi.rand, "semi.rand.txt")

# NONRANDOM REMOVAL OF INTERACTIONS - H2
#miners.redint <- reduce.interactions(miners)
#write.table(miners.redint, "miners.redint.txt")
#mobile.redint <- reduce.interactions(mobile)
#write.table(mobile.redint, "mobile.redint.txt")
#semi.redint <- reduce.interactions(semi)
#write.table(semi.redint, "semi.redint.txt")

# NONRANDOM REMOVAL of ARTHROPOD SPECIES - H2
#miners.arthroord <- reduce(miners, 1)
#write.table(miners.arthroord, "miners.arthroord.txt")
#mobile.arthroord <- reduce(mobile, 1)
#write.table(mobile.arthroord, "mobile.arthroord.txt")
#semi.arthroord <- reduce(semi, 1)
#write.table(semi.arthroord, "semi.arthroord.txt")

# Random REMOVAL OF ARTHROPOD SPECIES-H2
#miners.arthrorand <- rarefy.arthropods(t(miners))
#write.table(miners.arthrorand, "miners.arthrorand.txt")
# mobile.arthroprand <- rarefy.arthropods(t(mobile)) # done on Asia's comp and copied!
# semi.arthrorand <- rarefy.arthropods(t(semi)) # done on Asia's comp and copied!

#RANDOM REMOVAL OF Plants - H2
#miners.plantrand <- rarefy.arthropods(miners)
#write.table(miners.plantrand, "miners.plantrand.txt")
#mobile.plantrand <- rarefy.arthropods(mobile)
#write.table(mobile.plantrand, "mobile.plantrand.txt")
#semi.plantrand <- rarefy.arthropods(semi)
#write.table(semi.plantrand, "semi.plantrand.txt")

#NONRANDOM REMOVAL OF Plants - H2
#miners.plantord <- reduce(miners, 2)
#write.table(miners.plantord, "miners.plantord.txt")
#mobile.plantord <- reduce(mobile, 2)
#write.table(mobile.plantord, "mobile.plantord.txt")
#semi.plantord <- reduce(semi, 2)
#write.table(semi.plantord, "semi.plantord.txt")

setwd("C:\\Users\\szefe\\Desktop\\Work\\extrapolation\\datasets")
#setwd("C:\\R\\how simple are tropical food webs\\tritrophic data")
equal <- read.csv("ufwEqual.csv", header = TRUE, sep = "\t")
setwd("C:\\R\\how simple are tropical food webs")
equal <- as.matrix(equal[, c(-1,-2)])
equal[is.na(equal)] <- 0
parasites <- order.and.clear(equal)

dim(parasites)

# PARASITE DATASET H2
# Nonrandom Interactions
#par.intredH2 <- reduce.interactions(parasites) 
#write.table(par.intredH2, "par.intredH2.txt")

# Nonrandom Host
# par.hostredH2 <- reduce(parasites, 1)
# write.table(par.hostredH2, "par.hostredH2.txt")

# Nonrandom Parasites
#par.parredH2 <- reduce(parasites, 2)
#write.table(par.parredH2, "par.parredH2.txt")

# Random Interactions
#par.intrandH2 <- rarefy.interactions(parasites)
#write.table(par.intrandH2, "par.intrandH2.txt")

# Random Host (this is parasites reduced)
# par.hostrandH2 <- rarefy.arthropods(parasites)
# write.table(par.hostrandH2, "par.hostrandH2.txt")

# Random Parasites (this is host reduced)
# par.parrandH2 <- rarefy.arthropods(t(parasites))
# write.table(par.parrandH2, "par.parrandH2.txt")

##########################################################
######### GENERALITY and VULNERABILITY ###################

##########################################################
# INTERACTIONS
reduce.interactions <- function(dataset){
  wp.temp <- dataset
  no.of.interaction <- sum(dataset>0)
  print(no.of.interaction)
  result <- matrix(0, nrow = no.of.interaction, ncol = 4) # additional column for h2 res
  result[1,] <- c(1,1, networklevel(wp.temp, index="generality"))
  for (i in 1:(no.of.interaction-1)){
    print(no.of.interaction - i)
    coord <- which(wp.temp == min(wp.temp[wp.temp>0]), arr.ind = TRUE)
    print(wp.temp[coord[1,1], coord[1,2]])
    wp.temp[coord[1,1], coord[1,2]] <- 0
    #Count the interactions
    arthropods.abundance <- sum(wp.temp)
    arthropods.abundance.prop <- arthropods.abundance/sum(dataset)
    result[i + 1, ] <- c(arthropods.abundance.prop, 
                         (no.of.interaction - i)/no.of.interaction,
                         networklevel(wp.temp, index="generality"))
  }
  return(result)
}

rarefy.interactions <- function(dataset, cols = 4, index = "vulnerability", n = 100){
  #This function reduces randomly the number of interactions from interaction matrix
  result <- matrix(data = 0, nrow = sum(dataset>0), ncol = cols)
  colnames(result) <- c("Arthropod species [row]", "Individuals", "Generality", "Vulner")# one col for H2
  for (i in 1:n){
    n.arthropods <- sum(rowSums(dataset) > 0) #number of arthropod species
    abundance <- sum(dataset)
    temp.result <- matrix(data = 0, nrow = sum(dataset>0), ncol = cols) # temporal result matrix
    temp.result[1,1] <- n.arthropods # assigning first row of result matrix
    temp.result[1,2] <- abundance
    temp.result[1,c(3,4)] <- networklevel(dataset, index = index)
    temp.dataset <- as.matrix(dataset) # creating a temprary dataset for manipulations
    for (j in 2:sum(dataset>0)){
      print((i-1) + (j/sum(dataset>0)))
      #temp.dataset <- temp.dataset[, colSums(temp.dataset)>0] #Removing empty columns
      #temp.dataset <- temp.dataset[, rowSums(temp.dataset)>0] #Removing empty rows
      nonzero.entries <- which(temp.dataset>0, arr.ind = TRUE)
      coord <- nonzero.entries[sample(dim(nonzero.entries)[1],1),]
      temp.dataset[coord[1],coord[2]] <- 0
      temp.result[j,1] <- sum(rowSums(temp.dataset) > 0) #Number of arthropod species after reomoving one species
      temp.result[j,2] <- sum(temp.dataset)
      temp.result[j,c(3,4)] <- networklevel(temp.dataset, index=index)
    }
    result <- result + temp.result
  }
  result <- result/n
  return(result)
}

reduce <- function(dataset, margin, col=4, index = ""){
  # This function takes data matrix ordered by rows and by columns and reuces
  # it by removing least abundant components from given margin and calculates
  # proportion of abundance left.
  ws.temp <- dataset
  abu <- sum(dataset)
  prop <- matrix(0, nrow = dim(dataset)[margin], ncol = 4)
  prop [1, c(1,2)] <- c(1, dim(dataset)[margin]) # rows are species
  prop[1, c(3,4)] <- networklevel(ws.temp, index="vulnerability")
  for (i in 1:(dim(dataset)[margin]-1)){
    #print(i)
    #print(dim(ws.temp)[2] - i)
    if (margin == 2) {
      ws.temp <- ws.temp[, -dim(ws.temp)[margin]]
      res <- sum(ws.temp)/abu
      prop[i + 1, c(1,2)] <- c(res, dim(as.matrix(ws.temp))[margin])
      if (dim(as.matrix(ws.temp))[margin] > 1){
        prop[i + 1, c(3,4)] <- networklevel(ws.temp, index="vulnerability")
      }
    }
    else{
      ws.temp <- ws.temp[-dim(ws.temp)[margin], ]
      res <- sum(ws.temp)/abu
      prop[i + 1, c(1,2)] <- c(res, dim(ws.temp)[margin])
      if (dim(as.matrix(ws.temp))[margin] > 1){
        prop[i + 1, c(3,4)] <- networklevel(ws.temp, index="vulnerability")
      }
    }
    
  }
  prop[,2] <- prop[,2]/prop[1,2]
  return(prop)
}

rarefy.arthropods <- function(dataset, n = 100){
  #This function reduces randomly the number of arthropod species and looks at abundance
  result <- matrix(data = 0, nrow = dim(dataset)[2], ncol = 4)
  for (i in 1:n){
    n.arthropods <- sum(colSums(dataset) > 0) #number of arthropod species
    abundance <- sum(dataset)
    temp.result <- matrix(data = 0, nrow = dim(dataset)[2], ncol = 4) # temporal result matrix
    temp.result[1,1] <- n.arthropods # assigning first row of result matrix
    temp.result[1,2] <- abundance
    temp.result[1,c(3,4)] <- networklevel(dataset, index = "vulnerability")
    temp.dataset <- as.matrix(dataset) # creating a temprary dataset for manipulations
    for (j in 2:(dim(dataset)[2]-1)){ # no to reduce completely
      temp.dataset <- as.matrix(temp.dataset[, -sample(dim(temp.dataset)[2], 1)])
      print((i-1) + (j/sum(dataset>0)))
      temp.result[j,1] <- sum(colSums(temp.dataset) > 0) #Number of arthropod species after reomoving one species
      temp.result[j,2] <- sum(temp.dataset)
      temp.result[j,c(3,4)] <- networklevel(temp.dataset, index = "vulnerability")
    }
    result <- result + temp.result
  }
  result <- result/n
  return(result)
}
############ GUILDS INTERACTIONS VULN/GEN

# ORDERED INTRACTIONS
#networklevel(semi, index = "vulnerability")
#miners.intordV <- reduce.interactions(miners, cols=4,index = "vulnerability")
#write.table(miners.intordV, "miners.intordV.txt")
#mobile.intordV <- reduce.interactions(mobile, cols=4,index = "vulnerability")
#write.table(mobile.intordV, "mobile.intordV.txt")
#semi.intordV <- reduce.interactions(semi, cols=4, index = "vulnerability")
#write.table(semi.intordV, "semi.intordV.txt")

# RANDOMISED INTERACTIONS
#miners.intrandV <- rarefy.interactions(miners)
#write.table(miners.intrandV, "miners.intrandV.txt")
#mobile.intrandV <- rarefy.interactions(mobile, cols=4)
#write.table(mobile.intrandV, "mobile.intrandV.txt")
#semi.intrandV <- rarefy.interactions(semi, cols=4)
#write.table(semi.intrandV, "semi.intrandV.txt")

# ORDERED ARTHROPODS VULN
#miners.arthroordV <- reduce(miners,1)
#write.table(miners.arthroordV,"miners.arthroordV.txt")
#mobile.arthroordV <- reduce(mobile,1)
#write.table(mobile.arthroordV,"mobile.arthroordV.txt")
#semi.arthroordV <- reduce(semi, 1)
#write.table(semi.arthroordV,"semi.arthroordV.txt")

# RANDOM ARTHROPODS VULN (need to add function)
#miners.artrandV <- rarefy.arthropods(t(miners))
#write.table(miners.artrandV,"miners.artrandV.txt")
#mobile.artrandV <- rarefy.arthropods(t(mobile))
#write.table(mobile.artrandV,"mobile.artrandV.txt")
#semi.artrandV <- rarefy.arthropods(t(semi))
#write.table(semi.artrandV,"semi.artrandV.txt")

# ORDERED PLANTS
#miners.plantordV <- reduce(miners,2)
#write.table(miners.plantordV,"miners.plantordV.txt")
#mobile.plantordV <- reduce(mobile,2)
#write.table(mobile.plantordV,"mobile.plantordV.txt")
#semi.plantordV <- reduce(semi, 2)
#write.table(semi.plantordV,"semi.plantordV.txt")

# RANDOM PLANTS
#miners.plantrandV <- rarefy.arthropods(miners)
#write.table(miners.plantrandV,"miners.plantrandV.txt")
#mobile.plantrandV <- rarefy.arthropods(mobile)
#write.table(mobile.plantrandV,"mobile.plantrandV.txt")
#semi.plantrandV <- rarefy.arthropods(semi)
#write.table(semi.plantrandV,"semi.plantrandV.txt")


###############################################################
# PARASITES DATASET

# PAR INTERACTIONS
#par.randintV <- rarefy.interactions(parasites)
#write.table(par.randintV,"par.randintV.txt")
#par.ordintV <- reduce.interactions(parasites)
#write.table(par.ordintV,"par.ordintV.txt")
# PAR PAR
#par.randparV <- rarefy.arthropods(parasites)
#write.table(par.randparV,"par.randparV.txt")
#par.ordparV <- reduce(parasites,2)
#write.table(par.ordparV, "par.ordparV.txt")
# PAR HOST(caterpillars)
#par.randhostV <- rarefy.arthropods(t(parasites))
#write.table(par.randhostV,"par.randhostV.txt")
#par.ordhostV <- reduce(t(parasites),2)
#write.table(par.ordhostV,"par.ordhostV.txt")

################################################################
############# PLOTS ###########################################
#####################################

# PARASITES 
setwd("C://Users//szefe//Desktop//Work//extrapolation//datasets//randomisation with H2")
par.parrandH2 <- read.table("par.hostrandH2.txt")  #abs
par.parredH2 <- read.table("par.hostredH2.txt")   #abs
par.hostrandH2 <- read.table("par.parrandH2.txt")  #abs
par.hostredH2 <- read.table("par.parredH2.txt")    #rel
par.intrandH2 <- read.table("par.intrandH2.txt")   #abs
par.intredH2 <- read.table("par.intredH2.txt")     #rel
par.parrandH2[,2] <- par.parrandH2[,2]/par.parrandH2[1,2]
par.parredH2[,2] <- par.parredH2[,2]/par.parredH2[1,2]
par.hostrandH2[,2] <- par.hostrandH2[,2]/par.hostrandH2[1,2]
par.intrandH2[,2] <- par.intrandH2[,2]/par.intrandH2[1,2]


par.randintV <- read.table("par.randintV.txt")
par.randintV[,2]<- par.randintV[,2]/par.randintV[1,2]
par.ordintV<- read.table("par.ordintV.txt")
par.randparV<- read.table("par.randparV.txt")
par.randparV[,2] <- par.randparV[,2]/par.randparV[1,2]
par.ordparV<- read.table("par.ordparV.txt")
par.randhostV<- read.table("par.randhostV.txt")
par.randhostV[,2] <- par.randhostV[,2]/par.randhostV[1,2]
par.ordhostV<- read.table("par.ordhostV.txt")

# DATA FOR GUILDS ARTHROPODS
miners.arthrorand <- read.table("miners.arthrorand.txt")
mobile.arthrorand <- read.table("mobile.arthrorand.txt")
semi.arthrorand <- read.table("semi.arthrorand.txt")
miners.arthroord <- read.table("miners.arthroord.txt")
mobile.arthroord <- read.table("mobile.arthroord.txt")
semi.arthroord <- read.table("semi.arthroord.txt")
miners.arthrorand[,2]<-miners.arthrorand[,2]/miners.arthrorand[1,2]
mobile.arthrorand[,2]<-mobile.arthrorand[,2]/mobile.arthrorand[1,2]
semi.arthrorand[,2]<-semi.arthrorand[,2]/semi.arthrorand[1,2]
# READ DATA FOR GUILDS WITH PLANTS REDUCED
miners.plantrand<-read.table("miners.plantrand.txt")
mobile.plantrand<-read.table("mobile.plantrand.txt")
semi.plantrand<-read.table("semi.plantrand.txt")
miners.plantord<-read.table("miners.plantord.txt")
mobile.plantord<-read.table("mobile.plantord.txt")
semi.plantord<-read.table("semi.plantord.txt")
miners.plantrand[,2]<-miners.plantrand[,2]/miners.plantrand[1,2]
mobile.plantrand[,2]<-mobile.plantrand[,2]/mobile.plantrand[1,2]
semi.plantrand[,2]<-semi.plantrand[,2]/semi.plantrand[1,2]
# read datatables V1 abundance, V2 species
miners.arthroord <- read.table("miners.arthroord.txt", header = T)
mobile.arthroord <- read.table("mobile.arthroord.txt", header = T)
semi.arthroord <- read.table("semi.arthroord.txt", header = T)
miners.arthrorand <- read.table("miners.arthrorand.txt", header = T)
# INTERACTIONS
miners.intrand <- read.table("miners.rand.txt", header = T)
mobile.intrand <- read.table("mobile.rand.txt", header = T)
semi.intrand <- read.table("semi.rand.txt", header = T)
# Prepare the data, V1-number of species, V2-abundance, V3-H2
semi.intrand[1,3] <- semi.intrand[1,2]
semi.intrand[1,2] <- sum(semi)
semi.intrand[,2] <- semi.intrand[,2]/semi.intrand[1,2]
miners.intrand[1,3] <- miners.intrand[1,2]
miners.intrand[1,2] <- sum(miners)
miners.intrand[,2] <- miners.intrand[,2]/miners.intrand[1,2]
mobile.intrand[,2] <- mobile.intrand[,2]/mobile.intrand[1,2]

head(mobile.intrand)
head(miners.intrand)
head(semi.intrand)

# V1-abundance, V2-no of interactions,
miners.intord <- read.table("miners.redint.txt", header = T)
mobile.intord <- read.table("mobile.redint.txt", header = T)
semi.intord <- read.table("semi.redint.txt", header = T)

######## VULNERABILITY AND GENERALITY DATASETS ######

setwd("C:/R/how simple are tropical food webs/randomisation with H2")
miners.artrandV <- read.table("miners.artrandV.txt") #swiched C4-gen,C3-vul
mobile.artrandV <- read.table("mobile.artrandV.txt")
semi.artrandV <- read.table("semi.artrandV.txt")
miners.plantrandV <- read.table("miners.plantrandV.txt")
mobile.plantrandV <- read.table("mobile.plantrandV.txt")
semi.plantrandV <- read.table("semi.plantrandV.txt")
miners.plantordV <- read.table("miners.plantordV.txt")
mobile.plantordV <- read.table("mobile.plantordV.txt")
semi.plantordV <- read.table("semi.plantordV.txt")
miners.arthroordV <- read.table("miners.arthroordV.txt")
mobile.arthroordV <- read.table("mobile.arthroordV.txt")
semi.arthroordV <- read.table("semi.arthroordV.txt")
miners.intordV <- read.table("miners.intordV.txt")
mobile.intordV <- read.table("mobile.intordV.txt")
semi.intordV <- read.table("semi.intordV.txt")
miners.intrandV <- read.table("miners.intrandV.txt")
mobile.intrandV <- read.table("mobile.intrandV.txt")
semi.intrandV <- read.table("semi.intrandV.txt")

setwd("C:\\R\\how simple are tropical food webs\\simplification results")

# Reading the resuts
semi.row <- read.table("semi.row.txt")
mobi.row <- read.table("mobi.row.txt")
mine.row <- read.table("mine.row.txt")
para.row <- read.table("para.row.txt")

semi.col <- read.table("semi.col.txt")
mobi.col <- read.table("mobi.col.txt")
mine.col <- read.table("mine.col.txt")
para.col <- read.table("para.col.txt")

#list.files(getwd())

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# PLOTS PLOTS PLOTS
# Czy to jest dobrze???

pdf("parasites.pdf")
#windows(1600,1600)
par(mfrow=c(3,3),mar=c(3,3,3,3), cex.axis=0.6,cex.lab=0.6,cex.main=0.7,cex=1,
    mai=c(0.5,0.5,0.5,0.5), tck=-0.025, mgp=c(1,0.2,0), type="o")

# PLOT 1
plot(par.intrandH2[,2],par.intrandH2[,3], ylim=c(0.8, 1),
     axes=F,xlab="",ylab="",
     col=rgb(150,0,0,75,maxColorValue=255), pch=16)
par(new=TRUE)
plot(par.intredH2[,1],par.intredH2[,3], ylim=c(0.8, 1),
     axes=T,xlab="Prop. of parasites ind.",ylab="H2", main= "Interaction (186) removal",
     col=rgb(0,150,0,75,maxColorValue=255), pch=16)
abline(v=0.5, lty=2, col=rgb(150,0,0,75,maxColorValue=255))
text(0.9, 0.99, "A")

#PLOT 2
#par(new=TRUE)
plot(par.hostrandH2[,2],par.hostrandH2[,3], ylim=c(0.8, 1),
     axes=F,xlab="",ylab="",
     col=rgb(150,0,0,75,maxColorValue=255), pch=16)
par(new=TRUE)
plot(par.hostredH2[,1],par.hostredH2[,3], ylim=c(0.8, 1),
     axes=T,xlab="Prop. of parasites ind.",ylab="H2", main= "Host species (76) removal",
     col=rgb(0,150,0,75,maxColorValue=255), pch=16)
abline(v=0.5, lty=2, col=rgb(150,0,0,75,maxColorValue=255))
text(0.9, 0.99, "B")
# PLOT 3
#par(new=TRUE)
plot(par.parrandH2[,2],par.parrandH2[,3], ylim=c(0.8, 1),
     axes=F,xlab="",ylab="",
     col=rgb(150,0,0,75,maxColorValue=255), pch=16)
par(new=TRUE)
plot(par.parredH2[,1],par.parredH2[,3], ylim=c(0.8, 1),
     axes=T,xlab="Prop. of parasites ind.",ylab="H2",main= "Parasite species (121) removal",
     col=rgb(0,150,0,75,maxColorValue=255), pch=16)
abline(v=0.5, lty=2, col=rgb(150,0,0,75,maxColorValue=255))
text(0.9,0.99, "C")

#############################################################
# PARASITES GENERALITY
# PLOT 1
plot(par.randintV[,2],par.randintV[,3], ylim=c(1, 1.6),
     axes=F,xlab="",ylab="",
     col=rgb(150,0,0,75,maxColorValue=255), pch=16)
par(new=TRUE)
plot(par.ordintV[,2],par.ordintV[,3], ylim=c(1, 1.6),
     axes=T,xlab="Prop. of parasites ind.",ylab="Generality",main="Interaction (186) removal",
     col=rgb(0,150,0,75,maxColorValue=255), pch=16)
abline(v=0.5, lty=2, col=rgb(150,0,0,75,maxColorValue=255))
text(0.1,1.56, "D")
#PLOT 2
#par(new=TRUE)
plot(par.randhostV[,2],par.randhostV[,4], ylim=c(1, 2),  #
     axes=F,xlab="",ylab="",
     col=rgb(150,0,0,75,maxColorValue=255), pch=16)
par(new=TRUE)
plot(par.ordhostV[,2],par.ordhostV[,4], ylim=c(1, 2),    #
     axes=T,xlab="Prop. of parasites ind.",ylab="Generality",main="Host species (76) removal",
     col=rgb(0,150,0,75,maxColorValue=255), pch=16)
abline(v=0.5, lty=2, col=rgb(150,0,0,75,maxColorValue=255))
text(0.9,3.4, "E")
# PLOT 3
#par(new=TRUE)
plot(par.randparV[,2],par.randparV[,3], ylim=c(1, 2),
     axes=F,xlab="",ylab="",
     col=rgb(150,0,0,75,maxColorValue=255), pch=16)
par(new=TRUE)
plot(par.ordparV[,2],par.ordparV[,3], ylim=c(1, 2),
     axes=T,xlab="Prop. of parasites ind.",ylab="Generality",main="Parasite species (121) removal",
     col=rgb(0,150,0,75,maxColorValue=255), pch=16)
abline(v=0.5, lty=2, col=rgb(150,0,0,75,maxColorValue=255))
text(0.9,1.94, "F")

# PARASITES VULNERABILITY
# PLOT 1
plot(par.randintV[,2],par.randintV[,4], ylim=c(1, 2.8),
     axes=F,xlab="",ylab="",
     col=rgb(150,0,0,75,maxColorValue=255), pch=16)
par(new=TRUE)
plot(par.ordintV[,2],par.ordintV[,4], ylim=c(1, 2.8),
     axes=T,xlab="Prop. of parasites ind.",ylab="Vulnerability",main="Interaction (186) removal",
     col=rgb(0,150,0,75,maxColorValue=255), pch=16)
abline(v=0.5, lty=2, col=rgb(150,0,0,75,maxColorValue=255))
text(0.1,2.65, "G")
#PLOT 2
#par(new=TRUE)
plot(par.randhostV[,2],par.randhostV[,3], ylim=c(2, 3.5),
     axes=F,xlab="",ylab="",
     col=rgb(150,0,0,75,maxColorValue=255), pch=16)
par(new=TRUE)
plot(par.ordhostV[,2],par.ordhostV[,3], ylim=c(2, 3.5),
     axes=T,xlab="Prop. of parasites ind.",ylab="Vulnerability",main="Host species (76) removal",
     col=rgb(0,150,0,75,maxColorValue=255), pch=16)
abline(v=0.5, lty=2, col=rgb(150,0,0,75,maxColorValue=255))
text(0.1,1.55, "H")
# PLOT 3
#par(new=TRUE)
plot(par.randparV[,2],par.randparV[,4], ylim=c(1, 2.8),
     axes=F,xlab="",ylab="",
     col=rgb(150,0,0,75,maxColorValue=255), pch=16)
par(new=TRUE)
plot(par.ordparV[,2],par.ordparV[,4], ylim=c(1, 2.8),
     axes=T,xlab="Prop. of parasites ind.",ylab="Vulnerability",main="Parasite species (121) removal",
     col=rgb(0,150,0,75,maxColorValue=255), pch=16)
abline(v=0.5, lty=2, col=rgb(150,0,0,75,maxColorValue=255))
text(0.1,2.65, "I")

dev.off()





##############################################################
# GUILDS INTERACTION
# Random
pdf("guilds_int.pdf")
windows(1600,1600)
par(mfrow=c(3,3),mar=c(3,3,3,3), cex.axis=0.6,cex.lab=0.6,cex.main=0.7,cex=1,
    mai=c(0.5,0.5,0.5,0.5), tck=-0.025, mgp=c(1,0.2,0))
plot(semi.intrand[,2], semi.intrand[,3],
     axes=F, ylab="",xlab="", main = "Interaction removal",
     xlim = c(0,1), ylim=c(0.35,1),
     col=rgb(150,0,0,50,maxColorValue=255),pch=16)

par(new=TRUE)
plot(miners.intrand[,2], miners.intrand[,3],
     axes=F, ylab="",xlab="",
     xlim = c(0,1), ylim=c(0.35,1),
     col=rgb(150,0,0,50,maxColorValue=255),pch=16)

par(new=TRUE)
plot(mobile.intrand[,2], mobile.intrand[,3], 
     axes=T, ylab="",xlab="",
     xlim = c(0,1), ylim=c(0.35,1),
     col=rgb(150,0,0,50,maxColorValue=255),pch=16)

# And nonrandom
par(new=TRUE)
plus = 0.05
plot(semi.intord[,1], semi.intrand[,3], 
     axes=F, ylab="",xlab="",
     xlim = c(0,1), ylim=c(0.35,1),
     col=rgb(0,150,0,50,maxColorValue=255),pch=16)
text(semi.intrand[1,2], semi.intrand[1,3]+plus,"SC",cex=0.5)
par(new=TRUE)
plot(miners.intord[,1], miners.intrand[,3], 
     axes=F, ylab="",xlab="",
     xlim = c(0,1), ylim=c(0.35,1),
     col=rgb(0,150,0,50,maxColorValue=255),pch=16)
text(miners.intrand[1,2], miners.intrand[1,3]+plus,"MN",cex=0.5)
par(new=TRUE)
plot(mobile.intord[,1], mobile.intrand[,3], 
     axes= T, ylab = "H2", xlab = "Proportion of arthropod ind.", 
     xlim = c(0,1), ylim=c(0.35,1),
     col=rgb(0,150,0,50,maxColorValue=255),pch=16)
text(mobile.intrand[1,2], mobile.intrand[1,3]+plus,"MC",cex=0.5)
abline(v=0.5, lty=2, col=rgb(150,0,0,75,maxColorValue=255))


# PLANTS
# Plants random
plot(miners.plantrand[,2], miners.plantrand[,3],
     ylim=c(0.35,1), xlab="", ylab="", axes=F, main = "Plant species (38) removal",
     col=rgb(150,0,0,75,maxColorValue=255),pch=16)
par(new=T)
plot(mobile.plantrand[,2], mobile.plantrand[,3],
     ylim=c(0.35,1), xlab="", ylab="", axes=F,
     col=rgb(150,0,0,75,maxColorValue=255),pch=16)
par(new=T)
plot(semi.plantrand[,2], semi.plantrand[,3],
     ylim=c(0.35,1), xlab="", ylab="", axes=F,
     col=rgb(150,0,0,75,maxColorValue=255),pch=16)

#Ordered
par(new=T)
plot(miners.plantord[,1], miners.plantord[,3],
     ylim=c(0.35,1), xlab="", ylab="", axes=F,
     col=rgb(0,150,0,75,maxColorValue=255),pch=16)
par(new=T)
plot(mobile.plantord[,1], mobile.plantord[,3],
     ylim=c(0.35,1), xlab="", ylab="", axes=F,
     col=rgb(0,150,0,75,maxColorValue=255),pch=16)
par(new=T)
plot(semi.plantord[,1], semi.plantord[,3],
     ylim=c(0.35,1), xlab="Proportion of arthropod ind.", ylab="H2", axes=T,
     col=rgb(0,150,0,75,maxColorValue=255),pch=16)
abline(v=0.5, lty=2, col=rgb(150,0,0,75,maxColorValue=255))


# ARTHROPODS-CATERPILLARS

plot(miners.arthrorand[,2],miners.arthrorand[,3],
     ylim=c(0.25, 1), xlab="", ylab="", axes=F, main = "Arthropod species removal",
     col=rgb(150,0,0,75, maxColorValue=255),pch=16)
par(new=T)
plot(mobile.arthrorand[,2],mobile.arthrorand[,3],
     ylim=c(0.25, 1), xlab="", ylab="", axes=F,
     col=rgb(150,0,0,75, maxColorValue=255),pch=16)
par(new=T)
plot(semi.arthrorand[,2],semi.arthrorand[,3],
     ylim=c(0.25, 1), xlab="", ylab="", axes=F,
     col=rgb(150,0,0,75, maxColorValue=255),pch=16)

# Ordered removal
par(new=T)
plot(miners.arthroord[,1],miners.arthroord[,3],
     ylim=c(0.25, 1), xlab="", ylab="", axes=F,
     col=rgb(0,150,0,75, maxColorValue=255),pch=16)
par(new=T)
plot(mobile.arthroord[,1],mobile.arthroord[,3],
     ylim=c(0.25, 1), xlab="", ylab="", axes=F,
     col=rgb(0,150,0,75, maxColorValue=255),pch=16)
par(new=T)
plot(semi.arthroord[,1],semi.arthroord[,3],
     ylim=c(0.25, 1), xlab="Proportion of arthropod ind.", ylab="H2", axes=T,
     col=rgb(0,150,0,75, maxColorValue=255),pch=16)
abline(v=0.5, lty=2, col=rgb(150,0,0,75,maxColorValue=255))


#######  GENERALITY

#Interactions GENERALITY
plot(miners.intrandV[,2]/miners.intrandV[1,2], miners.intrandV[,3], #intrandV C3 is VULN, C4 GEN
     ylim=c(1,12.5), axes=F, xlab="", ylab="", main = "Interaction removal",
     col=rgb(150,0,0,75,maxColorValue=255),pch=16)
par(new=T)
plot(miners.intordV[,2],miners.intordV[,3],
     ylim=c(1,12.5), axes=F, xlab="", ylab="",
     col=rgb(0,150,0,75,maxColorValue=255),pch=16)
# Mobile
par(new=T)
plot(mobile.intrandV[,2]/mobile.intrandV[1,2], mobile.intrandV[,3],
     ylim=c(1,12.5), axes=F, xlab="", ylab="",
     col=rgb(150,0,0,75,maxColorValue=255),pch=16)
par(new=T)
plot(mobile.intordV[,2],mobile.intordV[,3],
     ylim=c(1,12.5), axes=F, xlab="", ylab="",
     col=rgb(0,150,0,75,maxColorValue=255),pch=16)
# Semi
par(new=T)
plot(semi.intrandV[,2]/semi.intrandV[1,2], semi.intrandV[,3],
     ylim=c(1,12.5), axes=T, xlab="Proportion of arthropod ind.", ylab="Generality",
     col=rgb(150,0,0,75,maxColorValue=255),pch=16)
par(new=T)
plot(semi.intordV[,2],semi.intordV[,3],
     ylim=c(1,12.5), axes=F, xlab="", ylab="",
     col=rgb(0,150,0,75,maxColorValue=255),pch=16)
abline(v=0.5,col=rgb(150,0,0,75,maxColorValue=255),lty=2)



#PLANTS GENERALITY (nothing changes, we remove plants with ind)
plot(miners.plantrandV[,2]/miners.plantrandV[1,2], miners.plantrandV[,3], #plantrandV C3 is VULN, C4 GEN
     ylim=c(1,12.5), axes=F, xlab="", ylab="", main = "Plant species removal",
     col=rgb(150,0,0,75,maxColorValue=255),pch=16)
par(new=T)
plot(miners.plantordV[,2],miners.plantordV[,3],
     ylim=c(1,12.5), axes=F, xlab="", ylab="",
     col=rgb(0,150,0,75,maxColorValue=255),pch=16)
# Mobile
par(new=T)
plot(mobile.plantrandV[,2]/mobile.plantrandV[1,2], mobile.plantrandV[,3],
     ylim=c(1,12.5), axes=F, xlab="", ylab="",
     col=rgb(150,0,0,75,maxColorValue=255),pch=16)
par(new=T)
plot(mobile.plantordV[,2],mobile.plantordV[,3],
     ylim=c(1,12.5), axes=F, xlab="", ylab="",
     col=rgb(0,150,0,75,maxColorValue=255),pch=16)
# Semi
par(new=T)
plot(semi.plantrandV[,2]/semi.plantrandV[1,2], semi.plantrandV[,3],
     ylim=c(1,12.5), axes=T, xlab="Proportion of arthropod ind.", ylab="Generality",
     col=rgb(150,0,0,75,maxColorValue=255),pch=16)
par(new=T)
plot(semi.plantordV[,2],semi.plantordV[,3],
     ylim=c(1,12.5), axes=F, xlab="", ylab="",
     col=rgb(0,150,0,75,maxColorValue=255),pch=16)
abline(v=0.5,col=rgb(150,0,0,75,maxColorValue=255),lty=2)


# Arthropods GENERALITY
plot(miners.artrandV[,2]/miners.artrandV[1,2], miners.artrandV[,4], #artrandV C4 is VULN, C3 GEN
     ylim=c(1,12.5), axes=F, xlab="", ylab="", main = "Arthropod species removal",
     col=rgb(150,0,0,75,maxColorValue=255),pch=16)
par(new=T)
plot(miners.arthroordV[,2],miners.arthroordV[,3],
     ylim=c(1,12.5), axes=F, xlab="", ylab="",
     col=rgb(0,150,0,75,maxColorValue=255),pch=16)
#Mobile
par(new=T)
plot(mobile.artrandV[,2]/mobile.artrandV[1,2], mobile.artrandV[,4],
     ylim=c(1,12.5), axes=F, xlab="", ylab="",
     col=rgb(150,0,0,75,maxColorValue=255),pch=16)
par(new=T)
plot(mobile.arthroordV[,2],mobile.arthroordV[,3],
     ylim=c(1,12.5), axes=F, xlab="", ylab="",
     col=rgb(0,150,0,75,maxColorValue=255),pch=16)

par(new=T)
plot(semi.artrandV[,2]/semi.artrandV[1,2], semi.artrandV[,4],
     ylim=c(1,12.5), axes=T, xlab="Proportion of arthropod ind.", ylab="Generality",
     col=rgb(150,0,0,75,maxColorValue=255),pch=16)
par(new=T)
plot(semi.arthroordV[,2],semi.arthroordV[,3],
     ylim=c(1,12.5), axes=F, xlab="", ylab="",
     col=rgb(0,150,0,75,maxColorValue=255),pch=16)
abline(v=0.5,col=rgb(150,0,0,75,maxColorValue=255),lty=2)




############################# UNDER CONSTRUCTION #################################
networklevel(miners, index="vulnerability")
networklevel(mobile, index="vulnerability")
networklevel(semi, index="vulnerability")

head(miners.intrandV)
head(mobile.intrandV)
head(semi.intrandV)

head(miners.intordV)
head(mobile.intordV)
head(semi.intordV)

# VULNERABILITY FOR GUILDS
#Interactions VULNERABILITY
plot(miners.intrandV[,2]/miners.intrandV[1,2], miners.intrandV[,4], #intrandV C4 is VULN, C3 GEN
     ylim=c(1,15), axes=F, xlab="", ylab="", main = "Interaction removal",
     col=rgb(150,0,0,75,maxColorValue=255),pch=16)
par(new=T)
plot(miners.intordV[,2],miners.intordV[,4],
     ylim=c(1,15), axes=F, xlab="", ylab="",
     col=rgb(0,150,0,75,maxColorValue=255),pch=16)
# Mobile
par(new=T)
plot(mobile.intrandV[,2]/mobile.intrandV[1,2], mobile.intrandV[,4],
     ylim=c(1,15), axes=F, xlab="", ylab="",
     col=rgb(150,0,0,75,maxColorValue=255),pch=16)
par(new=T)
plot(mobile.intordV[,2],mobile.intordV[,4],
     ylim=c(1,15), axes=F, xlab="", ylab="",
     col=rgb(0,150,0,75,maxColorValue=255),pch=16)
# Semi
par(new=T)
plot(semi.intrandV[,2]/semi.intrandV[1,2], semi.intrandV[,4],
     ylim=c(1,15), axes=T, xlab="Proportion of arthropod ind.", ylab="Vulnerability",
     col=rgb(150,0,0,75,maxColorValue=255),pch=16)
par(new=T)
plot(semi.intordV[,2],semi.intordV[,4],
     ylim=c(1,15), axes=F, xlab="", ylab="",
     col=rgb(0,150,0,75,maxColorValue=255),pch=16)
abline(v=0.5,col=rgb(150,0,0,75,maxColorValue=255),lty=2)



#PLANTS VULNERABILITY
plot(miners.plantrandV[,2]/miners.plantrandV[1,2], miners.plantrandV[,4], #plantrandV C3 is VULN, C4 GEN
     ylim=c(1,15), axes=F, xlab="", ylab="", main = "Plant species removal",
     col=rgb(150,0,0,75,maxColorValue=255),pch=16)
par(new=T)
plot(miners.plantordV[,2],miners.plantordV[,4],
     ylim=c(1,15), axes=F, xlab="", ylab="",
     col=rgb(0,150,0,75,maxColorValue=255),pch=16)
# Mobile
par(new=T)
plot(mobile.plantrandV[,2]/mobile.plantrandV[1,2], mobile.plantrandV[,4],
     ylim=c(1,15), axes=F, xlab="", ylab="",
     col=rgb(150,0,0,75,maxColorValue=255),pch=16)
par(new=T)
plot(mobile.plantordV[,2],mobile.plantordV[,4],
     ylim=c(1,15), axes=F, xlab="", ylab="",
     col=rgb(0,150,0,75,maxColorValue=255),pch=16)
# Semi
par(new=T)
plot(semi.plantrandV[,2]/semi.plantrandV[1,2], semi.plantrandV[,4],
     ylim=c(1,15), axes=T, xlab="Proportion of arthropod ind.", ylab="Vulnerability",
     col=rgb(150,0,0,75,maxColorValue=255),pch=16)
par(new=T)
plot(semi.plantordV[,2],semi.plantordV[,4],
     ylim=c(1,15), axes=F, xlab="", ylab="",
     col=rgb(0,150,0,75,maxColorValue=255),pch=16)
abline(v=0.5,col=rgb(150,0,0,75,maxColorValue=255),lty=2)


# Arthropods VULNERABILITY (WEEEEEEIRD!!!)
plot(miners.artrandV[,2]/miners.artrandV[1,2], miners.artrandV[,3],
     ylim=c(1,21.5), axes=F, xlab="", ylab="", main = "Arthropod species removal",
     col=rgb(150,0,0,75,maxColorValue=255),pch=16)
par(new=T)
plot(miners.arthroordV[,2],miners.arthroordV[,4],
     ylim=c(1,21.5), axes=F, xlab="", ylab="",
     col=rgb(0,150,0,75,maxColorValue=255),pch=16)
#Mobile
par(new=T)
plot(mobile.artrandV[,2]/mobile.artrandV[1,2], mobile.artrandV[,3],
     ylim=c(1,21.5), axes=F, xlab="", ylab="",
     col=rgb(150,0,0,75,maxColorValue=255),pch=16)
par(new=T)
plot(mobile.arthroordV[,2],mobile.arthroordV[,4],
     ylim=c(1,21.5), axes=F, xlab="", ylab="",
     col=rgb(0,150,0,75,maxColorValue=255),pch=16)

par(new=T)
plot(semi.artrandV[,2]/semi.artrandV[1,2], semi.artrandV[,3],
     ylim=c(1,21.5), axes=T, xlab="Proportion of arthropod ind.", ylab="Vulnerability",
     col=rgb(150,0,0,75,maxColorValue=255),pch=16)
par(new=T)
plot(semi.arthroordV[,2],semi.arthroordV[,4],
     ylim=c(1,21.5), axes=F, xlab="", ylab="",
     col=rgb(0,150,0,75,maxColorValue=255),pch=16)
abline(v=0.5,col=rgb(150,0,0,75,maxColorValue=255),lty=2)

dev.off()




################################# nEW CORRECTED PLOTS

setwd("C:\\R\\how simple are tropical food webs\\simplification results")
# Reading the resuts
semi.row <- read.table("semi.row.txt")
mobi.row <- read.table("mobi.row.txt")
mine.row <- read.table("mine.row.txt")
para.row <- read.table("para.row.txt")

semi.col <- read.table("semi.col.txt")
mobi.col <- read.table("mobi.col.txt")
mine.col <- read.table("mine.col.txt")
para.col <- read.table("para.col.txt")



# Plots

red <- rgb(150,0,0,75,maxColorValue=255) 
grn <- rgb(0,150,0,75,maxColorValue=255)

res.list <- list(mine.col, mobi.col, semi.col,
                 mine.row, mobi.row, semi.row)
colours <- c(rgb(150,0,0,75,maxColorValue=255),rgb(0,150,0,75,maxColorValue=255))


# Plot COLS

par(mfrow=c(2,3))
titles <- c("H2", "","Generality", "","Vulnerability")

for (num in c(10,12,14)){ # specify columns with H2 Vuln Gen
  vals <- c(res.list[[1]][,num],res.list[[1]][,num+1],
            res.list[[2]][,num],res.list[[2]][,num+1],
            res.list[[3]][,num],res.list[[3]][,num+1])
  xlim <- c(0  ,1)
  ylim <- c(min(vals[vals>0], na.rm=T),max(vals[vals>0], na.rm=T))
  
  plot(NULL, xlim=xlim, ylim=ylim, ylab=titles[num-9], xlab="Prop. of arthropods",
       main = "Arthropod species removal")
  
  for(j in 1:3){
    
    par(new=TRUE)
    plot(res.list[[j]][,2]/res.list[[j]][1,2], res.list[[j]][,num],
         col=colours[2], pch=16, ylim=ylim,
         xlim = xlim, axes=FALSE, xlab="",ylab="")
    par(new=TRUE)
    plot(res.list[[j]][,3]/res.list[[j]][1,3], res.list[[j]][,num + 1],
         col=colours[1], pch=16, ylim=ylim,
         xlim = xlim, axes=FALSE, xlab="",ylab="")
    abline(v=0.5,col=colours[1],lty=2)
    par(new=TRUE)
  }
  par(new=FALSE)
}

# Plot ROWS

#par(mfrow=c(1,3))
titles <- c("H2", "","Generality", "","Vulnerability")

for (num in c(10,12,14)){ # specify columns with H2 Vuln Gen
  vals <- c(res.list[[4]][,num],res.list[[4]][,num+1],     #
            res.list[[5]][,num],res.list[[5]][,num+1],
            res.list[[6]][,num],res.list[[6]][,num+1])
  xlim <- c(0  ,1)
  ylim <- c(min(vals[vals>0], na.rm=T),max(vals[vals>0], na.rm=T))
  print(ylim)
  plot(NULL, xlim=xlim, ylim=ylim, ylab=titles[num-9], xlab="Prop. of arthropods",
       main = "Plant species removal")
  
  for(j in 4:6){
    
    par(new=TRUE)
    plot(res.list[[j]][,2]/res.list[[j]][1,2], res.list[[j]][,num],
         col=colours[2], pch=16, ylim=ylim,
         xlim = xlim, axes=FALSE, xlab="",ylab="")
    par(new=TRUE)
    plot(res.list[[j]][,3]/res.list[[j]][1,3], res.list[[j]][,num + 1],
         col=colours[1], pch=16, ylim=ylim,
         xlim = xlim, axes=FALSE, xlab="",ylab="")
    abline(v=0.5,col=colours[1],lty=2)
    par(new=TRUE)
  }
  par(new=FALSE)
}
