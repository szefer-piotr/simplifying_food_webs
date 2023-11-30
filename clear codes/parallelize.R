# Installing necessary packages
install.packages("bipartite", "foreach", "parallel", "doParallel")

# Calculations 

setwd("Dropbox/Food_webs")
source("funct_simplify2.R")

guilds <- read.table("guilds.csv", header = TRUE, sep = "\t")
guilds[is.na(guilds)] <- 0
guilds.val <- guilds[,2:39] #Plants are in columns and arthropods are in rows
#setwd("C:\\R\\how simple are tropical food webs\\tritrophic data")
#equal <- read.csv("ufwEqual.csv", header = TRUE, sep = "\t")
#setwd("C:\\R\\how simple are tropical food webs")
#equal <- as.matrix(equal[, c(-1,-2)])
#equal[is.na(equal)] <- 0
#removing zeros from parasite data
#parasites <- equal[-which(rowSums(equal)==0),]
#miners <- guilds.val[guilds$Guild == unique(guilds$Guild)[3],]
mobile <- guilds.val[guilds$Guild == unique(guilds$Guild)[4],]
#semi   <- guilds.val[guilds$Guild == unique(guilds$Guild)[5],]


#Transposing matrices
#miners <- t(miners)
mobile <- t(mobile)
#semi   <- t(semi)

# Getting libraries loaded
library(parallel)
library(foreach)
library(doParallel)

# Register a cluster
# Detect the number of cores
numCores <-detectCores() -1
cl <- makeCluster(numCores)
registerDoParallel(cl)

randomizations <- 100
rand_per_core <- floor(randomizations/numCores)

# Paralellized simulation

numCores # Number of cores
rand_per_core # Number of iteration per core

# File simulation keeps randomisation results
# Randomising interactions
system.time({
  mobInt <- foreach(rep(rand_per_core, numCores),.combine = '+')  %dopar% {
    simplify2(mobile,  edge="int",n=rand_per_core,reduction.level=0.99)
  }
})

# The resulting list contains elements equals number of cores.
# Each is a sum of randomisations per core
write.table(simulations, "mobInterPar.txt")

stopCluster(cl)
