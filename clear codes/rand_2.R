# Guilds data
require(bipartite)
setwd("C:\\R\\how simple are tropical food webs")
source("funct_simplifyV2.R")

setwd("C:\\R\\how simple are tropical food webs")
guilds <- read.table("guilds.csv", header = TRUE, sep = "\t")
guilds[is.na(guilds)] <- 0
guilds.val <- guilds[,2:39] #Plants are in columns and arthropods are in rows

#Subsetting for five guilds
min <- guilds.val[guilds$Guild == unique(guilds$Guild)[3],]
#mob <- guilds.val[guilds$Guild == unique(guilds$Guild)[4],]
#sem <- guilds.val[guilds$Guild == unique(guilds$Guild)[5],]
#mes <- guilds.val[guilds$Guild == unique(guilds$Guild)[2],]
#exp <- guilds.val[guilds$Guild == unique(guilds$Guild)[1],]


min <- t(min)
#mob <- t(mob)
#sem <- t(sem)
#mes <- t(mes)
#exp <- t(exp)


# Random removal of interactions and saving all matrices in one list.
#min <- matrix(c(1,2,3,0,0,1,9,0,2), ncol=3, nrow=3 )

dataset <- as.matrix(min)
colnames(dataset) <- c()
rownames(dataset) <- c()


maxi <- sum(dataset>0) #Number of interactions
maxj <- 10 # number of iterations

# Create an empty list
result.list <- list()
# Specify the starting index value for list. Will be changed during iterations
list_index <- 1

for (j in 1:maxj){
  # The loop starts with a complete list and returns 
  # reduced matrix with the last one interaction present
  temp.dat<-dataset
  for (i in 1:maxi){
    print(list_index)                             # print the list index
    result.list[[list_index]] <- temp.dat         # add a new dataset to the list
    nonzero.int <- which(temp.dat>0, arr.ind=T)   # look for nonzero entries
    rnd.int <- sample(dim(nonzero.int)[1], 1)     # get the indices for nonzero vals
    row <- nonzero.int[rnd.int,1]                 # get the random row
    col <- nonzero.int[rnd.int,2]                 # get the random column
    temp.dat[row, col] <- 0                       # substitute given interaction with zero
    list_index <- list_index+1                    # increase the list coun var by 1
  }
}

system.time({
  res.calc <- lapply(result.list,calcChar)
})

#install.packages("abind")
library(abind)
start <- 1
end <- maxi
iterations.results <- t(abind(res.calc[start:end], along=2))

for (iter in 1:maxj){
  start <- 1+((iter-1)*maxi)
  end <- iter*maxi
  iterations <- abind(res.calc[start:end], along=2)
  iterations <- t(iterations)
  iterations.results <- iterations.results+iterations
}

iterations.results <- iterations.results/(maxi+1)


