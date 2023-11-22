
# Guilds data
require(bipartite)
setwd("C:\\Users\\Piotr Szefer\\Desktop\\Work\\extrapolation\\datasets")
guilds <- read.table("guilds.csv", header = TRUE, sep = "\t")
guilds[is.na(guilds)] <- 0
guilds.val <- guilds[,2:39] #Plants are in columns and arthropods are in rows

#Subsetting for five guilds
min <- guilds.val[guilds$Guild == unique(guilds$Guild)[3],]
mob <- guilds.val[guilds$Guild == unique(guilds$Guild)[4],]
sem <- guilds.val[guilds$Guild == unique(guilds$Guild)[5],]
mes <- guilds.val[guilds$Guild == unique(guilds$Guild)[2],]
exp <- guilds.val[guilds$Guild == unique(guilds$Guild)[1],]


min <- t(min)
mob <- t(mob)
sem <- t(sem)
mes <- t(mes)
exp <- t(exp)

# Parasites dataset
equal <- read.csv("ufwEqual.csv", header = TRUE, sep = "\t")
equal <- as.matrix(equal[, c(-1,-2)])
equal[is.na(equal)] <- 0

par <- equal[-which(rowSums(equal)==0),]
sum(colSums(par) == 0)
sum(rowSums(par) == 0)


# IBISCA temporal
tfl <- read.csv("flatidae.csv", header = TRUE, sep = "\t")
tar <- read.csv("arctiidae.csv", header = TRUE, sep = "\t")
tge <- read.csv("geometridae.csv", header = TRUE, sep = "\t")
tpy<- read.csv("pyraloidea.csv", header = TRUE, sep = "\t")
# Temporal dataset needs some cleaning.
tpy[is.na(tpy)] <- 0
tge[is.na(tge)] <- 0
tfl[is.na(tfl)] <- 0
tar[is.na(tar)] <- 0

tpy <- tpy[,-c(1,2,3,4)]
tge <- tge[,-c(1,2,3,4)]
tfl <- tfl[,-c(1,2,3,4)]
tar <- tar[,-c(1,2,3,4)]

# Spatial
read.table("BER.csv",header=T, sep = ",") -> ber
read.table("COP.csv",header=T, sep = ",") -> cop
read.table("MAA.csv",header=T, sep = ",") -> maa
read.table("SYW.csv",header=T, sep = ",") -> syw

ber <- ber[,-1]
cop <- cop[,-1]
maa <- maa[,-1]
syw <- syw[,-1]


# Keeping data together
datasets <- list(min,mob,sem,mes,exp,
                 tfl,tar,tge,tpy,
                 par,
                 ber,cop,maa,syw)


# Standard stuff with calcChar
setwd("C:\\Users\\Piotr Szefer\\Desktop\\Work\\extrapolation\\clear codes")
source("funct_simplifyV2.R")

indMat <- matrix(0,nrow=length(datasets), ncol=10)
colnames(indMat) <- c("Abu","Row","Col","Int","H2","Gen","Vul","50r","50c","50I")
rownames(indMat) <- c("Miners","Mobile","Semi","Mesoph","Exposed",
                      "Flatriidae","Arctidae","Geometridae","Pyralidae",
                      "Parasites","BER","COP","MAA","SYW")

for ( i in 1:length(datasets)){
  print(datasets[[i]][1:5,1:5])
  indMat[i,] <- calcChar(as.matrix(datasets[[i]]))
}



# Robustness
robRes <- matrix(0,ncol=1,nrow=length(datasets))
for (i in 1:length(datasets)){
  ex <- second.extinct(datasets[[i]], participant="lower", method="random", nrep=100,
                       details=FALSE)
  robRes[i,] <- robustness(ex)
}

indMat3 <- cbind(indMat2, robRes)
colnames(indMat3) <- c("Abu","Row","Col","Int","H2","Gen","Vul","50r","50c","50I","NODF", "Robust")

library(knitr)

#write.table(indMat3, "indMat.txt")

# Order and clear function

order.and.clear2 <- function(dataset){
  #This function order the dataset based on rows and columns
  #and removes zero rows and columns. Dataset should not contain any NAs!
  dataset <- dataset[rowSums(dataset)>0,]
  dataset <- dataset[, colSums(dataset)>0]
  dataset[dataset>0] <- 1
  dataset <- dataset[order(rowSums(dataset), decreasing = TRUE), ]
  dataset <- dataset[,order(colSums(dataset), decreasing = TRUE)]
  return(dataset)
}

nesRes <- matrix(0, ncol=1, nrow=length(datasets))
for ( i in 1:length(datasets)){
  nesRes[i,] <- nested(order.and.clear2(datasets[[i]]),method="NODF")
}

indMat2 <- cbind(indMat, nesRes)

# Nestedness
method="NODF"
min2 <- order.and.clear2(min)
mob2 <- order.and.clear2(mob)
sem2 <- order.and.clear2(sem)
mes2 <- order.and.clear2(mes)
exp2 <- order.and.clear2(exp)

nested(min2, method=method)
nested(mob2, method=method)
nested(sem2, method=method)
nested(mes2, method=method)
nested(exp2, method=method)

par(mfrow=c(3,2),mar=c(3,3,3,3))
image(as.matrix(min2),col=c("white",rep("black",120)),main="Miners")
image(as.matrix(mob2),col=c("white",rep("black",120)),main="Mobile")
image(as.matrix(sem2),col=c("white",rep("black",120)),main="Semi-C")
image(as.matrix(mes2),col=c("white",rep("black",120)),main="Mesoph")
image(as.matrix(exp2),col=c("white",rep("black",120)),main="Expose")


#removing zeros from parasite data
require("bipartite")
datasets <- list(miners,mobile,semi,parasites)

tab <- matrix(0,ncol=10,nrow=4)

for ( i in 1:4){
  tab[i,]<- calcChar(datasets[[i]])
}

colnames(tab) <- names(calcChar(miners))
rownames(tab) <- c("Miners","Mobile", "Semi","Prasitoids")
#write.table(tab,"network properties.txt")