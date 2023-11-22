# Food web analyses

setwd("C:\\Users\\Piotr Szefer\\Desktop\\Work\\extrapolation\\datasets")
guilds <- read.table("guilds.csv", header = TRUE, sep = "\t")
names <- as.character(guilds[,1])
names[5] <- "CHRY133a"
rownames(guilds) <- names
#Chunk of code for individual based rarefaction

miners <- t(guilds[guilds$Guild == unique(guilds$Guild)[3], 2:(dim(guilds)[2] - 6)])
mobile.chewers <- t(guilds[guilds$Guild == unique(guilds$Guild)[4], 2:(dim(guilds)[2] - 6)])
semi <- t(guilds[guilds$Guild == unique(guilds$Guild)[5], 2:(dim(guilds)[2] - 6)])

#  LINK STRENGTH DISTRIBUTION 
## Mobile chewers 

mc <- mobile.chewers
mc[is.na(mc)] <- 0

# Rossberg 2013
S <- dim(mc)[1] + dim(mc)[2]
Sc <- dim(mc)[2]
L <- sum(mc>0)
C <- L/((dim(mc)[1]+dim(mc)[2])^2)
Cc <- L/(S*Sc)
R <- 100
Zc <- L/Sc
Xmax <- sqrt(2*log(S-Sc))
sigma <- Xmax*(log(R)/log(Zc))
Xmax/sigma
log(R)/sigma

# Can I get trophic link strengths from it? This is not good...
hist(rlnorm(10000, mean, log(sigma)), breaks=50)
mean <- mean(log(mc+1))

# Diet partitioning function DPF
hist(as.vector(mc[mc>0]), breaks=50)

# DPF

# Sort values in falling order
sort(as.vector(mc))
