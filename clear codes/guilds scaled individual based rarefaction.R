### Creating a standarised individual based rarefaction curves for three guilds of herbivores

### Reading the data
setwd("C:\\R\\how simple are tropical food webs\\")
guilds <- read.table("guilds.csv", header = TRUE, sep = "\t")
library(vegan)

### Subsetting the data for miners, mobile chewers, semiconcealed chewers

miners <- t(guilds[guilds$Guild == unique(guilds$Guild)[3], 2:(dim(guilds)[2] - 6)])
mobile.chewers <- t(guilds[guilds$Guild == unique(guilds$Guild)[4], 2:(dim(guilds)[2] - 6)])
semiconcealed.chewers <- t(guilds[guilds$Guild == unique(guilds$Guild)[5], 2:(dim(guilds)[2] - 6)])

# Rarefaction for miners

minSums <- colSums(miners, na.rm = TRUE)
subs3 <- c(seq(1,sum(minSums)))
rar <- rarefy(minSums, sample = subs3, se=T, MARG = 2)
plot(subs3, rar[1,], type = "l", col = "red", lty = 2)

# Individual based rarefaction for mobile chewers

mcSums <- colSums(mobile.chewers, na.rm = TRUE)
mc.subs <- c(seq(1,sum(mcSums)))
mc.rar <- rarefy(mcSums, sample = mc.subs, se=T, MARG = 2)
plot(mc.subs, mc.rar[1,], type = "l", col = "red", lty = 2)

# Individual based rarefaction for sc chewers

scSums <- colSums(semiconcealed.chewers, na.rm = TRUE)
sc.subs <- c(seq(1,sum(scSums)))
sc.rar <- rarefy(scSums, sample = sc.subs, se=T, MARG = 2)
plot(sc.subs, sc.rar[1,], type = "l", col = "red", lty = 2)

# Rescaling number of individuals

n.miners <- sum(minSums)
n.mc <- sum(mcSums)
n.sc <- sum(scSums)

subs3 <- subs3/n.miners
mc.subs <- mc.subs/n.mc
sc.subs <- sc.subs/n.sc

# Rescaling number of species

s.miners <- length(minSums)
s.mc <- length(mcSums)
s.sc <- length(scSums)

mob <- rar[1,]
mc <- mc.rar[1,]
sc <- sc.rar[1,]

rar[1,] <- rar[1, ]/s.miners
mc.rar[1,] <- mc.rar[1, ]/s.mc
sc.rar[1,] <- sc.rar[1, ]/s.sc

windows(400,300)
plot(subs3, rar[1,], type = "l", col = "red", lty = 1, xlim = c(0,1), ylim = c(0,1),
     axes = F, ylab = "", xlab = "")
par(new = T)
plot(mc.subs, mc.rar[1,], type = "l", col = "black", lty = 2, xlim = c(0,1), ylim = c(0,1),
     axes = F, ylab = "", xlab = "")
par(new = T)
plot(sc.subs, sc.rar[1,], type = "l", col = "blue", lty = 3, xlim = c(0,1), ylim = c(0,1), ylab = "Fraction of individuals", xlab = "Fraction of species")

legend(0.6, 0.6, lty = c(1,2,3),
       col = c("red", "black","blue"), 
       c("Miners", "Mobile chewers", "Semiconcealed chewers"))
