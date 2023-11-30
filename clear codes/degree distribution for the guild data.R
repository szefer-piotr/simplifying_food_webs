setwd("C:\\R\\data\\how simple are tropical food webs\\guilds data from equal sampling\\")
guilds <- read.table("guilds.csv", header = TRUE, sep = "\t")

guilds[is.na(guilds)] = 0
guilds <- guilds[,2:39]

insects.degree <- rowSums(guilds>0)
insects.degree <- as.matrix(insects.degree)
dim(insects.degree)

plants.degree <- colSums(guilds>0)
plants.degree <- as.matrix(plants.degree)
dim(plants.degree)

degre.dist <- rbind(insects.degree, plants.degree)
dim(degre.dist)
hist(log(degre.dist))

dd <- degre.dist[,1]
degree <- table(dd)
sorted.degree <- sort(degree, decreasing = TRUE)
plot(log(sorted.degree))
dim(sorted.degree)
plot(log(seq(1:52)), log(sorted.degree))
log.fit.degree <- lm(log(sorted.degree) ~ log(seq(1:52)))
abline(log.fit.degree, col = "red", lty = 2)
