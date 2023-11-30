setwd("C:\\R\\data\\50perc")
parasite <- read.csv("tri_trophic_janda.csv", header = TRUE, sep = "\t")

abundance <- sum(as.matrix(table(parasite[, 1:2])))
interations <- sum(as.matrix(table(parasite[parasite>0])))

parasites1 <- parasite[, 1:2]
as.matrix(table(parasites1)) -> a
a <- as.matrix(a[a>0])
dim(a)
a[1:10, 1:10]


matr <- matrix(runif(16), nrow = 4, ncol = 4)
sum(matr)
matr

plant.cat <- table(parasite[, 1:2])
cat.par <- table(parasite[, 2:3])

plotweb(plant.cat)
plotweb(cat.par)
plotweb2(plant.cat, cat.par)
