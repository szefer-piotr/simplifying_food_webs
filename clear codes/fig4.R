# #Rarefaction plates
# 
# # Reading the data
# #setwd("D:\\PRACA\\R\\how simple are tropical food webs")
# setwd("C:\\Users\\Piotr Szefer\\Desktop\\Work\\extrapolation\\datasets")
# guilds <- read.table("guilds.csv", header = TRUE, sep = "\t")
# names <- as.character(guilds[,1])
# names[5] <- "CHRY133a"
# rownames(guilds) <- names
# #Chunk of code for individual based rarefaction
# 
# miners <- t(guilds[guilds$Guild == unique(guilds$Guild)[3], 2:(dim(guilds)[2] - 6)])
# mobile.chewers <- t(guilds[guilds$Guild == unique(guilds$Guild)[4], 2:(dim(guilds)[2] - 6)])
# semiconcealed.chewers <- t(guilds[guilds$Guild == unique(guilds$Guild)[5], 2:(dim(guilds)[2] - 6)])
# 
# # Rarefaction for miners
# 
# library(vegan)
# minSums <- colSums(miners, na.rm = TRUE)
# subs3 <- c(seq(1,sum(minSums)))
# rar <- rarefy(minSums, sample = subs3, se=T, MARG = 2)
# #plot(subs3, rar[1,], type = "l", col = "red", lty = 2)
# 
# # Individual based rarefaction for mobile chewers
# 
# mcSums <- colSums(mobile.chewers, na.rm = TRUE)
# mc.subs <- c(seq(1,sum(mcSums)))
# mc.rar <- rarefy(mcSums, sample = mc.subs, se=T, MARG = 2)
# #plot(mc.subs, mc.rar[1,], type = "l", col = "red", lty = 2)
# 
# # Individual based rarefaction for sc chewers
# 
# scSums <- colSums(semiconcealed.chewers, na.rm = TRUE)
# sc.subs <- c(seq(1,sum(scSums)))
# sc.rar <- rarefy(scSums, sample = sc.subs, se=T, MARG = 2)
# #plot(sc.subs, sc.rar[1,], type = "l", col = "red", lty = 2)
# 
# # Rescaling number of individuals
# 
# n.miners <- sum(minSums)
# n.mc <- sum(mcSums)
# n.sc <- sum(scSums)
# 
# subs3 <- subs3/n.miners
# mc.subs <- mc.subs/n.mc
# sc.subs <- sc.subs/n.sc
# 
# # Rescaling number of species
# 
# s.miners <- length(minSums)
# s.mc <- length(mcSums)
# s.sc <- length(scSums)
# 
# mob <- rar[1,]
# mc <- mc.rar[1,]
# sc <- sc.rar[1,]
# 
# rar[1,] <- rar[1, ]/s.miners
# mc.rar[1,] <- mc.rar[1, ]/s.mc
# sc.rar[1,] <- sc.rar[1, ]/s.sc
# 
# 
# 
# #Changing NA's to zeros and subsetting to only values from a table
# guilds[is.na(guilds)] <- 0
# guilds.val <- guilds[,2:39] #Plants are in columns and arthropods are in rows
# 
# #Subsetting for three guilds
# miners <- guilds.val[guilds$Guild == unique(guilds$Guild)[3],]
# mobile <- guilds.val[guilds$Guild == unique(guilds$Guild)[4],]
# semi <- guilds.val[guilds$Guild == unique(guilds$Guild)[5],]
# 
# # Rarefy with plant species
# rarefy.plants <- function(dataset, n = 100){
#   #This function reduces randomly the number of tree species
#   result <- matrix(data = 0, nrow = dim(dataset)[2], ncol = 2)
#   for (i in 1:n){
#     n.arthropods <- sum(rowSums(dataset) > 0) #number of arthropod species
#     abundance <- sum(dataset)
#     temp.result <- matrix(data = 0, nrow = dim(dataset)[2], ncol = 2) # temporal result matrix
#     temp.result[1,1] <- n.arthropods # assigning first row of result matrix
#     temp.result[1,2] <- abundance
#     temp.dataset <- as.matrix(dataset) # creating a temprary dataset for manipulations
#     for (j in 2:dim(dataset)[2]){
#       temp.dataset <- as.matrix(temp.dataset[, -sample(dim(temp.dataset)[2], 1)])
#       temp.result[j,1] <- sum(rowSums(temp.dataset) > 0) #Number of arthropod species after reomoving one species
#       temp.result[j,2] <- sum(temp.dataset)
#     }
#     result <- result + temp.result
#   }
#   result <- result/n
#   return(result)
# }
# 
# miners.rarefied <- rarefy.plants(miners)
# mobile.rarefied <- rarefy.plants(mobile)
# semi.rarefied <- rarefy.plants(semi)
# 
# #Standardising the results
# 
# miners.rarefied[,2] <- miners.rarefied[,2]/miners.rarefied[1,2]
# miners.rarefied[,1] <- miners.rarefied[,1]/miners.rarefied[1,1]
# 
# mobile.rarefied[,2] <- mobile.rarefied[,2]/mobile.rarefied[1,2]
# mobile.rarefied[,1] <- mobile.rarefied[,1]/mobile.rarefied[1,1]
# 
# semi.rarefied[,2] <- semi.rarefied[,2]/semi.rarefied[1,2]
# semi.rarefied[,1] <- semi.rarefied[,1]/semi.rarefied[1,1]
# 
# #The plot for plant removal
# 
# #windows(400,300), par(mfrow = c(1,2))
# 
# #legend(0, 1, lty = c(1,2,3),
#        #col = c("black","blue","red"), 
#        #c("Miners", "Mobile chewers", "Semiconcealed chewers"))
# #abline(v = 0.5, lty = 4)
# #text(0.2,0.95, c("Plant species"))
# 
# #####
# ##### Removing of arthropod species ## its taking the plant diversity into account
# #####
# 
# rarefy.arthropods <- function(dataset, n = 100){
#   #UNDER CONSTRUCTION - This function reduces randomly the number of arthropod species and look at abundance
#   result <- matrix(data = 0, nrow = dim(dataset)[2], ncol = 2)
#   for (i in 1:n){
#     n.arthropods <- sum(colSums(dataset) > 0) #number of arthropod species
#     abundance <- sum(dataset)
#     temp.result <- matrix(data = 0, nrow = dim(dataset)[2], ncol = 2) # temporal result matrix
#     temp.result[1,1] <- n.arthropods # assigning first row of result matrix
#     temp.result[1,2] <- abundance
#     temp.dataset <- as.matrix(dataset) # creating a temprary dataset for manipulations
#     for (j in 2:dim(dataset)[2]){
#       temp.dataset <- as.matrix(temp.dataset[, -sample(dim(temp.dataset)[2], 1)])
#       temp.result[j,1] <- sum(colSums(temp.dataset) > 0) #Number of arthropod species after reomoving one species
#       temp.result[j,2] <- sum(temp.dataset)
#     }
#     result <- result + temp.result
#   }
#   result <- result/n
#   return(result)
# }
# 
# s.min <- rarefy.arthropods(t(miners))
# s.mob <- rarefy.arthropods(t(mobile))
# s.sem <- rarefy.arthropods(t(semi))
# 
# # Plotting without rescaling
# 
# #windows(400,300)
# #plot(s.min[,1]~s.min[,2], ylim = c(0,230), xlim = c(0,20000), axes = FALSE, type = "l",
#      #lty = 1, col = "black", ylab = "", xlab = "")
# #par(new = TRUE)
# #plot(s.mob[,1]~s.mob[,2], ylim = c(0,230), xlim = c(0,20000), axes = FALSE, type = "l",
#      #lty = 2, col = "blue", ylab = "", xlab = "")
# #par(new = TRUE)
# #plot(s.sem[,1]~s.sem[,2], ylim = c(0,230), xlim = c(0,20000), type = "l",
#      #lty = 3, col = "red", ylab = "Fraction of species", xlab = "Fraction of individuals")
# 
# 
# #Rescaling i
# 
# s.min[,1] <- s.min[,1]/s.min[1,1]
# s.min[,2] <- s.min[,2]/s.min[1,2]
# 
# s.mob[,1] <- s.mob[,1]/s.mob[1,1]
# s.mob[,2] <- s.mob[,2]/s.mob[1,2]
# 
# s.sem[,1] <- s.sem[,1]/s.sem[1,1]
# s.sem[,2] <- s.sem[,2]/s.sem[1,2]
# 
# #Plotting after rescaling
# 
# #windows(400,300)
# 
# #####
# ##### Removing of interactions !!!! Working somewhat !!!
# #####
# 
# rarefy.interactions <- function(dataset, n = 100){
#   #This function reduces randomly the number of interactions from interaction matrix
#   result <- matrix(data = 0, nrow = sum(dataset>0), ncol = 2)
#   for (i in 1:n){
#     n.arthropods <- sum(rowSums(dataset) > 0) #number of arthropod species
#     abundance <- sum(dataset)
#     temp.result <- matrix(data = 0, nrow = sum(dataset>0), ncol = 2) # temporal result matrix
#     temp.result[1,1] <- n.arthropods # assigning first row of result matrix
#     temp.result[1,2] <- abundance
#     temp.dataset <- as.matrix(dataset) # creating a temprary dataset for manipulations
#     for (j in 2:sum(dataset>0)){
#       
#       #temp.dataset <- temp.dataset[, colSums(temp.dataset)>0] #Removing empty columns
#       #temp.dataset <- temp.dataset[, rowSums(temp.dataset)>0] #Removing empty rows
#       
#       nonzero.entries <- which(temp.dataset>0, arr.ind = TRUE)
#       
#       #print(length(nonzero.entries))
#       
#       coord <- nonzero.entries[sample(dim(nonzero.entries)[1],1),]
#       
#       #print(dataset[row,col])
#       
#       temp.dataset[coord[1],coord[2]] <- 0
#       temp.result[j,1] <- sum(rowSums(temp.dataset) > 0) #Number of arthropod species after reomoving one species
#       temp.result[j,2] <- sum(temp.dataset)
#     }
#     result <- result + temp.result
#   }
#   result <- result/n
#   return(result)
# }
# 
# mint <- rarefy.interactions(miners)
# mobint <- rarefy.interactions(mobile)
# semint <- rarefy.interactions(semi)
# 
# #Rescaling
# 
# mint[,1] <- mint[,1]/mint[1,1]
# mint[,2] <- mint[,2]/mint[1,2]
# 
# mobint[,1] <- mobint[,1]/mobint[1,1]
# mobint[,2] <- mobint[,2]/mobint[1,2]
# 
# semint[,1] <- semint[,1]/semint[1,1]
# semint[,2] <- semint[,2]/semint[1,2]
# 
# #Plotting
# #windows(400,300)
# 
# 
# #Nonrandom reduction of species and interactions (two functions)
# 
# reduce.interactions <- function(dataset){
#   wp.temp <- dataset
#   no.of.interaction <- sum(dataset>0)
#   print(no.of.interaction)
#   result <- matrix(0, nrow = no.of.interaction, ncol = 2)
#   result[1,] <- c(1,1)
#   for (i in 1:(no.of.interaction-1)){
#     print(no.of.interaction - i)
#     coord <- which(wp.temp == min(wp.temp[wp.temp>0]), arr.ind = TRUE)
#     print(wp.temp[coord[1,1], coord[1,2]])
#     wp.temp[coord[1,1], coord[1,2]] <- 0
#     #Count the interactions
#     arthropods.abundance <- sum(wp.temp)
#     arthropods.abundance.prop <- arthropods.abundance/sum(dataset)
#     result[i + 1, ] <- c(arthropods.abundance.prop, (no.of.interaction - i)/no.of.interaction)
#   }
#   return(result)
# }
# 
# reduce <- function(dataset, margin){
#   # This function takes data matrix ordered by rows and by columns and reuces
#   # it by removing least abundant components from given margin and calculates
#   # proportion of abundance left.
#   ws.temp <- dataset
#   abu <- sum(dataset)
#   prop <- matrix(0, nrow = dim(dataset)[margin], ncol = 2)
#   prop [1, ] <- c(1, dim(dataset)[margin])
#   for (i in 1:(dim(dataset)[margin]-1)){
#     #print(i)
#     #print(dim(ws.temp)[2] - i)
#     if (margin == 2) {
#       ws.temp <- ws.temp[, -dim(ws.temp)[margin]]
#       res <- sum(ws.temp)/abu
#       prop[i + 1, ] <- c(res, dim(as.matrix(ws.temp))[margin])
#     }
#     else{
#       ws.temp <- ws.temp[-dim(ws.temp)[margin], ]
#       res <- sum(ws.temp)/abu
#       prop[i + 1, ] <- c(res, dim(ws.temp)[margin])
#     }
#     
#   }
#   prop[,2] <- prop[,2]/prop[1,2]
#   return(prop)
# }
# 
# #Order the datasets and remove zero rows and columns
# 
# order.and.clear <- function(dataset){
#   #This function order the dataset based on rows and columns
#   #and removes zero rows and columns. Dataset should not contain any NAs!
#   dataset <- dataset[rowSums(dataset)>0,]
#   dataset <- dataset[, colSums(dataset)>0]
#   dataset <- dataset[order(rowSums(dataset), decreasing = TRUE), ]
#   dataset <- dataset[,order(colSums(dataset), decreasing = TRUE)]
#   return(dataset)
# }
# 
# #Ordering matrix for reduce fuctions
# miners <- order.and.clear(miners)
# mobile <- order.and.clear(mobile)
# semi <- order.and.clear(semi)
# 
# #Using functions to nonrandom reduction
# miners.plants.red <- reduce(miners, 2)
# miners.arth.red <- reduce(miners, 1)
# miners.int.red <- reduce.interactions(miners)
# 
# mobile.plants.red <- reduce(mobile, 2)
# mobile.arth.red <- reduce(mobile, 1)
# mobile.int.red <- reduce.interactions(mobile)
# 
# semi.plants.red <- reduce(semi, 2)
# semi.arth.red <- reduce(semi, 1)
# semi.int.red <- reduce.interactions(semi)
# 
# plot(rev(semi.int.red[,2]), rev(semi.int.red[,1]), type = "l")
# 
# plot(semi.plants.red)
# 
# # Dataset for ggplot
# 
# guildRar <- data.frame("Arthropods" = c(subs3, 
#                         mc.subs, 
#                         sc.subs),
#                        "Plants"         = c(as.vector(  rar[1, ]), 
#                         as.vector(mc.rar[1, ]), 
#                         as.vector(sc.rar[1,])),
#                        "Guild" = rep(c("Miners", "Mobile chewers", "Semiconcealed chewers"),
#                                      c(length(subs3), length(mc.subs), length(sc.subs))),
#                        "Type"  = "Random removal of arthropod individuals")
# guildRar2 <- data.frame("Arthropods" = c(miners.arth.red[,1], 
#                                          mobile.arth.red[,1], 
#                                          semi.arth.red[,1]),
#                         "Plants"         = c(miners.arth.red[,2], 
#                                              mobile.arth.red[,2], 
#                                              semi.arth.red[,2]),
#                         "Guild" = rep(c("Miners", "Mobile chewers", "Semiconcealed chewers"),
#                                       c(length(miners.arth.red[,1]), 
#                                         length(mobile.arth.red[,1]),
#                                         length(semi.arth.red[,1]))),
#                         "Type"  = "Random removal of arthropod species")
# 
# guildRar3 <- data.frame("Arthropods" = c(miners.rarefied[,2], 
#                                          mobile.rarefied[,2], 
#                                          semi.rarefied[,2]),
#                         "Plants"         = c(miners.rarefied[,1], 
#                                              mobile.rarefied[,1], 
#                                              semi.rarefied[,1]),
#                         "Guild" = rep(c("Miners", "Mobile chewers", "Semiconcealed chewers"),
#                                       c(length(miners.rarefied[,1]), 
#                                         length(mobile.rarefied[,1]),
#                                         length(semi.rarefied[,1]))),
#                         "Type"  = "Random removal of plant species")
# 
# guildRar4 <- data.frame("Arthropods" = c(miners.plants.red[,1], 
#                                          mobile.plants.red[,1], 
#                                          semi.plants.red[,1]),
#                         "Plants"         = c(miners.plants.red[,2], 
#                                              mobile.plants.red[,2], 
#                                              semi.plants.red[,2]),
#                         "Guild" = rep(c("Miners", "Mobile chewers", "Semiconcealed chewers"),
#                                       c(length(miners.plants.red[,1]), 
#                                         length(mobile.plants.red[,1]),
#                                         length(semi.plants.red[,1]))),
#                         "Type"  = "Ordered removal of plant species")
# 
# guildRar5 <- data.frame("Arthropods" = c(mint[,2], 
#                                          mobint[,2], 
#                                          semint[,2]),
#                         "Plants"         = c(mint[,1], 
#                                              mobint[,1], 
#                                              semint[,1]),
#                         "Guild" = rep(c("Miners", "Mobile chewers", "Semiconcealed chewers"),
#                                       c(length(mint[,1]), 
#                                         length(mobint[,1]),
#                                         length(semint[,1]))),
#                         "Type"  = "Random removal of plant-herbivore trophic interactions")
# 
# guildRar6 <- data.frame("Arthropods" = c(miners.int.red[,1], 
#                                          mobile.int.red[,1], 
#                                          semi.int.red[,1]),
#                         "Plants"         = c(miners.int.red[,2], 
#                                              mobile.int.red[,2], 
#                                              semi.int.red[,2]),
#                         "Guild" = rep(c("Miners", "Mobile chewers", "Semiconcealed chewers"),
#                                       c(length(miners.int.red[,1]), 
#                                         length(mobile.int.red[,1]),
#                                         length(semi.int.red[,1]))),
#                         "Type"  = "Ordered removal of plant-herbivore trophic interactions")
# 
# #### And continue.......
# 
# guildData <- rbind(guildRar,
#                    guildRar2,
#                    guildRar3,
#                    guildRar4,
#                    guildRar5,
#                    guildRar6)
# write.table(guildData, "guildData.txt")

setwd("C:/Users/Piotr Szefer/Desktop/Work/extrapolation/datasets")
guildData <- read.table("guildData.txt")

#### At the end merge all the dataframes, save it for the markdown

"Fraction of arthropod individuals"
"Fraction of plant species" 

library(ggplot2)

p1 <- ggplot(guildData, aes(x=Arthropods, 
                           y=Plants , 
                          group = Type, col = Guild)) +
  geom_point(shape=1, size = 2) + facet_wrap(~ Type, ncol=2, scales = "free")

p1
names(guildRar)

#### ALL PLOTS
#setwd("C:/R/how simple are tropical food webs/clear codes")
# Rarefaction
#pdf("fig4.pdf", width = 7, height=10)
#windows(800,1200)


# par(mfrow = c(3,2))
# plot(subs3, rar[1,], type = "l", col = "black", lty = 1, xlim = c(0,1), ylim = c(0,1),
#      axes = F, ylab = "", xlab = "", lwd = 2)
# par(new = T)
# plot(mc.subs, mc.rar[1,], type = "l", col = "blue", lty = 2, xlim = c(0,1), ylim = c(0,1),
#      axes = F, ylab = "", xlab = "", lwd = 2)
# par(new = T)
# plot(sc.subs, sc.rar[1,], type = "l", col = "red", lty = 3, xlim = c(0,1),
#      ylim = c(0,1), ylab = "Fraction of arthropod species",
#      xlab = "Fraction of arthropod individuals", lwd = 2)
# abline(v = 0.5, lty = 4)
# legend(0.3, 0.4, lty = c(1,2,3),
#        col = c("black", "blue","red"),
#        c("Miners", "Mobile chewers", "Semiconcealed chewers"))
# text(0,0.95, c("A"))

# Arthropods

# plot(miners.arth.red[,1], miners.arth.red[,2], axes = FALSE, ylab = "", xlab = "",
#      ylim = c(0,1), xlim = c(0,1), type = "l", pch = 1, col = "black", lty = 1,
#      lwd = 2)
# par(new = TRUE)
# plot(mobile.arth.red[,1], mobile.arth.red[,2], axes = FALSE, ylab = "", xlab = "",
#      ylim = c(0,1), xlim = c(0,1), type = "l", pch = 2, col = "blue", lty = 2,
#      lwd = 2)
# par(new = TRUE)
# plot(semi.arth.red[,1], semi.arth.red[,2], ylab = "Fraction of arthropod species",
#      xlab = "Fraction of arthropod individuals",
#      ylim = c(0,1), xlim = c(0,1), type = "l", pch = 3, col = "red", lty = 3,
#      lwd = 2)
# text(0,0.95, c("B"))
# abline(v = 0.5, lty = 4)
############## END OF INSERTED CODE
#plot(s.min[,1]~s.min[,2], ylim = c(0,1), xlim = c(0,1), axes = FALSE, type = "l",
#     lty = 1, col = "black", ylab = "", xlab = "",lwd = 2)
#par(new = TRUE)
#plot(s.mob[,1]~s.mob[,2], ylim = c(0,1), xlim = c(0,1), axes = FALSE, type = "l",
#     lty = 2, col = "blue", ylab = "", xlab = "",lwd = 2)
#par(new = TRUE)
#plot(s.sem[,1]~s.sem[,2], ylim = c(0,1), xlim = c(0,1), type = "l",
#     lty = 3, col = "red", ylab = "Fraction of arthropod species", 
#     xlab = "Fraction of arthropod individuals",lwd = 2)
#abline(v = 0.5, lty = 4)
#text(0,0.95, c("B"))

# Plants random

# plot(miners.rarefied[,1] ~ miners.rarefied[,2], axes = FALSE, ylab = "", xlab = "",
#      ylim = c(0,1), xlim = c(0,1), type = "l", pch = 1, col = "black", lty = 1,
#      lwd = 2)
# par(new = TRUE)
# plot(mobile.rarefied[,1] ~ mobile.rarefied[,2], axes = FALSE, ylab = "", xlab = "",
#      ylim = c(0,1), xlim = c(0,1), type = "l", pch = 2, col = "blue", lty = 2,
#      lwd = 2)
# par(new = TRUE)
# plot(semi.rarefied[,1] ~ semi.rarefied[,2], ylab = "Fraction of arthropod species",
#      xlab = "Fraction of arthropod individuals",
#      ylim = c(0,1), xlim = c(0,1), type = "l", pch = 3, col = "red", lty = 3,
#      lwd = 2)
# text(0,0.95, c("C"))
# abline(v = 0.5, lty = 4)

# ##          Nonrandom PLANTS
# #           
# plot(miners.plants.red[,1], miners.plants.red[,2], axes = FALSE, ylab = "", xlab = "",
#      ylim = c(0,1), xlim = c(0,1), type = "l", pch = 1, col = "black", lty = 1,
#      lwd = 2)
# par(new = TRUE)
# plot(mobile.plants.red[,1], mobile.plants.red[,2], axes = FALSE, ylab = "", xlab = "",
#      ylim = c(0,1), xlim = c(0,1), type = "l", pch = 2, col = "blue", lty = 2,
#      lwd = 2)
# par(new = TRUE)
# plot(semi.plants.red[,1], semi.plants.red[,2], ylab = "Fraction of arthropod species",
#      xlab = "Fraction of arthropod individuals",
#      ylim = c(0,1), xlim = c(0,1), type = "l", pch = 3, col = "red", lty = 3,
#      lwd = 2)
# text(0,0.95, c("D"))
# abline(v = 0.5, lty = 4)

# # Random interactions
# 
# plot(mint[,1]~mint[,2], ylim = c(0,1), xlim = c(0,1), axes = FALSE, type = "l",
#      lty = 1, col = "black", ylab = "", xlab = "",lwd = 2)
# par(new = TRUE)
# plot(mobint[,1]~mobint[,2], ylim = c(0,1), xlim = c(0,1), axes = FALSE, type = "l",
#      lty = 2, col = "blue", ylab = "", xlab = "",lwd = 2)
# par(new = TRUE)
# plot(semint[,1]~semint[,2], ylim = c(0,1), xlim = c(0,1), type = "l",
#      lty = 3, col = "red", ylab = "Fraction of arthropod species", 
#      xlab = "Fraction of arthropod individuals", lwd = 2)
# abline(v = 0.5, lty = 4)
# text(0,0.95, c("E"))

###     Nonrandom interactions

# plot(miners.int.red[,1], miners.int.red[,2], axes = FALSE, ylab = "", xlab = "",
#      ylim = c(0,1), xlim = c(0,1), type = "l", pch = 1, col = "black", lty = 1,
#      lwd = 2)
# par(new = TRUE)
# plot(mobile.int.red[,1], mobile.int.red[,2], axes = FALSE, ylab = "", xlab = "",
#      ylim = c(0,1), xlim = c(0,1), type = "l", pch = 2, col = "blue", lty = 2,
#      lwd = 2)
# par(new = TRUE)
# plot(semi.int.red[,1], semi.int.red[,2], ylab = "Fraction of arthropod species",
#      xlab = "Fraction of arthropod individuals",
#      ylim = c(0,1), xlim = c(0,1), type = "l", pch = 3, col = "red", lty = 3,
#      lwd = 2)
# text(0,0.95, c("F"))
# abline(v = 0.5, lty = 4)
#dev.off()
# STOP


#  Additional graph
# Arthropods nonrandom



# Creating a bipartite plot for three guilds miners, semiconcealed and mobile

#setwd("C:\\R\\how simple are tropical food webs")
#guilds <- read.table("guilds.csv", header = TRUE, sep = "\t")
#guilds[is.na(guilds)] <- 0
#guilds.val <- guilds[,2:39]
#dim(guilds.val)
#names <- unique(guilds$Guild)

#dataset <- guilds.val[guilds$Guild == "mobile chewers" | 
#                      guilds$Guild == "miners" |
#                      guilds$Guild == "semi-concealed chewers", ]

#col.fac <- as.factor(guilds$Guild[guilds$Guild == "mobile chewers" | 
#             guilds$Guild == "miners" |
#             guilds$Guild == "semi-concealed chewers"])

#factor.counter <- table(col.fac)

#col.f1 <- rep(c("red", "blue", "grey30"), c(69,221,122))

#library(bipartite)

#Trying to create a biplot with all guilds in different colours

#plotweb(t(dataset), method = "normal", high.lablength = 0,
#        low.lablength = 0)

# Three guilds on separate plots. Each time you need to repeat procedure for
#one graph otherwise its not going to work, i was lazy...
#windows(400, 100)
#plotweb(t(dataset), method = "normal", high.lablength = 0,
#        low.lablength = 0,
#        bor.col.interaction = col.f1,
#        col.interaction = col.f1,
#        col.high = col.f1,
#        bor.col.high = col.f1)

#Separate graphs
#mc <- guilds.val[guilds$Guild == "mobile chewers", ]
#m <- guilds.val[guilds$Guild == "miners", ]
#sc <- guilds.val[guilds$Guild == "semi-concealed chewers", ]

#png("all.png", height = 4, width = 8, unit = "in", res = 1200)
#tiff("guildsgraph.tif", height = 4, width = 8, unit = "in", res = 300)
#windows(3000,1000)
#par(mar = c(0.5, 0.5, 0.5, 0.5), xaxs     = "i",
#    yaxs     = "i")
#windows(400, 100)
#par(mfrow = c(1,1))
#plotweb(t(mc), method = "cca", high.lablength = 0,
#        low.lablength = 0,
#        bor.col.interaction = "grey50"
#        ,col.interaction = "grey80"
#        )
#plotweb(t(m), method = "normal", high.lablength = 0,
#        low.lablength = 0,
#        bor.col.interaction = "grey50"
#        , col.interaction = "grey80"
#        )
#plotweb(t(sc), method = "normal", high.lablength = 0,
#        low.lablength = 0,
#        bor.col.interaction = "grey50" 
#        , col.interaction = "grey80"
#        )
#dev.off()
#getwd()
