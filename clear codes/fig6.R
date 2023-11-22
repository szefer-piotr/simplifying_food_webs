setwd("D:\\PRACA\\R\\how simple are tropical food webs\\50perc\\equal")
guilds <- read.table("guilds.csv", header = TRUE, sep = "\t")
dim(guilds)
guilds[1:6, 1:6]
summary(guilds[,1:6])
guilds[is.na(guilds)] <- 0
names(guilds)

guilds50perc <- guilds[, 1:(dim(guilds)[2] - 6)]
names(guilds50perc)

class(guilds50perc[5,5])

guilds50perc <- t(guilds50perc[, 2:dim(guilds50perc)[2]])
summary(guilds50perc[,1:5])

guilds50perc[1:5, 1:5]


top.50pc.function.insects<-function(species){
  sum(colSums(species)>0)->total.sp.rich                                                        #total number of species of insect in the dataset
  sum(colSums(species))*0.50->abundance                                                         #half the total abundance - need to know the minimum number of species to accumulate to get this
  sum((cumsum(sort(colSums(species[,-1]),decreasing=TRUE))>abundance)==FALSE)+1->n.arthropod.species #this is the minimum number - note that the sum of individuals from these species will always be equal to or larger than "abundance", since unless the final species added brings the totals equal there will be an overshoot
  
  ##part for looking at interactions
  
  sum(c(as.matrix(species)))*0.50->int.abundance
  sum(species>0)->total.interactions
  sum((cumsum(sort(c(as.matrix(species)),decreasing=TRUE))>int.abundance)==FALSE)+1->n.interactions
  c(ifelse(total.sp.rich==0,0,n.arthropod.species),total.sp.rich,ifelse(total.interactions==0,0,n.interactions),total.interactions) #when the total species richness is zero, so is the 50% (code above does odd things when there are no species left)
}

RESULTS.50.PERC <- matrix(nrow = 6, ncol = 4) 
colnames(RESULTS.50.PERC) <- c("50%abu", "Richness", "50%int", "Inter")
rownames(RESULTS.50.PERC) <- c("All guilds", "Expo chew", "Meso suc", "Miners", "Mob chew", "S-c chew")
RESULTS.50.PERC
RESULTS.50.PERC[1,] <- top.50pc.function.insects(guilds50perc)

#EXPOSED CHEWERS

exposed <- t(guilds[guilds$Guild == "exposed chewers", 2:(dim(guilds)[2] - 6)])
colnames(exposed) <- guilds[guilds$Guild == "exposed chewers",1]
getwd()

write.table(exposed,"exposed.txt")

RESULTS.50.PERC[2,] <- top.50pc.function.insects(exposed)

#MESOPHYL SUCKERS

unique(guilds$Guild)
meso <- t(guilds[guilds$Guild == unique(guilds$Guild)[2], 2:(dim(guilds)[2] - 6)])
RESULTS.50.PERC[3,] <- top.50pc.function.insects(meso)

#MINERS

miners <- t(guilds[guilds$Guild == unique(guilds$Guild)[3], 2:(dim(guilds)[2] - 6)])
RESULTS.50.PERC[4,] <- top.50pc.function.insects(miners)
RESULTS.50.PERC

#MOBILE CHEWERS

mobile <- t(guilds[guilds$Guild == unique(guilds$Guild)[4], 2:(dim(guilds)[2] - 6)])
RESULTS.50.PERC[5,] <- top.50pc.function.insects(mobile)
RESULTS.50.PERC

#SEMI CONCEALED CHEWERS

semi <- t(guilds[guilds$Guild == unique(guilds$Guild)[5], 2:(dim(guilds)[2] - 6)])
RESULTS.50.PERC[6,] <- top.50pc.function.insects(semi)
RESULTS.50.PERC

par(mfrow= c(1,2))

#PLOTS
rownames(RESULTS.50.PERC) <- c("ALL", "G1", "G2", "G3", "G4", "G5")
barplot(RESULTS.50.PERC[,2], ylab = "Number of species", cex.names = 0.5)
barplot(RESULTS.50.PERC[,1], add = TRUE, col = "black", cex.names = 0.5)
barplot(RESULTS.50.PERC[,4], ylab = "Number of interactions", cex.names = 0.5)
barplot(RESULTS.50.PERC[,3], add = TRUE, col = "black", cex.names = 0.5)
legend(2,500, c("G1 - exposed chewers",
"G2 - Mesophyll suckers",
"G3 - Miners",
"G4 - Mobile chewers",
"G5 - Semi-concealed chewers"), cex = 0.45)

plot(RESULTS.50.PERC[-1,4], RESULTS.50.PERC[-1,3]/RESULTS.50.PERC[-1,4], ylab = "Number of species involved in 50% of interactions",
     xlab = "Number of interactions in web")
lm1 <- lm(RESULTS.50.PERC[-1,3]/RESULTS.50.PERC[-1,4] ~ RESULTS.50.PERC[-1,4])
abline(lm1, lty = 2, col = "red")
text(RESULTS.50.PERC[-1,4]+100, RESULTS.50.PERC[-1,3]/RESULTS.50.PERC[-1,4],labels=c("G1", "G2", "G3", "G4","G5"))
plot(RESULTS.50.PERC[-1,2], RESULTS.50.PERC[-1,1]/RESULTS.50.PERC[-1,2], ylab = "Number of species comprising 50% of abundance",
     xlab = "Community abundance")
lm2 <- lm(RESULTS.50.PERC[-1,1]/RESULTS.50.PERC[-1,2] ~ RESULTS.50.PERC[-1,2])
abline(lm2, lty = 2, col = "red")
text(RESULTS.50.PERC[-1,2]+10, RESULTS.50.PERC[-1,1]/RESULTS.50.PERC[-1,2],labels=c("G1", "G2", "G3", "G4","G5"))

#####################################
########### Rarefaction##############
#####################################

# TOTAL

#Plotting and computing function

rarefy <- function(guilds50perc, n = 100){
  grand.out<-matrix(nrow=dim(guilds50perc)[1],ncol=4,data=0) #create output matrix ready to receive inputs
  for (i in 1:n){ #loop for replications
    out<-matrix(nrow=dim(guilds50perc)[1],ncol=4,data=0) #output for each replication
    top.50pc.function.insects(guilds50perc)->out[1,] #calculate arthropod species numbers for whole dataset
    guilds50perc->guilds50perc.temp #create temporary dataset that can be manipulated&
    print(i/100)
    for (j in 2:dim(guilds50perc.temp)[1]){ #loop for removal of randomly selected tree species
      guilds50perc.temp[-sample(dim(guilds50perc.temp)[1],1),]->guilds50perc.temp #remove a randomly selected tree species
      top.50pc.function.insects(guilds50perc.temp)->out[j,] #re calculate arthropod numbers and add them to output
    }
    out+grand.out->grand.out #at the end of each of the n loops, add the most recent output to the running total and...
  }
  
  grand.out/n->grand.out  #divide by n to get a mean
  
  par(mfrow=c(2,2)) #plot means (code above could be modified to give confidence intervals if needed)
  plot(grand.out[,2]~rev(1:dim(guilds50perc)[1]),ylab="Total athropod species",xlab="Number of tree species")
  plot(grand.out[,1]~rev(1:dim(guilds50perc)[1]),ylab="Arthropod species making up 50% abundance",xlab="Number of tree species")
  plot(grand.out[,4]~rev(1:dim(guilds50perc)[1]),ylab="Total interactions",xlab="Number of tree species")
  plot(grand.out[,3]~rev(1:dim(guilds50perc)[1]),ylab="Interactions making up 50% of interaction abundance",xlab="Number of tree species")
  return(grand.out)
}

colnames(guilds50perc) <- guilds[,1]
dim(t(as.data.frame(guilds50perc[1,])))
guilds50perc <- as.data.frame(guilds50perc)
top.50pc.function.insects(guilds50perc[1,])
total <- rarefy(guilds50perc)

#GUILD1 EXOPSED CHEWERS

exposed <- t(guilds[guilds$Guild == unique(guilds$Guild)[1], 2:(dim(guilds)[2] - 6)])
colnames(exposed) <- guilds[guilds$Guild == "exposed chewers",1]
exposed <- as.data.frame(exposed)
exposed.data <- rarefy(exposed)

# GUILD2 MESOPHYL SUCKERS

meso <- t(guilds[guilds$Guild == unique(guilds$Guild)[2], 2:(dim(guilds)[2] - 6)])
colnames(meso) <- guilds[guilds$Guild == unique(guilds$Guild)[2],1]
meso <- as.data.frame(meso)
meso.data <- rarefy(meso, n = 100)

#GUild 3 miners

miners <- t(guilds[guilds$Guild == unique(guilds$Guild)[3], 2:(dim(guilds)[2] - 6)])
colnames(miners) <- guilds[guilds$Guild == unique(guilds$Guild)[3],1]
miners <- as.data.frame(miners)
miners.data <- rarefy(miners, n = 100)

#guild 4 mobile chewers

chewers <- t(guilds[guilds$Guild == unique(guilds$Guild)[4], 2:(dim(guilds)[2] - 6)])
colnames(chewers) <- guilds[guilds$Guild == unique(guilds$Guild)[4],1]
chewers <- as.data.frame(chewers)
chewers.data <- rarefy(chewers, n = 100)

#GUILD 5 Semi-concealed chewers

scchewers <- t(guilds[guilds$Guild == unique(guilds$Guild)[5], 2:(dim(guilds)[2] - 6)])
colnames(scchewers) <- guilds[guilds$Guild == unique(guilds$Guild)[5],1]
scchewers <- as.data.frame(scchewers)
scchewers.data <- rarefy(scchewers, n = 100)

#Whole dataset
#names(guilds)
#guilds <- t(guilds[, 2:(dim(guilds)[2] - 6)])
#guilds <- as.data.frame(guilds)
#guilds[, 1]
#all.data <- rarefy(guilds, n = 100)

# Joined graph for all guilds
#exposed.data
#meso.data
#miners.data
#chewers.data
#scchewers.data
#all.data

# windows(600,300)
# 
# par(mfrow = c(1,2), cex = 0.8)
# 
# #par(mfrow=c(1,1))
# 
# plot(exposed.data[,1] ~ rev(1:38), cex = 0.8, col = "black", 
#      ylim = c(0,31), xlim = c(1, 38), xlab = "Tree species",
#      ylab = "Arthropod species making up to 50% of abundance",
#      type = "l", lwd = 1, lty = 1)
# par(new=TRUE)
# plot(rev(1:38), meso.data[,1], col = "red", 
#      axes = FALSE, xlab = "", ylab = "", ylim=c(0,31),
#      type = "l", lwd = 2, lty = 1)
# par(new=TRUE)
# plot(rev(1:38), miners.data[,1],
#      col = "blue", axes = FALSE, xlab = "", ylab = "", ylim=c(0,31),
#      type = "l", lwd = 1, lty = 2)
# par(new=TRUE)
# plot(rev(1:38), chewers.data[,1], 
#      col = "brown", axes = FALSE, xlab = "", ylab = "", ylim=c(0,31),
#      type = "l", lwd = 2, lty = 2)
# par(new=TRUE)
# plot(rev(1:38), scchewers.data[,1],
#      col = "gray30", axes = FALSE, xlab = "", ylab = "", ylim=c(0,31),
#      type = "l", lwd = 1, lty = 3)
# par(new=TRUE)
# plot(rev(1:38), all.data[,1],
#      col = "gray60", axes = FALSE, xlab = "", ylab = "", ylim=c(0,31),
#      type = "l", lwd = 2, lty = 3)
# 
# legend(0, 30, cex = 0.5, lwd = c(1,2,1,2,1,2), 
#        col = c("black", "red","blue","brown", "gray30", "gray60"), 
#        c("Exposed chewers", "Mesophyll suckers", 
#                 "Miners", "Chewers", "Semiconcealed chewers", "All data"),
#        lty = c(1,1,2,2,3,3))
# 
# plot(exposed.data[,3] ~ rev(1:38), cex = 0.8, pch = 1, col = "black", 
#      ylim = c(0,100), xlim = c(1, 38), xlab = "Tree species",
#      ylab = "Interactions making up to 50% of interactions",
#      type = "l", lwd = 1, lty = 1)
# par(new=TRUE)
# plot(rev(1:38), meso.data[,3], pch = 2, col = "red", 
#      axes = FALSE, xlab = "", ylab = "", ylim=c(0,100),
#      type = "l", lwd = 2, lty = 1)
# par(new=TRUE)
# plot(rev(1:38), miners.data[,3], pch = 3, col = "blue",
#      axes = FALSE, xlab = "", ylab = "", ylim=c(0,100),
#      type = "l", lwd = 1, lty = 2)
# par(new=TRUE)
# plot(rev(1:38), chewers.data[,3], pch = 4, col = "brown",
#      axes = FALSE, xlab = "", ylab = "", ylim=c(0,100),
#      type = "l", lwd = 2, lty = 2)
# par(new=TRUE)
# plot(rev(1:38), scchewers.data[,3],  pch = 5, col = "gray30",
#      axes = FALSE, xlab = "", ylab = "", ylim=c(0,100),
#      type = "l", lwd = 1, lty = 3)
# par(new=TRUE)
# plot(rev(1:38), all.data[,3],  pch = 6, col = "gray60",
#     axes = FALSE, xlab = "", ylab = "", ylim=c(0,100),
#     type = "l", lwd = 2, lty = 3)

#legend(0, 100, cex = 0.5, pch = c(1,2,3,4,5,6), col = c("black", "red","blue","brown", "gray30", "gray60"), c("Exposed chewers", "Mesophyll suckers", 
                                                                                                             "Miners", "Chewers", "Semiconcealed chewers", "All data"))
### rAREFACTION WHILE SAVING ALL OF THE RANDOMISATIONS FOR PREDICTIONS

# rarefy.all.points <- function(guilds50perc, n = 100){
#   grand.out<-matrix(nrow=1,ncol=4,data=0) #create output matrix ready to receive inputs
#   for (i in 1:n){ #loop for replications
#     out<-matrix(nrow=(dim(guilds50perc)[1]),ncol=4,data=0) #output for each replication
#     top.50pc.function.insects(guilds50perc)->out[1,] #calculate arthropod species numbers for whole dataset
#     guilds50perc->guilds50perc.temp #create temporary dataset that can be manipulated&
#     print(i/100)
#     for (j in 2:dim(guilds50perc.temp)[1]){ #loop for removal of randomly selected tree species
#       guilds50perc.temp[-sample(dim(guilds50perc.temp)[1],1),]->guilds50perc.temp #remove a randomly selected tree species
#       top.50pc.function.insects(guilds50perc.temp)->out[j,] #re calculate arthropod numbers and add them to output
#     }
#     #print(out)
#     grand.out <- rbind(grand.out, out) #at the end of each of the n loops, add the most recent output to the running total and...
#   }
#   return(grand.out)
# }
# 
# #Preaparing the data set
# # I can use all guilds to predict or predict for each guild separately
# 
# guilds.all <- as.data.frame(t(guilds[2:(dim(guilds)[2] - 6)]))
# guilds.all[1:5,1:5]
# 
# all.data.all.points <- rarefy.all.points(guilds.all, n = 1000)
# all.data.all.points <- all.data.all.points[-1,]
# 
# # Results are saved here
# write.table(all.data.all.points, "all_data.txt")



# # Predictios for arthropods making up to 50% of total abundance
# 
# discard <- 10 #identifies how many species needs to be discarded
# plant.species <- 38 # plant species number
# results <- matrix(0, nrow = 1000, ncol = 1)
# 
# for (k in 1:1000){
#   entries = ((k-1) * 38) + (1:38) #creates indexes of used points from big 3800 row dataset
#   x <- (discard+1):plant.species
#   print(x)# plant species used in prediction
#   y <- rev(all.data.all.points[entries, 1]) #points extracted from dataset
#   y <- y[(discard+1):plant.species]
#   print(y)# points used for prediction
#   model <- lm(log(y) ~ log(x)) #fitting linear model to points
#   new <- log(39:227) #creating a new dataset for simple prediction
#   modeled <- model$coefficient[1] + model$coefficient[2] * log(x) #modeled values
#   predicted <- model$coefficient[1] + model$coefficient[2] * new # predicted values for 227 plant species
#   results[k, ] <- predicted[length(predicted)]
#   #par(ask = TRUE)
#   #plot(log(y)~log(x),
#        #xlim = c(0,log(227)), ylim = c(0,6))
#   #points(new, predicted, type = "l", col = "red", lty = 5, lwd = 2)
#   #points(log(x), modeled, type = "l", col = "blue", lty = 4, lwd = 2)
# }
# 
# par(ask = FALSE)
# 
# t.test(results)
# hist(exp(results), col = "black", freq = FALSE, breaks = 25)
# res.quant <- quantile(exp(results), probs = c(0.25, 0.5, 0.75))
# abline(v = res.quant[1], lty = 2, col = "red")
# abline(v = res.quant[3], lty = 2, col = "red")
# abline(v = res.quant[2], lty = 1, col = "red", lwd = 2)
# library(lattice)
# dens <- density(exp(results))
# names(dens)
# dens$as.table
# lines(dens, lwd = 2, col = "blue")
# plot(dens, add = TRUE)
# dens




#################
####NEW PLOTS####
#################

# # Checking the data 
# setwd("D:\\PRACA\\R\\how simple are tropical food webs")
# source("funct_simplifyV1.R")
# source("funct_simplifyV2.R")
# setwd("C:\\R\\how simple are tropical food webs\\simplification results")
# mobile <- simplify(mobile, edge="col", reduction.level = 0.9)
# expo.row <- simplify(exposed, edge = "row", n=100, reduction.level = 0.95)
# #write.table(expo.row, "expo.row.txt")
# #meso <- meso[rowSums(meso)>0,]
# #meso.row <- simplify(meso, edge = "row", n=100, reduction.level = 0.92)
# #write.table(meso.row, "meso.row.txt")



# 1/6/2017
###############

# All data
setwd("D:\\PRACA\\R\\how simple are tropical food webs\\50perc\\equal")

all_data <- read.table("all_data.txt")
#plot(all_data)

setwd("D:\\PRACA\\R\\how simple are tropical food webs\\simplification results")

# Reading the resuts obtained with function simplify (funct_simplifyV1.R)
semi.row <- read.table("semi.row.txt")
mobi.row <- read.table("mobi.row.txt")
mine.row <- read.table("mine.row.txt")
meso.row <- read.table("meso.row.txt")
expo.row <- read.table("expo.row.txt")

datasets <- list(semi.row,mobi.row, mine.row, meso.row, expo.row)

red <- rgb(150,0,0,150, maxColorValue = 255)
grn <- rgb(0,150,0,150, maxColorValue = 255)
blu <- rgb(0, 0,150,150, maxColorValue = 255)
bck <- rgb(0,0,  0, 150, maxColorValue = 255)
gre <- rgb(204,204,204,150, maxColorValue = 255)

colours <- c(red,grn,blu,bck,gre)
names <- c("SEMI", "MOBI", "MINE", "MESO", "EXPO")
# Simple rarefaction plots

windows(140,60)
setwd("D:\\PRACA\\R\\how simple are tropical food webs\\clear codes\\figs")
#pdf("fig6.pdf", width = 10, height = 5 )
par(mfrow=c(1,2))

#allx <- c(semi.row[,4],mobi.row[,4], mine.row[,4], meso.row[,4],expo.row[,4])
#ally <- c(semi.row[,7],mobi.row[,7], mine.row[,7], meso.row[,7],expo.row[,7])
datasets <- list(scchewers.data,
                 chewers.data, 
                 miners.data, 
                 meso.data, 
                 exposed.data)
NAMES <- c("SEMI","MOBI","MINE","MESO","EXPO")
# column 2 is for the arthropod number 
# column 1 is for the arthropod 50 %
allx <- c(1:38,1:38, 1:38, 1:38,1:38)
ally <- c(scchewers.data[,2],chewers.data[,2], miners.data[,2], meso.data[,2],exposed.data[,2])

xlim <- c(min(allx,na.rm=T),max(allx,na.rm=T))
ylim <- c(min(ally,na.rm=T),max(ally,na.rm=T))

plot(NULL, log="yx", cex=1.5,
     xlim = xlim, ylim=ylim,
     xlab="Plant species", ylab="Herbivores species")

for (i in 1:length(datasets)){
  par(new=T)
  plot(1:38, rev(datasets[[i]][,2]), log="yx", cex=1.5,
       col = colours[i], pch=16,xlim = xlim, ylim=ylim,
       axes=FALSE, xlab="", ylab="")
}
legend("bottomright", NAMES, col = colours, pch = 16, pt.cex = rep(1.5, 5),bty = "n")

# 50% plots ###!!!! 19 or 17 ???### OLD PLOT
#allx <- c(semi.row[,4],mobi.row[,4], mine.row[,4], meso.row[,4],expo.row[,4])
#ally <- c(semi.row[,19],mobi.row[,19], mine.row[,19], meso.row[,19],expo.row[,19])
#xlim <- c(2,max(allx,na.rm=T))+0.1
#ylim <- c(min(ally,na.rm=T),max(ally,na.rm=T))+0.1
#plot(NULL, log="yx", cex=1.5,
#     xlim = xlim, ylim=ylim,
#     xlab="Plant species", ylab="Herbivores species making up to 50% of abundance")
#for (i in 1:length(datasets)){
#  par(new=T)
#  plot(datasets[[i]][,4], datasets[[i]][,19], log="yx", cex=1.5,
#       col = colours[i], pch=16,xlim = xlim, ylim=ylim,
#        axes=FALSE, xlab="", ylab="")
#   text(2.5, datasets[[i]][datasets[[i]][,4]==3,19], names[i])
# }


# NEW PLOT corrected for the mobile chewers 50%
allx <- c(1:38,1:38, 1:38, 1:38,1:38)
ally <- c(scchewers.data[,1],chewers.data[,1], miners.data[,1], meso.data[,1],exposed.data[,1])

xlim <- c(min(allx,na.rm=T),max(allx,na.rm=T))
ylim <- c(min(ally,na.rm=T),max(ally,na.rm=T))

plot(NULL, log="yx", cex=1.5,
     xlim = xlim, ylim=ylim,
     xlab="Plant species", ylab="Herbivores species making up to 50% of abundance")

for (i in 1:length(datasets)){
  par(new=T)
  plot(1:38, rev(datasets[[i]][, 1]), log="yx", cex=1.5,
       col = colours[i], pch=16,xlim = xlim, ylim=ylim,
       axes=FALSE, xlab="", ylab="")
  #text(1.5, datasets[[i]][datasets[[i]][,2]==2,7], names[i])
}

#dev.off()


# Predictions for 50% but with reduced number of observations (DROP)

# to get these data run the code rarefy each dataset separately. lines: 134 - 167
#pdf("fig6b.pdf", width=6, height=6)
semi.graph <- data.frame("Plants" = rev(1:38), "50_sp" = scchewers.data[,1])
mobi.graph <- data.frame("Plants" = rev(1:38), "50_sp" = chewers.data[,1])
mine.graph <- data.frame("Plants" = rev(1:38), "50_sp" = miners.data[,1])
meso.graph <- data.frame("Plants" = rev(1:38), "50_sp" = meso.data[,1])
expo.graph <- data.frame("Plants" = rev(1:38), "50_sp" = exposed.data[,1])

ylim = c(0,5)

# Plots
par(mfrow=c(1,1))
plot(log(semi.graph[,2])~log(semi.graph[,1]), 
     xlim = c(1, log(227)), col = NULL, pch = 21,
     ylim = ylim, cex = 1, bg=red, axes = TRUE, 
     xlab = "LOG[#Tree species]", 
     ylab = "LOG[50% of arthropod individuals]")
par(new=T)
plot(log(mobi.graph[,2])~log(mobi.graph[,1]), 
     xlim = c(1, log(227)), col = NULL, pch = 21,
     ylim = ylim, cex = 1, bg=grn, axes = F, xlab = "", ylab = "")
par(new=T)
plot(log(mine.graph[,2])~log(mine.graph[,1]), 
     xlim = c(1, log(227)), col = NULL, pch = 21,
     ylim = ylim, cex = 1, bg=blu, axes = F, xlab = "", ylab = "")
par(new=T)
plot(log(meso.graph[,2])~log(meso.graph[,1]), 
     xlim = c(1, log(227)), col = NULL, pch = 21,
     ylim = ylim, cex = 1, bg=bck, axes = F, xlab = "", ylab = "")
par(new=T)
plot(log(expo.graph[,2])~log(expo.graph[,1]), 
     xlim = c(1, log(227)), col = NULL, pch = 21,
     ylim = ylim, cex = 1, bg=gre, axes = F, xlab = "", ylab = "")

legend("topleft", c("SEMI", "MOBI", "MINE", "MESO", "EXPO"),  lty = 1,lwd=3, 
       col = c(red,grn,blu,bck,gre), bty = "n")

polred <- rgb(150,0,0,100, maxColorValue = 255)
rect(log(38), -1.5, log(227), 6.5, col = polred, border = rgb(0,0,0,0,maxColorValue = 255))

# Prediction plots for 227 plant species - LINEAR

DROP <- 10 # how many initial observaions should be dropped.
ROWS <- 1:(38-DROP)

SEMI_lm_est <- lm(log(semi.graph[ROWS,2])~log(semi.graph[ROWS,1])) #red
MOBI_lm_est <- lm(log(mobi.graph[ROWS,2])~log(mobi.graph[ROWS,1])) #grn
MINE_lm_est <- lm(log(mine.graph[ROWS,2])~log(mine.graph[ROWS,1])) #blu
MESO_lm_est <- lm(log(meso.graph[ROWS,2])~log(meso.graph[ROWS,1])) #bck
EXPO_lm_est <- lm(log(expo.graph[ROWS,2])~log(expo.graph[ROWS,1])) #gre

LWD = 2
LTY = 1

abline(SEMI_lm_est, col = red, lty = LTY, lwd = LWD)
abline(MOBI_lm_est, col = grn, lty = LTY, lwd = LWD)
abline(MINE_lm_est, col = blu, lty = LTY, lwd = LWD)
abline(MESO_lm_est, col = bck, lty = LTY, lwd = LWD)
abline(EXPO_lm_est, col = gre, lty = LTY, lwd = LWD)

dev.off()

SEMI_pred <- exp(SEMI_lm_est$coefficients[1] + log(227) * SEMI_lm_est$coefficients[2])
MOBI_pred <- exp(MOBI_lm_est$coefficients[1] + log(227) * MOBI_lm_est$coefficients[2])
MINE_pred <- exp(MINE_lm_est$coefficients[1] + log(227) * MINE_lm_est$coefficients[2])
MESO_pred <- exp(MESO_lm_est$coefficients[1] + log(227) * MESO_lm_est$coefficients[2])
EXPO_pred <- exp(EXPO_lm_est$coefficients[1] + log(227) * EXPO_lm_est$coefficients[2])

summary(SEMI_lm_est)
summary(MOBI_lm_est)
summary(MINE_lm_est)
summary(MESO_lm_est)
summary(EXPO_lm_est)

data.frame(Network = NAMES, Estimates = c(SEMI_pred,
                              MOBI_pred,
                              MINE_pred,
                              MESO_pred,
                              EXPO_pred))

exp(SEMI_lm_est$coefficients[1] + log(227) * SEMI_lm_est$coefficients[2])
exp(MOBI_lm_est$coefficients[1] + log(227) * MOBI_lm_est$coefficients[2])
exp(MINE_lm_est$coefficients[1] + log(227) * MINE_lm_est$coefficients[2])
exp(MESO_lm_est$coefficients[1] + log(227) * MESO_lm_est$coefficients[2])
exp(EXPO_lm_est$coefficients[1] + log(227) * EXPO_lm_est$coefficients[2])

# R^2 for the MOBI
library(nlme)
x <- log(as.vector(mobi.graph[ROWS,1]))
y <- log(as.vector(mobi.graph[ROWS,2]))
summary(gls(y~x))
MOBI_lm_est$residuals -> RES #Residuals
Ybar <- mean(log(mobi.graph[ROWS,2]))
SStot <- sum((log(mobi.graph[ROWS,2]) - Ybar)^2)
SSres <- sum(RES^2)
R2 <- 1 - (SSres/SStot)



# par(mfrow=c(1,1))
# #warnings()
# getwd()
# #pdf("linear_extrap.pdf", width=8,height=8)
# plot(log(datasets[[1]][1:36,17])~log(datasets[[1]][1:36,4]), 
#      xlim = c(1, log(227)), col = NULL, pch = 21,
#      ylim = c(-1, 5), cex = 1, bg=red, axes = TRUE, xlab = "LOG[#Tree species]", ylab = "LOG[50% of arthropod individuals]")
# par(new=T)
# plot(log(datasets[[2]][1:36,17])~log(datasets[[2]][1:36,4]), 
#      xlim = c(1, log(227)), col = NULL, pch = 21,
#      ylim = c(-1, 5), cex = 1, bg=grn, axes = F, xlab = "", ylab = "")
# par(new=T)
# plot(log(datasets[[3]][1:32,17])~log(datasets[[3]][1:32,4]), 
#      xlim = c(1, log(227)), col = NULL, pch = 21,
#      ylim = c(-1, 5), cex = 1, bg=blu, axes = F, xlab = "", ylab = "")
# par(new=T)
# plot(log(datasets[[4]][1:23,17])~log(datasets[[4]][1:23,4]), 
#      xlim = c(1, log(227)), col = NULL, pch = 21,
#      ylim = c(-1, 5), cex = 1, bg=bck, axes = F, xlab = "", ylab = "")
# par(new=T)
# plot(log(datasets[[5]][1:36,17])~log(datasets[[5]][1:36,4]), 
#      xlim = c(1, log(227)), col = NULL, pch = 21,
#      ylim = c(-1, 5), cex = 1, bg=gre, axes = F, xlab = "", ylab = "")

#plot(NULL, 
#     xlim = c(1, log(227)), col = NULL, pch = 21,
#     ylim = c(-1, 6), cex = 1, bg=gre)

# abline(SEMI_lm_est, col=red, lty = 1, lwd = 2)
# abline(MOBI_lm_est, col=grn, lty = 1, lwd = 2)
# abline(MINE_lm_est, col=blu, lty = 1, lwd = 2)
# abline(MESO_lm_est, col=bck, lty = 1, lwd = 2)
# abline(EXPO_lm_est, col=gre, lty = 1, lwd = 2)
# abline(v=log(227), col = red, lty = 2)
#dev.off()

polred <- rgb(150,0,0,100, maxColorValue = 255)
rect(log(38), -1.5, log(227), 6.5, col = polred, border = 0)

        
# For the SEMICONCEALED CHEWERS - estimation of number of species 
#nmaking up 50% of teh abundance.
exp(SEMI_lm_est$coefficients[1] + log(227) * SEMI_lm_est$coefficients[2])
exp(MOBI_lm_est$coefficients[1] + log(227) * MOBI_lm_est$coefficients[2])
exp(MINE_lm_est$coefficients[1] + log(227) * MINE_lm_est$coefficients[2])
exp(MESO_lm_est$coefficients[1] + log(227) * MESO_lm_est$coefficients[2])
exp(EXPO_lm_est$coefficients[1] + log(227) * EXPO_lm_est$coefficients[2])

#     #  #     # 
##   ##  ##   ##
### ###  ### ###
#######  #######
## # ##  ## # ##
##   ##  ##   ##
##   ##  ##   ##

# Prediction plots for 227 plant species - MM function

ymax <- 12
xmax <- 50
x <- 1:100
par(mfrow=c(1,1))
#warnings()
plot(datasets[[1]][1:36,17]~datasets[[1]][1:36,4], 
     col = red, pch = 20, ylim = c(0, ymax), xlim = c(1, xmax))
y <- exp(SEMI_lm_est$coefficients[1])* x^(SEMI_lm_est$coefficients[2]-0.05)

lines(x, y)

par(new=T)
plot(datasets[[2]][1:36,17]~datasets[[2]][1:36,4],
     col = grn, pch = 20, ylim = c(0, ymax), xlim = c(1, xmax))
par(new=T)
plot(datasets[[3]][1:32,17]~datasets[[3]][1:32,4],
     col = blu, pch = 20, ylim = c(0, ymax), xlim = c(1, xmax))
par(new=T)
plot(datasets[[4]][1:23,17]~datasets[[4]][1:23,4], 
     col = bck, pch = 20, ylim = c(0, ymax), xlim = c(1, xmax))
par(new=T)
plot(datasets[[5]][1:36,17]~datasets[[5]][1:36,4], 
     col = gre, pch = 20, ylim = c(0, ymax), xlim = c(1, xmax))

# Fitting curves
#SEMI
install.packages("drc")
library(drc)

SEMI50 <- rev(datasets[[1]][1:36,17])
SPEC <- rev(datasets[[1]][1:36,4])
DATA <- data.frame(SEMI50=SEMI50,SPEC=SPEC)

drm(SEMI50~SPEC, data = DATA, fct = MM.2())

a <- 16
b <- 40
SEMI50pred <- a*SPEC/(b + SPEC)
lines(SPEC, SEMI50pred)

# FROM ESTIMATE S 

DATA <- read.delim("clipboard") # from 50perc/equal...

getwd()

pdf("extrap_exposed.pdf", width = 16, height = 8)
par(mfrow = c(1,2))
matplot(DATA[, c(1,2,3)], type = "l", ylab = "No. of species making up to 50% of individuals",
        xlab = "Number of plant species", ylim = c(-5, 80),
        lty = c(1,2,2), col = c("black","red","red"),
        lwd = c(2,2,2), main = "Exposed chewers, abundance data")

rect(38, -10, 300, 100, col = rgb(255, 0, 0, 50, maxColorValue = 255), border = "white")

matplot(DATA[, c(4,5,6)], type = "l", ylab = "No. of species making up to 50% of individuals",
        xlab = "Number of plant species",, ylim = c(-5, 80),
        lty = c(1,2,2), col = c("black","red","red"),
        lwd = c(2,2,2), , main = "Exposed chewers, incidence data")


rect(38, -10, 300, 100, col = rgb(255, 0, 0, 50, maxColorValue = 255), border = "white")
dev.off()

