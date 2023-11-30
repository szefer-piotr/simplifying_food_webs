#PLOTing
int.prim
int.sec
abu.prim
abu.sec

par(mfrow=c(2,2))

#plot for abundance and then interactions

plot(abu.prim[,2]~rev(1:25),ylab="Total athropod species",xlab="Number of 0.04ha plots",  ylim = c(0, 300))
par(new = TRUE)
plot(abu.sec[,2]~rev(1:25),ylab="",xlab="", ylim = c(0, 300), pch = 2, col = "red")

plot(abu.prim[,1]~rev(1:25),ylab="Arthropod species making up 50% abundance",xlab="Number of 0.04ha plots", ylim = c(0,15))
par(new = TRUE)
plot(abu.sec[,1]~rev(1:25),ylab="",xlab="", col="red", pch = 2, ylim = c(0,15))

#Interactions

plot(int.prim[,2]~rev(1:25),ylab="Total interactions",xlab="Number of 0.04ha plots",  ylim = c(0, 500))
par(new = TRUE)
plot(int.sec[,2]~rev(1:25),ylab="",xlab="", ylim = c(0, 500), pch = 2, col = "red")

plot(int.prim[,1]~rev(1:25),ylab="Interactions making up 50% of interactions",xlab="Number of 0.04ha plots", ylim = c(0,16))
par(new = TRUE)
plot(int.sec[,1]~rev(1:25),ylab="",xlab="", col="red", pch = 2, ylim = c(0,16))

#PERCENTAGE OF DROP

par(mfrow = c(1,1))

maximum <- c(int.prim[1,2],int.sec[1,2],abu.prim[1,2],abu.sec[1,2])
perc <- c(int.prim[1,1],int.sec[1,1],abu.prim[1,1],abu.sec[1,1])

names(maximum) <- c("IP", "IS", "SP", "SS" )
names(perc) <- c("IP", "IS", "SP", "SS" )

xcord <- barplot(maximum)
barplot(perc, add = TRUE, col = "black")

vals <- c(int.prim[1,1]/int.prim[1,2], int.sec[1,1]/int.sec[1,2],abu.prim[1,1]/abu.prim[1,2],abu.sec[1,1]/abu.sec[1,2])
vals <- round(vals*100, 2)
text(xcord, (perc + 25), vals)
