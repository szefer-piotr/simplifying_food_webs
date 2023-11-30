

test <- cbind(primary.out, secondary.out)

addition <- matrix(NA, nrow = 132, ncol = 4)
test1 <- rbind(addition, secondary.out)

par(mfrow=c(2,2))                                                                      #plot means (code above could be modified to give confidence intervals if needed)

plot(primary.out[,2]~rev(1:223),
     ylab="Total athropod species",
     xlab="Number of tree species", 
     ylim = c(1, 310))
par(new = TRUE)
plot(test1[,2]~rev(1:223), 
     col = "red", 
     xlab = "", 
     ylab = "", 
     axes = FALSE, 
     ylim = c(1, 310))


plot(primary.out[,1]~rev(1:223),
     ylab="Arthropod species making up 50% abundance",
     xlab="Number of tree species", 
     ylim = c(1, 14))
par(new = TRUE)
plot(test1[,1]~rev(1:223), 
     col = "red", 
     xlab = "", 
     ylab = "", 
     axes = FALSE, 
     ylim = c(1, 14))

plot(primary.out[,4]~rev(1:223),
     ylab="Total interactions",
     xlab="Number of tree species", 
     ylim = c(1, 450))
par(new = TRUE)
plot(test1[,4]~rev(1:223), 
     col = "red", 
     xlab = "", 
     ylab = "", 
     axes = FALSE, 
     ylim = c(1, 450))

plot(primary.out[,3]~rev(1:223),
     ylab="Interactions making up 50% of interaction abundance",
     xlab="Number of tree species", 
     ylim = c(1, 16))
par(new = TRUE)
plot(test1[,3]~rev(1:223), 
     col = "red", 
     xlab = "", 
     ylab = "", 
     axes = FALSE, 
     ylim = c(1, 16))