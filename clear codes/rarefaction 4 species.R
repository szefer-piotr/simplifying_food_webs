setwd("C:\\R\\data\\50perc")
ber <- read.csv("ber.csv", header = TRUE, sep = ",")
cop <- read.csv("cop.csv", header = TRUE, sep = ",")
maa <- read.csv("maa.csv", header = TRUE, sep = ",")
syw <- read.csv("syw.csv", header = TRUE, sep = ",")

#ELEM
m3 <- merge(ber[,1:2], cop[,1:2], by = c("Sp", "Sp"), all.x = TRUE, all.y = TRUE,
            suffixes = c("Sp1", "Sp2"))
m4 <- merge(m3, maa[1:2], by = c("Sp", "Sp"), all.x = TRUE, all.y = TRUE)
Elem <- merge(m4, syw[1:2], by = c("Sp", "Sp"), all.x = TRUE, all.y = TRUE)

elem <- as.data.frame(t(Elem[2:5]))
elem[is.na(elem)] <- 0
elem

elem.data <- rarefy(elem)
elem.data
sum(elem>0)

#MOROX

morox <- foodmerge(3)
morox.data <- rarefy(morox)

#NIKSEK

niksek <- foodmerge(4)
niksek.data <- rarefy(niksek)

#OHU

ohu <- foodmerge(5)
ohu.data <- rarefy(ohu)

# Utai

utai <- foodmerge(6)
utai.data <- rarefy(utai)

#WAMANGA

wamanga <- foodmerge(7)
wan.data <- rarefy(wamanga)

#WANANG
wanang <- foodmerge(8)
wanang.data <- rarefy(wanang)

#Yapsiei

yap <- foodmerge(9)
yap.data <- rarefy(wanang)

#50pc abundance

cbind(Elem = elem.data[,1], Morox = morox.data[,1],
      Niksek = niksek.data[,1], Ohu = ohu.data[,1],
      Utai = utai.data[,1], Wamanga = wan.data[,1], 
      Wanang = wanang.data[,1], Yapsiei = yap.data[,1]) -> abu50pc

par(mfrow=c(1,1))
plot(rev(1:4), apply(abu50pc, 1, mean), ylim = c(0, 6), 
     ylab = "Arthropods species make up 50 % abundance",
     xlab = "Tree species")

originx <- c(1,2,3,4)
originy <- apply(abu50pc, 1, mean)
se <- apply(abu50pc, 1, sd)/sqrt(8)
segments(originx, rev(originy), originx, rev(originy + se))
segments(originx, rev(originy), originx, rev(originy - se))
lines(originx, rev(originy), lty = 2)

segments(originx - 0.05, rev(originy+se), originx + 0.05, rev(originy+se))
segments(originx - 0.05, rev(originy-se), originx + 0.05, rev(originy-se))

# Interactions

cbind(Elem = elem.data[,3], Morox = morox.data[,3],
      Niksek = niksek.data[,3], Ohu = ohu.data[,3],
      Utai = utai.data[,3], Wamanga = wan.data[,3], 
      Wanang = wanang.data[,3], Yapsiei = yap.data[,3]) -> int50pc

par(mfrow=c(1,1))
plot(rev(1:4), apply(int50pc, 1, mean), ylim = c(0,6), 
     ylab = "Arthropods species make up 50 % interactions",
     xlab = "Tree species")

originx <- c(1,2,3,4)
originy <- apply(int50pc, 1, mean)
se <- apply(int50pc, 1, sd)/sqrt(8)
segments(originx, rev(originy), originx, rev(originy + se))
segments(originx, rev(originy), originx, rev(originy - se))
lines(originx, rev(originy), lty = 2)

segments(originx - 0.05, rev(originy+se), originx + 0.05, rev(originy+se))
segments(originx - 0.05, rev(originy-se), originx + 0.05, rev(originy-se))
