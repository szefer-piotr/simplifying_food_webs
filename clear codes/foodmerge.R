foodmerge <- function(column){
  m3 <- merge(ber[,c(1,column)], cop[,c(1,column)], by = c("Sp", "Sp"), all.x = TRUE, all.y = TRUE,
              suffixes = c("Sp1", "Sp2"))
  m4 <- merge(m3, maa[,c(1,column)], by = c("Sp", "Sp"), all.x = TRUE, all.y = TRUE)
  Elem <- merge(m4, syw[,c(1,column)], by = c("Sp", "Sp"), all.x = TRUE, all.y = TRUE)
  Elem <- as.data.frame(t(Elem[2:5]))
  Elem[is.na(Elem)] <- 0
  return(Elem)
}

elem <- foodmerge(2)
elem
ord <- sort(colSums(elem), decreasing = TRUE)
sum(ord>0)
