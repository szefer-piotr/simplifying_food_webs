calcChar <- function(dataset){
  # Helper function that calculates focal parameters
  GenVul=networklevel(dataset, index="vulnerability")
  results <- c("Abu" =sum(dataset),
               "Row" =sum(rowSums(dataset)>0),
               "Col" =sum(colSums(dataset)>0),
               "Int" =sum(dataset>0),
               "H2"  =networklevel(dataset, index="H2"),
               "Gen" =GenVul[1],
               "Vul" =GenVul[2],
               "50r" =sum(cumsum(sort(rowSums(dataset),decreasing=T))<=(sum(dataset)/2)) ,
               "50c" =sum(cumsum(sort(colSums(dataset),decreasing=T))<=(sum(dataset)/2)) ,
               "50rI"=sum(cumsum(sort(dataset[dataset>0],decreasing=T))<=(sum(dataset)/2)))
  return(results)
}

simplify <- function(dataset, edge = "ind", n = 10, reduction.level=0.9){
  # Function takes interaction matrix, randomise it and reduce it
  require(bipartite)
  if       (edge == "ind")   {res.rows <- sum(dataset)
  }else if (edge == "row") {res.rows <- dim(dataset)[1]
  }else if (edge == "col") {res.rows <- dim(dataset)[2]
  }else if (edge == "int") {res.rows <- sum(dataset>0)
  }else    {print("Specify the edge: 'row, 'col', 'ind', 'int'")}
  results <- matrix(0, nrow=res.rows+1, ncol=21)
  results[,1] <- 0:res.rows
  colnames(results) <- c("Obj", "AbuOrd", "AbuRnd", "RowSpO", "RowSpR",
                         "ColSpO", "ColSpR","IntOrd","IntRnd", "H2Ord",
                         "H2Rnd", "GenOrd","GenRnd","VulOrd","VulRnd",
                         "50%rOrd", "50%rRnd", "50%cOrd","50%cRnd",
                         "50%intOrd","50%intRnd")
  ord.cols <- c(2,4,6,8,10,12,14,16,18,20)
  rnd.cols <- c(3,5,7,9,11,13,15,17,19,21)
  
  # calculations for the progress bar
  ####################################
  maxi <- round(res.rows*reduction.level,0)
  maxj <- n
  print(paste("Number of randomisation=",n,". Maxj=",maxj))            #DEBUG
  print(paste("Number of interaction=",res.rows,". Maxi=", maxi))      #DEBUG
  pbOrd <- txtProgressBar(min = 0, max = maxi, style = 3)
  pb <- txtProgressBar(min = 0, max = maxi*maxj, style = 3)
  ####################################
  
  if      (edge=="ind"){
    
    # Individuals module
    
    # Ordered individuals
    print("Reduction")
    
    temp.dat<-dataset # temporal dataset
    results[1,ord.cols]<-calcChar(as.matrix(dataset))
    
    for (i in 1:maxi){
      
      ####################################
      # Reduction process progress bar
      setTxtProgressBar(pbOrd, i)
      ####################################
      
      lowest.int <- which(temp.dat==min(temp.dat[temp.dat>0]),arr.ind=T)
      row <- lowest.int[1,1]
      col <- lowest.int[1,2]
      temp.dat[row,col] <- temp.dat[row,col]-1
      results[i+1, ord.cols] <- calcChar(as.matrix(temp.dat))
    }
    close(pb)
    
    # Randomised individuals
    print("Randomisation")
    
    for(j in 1:n){
      temp.dat<-dataset
      for (i in 1:maxi){
        
        ####################################
        # Randomisation process progress bar
        progres <- ((j-1)*maxi)+i
        setTxtProgressBar(pb, progres)
        ####################################
        
        nonzero.int <- which(temp.dat>0, arr.ind=T)
        rnd.int <- sample(dim(nonzero.int)[1], 1)
        row <- nonzero.int[rnd.int,1]
        col <- nonzero.int[rnd.int,2]
        temp.dat[row, col] <- temp.dat[row, col] - 1
        results[i+1,rnd.cols] <- results[i+1,rnd.cols]+calcChar(as.matrix(temp.dat))
      }
    }
    results[,rnd.cols]  <- results[,rnd.cols]/n
    results[1,rnd.cols] <- calcChar(as.matrix(dataset))
    close(pb)
    
  }else if(edge == "int"){
    
    # Interactions module
    
    # Ordered interactions
    print("Reduction")
    
    temp.dat<-dataset # temporal dataset
    results[1,ord.cols]<-calcChar(as.matrix(temp.dat))
    for (i in 1:maxi){
      
      ####################################
      # Reduction process progress bar
      setTxtProgressBar(pbOrd, i)
      ####################################
      
      lowest.int <- which(temp.dat==min(temp.dat[temp.dat>0]),arr.ind=T)
      row <- lowest.int[1,1]
      col <- lowest.int[1,2]
      temp.dat[row,col] <- 0
      results[i+1, ord.cols] <- calcChar(as.matrix(temp.dat))
    }
    
    # Ramdomised interactions
    print("Randomisation")
    
    for (j in 1:maxj){
      temp.dat<-dataset
      for (i in 1:maxi){
        
        ####################################
        # Randomisation process progress bar
        progres <- ((j-1)*maxi)+i
        setTxtProgressBar(pb, progres)
        ####################################
        
        nonzero.int <- which(temp.dat>0, arr.ind=T)
        rnd.int <- sample(dim(nonzero.int)[1], 1)
        row <- nonzero.int[rnd.int,1]
        col <- nonzero.int[rnd.int,2]
        temp.dat[row, col] <- 0
        results[i+1,rnd.cols] <- results[i+1,rnd.cols]+calcChar(as.matrix(temp.dat))
      }
      
    }
    results[,rnd.cols]  <- results[,rnd.cols]/n
    results[1,rnd.cols] <- calcChar(as.matrix(dataset))
    close(pb)
  }else if (edge == "row"){
    
    # Rows module
    
    # Ordered rows
    print("Reduction")
    
    temp.dat<-dataset # temporal dataset
    results[1,ord.cols]<-calcChar(as.matrix(temp.dat))
    for (i in 1:maxi){
      
      ####################################
      # Reduction process progress bar
      setTxtProgressBar(pbOrd, i)
      ####################################
      
      row.sums <- rowSums(as.matrix(temp.dat))
      row.to.delete <- which(row.sums==min(row.sums[row.sums>0]))
      temp.dat <- temp.dat[-row.to.delete[1],]
      results[i+1, ord.cols] <- calcChar(as.matrix(temp.dat))
    }
    
    # Ramdomised rows
    print("Randomisation")
    
    for (j in 1:maxj){
      temp.dat<-dataset
      for (i in 1:maxi){
        
        ####################################
        # Randomisation process progress bar
        progres <- ((j-1)*maxi)+i
        setTxtProgressBar(pb, progres)
        ####################################
        
        row.sums <- rowSums(as.matrix(temp.dat))
        rows.to.sample <- which(row.sums>0)
        rnd.row <- sample(rows.to.sample, 1)
        temp.dat <- temp.dat[-rnd.row,]
        results[i+1,rnd.cols] <- results[i+1,rnd.cols]+calcChar(as.matrix(temp.dat))
      }
      
    }
    results[,rnd.cols]  <- results[,rnd.cols]/n
    results[1,rnd.cols] <- calcChar(as.matrix(dataset))
    close(pb)
    
  }else if (edge == "col"){
    
    # Columns module
    
    # Ordered columns
    print("Reduction")
    
    temp.dat<-dataset # temporal dataset
    results[1,ord.cols]<-calcChar(as.matrix(temp.dat))
    for (i in 1:maxi){
      
      ####################################
      # Ordering process progress bar
      setTxtProgressBar(pbOrd, i)
      ####################################
      
      col.sums <- colSums(as.matrix(temp.dat))
      columns.to.delete <- which(col.sums==min(col.sums[col.sums>0]))
      temp.dat <- as.matrix(temp.dat)[,-columns.to.delete[1]]
      results[i+1, ord.cols] <- calcChar(as.matrix(temp.dat))
    }
    
    # Ramdomised columns
    print("Randomisation")
    
    for (j in 1:maxj){
      temp.dat<-dataset
      for (i in 1:maxi){
        
        ####################################
        # Randomisation process progress bar
        progres <- ((j-1)*maxi)+i
        setTxtProgressBar(pb, progres)
        ####################################
        
        col.sums <- colSums(as.matrix(temp.dat))
        columns.to.sample <- which(col.sums>0)
        rnd.column <- sample(columns.to.sample, 1)
        temp.dat <- temp.dat[,-rnd.column]
        results[i+1,rnd.cols] <- results[i+1,rnd.cols]+calcChar(as.matrix(temp.dat))
      }
      
    }
    results[,rnd.cols]  <- results[,rnd.cols]/n
    results[1,rnd.cols] <- calcChar(as.matrix(dataset))
    close(pb)
  }
  return(results)
}

############### END OF THE FUNCTION ##########################
