as.map<-function(x){
  map<-x
  mnames <- map[,1]
  uchr <- unique(map[,2])
  n.chr <- length(uchr)
  geno <- vector("list",n.chr)
  names(geno) <- uchr
  min.mar <- 1
  for(i in 1:n.chr) { 
    temp.map <- round(map[map[,2]==uchr[i],3],1)
    names(temp.map) <- mnames[map[,2]==uchr[i]]
    geno[[i]] <- temp.map[order(temp.map)]
  }
  geno
}