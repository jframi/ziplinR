#'A wrapper for the cM.pos function
#'
#'A wrapper for the cM.pos function
#'
#'@param a map
#'@param a conv object as constructed by make.conv
#'@author J.-F. Rami \email{rami@@cirad.fr}
#'@examples
#'data(sorgho_p118)
#'genomv2<-as.map(genomv2.df)
#'p118.conv<-make.conv(mapp = genomv2,mapg = p118.genet, exclude = "SB_06_045")
#'#create a new dummy map
#'newmap<-as.map(data.frame(mk=paste("m",1:1000,sep=""),chr=c(rep(1,500),rep(2,500)),Pos=round(c(sort(runif(n=500,min=1,max=1e8)),sort(runif(n=500,min=1,max=1e8))),0)))
#'newmap.genet<-cMconvertR(newmap,p118.conv)
cMconvertR<-function(map,conv,named.chr=F){
  if (!named.chr){
    nchr<-length(map)
    map.cm<-lapply(1:nchr,function(a)
      sapply(map[[a]], function(b)
        cM.pos(a = c(a,b),conv)
        )
      )    
  }
  names(map.cm)<-names(map)
  return(map.cm)
}