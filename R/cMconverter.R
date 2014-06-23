#'A wrapper for the cM.pos function
#'
#'A wrapper for the cM.pos function
#'
#'@param a map
#'@param a conv object as constructed by make.conv
#'@author J.-F. Rami \email{rami@@cirad.fr}
#'@examples
#'
#'
cMconverter<-function(map,conv,named.chr=F){
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