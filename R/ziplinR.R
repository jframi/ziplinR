#'The main function to project positions from one map to another
#'
#'The main function to project positions from one map to another
#'
#'@param map a map containing the positions to be projected. The map format is the same as the one used by Rqtl, ie a list of vectors of positions
#'@param map1 the first reference map
#'@param map2 the second reference map
#'@param map2tomap1 if TRUE the positions of map will be projected from map2 to map1. Default is FALSE
#'@param exclude a vector of marker names to exclude from references, typically showing discrepancies
#'@param shift.extrem if TRUE (default) the new map will be shifted in case some positions are projected to negative values
#'@return a map of new projected positions
#'@author J.-F. Rami \email{rami@@cirad.fr}
#'@examples
#'data(sorgho_p118)
#'genomv2<-as.map(genomv2.df)
#'#create a new dummy map
#'newmap<-as.map(data.frame(mk=paste("m",1:1000,sep=""),chr=c(rep(1,500),rep(2,500)),Pos=round(c(sort(runif(n=500,min=1,max=1e8)),sort(runif(n=500,min=1,max=1e8))),0)))
#'newmap.genet<-ziplinR(map=newmap,map1=genomv2,map2=p118.genet,exclude = "SB_06_045",shift.extrem = T)
ziplinR<-function(map, map1, map2, named.chr=T, map2tomap1 = F, shift.extrem=T,exclude=NULL){
  if (!map2tomap1){
    conv<-make.conv(map1=map1,map2=map2,exclude=exclude)
  } else{
    conv<-make.conv(map1=map2,map2=map1,exclude = exclude)
  }
  if (!named.chr){
    nchr<-length(map)
    if (nchr!=length(map1)) stop("map must have the same number of chromosomes as map1 and map2 when named.chr=F")
    map.cm<-lapply(1:nchr,function(a)
      sapply(map[[a]], function(b)
               new.pos(a = c(a,b),conv)
        )
      )    
  }else{
    map.cm<-lapply(match(names(map),names(map1)),function(a){
      sapply(map[[which(names(map)==a)]], function(b)
        new.pos(a = c(a,b),conv)
      )}
    )
  }
  names(map.cm)<-names(map)
  if (shift.extrem){
    map.cm<-lapply(map.cm, function(a) a-ifelse(min(a)<0,min(a),0))
  }
  return(map.cm)
}