#'Compute the position on map2 from a position on map1
#'
#'Compute the position on map2 from a position on map1
#'
#'@param a a vector of length 2 with chromosome and position as elements
#'@param a conversion list as constructed by make.conv
#'@author J.-F. Rami \email{rami@@cirad.fr}
#'
#'
new.pos<-function(a,conv){
  extrem<-NULL
  chr<-conv[[a[1]]]
  if (a[2]<chr$p1[1]){
    extrem<-1
  }else{
    if (a[2]>=tail(chr$p2,1)){
      extrem<-2
    }else{
      lign<-which((a[2]>=chr$p1 & a[2]<chr$p2))
    }
  }
  if (is.null(extrem)){
    f<-chr$ratio[lign]
    stg<-chr$g1[lign]
    stp<-chr$p1[lign]
    ret<-stg+(a[2]-stp)*f
  } else{
    if (extrem==1){
      ret<-(a[2]-chr$p1[1])*mean(chr$ratio)
    } else{
      ret<-tail(chr$g2,1)+(a[2]-tail(chr$p2,1))*mean(chr$ratio)
    }
  }
  return(ret)
}

# cM.pos2<-function(a,conv){
#   chr<-conv[[a[1]]]
#   fs<-chr$ratio
#   stgs<-chr$g1
#   stps<-chr$p1
#   edps<-chr$p2
# if (a[2]==stps[2]){
#     lign<-2
#   }else{
#     lign<-which(!(stps>=a[2]|edps<a[2]))
#   }
#   f<-fs[lign]
#   stg<-stgs[lign]
#   stp<-stps[lign]
#   return(stg+(a[2]-stp)*f)
# }