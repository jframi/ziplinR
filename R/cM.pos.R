#'Compute the position in cM from a position in bp
#'
#'Compute the position in cM from a position in bp
#'
#'@param a a vector of length 2 with chromosome and position in bp as elements
#'@param a conv object as constructed by make.conv
#'@author J.-F. Rami \email{rami@@cirad.fr}
#'
#'
cM.pos<-function(a,conv){
  chr<-conv[[a[1]]]
  if (a[2]==chr$p1[2]){
    lign<-2
  }else{
    lign<-which(!(chr$p1>=a[2]|chr$p2<a[2]))
  }
  f<-chr$ratio[lign]
  stg<-chr$g1[lign]
  stp<-chr$p1[lign]
  return(stg+(a[2]-stp)*f)
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