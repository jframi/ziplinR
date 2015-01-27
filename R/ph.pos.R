#'Compute the position in bp from a position in cM
#'
#'Compute the position in bp from a position in cM
#'
#'@param a a vector of length 2 with chromosome and position in cM as elements
#'@param a conv object as constructed by make.conv
#'@author J.-F. Rami \email{rami@@cirad.fr}
#'
#'
ph.pos<-function(a,conv){
  chr<-conv[[a[1]]]
  if (a[2]==chr$g1[2]){
    lign<-2
  }else{
    lign<-which(!(chr$g1>=a[2]|chr$g2<a[2]))
  }
  f<-1/chr$ratio[lign]
  stg<-chr$g1[lign]
  stp<-chr$p1[lign]
  return(stp+(a[2]-stg)*f)
}