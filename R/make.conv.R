#'Constuctor for a conversion list
#'
#'Constuctor for a conversion list
#'
#'@param map1
#'@param map2
#'@param exclude
#'@author J.-F. Rami \email{rami@@cirad.fr}
#'@examples
#'data(sorgho_p118)
#'genomv2<-as.map(genomv2.df)
#'p118.conv<-make.conv(map1 = genomv2,map2 = p118.genet, exclude = "SB_06_045")
#'
# make.conv<-function(mapp,mapg,exclude=NULL){
#   nchr<-length(mapp)
#   if (!is.null(exclude)){
#     mapp<-lapply(mapp,function(a) a[!names(a)%in%exclude])
#     mapg<-lapply(mapg,function(a) a[!names(a)%in%exclude])
#   }
#   mapp<-lapply(mapp,function(a) a[names(a)%in%unlist(lapply(mapg,names))])
#   mapg<-lapply(mapg,function(a) a[names(a)%in%unlist(lapply(mapp,names))])
#   mapp.interv<-lapply(mapp,function(a) data.frame(p1=c(0,a),p2=c(a,1e20)))
#   mapg.interv<-lapply(mapg,function(a) data.frame(g1=c(0,a),g2=c(a,99999)))
#   conv<-lapply(1:nchr,function(a) data.frame(mapg.interv[[a]],mapp.interv[[a]]))
#   conv<-lapply(conv,function(a) data.frame(a,ratio=(a$g2-a$g1)/(a$p2-a$p1)))
#   # Apply the average ratio to first and last intervals (extremities)
#   conv<-lapply(conv,function(a) data.frame(a[,1:4],ratio=c(mean(head(a$ratio[-1],-1)),head(a$ratio[-1],-1),mean(head(a$ratio[-1],-1)))))
#   # Deactivate the following intended at dealing with chrom origin
#   # will be handled bi cM.pos and ph.pos instead
#   #conv<-lapply(conv,function(a) data.frame(a[,1:2]+(a[1,4]*a[1,5]),a[,3:5]))
#   #conv<-lapply(conv,function(a) data.frame(g1=c(0,a[-1,1]),a[,2:5]))                          
#   return(conv)
# }

make.conv<-function(map1,map2,exclude=NULL){
  nchr<-length(map1)
  if (!is.null(exclude)){
    map1<-lapply(map1,function(a) a[!names(a)%in%exclude])
    map2<-lapply(map2,function(a) a[!names(a)%in%exclude])
  }
  map1<-lapply(map1,function(a) a[names(a)%in%unlist(lapply(map2,names))])
  map2<-lapply(map2,function(a) a[names(a)%in%unlist(lapply(map1,names))])
  map1.interv<-lapply(map1,function(a) data.frame(p1=head(a,-1),p2=a[-1]))
  map2.interv<-lapply(map2,function(a) data.frame(g1=head(a,-1),g2=a[-1]))
  conv<-lapply(1:nchr,function(a) data.frame(map2.interv[[a]],map1.interv[[a]]))
  conv<-lapply(conv,function(a) data.frame(a,ratio=(a$g2-a$g1)/(a$p2-a$p1)))
  # Apply the average ratio to first and last intervals (extremities)
  #conv<-lapply(conv,function(a) data.frame(a[,1:4],ratio=c(mean(head(a$ratio[-1],-1)),head(a$ratio[-1],-1),mean(head(a$ratio[-1],-1)))))
  # Deactivate the following intended at dealing with chrom origin
  # will be handled bi cM.pos and ph.pos instead
  #conv<-lapply(conv,function(a) data.frame(a[,1:2]+(a[1,4]*a[1,5]),a[,3:5]))
  #conv<-lapply(conv,function(a) data.frame(g1=c(0,a[-1,1]),a[,2:5]))                          
  return(conv)
}