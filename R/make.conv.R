#'Constuctor for a conv object
#'
#'Constuctor for a conv object
#'
#'@param mapp
#'@param mapg
#'@param exclude
#'@author J.-F. Rami \email{rami@@cirad.fr}
#'@examples
#'data(sorgho_p118)
#'genomv2<-as.map(genomv2.df)
#'p118.conv<-make.conv(mapp = genomv2,mapg = p118.genet, exclude = "SB_06_045")
#'
make.conv<-function(mapp,mapg,exclude=NULL){
  nchr<-length(mapp)
  if (!is.null(exclude)){
    mapp<-lapply(mapp,function(a) a[!names(a)%in%exclude])
    mapg<-lapply(mapg,function(a) a[!names(a)%in%exclude])
  }
  mapp<-lapply(mapp,function(a) a[names(a)%in%unlist(lapply(mapg,names))])
  mapg<-lapply(mapg,function(a) a[names(a)%in%unlist(lapply(mapp,names))])
  mapp.interv<-lapply(mapp,function(a) data.frame(p1=c(0,a),p2=c(a,1e20)))
  mapg.interv<-lapply(mapg,function(a) data.frame(g1=c(0,a),g2=c(a,99999)))
  conv<-lapply(1:nchr,function(a) data.frame(mapg.interv[[a]],mapp.interv[[a]]))
  conv<-lapply(conv,function(a) data.frame(a,ratio=(a$g2-a$g1)/(a$p2-a$p1)))
  conv<-lapply(conv,function(a) data.frame(a[,1:4],ratio=c(mean(head(a$ratio[-1],-1)),head(a$ratio[-1],-1),mean(head(a$ratio[-1],-1)))))
  conv<-lapply(conv,function(a) data.frame(a[,1:2]+(a[1,4]*a[1,5]),a[,3:5]))
  conv<-lapply(conv,function(a) data.frame(g1=c(0,a[-1,1]),a[,2:5]))                          
  return(conv)
}