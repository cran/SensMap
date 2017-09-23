# Map with CA

map.with.ca=function(X,S,Y){
  E=S[,-c(1:2)] # mean judge product
  des1=S[,3]
  juge=S[,2]
  produit=S[,1]
  D=model.matrix(des1~produit,data=S)[,-1] # disjonctive matrix
  cc=cancor(E,D)
  norm.xcoef=cc$xcoef%*%diag(rep(1,ncol(cc$xcoef))/sqrt(apply(cc$xcoef^2,2,sum)))
  eta=data.frame(as.matrix(E)%*%norm.xcoef)
  names(eta)=paste('F',1:ncol(eta),sep='')

  mean.fact=aggregate(eta,by=list(produit),mean)
  names(mean.fact)[1]="Produit"
  names(X)[1]="Produit"

  intersect(names(X),names(mean.fact)) # by=product
  map=cbind.data.frame(X,mean.fact[,-1],Y)

  F1 = map$F1
  F2 = map$F2

  return(list(F1=F1,F2=F2))
}








