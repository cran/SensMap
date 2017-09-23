# map with PCA

map.with.pca=function(X,axis=c(1,2)){
  res.pca = PCA(X,ncp = max(axis), graph = FALSE, scale.unit=1)
  F1=res.pca$ind$coord[,axis[1]]
  F2=res.pca$ind$coord[,axis[2]]
  map=cbind(F1,F2)
  return(list(F1=F1,F2=F2,map=map))
}


