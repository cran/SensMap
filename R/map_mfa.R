# map with MFA

map.with.mfa=function(X,Y,axis){
  data.mfa=cbind.data.frame(X,Y)
  res.mfa=MFA(data.mfa,group=c(ncol(X),ncol(Y)),ncp=max(axis),
              type=rep("s",2),name.group = c("Sensory","Hedonic"), graph=FALSE)
  senso.select=grep(".Sensory",rownames(res.mfa$ind$coord.partiel))
  coord.partial=res.mfa$ind$coord.partiel[senso.select,1:2]
  rownames(coord.partial)=rownames(Y)
  F1=coord.partial[,axis[1]]
  F2=coord.partial[,axis[2]]
  map=cbind(F1,F2,Y)
  return(list(F1=F1,F2=F2,map=map))
}



