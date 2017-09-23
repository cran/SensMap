

PrefMap <-function (Y,X,S,axis=c(1,2),formula, dimredumethod=1, predmodel=1, nbpoints=50,
                    pred.na =FALSE, graph.pred =FALSE, graph.map =TRUE,
                    graph.map.3D =FALSE){


  if(dimredumethod==1) #PCA on Y
  {
    map<-map.with.pca(X = X,axis = axis)
    map<-cbind.data.frame(map$F1,map$F2)
    colnames(map)=c("F1","F2")
  }

  if(dimredumethod==2) # MFA
  {
    map<-map.with.mfa(X = X,Y = Y,axis = axis)
    map<-cbind.data.frame(map$F1,map$F2)
    colnames(map)=c("F1","F2")

  }

  if(dimredumethod==3) #Canonical Analysis
  {
    map<-map.with.ca(X=X,S=S,Y=Y)
    map<-cbind.data.frame(map$F1,map$F2)
    colnames(map)=c("F1","F2")

  }

  discretspace=discrete.function(map = map)

  if(predmodel==1)  reg<-predict.scores.lm(Y = Y,formula = formula,discretspace = discretspace,map = map)
  if(predmodel==2)  reg<-predict.scores.gam(Y = Y,formula_gam = formula,discretspace = discretspace,map = map)
  if(predmodel==3)  reg<-predict.scores.glmulti(Y = Y,formula_glm = formula,discretspace = discretspace,map = map)
  if(predmodel==4)  reg<-predict.scores.bayes(Y = Y,formula_bayes = formula,discretspace = discretspace,map = map)

  z=rowMeans(reg$pred.conso)
  p=100*rowMeans(reg$preference)

  if(pred.na==TRUE)
  {
    if(predmodel==1)  reg<-predict.scores.lm(Y = Y,pred.na=TRUE,formula = formula,discretspace = discretspace,map = map)
    if(predmodel==2)  reg<-predict.scores.gam(Y = Y,pred.na=TRUE,formula_gam = formula,discretspace = discretspace,map = map)
    if(predmodel==3)  reg<-predict.scores.glmulti(Y = Y,pred.na=TRUE,formula_glm = formula,discretspace = discretspace,map = map)
    if(predmodel==4)  reg<-predict.scores.bayes(Y = Y,formula_bayes = formula,discretspace = discretspace,map = map)
    z=rowMeans(reg$pred.conso, na.rm = TRUE)
    p=100*rowMeans(reg$preference, na.rm = TRUE)


  }

  nb.NA=reg$nb.NA
  pos.NA=reg$pos.NA
  occur.NA=reg$occur.NA
  graph.predconso = as.image(Z=z,x=discretspace,nrow=nbpoints,ncol=nbpoints)
  graph.surfconso=as.image(Z=p,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso

  if (graph.pred == TRUE){
    image.plot(graph.predconso,main="Prediction surface from consumer and sensory data mapping")
    contour(x=graph.predconso$x,y=graph.predconso$y,z=graph.predconso$z,add=TRUE,levels=seq(from=0,to=10,by=0.25))
    text(x=map$F1,y=map$F2,labels=rownames(X),pos=3)
    points(x=map$F1,y=map$F2,pch=20)
  }

  if (graph.map == TRUE){
    image.plot(graph.surfconso,col=terrain.colors(60),main="External Preference Mapping ")
    contour(x=graph.surfconso$x,y=graph.surfconso$y,z=graph.surfconso$z,add=T,levels=seq(from=0,to=100,by=5))
    text(x=map$F1,y=map$F2,labels=rownames(X),pos=3)
    points(x=map$F1,y=map$F2,pch=20)
  }


  if (graph.map.3D == TRUE){
    plot_ly(x=graph.surfconso$x,y=graph.surfconso$y ,z=graph.surfconso$z ,  type = "surface")

  }
  return(list(graph.pred = graph.pred, graph.map =graph.map,
              graph.map.3D = graph.map.3D, nb.NA=nb.NA,
              pos.NA=pos.NA, occur.NA=occur.NA) )

}













