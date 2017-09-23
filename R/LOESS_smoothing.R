# loess denoising

denoising.loess.global = function ( Y,X,S, axis=c(1,2), discretspace,formula, dimredumethod=1,
                                    pred.na=FALSE, predmodel=1,span=.5,degree=2,
                                    graphpred=FALSE, drawmap=FALSE, dmap.loess=FALSE,
                                    nbpoints=50){

  if(dimredumethod==1) #PCA on X
  {
    map<-map.with.pca(X = X,axis = axis)
    map<-cbind.data.frame(map$F1,map$F2)
    colnames(map)=c("F1","F2")
  }
  if(dimredumethod==2) # MFA on X
  {
    map<-map.with.mfa(X = X,Y = Y,axis = axis)
    map<-map$map[,1:2]
  }
  if(dimredumethod==3) #Canonical Analysis on X
  {
    map<-map.with.ca(X=X,S=S,Y=Y)
    map<-cbind.data.frame(map$F1,map$F2)
    colnames(map)=c("F1","F2")
  }

  if(predmodel==1)  reg<-predict.scores.lm(Y = Y,formula = formula,discretspace = discretspace,map = map)
  if(predmodel==2)  reg<-predict.scores.gam(Y = Y,formula_gam = formula,discretspace = discretspace,map = map)
  if(predmodel==3)  reg<-predict.scores.glmulti(Y = Y,formula_glm = formula,discretspace = discretspace,map = map)
  if(predmodel==4)  reg<-predict.scores.bayes(Y = Y,formula_bayes = formula,discretspace = discretspace,map = map)
  z.lm=rowMeans(reg$pred.conso)
  p.lm=100*rowMeans(reg$preference)

  if(pred.na==TRUE)
  {
    if(predmodel==1)  reg<-predict.scores.lm(Y = Y,pred.na=TRUE,formula = formula,discretspace = discretspace,map = map)
    if(predmodel==2)  reg<-predict.scores.gam(Y = Y,pred.na=TRUE,formula_gam = formula,discretspace = discretspace,map = map)
    if(predmodel==3)  reg<-predict.scores.glmulti(Y = Y,pred.na=TRUE,formula_glm = formula,discretspace = discretspace,map = map)
    if(predmodel==4)  reg<-predict.scores.bayes(Y = Y,formula_bayes = formula,discretspace = discretspace,map = map)
    z=rowMeans(reg$pred.conso, na.rm = TRUE)
    p=100*rowMeans(reg$preference, na.rm = TRUE)
  }
  mlow=cbind.data.frame(discretspace,z.lm)
  colnames(mlow)=c("x","y","z")

  z.loess<-loess(z~x+y,span=span,data=mlow,degree=degree)
  pred.conso=z.loess$fitted

  dlow=cbind.data.frame(discretspace,p.lm)
  colnames(dlow)=c("x","y","z")

  m.loess<-loess(z~x+y,span=span,data=dlow,degree=degree)
  preference=m.loess$fitted

  graph.predconso = as.image(Z=pred.conso,x=discretspace,nrow=nbpoints,ncol=nbpoints)
  graph.surfconso=as.image(Z=preference,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso

  if (graphpred == TRUE)
  {

    image.plot(graph.predconso)
    contour(x=graph.predconso$x,y=graph.predconso$y,z=graph.predconso$z,add=TRUE,levels=seq(from=0,to=10,by=0.25))
    text(x=map$F1,y=map$F2,labels=rownames(X),pos=3)
    points(x=map$F1,y=map$F2,pch=20)
  }

  if (drawmap == TRUE)  {
    image.plot(graph.surfconso,col=terrain.colors(60), main="Smoothed EPM from GAM model (8)")
    contour(x=graph.surfconso$x,y=graph.surfconso$y,z=graph.surfconso$z,add=T,levels=seq(from=0,to=100,by=5))
    text(x=map$F1,y=map$F2,labels=rownames(Y),pos=3)
    points(x=map$F1,y=map$F2,pch=20)

  }

  if (dmap.loess == TRUE) {

    wireframe(z~x*y,data = dlow,shade = TRUE,distance=0,
              screen=list(z=50,x=-60),xlab="",ylab="",zlab="",drape = TRUE,
              colorkey = TRUE,
              scales=list(draw=FALSE))

    dt.loess=cbind.data.frame(discretspace,preference)
    colnames(dt.loess)=c("x","y","z")
    wireframe(z~x*y,data = dt.loess,shade = TRUE,distance=0,
              screen=list(z=50,x=-60),xlab="",ylab="",zlab="",drape = TRUE,
              colorkey = TRUE, scales=list(draw=FALSE))
  }
  return(list(z.loess=z.loess,m.loess=m.loess,pred.conso=pred.conso,
              preference=preference,graphpred=graphpred, drawmap=drawmap,
              dmap.loess=dmap.loess))
}
