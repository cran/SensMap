

nbpoints=50
# map PCA
map.with.pca=function(X,axis=c(1,2)){
  res.pca = PCA(X,ncp = max(axis), graph = FALSE, scale.unit=1)
  F1=res.pca$ind$coord[,axis[1]]
  F2=res.pca$ind$coord[,axis[2]]
  return(list(F1=F1,F2=F2))
}

# map MFA
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


# MAP CA
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

  intersect(names(X),names(mean.fact)) # by=produit
  map=cbind.data.frame(X,mean.fact[,-1],Y)

  F1 = map$F1
  F2 = map$F2

  return(list(F1=F1,F2=F2))
}


# Discrete grid
discrete.function=function(nbpoints=50,map){
  xmin=floor(10*min(map$F1))/10
  xmax=ceiling(10*max(map$F1))/10
  ymin=floor(2*min(map$F2))/2
  ymax=ceiling(2*max(map$F2))/2
  f1.array=seq(xmin,xmax,length.out=nbpoints)
  f2.array=seq(ymin,ymax,length.out=nbpoints)
  discretspace=expand.grid(F1=f1.array,F2=f2.array)
  return(discretspace)
}




#### For vect, circul, elliptic, quadratic, gam, glm modelisation
# prediction with lm
predict.scores.lm=function(Y,discretspace,map,
                           formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                           pred.na=FALSE){
  ## map is a data.frame with F1 and F2 obtained after DR on the explained data Y
  notespr= nbconsos=matrix(0,nrow(discretspace),ncol(discretspace))
  pred.conso=preference=matrix(0,nrow = nrow(discretspace),ncol=ncol(Y))
  regs=vector("list",ncol(Y))
  nb.NA=vector("list",ncol(Y))
  pos.NA=vector("list",ncol(Y))
  ## Firts we preform all regressions
  for(j in 1:ncol(Y)){
    map.reg=cbind.data.frame(Y[,j],map)
    colnames(map.reg)[1]="Conso"
    modele=as.formula(paste("Conso",formula))
    regs[[j]]=lm(modele,data=map.reg)
    pred.conso[,j]=predict(regs[[j]],newdata=discretspace)

    if (pred.na==TRUE) {
      x=pred.conso[,j]
      x[x<0]=NA
      x[x>10]=NA
      pred.conso[,j]=x
      x=as.data.frame(x)
      nb.NA[[j]] <- apply(x,2,function(a) sum(is.na(a)))# nbre de NA pour chaque conso
      pos.NA[[j]]=which(is.na(x))# le point qui contient NA pour chaque consom
      occur.NA <- unlist(pos.NA)
      occur.NA=as.vector(occur.NA)
      occur.NA=as.data.frame(table(occur.NA))# nbre de NA en chaque point du plan pour tous les consos

    }
    else {
      nb.NA=0
      pos.NA=0
      occur.NA=0
    }
    preference[,j]=(pred.conso[,j]> mean(Y[,j]))
  }
  return(list(regression=regs,pred.conso=pred.conso,preference=preference,nb.NA=nb.NA,
              pos.NA=pos.NA, occur.NA=occur.NA))
}




# prediction with gam with and without rejection from [0.10]
predict.scores.gam=function(Y,discretspace,map,formula="~s(F1,k=3)+s(F2,k=3)"
                            ,pred.na=FALSE){
  ## map is a data.frame with F1 and F2 obtained after DR on the explained data Y
  notespr= nbconsos=matrix(0,nrow(discretspace),ncol(discretspace))
  regs=vector("list",ncol(Y))
  # preference=array(0,dim=c(nrow(discretspace),ncol(discretspace),ncol(X)))
  pred.conso=preference=matrix(0,nrow(discretspace),ncol(Y))
  ## Firts we preform all regressions
  nb.NA=vector("list",ncol(Y))
  pos.NA=vector("list",ncol(Y))
  nbconsos=c()
  for(j in 1:ncol(Y)){
    map.reg=cbind.data.frame(Y[,j],map)
    colnames(map.reg)[1]="Conso"
    modele=as.formula(paste("Conso",formula))
    regs[[j]]=gam(modele,data=map.reg)
    pred.conso[,j]=predict(regs[[j]],newdata=discretspace)

    if (pred.na==TRUE) {
      x=pred.conso[,j]
      x[x<0]=NA
      x[x>10]=NA
      pred.conso[,j]=x
      x=as.data.frame(x)
      nb.NA[[j]] <- apply(x,2,function(a) sum(is.na(a)))
      pos.NA[[j]]=which(is.na(x))
      occur.NA <- unlist(pos.NA)
      occur.NA=as.vector(occur.NA)
      occur.NA=as.data.frame(table(occur.NA))

    }
    else {
      nb.NA=0
      pos.NA=0
      occur.NA=0
    }
    preference[,j]=(pred.conso[,j]> mean(Y[,j]))
  }
  return(list(regression=regs,pred.conso=pred.conso,preference=preference,nb.NA=nb.NA,
              pos.NA=pos.NA, occur.NA=occur.NA))

}



# predict scores glmulti with and without rejection from [0.10]
predict.scores.glmulti=function(Y,discretspace,map,formula="~I(F1*F1)+I(F2*F2)+F1*F2"
                                ,pred.na=FALSE){
  ## map is a data.frame with F1 and F2 obtained after DR on the explained data Y
  notespr= nbconsos=matrix(0,nrow(discretspace),ncol(discretspace))
  regs=datas=vector("list",ncol(Y))
  pred.conso=preference=matrix(0,nrow = nrow(discretspace),ncol=ncol(Y))
  nb.NA=vector("list",ncol(Y))
  pos.NA=vector("list",ncol(Y))
  dt=cbind.data.frame(rep(1,nrow(map)),map)
  colnames(dt)[1]="y"
  modele=as.formula(paste("y",formula))
  m0=lm(modele,data=dt)
  X0=model.matrix(m0)
  X0=X0[,-1]
  p=ncol(X0)
  cn=paste("x",1:p,sep="")
  colnames(X0)=cn

  dt=cbind.data.frame(rep(1,nrow(discretspace)),discretspace)
  colnames(dt)[1]="y"
  m.pred=lm(modele,data=dt)
  Xpred=model.matrix(m.pred)
  Xpred=Xpred[,-1]
  colnames(Xpred)=cn
  Xpred=data.frame(Xpred)
  ## Firts we preform all regressions
  for(j in 1:ncol(Y)){

    print(j)
    temp=cbind.data.frame(Y[,j],X0)

    colnames(temp)[1]="y"

    m1=c()
    m1=glmulti(y = "y",xr = cn,data=temp,level=1,method = "h",
               fitfunction=lm,plotty=F)

    regs[[j]]=m1@objects[[1]]
    rm(list=c("m1","temp"))
    print(regs[[j]])
    pred.conso[,j]=predict(regs[[j]],newdata=Xpred)

    if (pred.na==TRUE) {
      x=pred.conso[,j]
      x[x<0]=NA
      x[x>10]=NA
      pred.conso[,j]=x
      x=as.data.frame(x)
      nb.NA[[j]] <- apply(x,2,function(a) sum(is.na(a)))
      pos.NA[[j]]=which(is.na(x))
      occur.NA <- unlist(pos.NA)
      occur.NA=as.vector(occur.NA)
      occur.NA=as.data.frame(table(occur.NA))

    }
    else {
      nb.NA=0
      pos.NA=0
      occur.NA=0
    }
    preference[,j]=(pred.conso[,j]> mean(Y[,j]))
  }
  return(list(regression=regs,pred.conso=pred.conso,preference=preference,nb.NA=nb.NA,
              pos.NA=pos.NA, occur.NA=occur.NA))
}




#### Bayes
# predict scores with bayes mcmcpack

predict.scores.bayes=function(Y,discretspace,map,formula="~I(F1*F1)+I(F2*F2)+F1*F2",burnin = 1000,
                              mcmc = 2000,verbose=0,seed = NA, beta.start = NA,
                              b0 = 0, B0 = 0, c0 = 0.001, d0 = 0.001, sigma.mu = NA,
                              sigma.var = NA, pred.na=FALSE){
  ## map is a data.frame with F1 and F2 obtained after DR on the explained data Y
  notespr= nbconsos=matrix(0,nrow(discretspace),ncol(discretspace))
  regs=vector("list",ncol(Y))
  # preference=array(0,dim=c(nrow(discretspace),ncol(discretspace),ncol(X)))
  pred.conso=preference=matrix(0,nrow(discretspace),ncol(Y))
  nb.NA=vector("list",ncol(Y))
  pos.NA=vector("list",ncol(Y))
  ## First we preform all regressions
  nbconsos=c()
  for(j in 1:ncol(Y)){
    #print(j)
    dt=cbind.data.frame(Y[,j],map)
    colnames(dt)[1]="Conso"
    modele=as.formula(paste("Conso",formula))

    dt.list=list(Conso=dt$Conso,F1=dt$F1,F2=dt$F2)

    regs[[j]]=MCMCregress(modele, data=dt.list, burnin = burnin,
                          mcmc = mcmc,verbose=verbose,seed = seed, beta.start = beta.start,
                          b0 = b0, B0 = B0, c0 = c0, d0 = d0, sigma.mu = sigma.mu,
                          sigma.var =  sigma.var )
    discretspace2=cbind.data.frame(rep(1,nrow(discretspace)),discretspace)
    colnames(discretspace2)[1]="y"
    formula2=paste("y",formula,sep="")
    m=lm(formula2,data=discretspace2)

    mod.mat=model.matrix(m)
    p=ncol(mod.mat)
    pred1=regs[[j]][,-(p+1)]%*%t(mod.mat)

    x=pred1
    if (pred.na==TRUE) {
      x[x<0]=NA
      x[x>10]=NA
      pred1a=x
      x=as.data.frame(x)
      nb.NA[[j]] <- apply(x,2,function(a) sum(is.na(a)))
      pos.NA[[j]]=which(is.na(x))
      occur.NA <- unlist(pos.NA)
      occur.NA=as.vector(occur.NA)
      occur.NA=as.data.frame(table(occur.NA))

    }
    else {
      nb.NA=0
      pos.NA=0
      occur.NA=0
    }
    pred1a=x
    pred.conso[,j]=colMeans(pred1a,na.rm=T)
    zz=(pred1a> mean(dt$Conso))
    preference[,j]=colMeans(zz,na.rm=T)

  }
  return(list(regression=regs,pred.conso=pred.conso,preference=preference,nb.NA=nb.NA,
              pos.NA=pos.NA, occur.NA=occur.NA))
}




### Draw map for all strategies
drawmap <-function ( Y,X,S,axis=c(1,2),formula,
                     dimredumethod=1, predmodel=1, nbpoints=50,
                     pred.na =FALSE, graph.pred =FALSE, graph.map =FALSE,
                     graph.map.3D =FALSE ){

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
  if(predmodel==2)  reg<-predict.scores.gam(Y = Y,formula = formula,discretspace = discretspace,map = map)
  if(predmodel==3)  reg<-predict.scores.glmulti(Y = Y,formula = formula,discretspace = discretspace,map = map)
  if(predmodel==4)  reg<-predict.scores.bayes(Y = Y,formula = formula,discretspace = discretspace,map = map)

  z=rowMeans(reg$pred.conso)
  p=100*rowMeans(reg$preference)

  if(pred.na==TRUE)
  {
    if(predmodel==1)  reg<-predict.scores.lm(Y = Y,pred.na=TRUE,formula = formula,discretspace = discretspace,map = map)
    if(predmodel==2)  reg<-predict.scores.gam(Y = Y,pred.na=TRUE,formula = formula,discretspace = discretspace,map = map)
    if(predmodel==3)  reg<-predict.scores.glmulti(Y = Y,pred.na=TRUE,formula = formula,discretspace = discretspace,map = map)
    if(predmodel==4)  reg<-predict.scores.bayes(Y = Y,pred.na=TRUE,formula = formula,discretspace = discretspace,map = map)
    z=rowMeans(reg$pred.conso, na.rm = TRUE)
    p=100*rowMeans(reg$preference, na.rm = TRUE)
  }


  graph.predconso = as.image(Z=z,x=discretspace,nrow=nbpoints,ncol=nbpoints)
  graph.surfconso=as.image(Z=p,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso

  if (graph.pred == TRUE){
    image.plot(graph.predconso,main="Prediction surface for all consumers")
    contour(x=graph.predconso$x,y=graph.predconso$y,z=graph.predconso$z,add=TRUE,levels=seq(from=0,to=10,by=0.25))
    text(x=map$F1,y=map$F2,labels=rownames(X),pos=3)
    points(x=map$F1,y=map$F2,pch=20)
  }

  if (graph.map == TRUE){
    image.plot(graph.surfconso,col=terrain.colors(60),main="External Preference Mapping Construction")
    contour(x=graph.surfconso$x,y=graph.surfconso$y,z=graph.surfconso$z,add=T,levels=seq(from=0,to=100,by=5))
    text(x=map$F1,y=map$F2,labels=rownames(X),pos=3)
    points(x=map$F1,y=map$F2,pch=20)
  }


  if (graph.map.3D == TRUE){
   return(plot_ly(x=graph.surfconso$x,y=graph.surfconso$y ,z=graph.surfconso$z ,  type = "surface"))
    # plot_ly(x=graph.predconso$x,y=graph.predconso$y ,z=graph.predconso$z,  type = "surface")
  }
  return(list(graph.pred = graph.pred, graph.map =graph.map,
              graph.map.3D = graph.map.3D) )

}




# Extraction of AIC, R2 and fstatistic

extract.lm=function(pred.obj,what=c("rsquare")){
  if (what=="rsquare") z=unlist(lapply(pred.obj$regression,function(x)summary(x)$r.squared))
  if  (what=="fstastic") z=unlist(lapply(pred.obj$regression,function(x)summary(x)$fstatistic[1]))
  if (what=="aic") z=unlist(lapply(pred.obj$regression,function(x)extractAIC(x)[2]))
  return(z)
}


extract.gam=function(pred.obj,what=c("rsquare")){
  if (what=="rsquare") z=unlist(lapply(pred.obj$regression,function(x)summary(x)$r.sq))
  if  (what=="fstastic") z=unlist(lapply(pred.obj$regression,function(x)coef(x) %*% solve(x$Vp) %*% coef(x)))
  if (what=="aic") z=unlist(lapply(pred.obj$regression,function(x)extractAIC(x)[2]))
  return(z)
}



extract.glm=function(pred.obj,what=c("rsquare")){
  p=length(pred.obj$regression)
  z=vector("list",p)
  if (what=="rsquare") z=unlist(lapply(pred.obj$regression,function(x)summary(x)$r.squared))
  if  (what=="fstastic"){
    for(j in 1:p){

      x=summary(pred.obj$regression[[j]])

      x1<- try(x$fstatistic[1], silent=TRUE)
      if ('try-error' %in% class(x1)) next
      else z[[j]]=x1
    }
  }
  if (what=="aic") z=unlist(lapply(pred.obj$regression,function(x)extractAIC(x)[2]))
  return(z)
}


# denoising with loess for all cases and map drawing
denoising.loess.global = function ( Y,X,S, axis=c(1,2), discretspace,formula, dimredumethod=1,
                                    pred.na=FALSE, predmodel=1,span=.5,degree=2,
                                    graphpred=FALSE, drawmap=FALSE, dmap.loess=FALSE){

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

  discretspace=discrete.function(map = map,nbpoints=50)
  if(pred.na==FALSE){
  if(predmodel==1)  reg<-predict.scores.lm(Y = Y,formula = formula,discretspace = discretspace,map = map)
  if(predmodel==2)  reg<-predict.scores.gam(Y = Y,formula = formula,discretspace = discretspace,map = map)
  if(predmodel==3)  reg<-predict.scores.glmulti(Y = Y,formula = formula,discretspace = discretspace,map = map)
  if(predmodel==4)  reg<-predict.scores.bayes(Y = Y,formula = formula,discretspace = discretspace,map = map)
  z.lm=rowMeans(reg$pred.conso)
  p.lm=100*rowMeans(reg$preference)}

  if(pred.na==TRUE)
  {
    if(predmodel==1)  reg<-predict.scores.lm(Y = Y,pred.na=TRUE,formula = formula,discretspace = discretspace,map = map)
    if(predmodel==2)  reg<-predict.scores.gam(Y = Y,pred.na=TRUE,formula = formula,discretspace = discretspace,map = map)
    if(predmodel==3)  reg<-predict.scores.glmulti(Y = Y,pred.na=TRUE,formula = formula,discretspace = discretspace,map = map)
    if(predmodel==4)  reg<-predict.scores.bayes(Y = Y,pred.na=TRUE,formula = formula,discretspace = discretspace,map = map)
    z.lm=rowMeans(reg$pred.conso, na.rm = TRUE)
    p.lm=100*rowMeans(reg$preference, na.rm = TRUE)
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
    image.plot(graph.surfconso,col=terrain.colors(60), main="Smoothed External Preference Mapping")
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



######  Distances computing before and after LOES Smoothing  when PCA for all
# prediction models

  Dist_prob<-function(Y,X,S,n,axis=c(1,2),formula_lm,
                           formula_gam,dimredumethod=1,
                           pred.na=FALSE,
                           nbpoints=50){

    hedotrain <- hedotest <- list()
    dprob_lm <- dprob_lm_loess <- 0
    dprob_gam <- dprob_gam_loess <- 0
   dprob_glm <- dprob_glm_loess <- 0


    for (i in (1:n)) {

      y1=sample(1:ncol(Y),size=round(ncol(Y)/2))
      hedotrain[[i]]=as.data.frame(Y[,y1]) # sapmle 1
      hedotest[[i]]=as.data.frame(Y[,setdiff(1:ncol(Y),y1)]) # complementary sample

      if(dimredumethod==1) #PCA on X
      {
        map<-map.with.pca(X = X,axis = axis)
        map<-cbind.data.frame(map$F1,map$F2)
        colnames(map)=c("F1","F2")
      }

      if(dimredumethod==2) # MFA on X
      {
        map<-map.with.mfa(X = X,Y = Y,axis = axis)
        map<-cbind.data.frame(map$F1,map$F2)
        colnames(map)=c("F1","F2")
      }

      if(dimredumethod==3) #Canonical Analysis on X
      {
        map<-map.with.ca(X=X,S=S,Y=Y)
        map<-cbind.data.frame(map$F1,map$F2)
        colnames(map)=c("F1","F2")
      }

      discretspace=discrete.function(map = map)


      ## LM before loess
      reg1<-predict.scores.lm(Y = hedotrain[[i]],formula = formula_lm,discretspace = discretspace,map = map)
      reg2<-predict.scores.lm(Y = hedotest[[i]],formula =  formula_lm,discretspace = discretspace,map = map)
      z.lm1=rowMeans(reg1$pred.conso)
      z.lm2=rowMeans(reg2$pred.conso)
      p.lm1=100*rowMeans(reg1$preference)
      p.lm2=100*rowMeans(reg2$preference)
      graph.surfconso1=as.image(Z=p.lm1,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
      graph.surfconso2=as.image(Z=p.lm2,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
      graph.predconso1=as.image(Z=z.lm1,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
      graph.predconso2=as.image(Z=z.lm2,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso

      dprob_lm[[i]]=sum((graph.surfconso1$z-graph.surfconso2$z)^2)/(nbpoints^2)


      ## LM after loess

      reg1.loess<-denoising.loess.global(Y = hedotrain[[i]],X,formula = formula_lm,dimredumethod=1,
                                         predmodel=1,discretspace = discretspace)
      reg2.loess<-denoising.loess.global(Y = hedotest[[i]],X,formula = formula_lm,dimredumethod=1,
                                         predmodel=1,discretspace = discretspace)
      z1.loess=reg1.loess$pred.conso
      p1.loess=reg1.loess$preference
      z2.loess=reg2.loess$pred.conso
      p2.loess=reg2.loess$preference
      graph.surfconso.l1=as.image(Z=p1.loess,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
      graph.surfconso.l2=as.image(Z=p2.loess,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
      graph.predconso.l1=as.image(Z=z1.loess,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
      graph.predconso.l2=as.image(Z=z2.loess,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso

      dprob_lm_loess[[i]]=sum((graph.surfconso.l1$z-graph.surfconso.l2$z)^2)/(nbpoints^2)




      ##GAM before loess

      reg1gam<-predict.scores.gam(Y = hedotrain[[i]],formula = formula_gam,discretspace = discretspace,map = map)
      reg2gam<-predict.scores.gam(Y = hedotest[[i]],formula = formula_gam,discretspace = discretspace,map = map)
      z.gam1=rowMeans(reg1gam$pred.conso)
      z.gam2=rowMeans(reg2gam$pred.conso)
      p.gam1=100*rowMeans(reg1gam$preference)
      p.gam2=100*rowMeans(reg2gam$preference)
      graph.surfconso1g=as.image(Z=p.gam1,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
      graph.surfconso2g=as.image(Z=p.gam2,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
      graph.predconso1g=as.image(Z=z.gam1,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
      graph.predconso2g=as.image(Z=z.gam2,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso

      dprob_gam[[i]]=sum((graph.surfconso1g$z-graph.surfconso2g$z)^2)/(nbpoints^2)


      ## GAM after loess
      reg1gam.loess<-denoising.loess.global(Y = hedotrain[[i]],Y,formula = formula_gam,dimredumethod=1,
                                            predmodel=2,discretspace = discretspace)
      reg2gam.loess<-denoising.loess.global(Y = hedotest[[i]],Y,formula = formula_gam,dimredumethod=1,
                                            predmodel=2,discretspace = discretspace)
      z1g.loess=reg1gam.loess$pred.conso
      p1g.loess=reg1gam.loess$preference
      z2g.loess=reg2gam.loess$pred.conso
      p2g.loess=reg2gam.loess$preference

      graph.surfconso.l.g1=as.image(Z=p1g.loess,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
      graph.surfconso.l.g2=as.image(Z=p2g.loess,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
      graph.predconso.l.g1=as.image(Z=z1g.loess,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
      graph.predconso.l.g2=as.image(Z=z2g.loess,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso

      dprob_gam_loess[[i]]=sum((graph.surfconso.l.g1$z-graph.surfconso.l.g2$z)^2)/(nbpoints^2)



      ##glmulti before loess

      reg1glm<-predict.scores.glmulti(Y = hedotrain[[i]],formula = formula_lm,discretspace = discretspace,map = map)
      reg2glm<-predict.scores.glmulti(Y = hedotest[[i]],formula = formula_lm,discretspace = discretspace,map = map)
      z.glm1=rowMeans(reg1glm$pred.conso)
      z.glm2=rowMeans(reg2glm$pred.conso)
      p.glm1=100*rowMeans(reg1glm$preference)
      p.glm2=100*rowMeans(reg2glm$preference)
      graph.surfconso1glm=as.image(Z=p.glm1,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
      graph.surfconso2glm=as.image(Z=p.glm2,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
      graph.predconso1glm=as.image(Z=z.glm1,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
      graph.predconso2glm=as.image(Z=z.glm2,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso

      dprob_glm[[i]]=sum((graph.surfconso1glm$z-graph.surfconso2glm$z)^2)/(nbpoints^2)


      ## glmulti after loess
      reg1glm.loess<-denoising.loess.global(Y = hedotrain[[i]],Y,formula = formula_lm,dimredumethod=1,
                                            predmodel=3,discretspace = discretspace)
      reg2glm.loess<-denoising.loess.global(Y = hedotest[[i]],Y,formula = formula_lm,dimredumethod=1,
                                            predmodel=3,discretspace = discretspace)
      z1glm.loess=reg1glm.loess$pred.conso
      p1glm.loess=reg1glm.loess$preference
      z2glm.loess=reg2glm.loess$pred.conso
      p2glm.loess=reg2glm.loess$preference

      graph.surfconso.l.glm1=as.image(Z=p1glm.loess,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
      graph.surfconso.l.glm2=as.image(Z=p2glm.loess,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
      graph.predconso.l.glm1=as.image(Z=z1glm.loess,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
      graph.predconso.l.glm2=as.image(Z=z2glm.loess,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso

      dprob_glm_loess[[i]]=sum((graph.surfconso.l.glm1$z-graph.surfconso.l.glm2$z)^2)/(nbpoints^2)

    }


    result <- rbind(dprob_lm,dprob_lm_loess,
                    dprob_gam,dprob_gam_loess,
                    dprob_glm,dprob_glm_loess)

    rownames(result) <- c("dprob_lm","dprob_lm_loess",
                          "dprob_gam","dprob_gam_loess",
                          "dprob_glm","dprob_glm_loess")


    return(result)


  }
