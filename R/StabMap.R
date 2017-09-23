
StabMap<-function(Y,X,S,n,axis=c(1,2),formula_lm,
                  formula_gam,formula_glm,dimredumethod=1,
                  pred.na=FALSE,
                  nbpoints=50) {

  hedotrain <- hedotest <- list()
  dprob_lm <- dprob_lm_loess <- 0
  dprob_gam <- dprob_gam_loess <- 0
  dprob_glmulti <- dprob_glmulti_loess <- 0



  for (i in (1:n)) {

    y1=sample(1:ncol(Y),size=round(ncol(Y)/2))
    hedotrain[[i]]=as.data.frame(Y[,y1]) # sapmle 1
    hedotest[[i]]=as.data.frame(Y[,setdiff(1:ncol(Y),y1)]) # complementary sample

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

    ## LM before loess
    reg1<-predict.scores.lm(Y = hedotrain[[i]],formula = formula_lm,discretspace = discretspace,map = map)
    reg2<-predict.scores.lm(Y = hedotest[[i]],formula =  formula_lm,discretspace = discretspace,map = map)
    p.lm1=100*rowMeans(reg1$preference)
    p.lm2=100*rowMeans(reg2$preference)
    graph.surfconso1=as.image(Z=p.lm1,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
    graph.surfconso2=as.image(Z=p.lm2,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso

    dprob_lm[[i]]=sum((graph.surfconso1$z-graph.surfconso2$z)^2)/(nbpoints^2)


    ## LM after loess

    reg1.loess<-denoising.loess.global(Y = hedotrain[[i]],X,formula = formula_lm,dimredumethod=1,
                                       predmodel=1,discretspace = discretspace)
    reg2.loess<-denoising.loess.global(Y = hedotest[[i]],X,formula = formula_lm,dimredumethod=1,
                                       predmodel=1,discretspace = discretspace)

    p1.loess=reg1.loess$preference

    p2.loess=reg2.loess$preference
    graph.surfconso.l1=as.image(Z=p1.loess,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
    graph.surfconso.l2=as.image(Z=p2.loess,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso

    dprob_lm_loess[[i]]=sum((graph.surfconso.l1$z-graph.surfconso.l2$z)^2)/(nbpoints^2)




    ##GAM before loess

    reg1gam<-predict.scores.gam(Y = hedotrain[[i]],formula_gam = formula_gam,discretspace = discretspace,map = map)
    reg2gam<-predict.scores.gam(Y = hedotest[[i]],formula_gam = formula_gam,discretspace = discretspace,map = map)

    p.gam1=100*rowMeans(reg1gam$preference)
    p.gam2=100*rowMeans(reg2gam$preference)
    graph.surfconso1g=as.image(Z=p.gam1,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
    graph.surfconso2g=as.image(Z=p.gam2,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso

    dprob_gam[[i]]=sum((graph.surfconso1g$z-graph.surfconso2g$z)^2)/(nbpoints^2)


    ## GAM after loess
    reg1gam.loess<-denoising.loess.global(Y = hedotrain[[i]],Y,formula = formula_gam,dimredumethod=1,
                                          predmodel=2,discretspace = discretspace)
    reg2gam.loess<-denoising.loess.global(Y = hedotest[[i]],Y,formula = formula_gam,dimredumethod=1,
                                          predmodel=2,discretspace = discretspace)

    p1g.loess=reg1gam.loess$preference

    p2g.loess=reg2gam.loess$preference

    graph.surfconso.l.g1=as.image(Z=p1g.loess,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
    graph.surfconso.l.g2=as.image(Z=p2g.loess,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso

    dprob_gam_loess[[i]]=sum((graph.surfconso.l.g1$z-graph.surfconso.l.g2$z)^2)/(nbpoints^2)



    ##glmulti before loess

    reg1glm<-predict.scores.glmulti(Y = hedotrain[[i]],formula_glm = formula_glm,discretspace = discretspace,map = map)
    reg2glm<-predict.scores.glmulti(Y = hedotest[[i]],formula_glm = formula_glm,discretspace = discretspace,map = map)

    p.glm1=100*rowMeans(reg1glm$preference)
    p.glm2=100*rowMeans(reg2glm$preference)
    graph.surfconso1glm=as.image(Z=p.glm1,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
    graph.surfconso2glm=as.image(Z=p.glm2,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso

    dprob_glmulti[[i]]=sum((graph.surfconso1glm$z-graph.surfconso2glm$z)^2)/(nbpoints^2)


    ## glmulti after loess
    reg1glm.loess<-denoising.loess.global(Y = hedotrain[[i]],Y,formula = formula_glm,dimredumethod=1,
                                          predmodel=3,discretspace = discretspace)
    reg2glm.loess<-denoising.loess.global(Y = hedotest[[i]],Y,formula = formula_glm,dimredumethod=1,
                                          predmodel=3,discretspace = discretspace)

    p1glm.loess=reg1glm.loess$preference

    p2glm.loess=reg2glm.loess$preference

    graph.surfconso.l.glm1=as.image(Z=p1glm.loess,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso
    graph.surfconso.l.glm2=as.image(Z=p2glm.loess,x=discretspace,nrow=nbpoints,ncol=nbpoints) # Surface d'un seul conso

    dprob_glmulti_loess[[i]]=sum((graph.surfconso.l.glm1$z-graph.surfconso.l.glm2$z)^2)/(nbpoints^2)

  }


  result <- rbind(dprob_lm,dprob_lm_loess,
                  dprob_gam,dprob_gam_loess,
                  dprob_glmulti,dprob_glmulti_loess)

  rownames(result) <- c("dprob_lm","dprob_lm_loess",
                        "dprob_gam","dprob_gam_loess",
                        "dprob_glmulti","dprob_glmulti_loess")


  return(result)


}
