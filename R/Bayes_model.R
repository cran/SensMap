# predict scores with bayes mcmcpack

predict.scores.bayes=function(Y,discretspace,map,
                              formula_bayes, burnin = 1000,
                              mcmc = 2000,verbose=0,seed = NA, beta.start = NA,
                              b0 = 0, B0 = 0, c0 = 0.001, d0 = 0.001, sigma.mu = NA,
                              sigma.var = NA){
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
    print(j)
    dt=cbind.data.frame(Y[,j],map)
    colnames(dt)[1]="Conso"
    modele=as.formula(paste("Conso",formula_bayes))

    dt.list=list(Conso=dt$Conso,F1=dt$F1,F2=dt$F2)

    regs[[j]]=MCMCregress(modele, data=dt.list, burnin = burnin,
                          mcmc = mcmc,verbose=verbose,seed = seed, beta.start = beta.start,
                          b0 = b0, B0 = B0, c0 = c0, d0 = d0, sigma.mu = sigma.mu,
                          sigma.var =  sigma.var )
    discretspace2=cbind.data.frame(rep(1,nrow(discretspace)),discretspace)
    colnames(discretspace2)[1]="y"
    formula2=paste("y",formula_bayes,sep="")
    m=lm(formula2,data=discretspace2)

    mod.mat=model.matrix(m)
    p=ncol(mod.mat)
    pred1=regs[[j]][,-(p+1)]%*%t(mod.mat)

    x=pred1
    pred1a=x
    pred.conso[,j]=colMeans(pred1a,na.rm=T)
    zz=(pred1a> mean(dt$Conso))
    preference[,j]=colMeans(zz,na.rm=T)

  }
  return(list(regression=regs,pred.conso=pred.conso,preference=preference))
}
