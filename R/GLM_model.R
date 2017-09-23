predict.scores.glmulti=function(Y,discretspace,
                                map,formula_glm
                                ,pred.na=FALSE){
  ## map is a data.frame with F1 and F2 obtained after DR on the explained data Y
  notespr= nbconsos=matrix(0,nrow(discretspace),ncol(discretspace))
  regs=datas=vector("list",ncol(Y))
  pred.conso=preference=matrix(0,nrow = nrow(discretspace),ncol=ncol(Y))
  nb.NA=vector("list",ncol(Y))
  pos.NA=vector("list",ncol(Y))
  nbconsos=c()
  for(j in 1:ncol(Y)){
    map.reg=cbind.data.frame(Y[,j],map)
    colnames(map.reg)[1]="Conso"
    modele=as.formula(paste("Conso",formula_glm))
    regs[[j]]=glm(modele,data=map.reg)
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
