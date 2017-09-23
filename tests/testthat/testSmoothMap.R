data(hedo_biscuit)
data(sens_biscuit)
rownames(hedo_biscuit)=hedo_biscuit[,1]
Y=hedo_biscuit[,-1]
juge_senso_bisc<- summaryBy(. ~ produit + juge,
                            data=sens_biscuit, FUN=c(mean),keep.names = TRUE,na.rm=TRUE)
S=juge_senso_bisc[,-3]
prod_bisc=summaryBy(. ~ produit, data=sens_biscuit,
                    FUN=c(mean),keep.names = TRUE,na.rm=TRUE)
rownames(prod_bisc)= prod_bisc[,1]
X= prod_bisc[,-c(1:3)]
# Example of smoothed version of external preference mapping for quadratic model.
Smap= SmoothMap(Y,X,S, axis=c(1,2),
                formula="~I(F1*F1)+I(F2*F2)+F1*F2",
                dimredumethod=1,predmodel=1,span=.5,degree=2,
                graphpred=FALSE, drawmap=TRUE, dmap.loess=FALSE)

