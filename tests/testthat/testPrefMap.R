# Example of external preference mapping performed from PCA
# dimension reduction method and quadratic regression model.
data(hedo_biscuit)
data(sens_biscuit)
# Make data compatible to the form of X, Y and S
rownames(hedo_biscuit)=hedo_biscuit[,1]
Y=hedo_biscuit[,-1]
library(doBy)
juge_senso_bisc<- summaryBy(. ~ produit + juge,
                            data=sens_biscuit, FUN=c(mean),keep.names = TRUE,na.rm=TRUE)
S=juge_senso_bisc[,-3]
prod_bisc=summaryBy(. ~ produit, data=sens_biscuit,
                    FUN=c(mean),keep.names = TRUE,na.rm=TRUE)
rownames(prod_bisc)= prod_bisc[,1]
X= prod_bisc[,-c(1:3)]
# Map drawing in 2D
map_QR= PrefMap(Y,X,S,axis=c(1,2),
                formula="~I(F1*F1)+ I(F2*F2)+ F1*F2",
                dimredumethod=1, predmodel=1, nbpoints=50,pred.na =FALSE,
                graph.pred =FALSE, graph.map =FALSE, graph.map.3D =TRUE )

