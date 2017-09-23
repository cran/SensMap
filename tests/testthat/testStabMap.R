library(SensMap)
library(doBy)
library(fields)
library(glmulti)
library(data.table)
library(mgcv)
library(ggplot2)

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

# Results of comparing maps stability
res_stab= StabMap(Y,X,S,n=2,axis=c(1,2),
                  formula_lm="~I(F1*F1)+I(F2*F2)+F1*F2",
                  formula_gam="~s(F1,k=3)+s(F2,k=3)",
                  formula_glm="~I(F1*F1)+I(F2*F2)+F1*F2",
                  dimredumethod=1, nbpoints=50)
