\name{SmoothMap}
\alias{SmoothMap}
\title{Smoothed version of External Preference Mapping by LOESS}
\description{
a function that provides a smoothed version of external preference mapping by performing a locally weighted regression smoothing
(LOESS) on the curves of preference level.
}
\usage{
SmoothMap(Y,X,S,axis=c(1,2),
formula,dimredumethod=1,
predmodel=1,pred.na =FALSE,span=.5,
degree=2, graphpred=FALSE, drawmap=TRUE, dmap.loess=FALSE,
nbpoints=50)
}

\arguments{
\item{Y}{a data frame in which each row represents a product
and each column represents hedonic scores of a
given consumer for the products.}

\item{X}{a data frame in which each row represents a product and
each column represents evaluations of a given sensory
or physico-chemical attribute for the products.}

\item{S}{a data frame crossing products and panelists evaluations
of a given sensory or physico-chemical attribute for the product.
It's required when choosing Canonical Analysis as dimension reduction method.}

\item{axis}{a length 2 vector specifying the components of dimension reduction method to plot.}

\item{formula}{a formula expression to be introduced. If predmodel=1, it corresponds to four polynomial regression models performed using
\code{\link[stats]{lm}} : Quadratic model, the corresponding
formula = "~I(F1*F1)+I(F2*F2)+F1*F2". Vector model,
the formula = "~ F1 + F2". Circular model,
the formula="~ F1 + F2 + (F1*F1 + F2*F2)". Elliptic model,
the formula = "~I(F1*F1)+I(F2*F2)". If predmodel= 2, Generalized Additive models (GAM) structure performed using \code{\link[mgcv]{gam}}, proposed formula="~s(F1,k=3)+s(F2,k=3)". If predmodel= 3, Generalized Linear Models(GLM) structure performed using \code{\link[stats]{glm}}, proposed
formula = "~I(F1*F1)+ I(F2*F2)+ F1*F2". If predmodel= 4, Bayes structure performed using \code{\link[MCMCpack]{MCMCregress}}, proposed
formula="~I(F1*F1)+I(F2*F2)+F1*F2".}

\item{dimredumethod}{1 is assignedd to Principal Component Analysis(PCA). If 2, Multiple Factor Analysis(MFA). If 3, Canonical Analysis(CA).}

\item{predmodel}{the type of prediction model :
If predmodel = 1 : Polynomial regression models.
If predmodel = 2 : Generalized Additive models (GAM).
If predmodel = 3 : Generalized Linear Models (GLM).
If predmodel = 4 : Bayesian models.}

\item{pred.na}{boolean, if TRUE, predictions outside the set of liking scores are removed (generally [0:10]), if FALSE, we keep all predictions in and outside the set of liking scores.}

\item{span}{the parameter which controls the degree of smoothing by LOESS.}

\item{degree}{the degree of the polynomials to be used, normally
between 1 and 2.}

\item{graphpred}{boolean, if TRUE, plot of smoothed surface prediction of all consumers is posted.}

\item{drawmap}{boolean, if TRUE, smoothed version of extrenal preference mapping in 2 dimensions is posted.}

\item{dmap.loess}{boolean, if TRUE, smoothed version of extrenal preference mapping is 3 dimensions is posted.}

\item{nbpoints}{an integer. The number of points in th grid-point of the discretized plan of the map provided with the two first components. By default 50.}
}
\value{
z.loess object of class loess obtained from
       fitting a polynomial surface determined by
       the components of dimension reduction method as predictors
       and consumers predictions as explained variable.

m.loess object of class loess obtained from
       fitting a polynomial surface determined by
       the components of dimension reduction method as predictors
       and consumers preferences as explained variable.

plots of the smoothed prediction surface, smoothed external preference mapping in 2D and in 3D
}

\examples{
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
# Example of smoothed version of external preference mapping for quadratic
#  model.
# Smap= SmoothMap(Y,X,S, axis=c(1,2),
# formula="~I(F1*F1)+I(F2*F2)+F1*F2",
# dimredumethod=1,predmodel=1,span=.5,degree=2,
 #graphpred=FALSE, drawmap=TRUE, dmap.loess=FALSE)
# results of predictions by LOESS smoothing
#  Smap$z.loess
#  Smap$m.loess
}
\author{
Ibtihel Rebhi, Dhafer Malouche
}


\seealso{
\code{\link{PrefMap}}, \code{\link{StabMap}},
\code{\link{SensMapUI}}
}
