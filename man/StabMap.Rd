\name{StabMap}
\alias{StabMap}
\title{Evaluation of External Prefernce mapping stability}
\description{
A function that introduces a comparison approach of maps stability from different strategies before and after LOESS smoothing. It is based on the calculation of an average squared distance of preferences (ASDP) between maps obtained from subsamples of hedonic data through a resampling approach. The strategy with an overall ASDP lower than the other is preferred.}
\usage{
StabMap(Y,X,S,n,axis=c(1,2),formula_lm,
formula_gam,formula_glm, dimredumethod=1,
pred.na=FALSE,nbpoints=50)
}
\arguments{
\item{Y}{a data frame in which each row represents a product
and each column represents hedonic scores of a
given consumer for the products.}

\item{X}{a data frame in which each row represents a product and
each column represents evaluations of a given sensory
or physico-chemical attribute for the products.}

\item{S}{a data frame crossing products and panelists evaluations
of a given sensory or physico-chemical attribute for the products.
It's required when choosing Canonical Analysis ad dimension reduction method.}

\item{n}{number of selection of random samples from hedonic data for which maps are compared before and after LOESS smoothing.}

\item{axis}{a length 2 vector specifying the components of dimension reduction method to plot.}

\item{formula_lm}{a formula expression for polynomial regression model for example
formula_lm = "~I(F1*F1)+I(F2*F2)+F1*F2".}

\item{formula_gam}{a formula expression for GAM model fitting for example
formula_gam="~s(F1,k=3)+s(F2,k=3)".}

\item{formula_glm}{a formula expression for GLM model fitting  for example
formula_glm = "~I(F1*F1)+I(F2*F2)+F1*F2".}

\item{dimredumethod}{1 is assigned to Principal Component Analysis(PCA). If 2, Multiple Factor Analysis(MFA). If 3, Canonical Analysis(CA).}

\item{pred.na}{boolean, if TRUE, predictions outside the set of liking scores are removed, if FALSE, all predections are kept in and out the set of liking scores.}

\item{nbpoints}{an integer. The number of points in the grid-point of the discretized plan. By default = 50.}
}
\value{
sum squared distances of preferences computed between maps before and after LOESS smoothing for each prediction model.}

\author{
Ibtihel Rebhi, Rihab Boubakri, Dhafer Malouche
}

\examples{
\dontrun{
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
# Results of comparing maps stability
res_stab= StabMap(Y,X,S,n=2,axis=c(1,2),
formula_lm="~I(F1*F1)+I(F2*F2)+F1*F2",
formula_gam="~s(F1,k=3)+s(F2,k=3)",
formula_glm="~I(F1*F1)+I(F2*F2)+F1*F2",
dimredumethod=1, nbpoints=50)
res_stab=melt(res_stab)
colnames(res_stab)=c("prob", "var", "value")
# Visualization of stability results
gr<-ggplot(res_stab,aes(x=prob,y=value,fill=prob))+geom_boxplot()+xlab("Methods")+
ylab("Average Squared Distance of Preferences")
gr<-gr+theme_bw()+theme(legend.position = "none",
    axis.text.x = element_text(angle = 90))
gr
}
}
\seealso{
\code{\link{PrefMap}}, \code{\link{SmoothMap}},\code{\link{SensMapUI}}
}

