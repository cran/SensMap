\name{PrefMap}
\alias{PrefMap}
\title{External Prefernce mapping tools}
\usage{
PrefMap(Y,X,S,axis=c(1,2),formula,dimredumethod=1,
predmodel=1,nbpoints=50,pred.na =FALSE,
graph.pred =FALSE,graph.map =TRUE, graph.map.3D =FALSE)
}
\arguments{
\item{Y}{a data frame in which each row represents a product
and each column represents hedonic scores of a
given consumer for the products.}

\item{X}{a data frame in which each row represents a product and
each column represents evaluations of a given sensory
or physico-chemical attribute for the products.}

\item{S}{a data frame crossing products and panelists evaluations
of a given sensory or physico-chemical attribute for each product. It's required when choosing Canonical Analysis as dimension reduction method.}

\item{axis}{a length 2 vector specifying the components of dimension reduction method to plot.}

\item{formula}{a formula expression to be introduced.
If predmodel=1, it corresponds to four polynomial regression models
performed using \code{\link[stats]{lm}}:
Quadratic model, the corresponding formula = "~I(F1*F1)+I(F2*F2)+F1*F2".
Vector model, the formula ="~ F1 + F2".
Circular model, the formula="~ F1 + F2 + (F1*F1 + F2*F2)".
Elliptic model, the formula="~I(F1*F1)+I(F2*F2)".
If predmodel= 2, Generalized Additive models(GAM) structure performed using \code{\link[mgcv]{gam}}, proposed formula="~s(F1,k=3)+s(F2,k=3)".
If predmodel= 3, Generalized Linear Models(GLM) structure performed using \code{\link[stats]{glm}}, proposed formula="~I(F1*F1)+I(F2*F2)+F1*F2" .
If predmodel= 4, Bayes structure performed using \code{\link[MCMCpack]{MCMCregress}}, proposed formula="~I(F1*F1)+I(F2*F2)+F1*F2".}

\item{dimredumethod}{1 is assigned to Principal Component Analysis(PCA). If 2, Multiple Factor Analysis(MFA). If 3, Canonical Analysis(CA).}

\item{predmodel}{the type of prediction model :
If predmodel = 1 : Polynomial regression models.
If predmodel = 2 : Generalized Additive models (GAM).
If predmodel = 3 : Generalized Linear Models (GLM).
If predmodel = 4 : Bayesian models.}

\item{nbpoints}{an integer. The number of points in th grid-point of the discretized plan of the map provided by the two first components. By default 50.}

\item{pred.na}{boolean, if TRUE, predictions outside the set of liking scores are removed (generally [0:10]), if FALSE, we keep all predictions in and outside the set of liking scores.}

\item{graph.pred}{boolean, if TRUE, plot of surface prediction
of all consumers is posted.}

\item{graph.map}{boolean, if TRUE, extrenal preference mapping in 2 dimensions is posted.}

\item{graph.map.3D}{boolean, if TRUE, extrenal preference mapping is 3 dimensions is posted.}
}
\value{
graph.pred plot of surface predictions

graph.map plot of external preference mapping

graph.map.3D plot of external preference mapping in 3D

nb.NA if pred.na = TRUE, it returns a list of the number of predictions outside the liking scores space for each consumer prediction surface.

pos.NA if pred.na=TRUE, it returns a list of positions of predictions outside the liking scores space for each consumer prediction surface.

occur.NA if pred.na=TRUE, it returns a data frame with total number of predictions outside the liking scores space at each point of the prediction surface of all consumers.
}
\description{
function that provides External Preference Mapping technique and visualizes consumers preferences towards products in function of the principal components of dimension reduction method obtained from quantitative descriptive variables. The dimension reduction can be performed from PCA, MFA and CA. The prediction can be fitted either from Polynomial regressions, GAM, GLM or Bayesian models. The surfaces of predictions and preferences are displayed in 2D and 3D.}
\details{
The external preference mapping method is a very common way used
in sensory analysis field to explain hedonic judgments towards products in function of analytic attributes of these products in order to understand the consumers' sensory profile and more especially which characteristics influence positively or negatively consumers' liking (Danzart,2004). A Principal Component Analysis is performed (PCA) on the descriptive data. Individual consumer responses are then regressed according to the chosen two principal components of PCA. Cards of prediction response surface for each consumer are displayed and then superposed. At each point of the grid, if the predicted value is higher than the mean value of liking in the hedonic data it is considered as preference point. The map of preferences for all consumers is then obtained. Areas of rejection and preferences are underlined via showing the percentage of consumers. This function refers to the method introduced by M. Danzart. It was implemented also in 'SensoMineR' package.
}
\examples{
# Example of external preference mapping performed from PCA and quadratic
# regression model.
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
 formula="~I(F1*F1)+I(F2*F2)+F1*F2",
 dimredumethod=1, predmodel=1, nbpoints=50,pred.na =FALSE,
 graph.pred =FALSE, graph.map =TRUE, graph.map.3D =FALSE )
 # when pred.na= TRUE total number of NA in each point of the grid-map
 nb.QR=map_QR$occur.NA
 # when pred.na=TRUE List of positions of predictions outside [0:10]
 pos.QR=map_QR$pos.NA
}
\author{
Ibtihel Rebhi, Rihab Boubakri, Dhafer Malouche
}
\references{
Sebastien Le, Francois Husson (2008).SensoMineR:
A package for sensory data analysis,
 Journal of Sensory Studies Volume 23, Issue 1, Pages 14-25

Danzart M., Sieffermann J.M., Delarue J. (2004). New developments in preference mapping techniques: finding out a consumer optimal product, its sensory profile and the key sensory attributes. 7th Sensometrics Conference, July 27-30, 2004, Davis, CA
}

\seealso{
\code{\link{SmoothMap}}, \code{\link{StabMap}}, \code{\link{SensMapUI}}
}
