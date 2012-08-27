\name{getPortfolio}
\alias{getPortfolio}
\title{
Construct the portfolio with expected return vector and covariance matrix.
}
\description{
Construct the portfolio with expected return vector and covariance matrix.
}
\usage{
getPortfolio(er, cov.mat, weights)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{er}{
N x 1 vector of expected returns
}
  \item{cov.mat}{
N x N covariance matrix of returns
}
  \item{weights}{
N x 1 vector of portfolio weights
}
}
\value{
   \item{call}{
The function call.
}
   \item{er}{
Portfolio expected return. 
} 
   \item{sd}{
Portfolio standard deviation.   
}   
   \item{weights}{
Portfolio weights.   
}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Eric Zivot
}

\examples{
#
# Examples from Introduction to Financial Econometrics
#
asset.names = c("MSFT", "NORD", "SBUX")
er = c(0.0427, 0.0015, 0.0285)
names(er) = asset.names
covmat = matrix(c(0.0100, 0.0018, 0.0011,
  	              0.0018, 0.0109, 0.0026,
		              0.0011, 0.0026, 0.0199),
		             nrow=3, ncol=3)
r.free = 0.005
dimnames(covmat) = list(asset.names, asset.names)
er
covmat
r.free

#
# compute equally weighted portfolio
ew = rep(1,3)/3
equalWeight.portfolio = getPortfolio(er=er,cov.mat=covmat,weights=ew)
class(equalWeight.portfolio)
names(equalWeight.portfolio)
equalWeight.portfolio
summary(equalWeight.portfolio)
plot(equalWeight.portfolio, col="blue")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
