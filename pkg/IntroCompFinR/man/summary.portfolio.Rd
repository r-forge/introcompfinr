\name{summary.portfolio}
\alias{summary.portfolio}
\title{
summary method of class portfolio
}
\description{
summary method of class portfolio
}
\usage{
\method{summary}{portfolio}(object, risk.free = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
object of class portfolio
}
  \item{risk.free}{
Numeric, risk free rate.
}
  \item{\dots}{
controlled variables for \code{summary}
}
}

\author{
Eric Zivot
}

\examples{
asset.names = c("MSFT", "NORD", "SBUX")
er = c(0.0427, 0.0015, 0.0285)
names(er) = asset.names
covmat = matrix(c(0.0100, 0.0018, 0.0011,
                  0.0018, 0.0109, 0.0026,
  	              0.0011, 0.0026, 0.0199),
		             nrow=3, ncol=3)
r.free = 0.005
dimnames(covmat) = list(asset.names, asset.names)

#
# compute equally weighted portfolio
ew = rep(1,3)/3
equalWeight.portfolio = getPortfolio(er=er,cov.mat=covmat,weights=ew)
summary(equalWeight.portfolio)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
