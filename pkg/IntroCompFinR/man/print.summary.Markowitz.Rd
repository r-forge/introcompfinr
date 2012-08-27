\name{print.summary.Markowitz}
\alias{print.summary.Markowitz}
\title{
print method of class summary.Markozitz
}
\description{
print method of class summary.Markozitz
}
\usage{
\method{print}{summary.Markowitz}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
object of class summary.Markowitz
}
  \item{\dots}{
controlled variables for print
}
}

\author{
Eric Zivot
}

\examples{
# construct the data
asset.names = c("MSFT", "NORD", "SBUX")
er = c(0.0427, 0.0015, 0.0285)
names(er) = asset.names
covmat = matrix(c(0.0100, 0.0018, 0.0011,
                  0.0018, 0.0109, 0.0026,
                  0.0011, 0.0026, 0.0199),
		             nrow=3, ncol=3)
r.free = 0.005
dimnames(covmat) = list(asset.names, asset.names)

# tangency portfolio
tan.port <- tangency.portfolio(er, covmat, r.free)
# compute global minimum variance portfolio
gmin.port = globalMin.portfolio(er, covmat)

#
# compute portfolio frontier
ef <- efficient.frontier(er, covmat, alpha.min=-2, 
                         alpha.max=1.5, nport=20)
attributes(ef)
s.ef <- summary(ef)
print(s.ef)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
