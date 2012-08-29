\name{plot.Markowitz}
\alias{plot.Markowitz}
\title{
Plot method of Markowitz class
}
\description{
plot efficient frontier. 
}
\usage{
\S3method{plot}{Markowitz}(object, plot.assets = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
object of class Markowitz.
}
  \item{plot.assets}{
logical. If true then plot asset sd and er
}
  \item{\dots}{
controlled variables for \code{plot()}
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

plot(ef)
plot(ef, plot.assets=TRUE, col="blue", pch=16)
points(gmin.port$sd, gmin.port$er, col="green", pch=16, cex=2)
points(tan.port$sd, tan.port$er, col="red", pch=16, cex=2)
text(gmin.port$sd, gmin.port$er, labels="GLOBAL MIN", pos=2)
text(tan.port$sd, tan.port$er, labels="TANGENCY", pos=2)    
sr.tan = (tan.port$er - r.free)/tan.port$sd
abline(a=r.free, b=sr.tan, col="green", lwd=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
