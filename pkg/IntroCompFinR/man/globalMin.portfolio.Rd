\name{globalMin.portfolio}
\alias{globalMin.portfolio}
\title{
compute global minimum variance portfolio
}
\description{
compute global minimum variance portfolio
}
\usage{
globalMin.portfolio(er, cov.mat, shorts = TRUE)

\method{plot}{portfolio}(object, ...)
\method{print}{portfolio}(object, ...)
\method{summary}{portfolio}(object, risk.free = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{er}{
N x 1 vector of expected returns
}
  \item{cov.mat}{
N x N return covariance matrix
}
  \item{shorts}{
logical, allow shorts is TRUE
}
 \item{object}{
object of class portfolio
}
  \item{risk.free}{
Numeric, risk free rate.
}
  \item{\dots}{
controlled variables for generic function \code{print},\code{plot} and \code{summary} 
}
}
\details{
The global minimum variance portfolio (allowing for short sales) m solves the optimization problem: 
min \eqn{ t(m)\Sigma m } st. t(m)1=1
}
\value{
  \item{call}{
  original function call
  }
  \item{er}{
  portfolio expected return
  }
  \item{sd}{
  portfolio standard deviation
  }
  \item{weights}{
  N x 1 vector of portfolio weights
  }
}
\references{
%% ~put references to the literature/web site here ~
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
# compute global minimum variance portfolio
gmin.port = globalMin.portfolio(er, covmat)
attributes(gmin.port)
print(gmin.port)
summary(gmin.port, risk.free=r.free)
plot(gmin.port, col="blue")

#
# compute global minimum variance portfolio with no short sales
gmin.port.ns = globalMin.portfolio(er, covmat, shorts=FALSE)
attributes(gmin.port.ns)
print(gmin.port.ns)
summary(gmin.port.ns, risk.free=r.free)
plot(gmin.port.ns, col="blue")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
