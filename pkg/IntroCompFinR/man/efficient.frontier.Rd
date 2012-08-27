\name{efficient.frontier}
\alias{efficient.frontier}
\title{
Compute efficient frontier with no short-sales constraints
}
\description{
The function constructs the set of efficient portfolios using this method for a collection of alpha values.
}
\usage{
efficient.frontier(er, cov.mat, nport = 20, alpha.min = -0.5, alpha.max = 1.5, shorts = TRUE)
\method{plot}{Markowitz}(object, plot.assets = FALSE, ...)
\method{print}{Markowitz}(object, ...)
\method{summary}{Markowitz}(object, risk.free = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{er}{
N x 1 vector of expected returns
}
  \item{cov.mat}{
N x N return covariance matrix
}
  \item{nport}{
scalar, number of efficient portfolios to compute
}
  \item{alpha.min}{
minimum value of alpha, default is -.5
}
  \item{alpha.max}{
maximum value of alpha, defualt is 1.5
}
  \item{shorts}{
logical, allow shorts is TRUE
}
  \item{object}{
object of class Markowitz.
}
  \item{plot.assets}{
logical. If true then plot asset sd and er
}
  \item{risk.free}{
Numeric number of risk free rate
}
  \item{\dots}{
controlled variables for \code{plot()} or \code{print()}
}

}
\details{
The the set of efficient portfolios of risky assets can be computed as a convex combination of any two efficient portfolios. It is convenient to use the global minimum variance portfolio as one portfolio and an efficient portfolio with target expected return equal to the maximum expected return of the assets under consideration as the other portfolio. Call these portfolios m and x, respectively. For any number alpha,
another efficient portfolio can be computed as \eqn{z = \alpha m+ (1 ??? \alpha) x }
}
\value{
  \item{call}{
  captures function call
  }
  \item{er}{
  nport x 1 vector of expected returns on efficient porfolios
  }
  \item{sd}{
  nport x 1 vector of std deviations on efficient portfolios
  }
  \item{weights}{
  nport x N matrix of weights on efficient portfolios
  }
}
\references{
%% ~put references to the literature/web site here ~
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
