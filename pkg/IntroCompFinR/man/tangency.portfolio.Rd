\name{tangency.portfolio}
\alias{tangency.portfolio}
\title{
compute tangency portfolio
}
\description{
compute tangency portfolio
}
\usage{
tangency.portfolio(er, cov.mat, risk.free, shorts = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{er}{
N x 1 vector of expected returns
}
  \item{cov.mat}{
N x N return covariance matrix
}
  \item{risk.free}{
scalar, risk-free rate
}
  \item{shorts}{
logical, allow shorts is TRUE
}
}
\details{
The tangency portfolio t is the portfolio of risky assets with the highest Sharpe's slope and solves the optimization problem: 
max \eqn{\frac{t(t)\mu - r_f }{t(t)\Sigma t ^{1/2}}} st. t(t)1 = 1
where \eqn{r_f} denotes the risk-free rate.
}
\value{
  \item{call}{
  captures function call
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
# compute tangency portfolio
tan.port <- tangency.portfolio(er, covmat, r.free)
tan.port
summary(tan.port, risk.free=r.free)
plot(tan.port, col="blue")

#
# compute tangency portfolio with no short sales
tan.port.ns <- tangency.portfolio(er, covmat, r.free, shorts=FALSE)
tan.port.ns
summary(tan.port.ns, risk.free=r.free)
plot(tan.port.ns, col="blue")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
