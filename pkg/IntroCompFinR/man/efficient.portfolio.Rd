\name{efficient.portfolio}
\alias{efficient.portfolio}
\title{
compute minimum variance portfolio subject to target return
}
\description{
compute minimum variance portfolio subject to target return
}
\usage{
efficient.portfolio(er, cov.mat, target.return, shorts = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{er}{
N x 1 vector of expected returns
}
  \item{cov.mat}{
N x N covariance matrix of returns
}
  \item{target.return}{
scalar, target expected return
}
  \item{shorts}{
logical, allow shorts is TRUE
}
}
\details{
A mean-variance efficient portfolio x that achieves the target expected return \eqn{\mu_0}
solves the optimization problem:
min \eqn{t(x)\Sigma x} s.t t(x)1 =1 and \eqn{ t(x)\mu = \mu_0}  
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
# compute efficient portfolio subject to target return
target.return = er["MSFT"]
e.port.msft = efficient.portfolio(er, covmat, target.return)
e.port.msft
summary(e.port.msft, risk.free=r.free)
plot(e.port.msft, col="blue")

#
# compute efficient portfolio subject to target return with no short sales
target.return = er["MSFT"]
e.port.msft.ns = efficient.portfolio(er, covmat, target.return, shorts=FALSE)
e.port.msft.ns
summary(e.port.msft.ns, risk.free=r.free)
plot(e.port.msft.ns, col="blue")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
