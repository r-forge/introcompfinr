#' @title Compute tangency portfolio
#' 
#' @author Eric Zivot
#' 
#' @description
#' Compute tangency portfolio.
#' 
#' @details 
#' The tangency portfolio t is the portfolio of risky assets with the highest Sharpe's slope and
#' solves the optimization problem: max \eqn{(t(t)\mu-r_f)/(t(t)\Sigma t^{1/2})} s.t. \eqn{t(t)1=1}
#' where \eqn{r_f} denotes the risk-free rate.
#' 
#' @param er N x 1 vector of expected returns
#' @param cov.mat N x N return covariance matrix
#' @param risk.free numeric, risk free rate
#' @param shorts logical, allow shorts is \code{TRUE}
#' @param object object of class Markowitz
#' @param ... controlled variables for \code{plot()}, \code{print()} and \code{summary()}
#' 
#' @return 
#'  \item{call}{captures function call}
#'  \item{er}{portfolio expected return}
#'  \item{sd}{portfolio standard deviation}
#'  \item{weights}{N x 1 vector of portfolio weights}
#' 
#' @examples
#' asset.names = c("MSFT", "NORD", "SBUX")
#' er = c(0.0427, 0.0015, 0.0285)
#' names(er) = asset.names
#' covmat = matrix(c(0.0100, 0.0018, 0.0011,
#'                   0.0018, 0.0109, 0.0026,
#'                   0.0011, 0.0026, 0.0199),
#'                 nrow=3, ncol=3)
#' r.free = 0.005
#' dimnames(covmat) = list(asset.names, asset.names)
#' 
#' # compute tangency portfolio
#' tan.port <- tangency.portfolio(er, covmat, r.free)
#' tan.port
#' summary(tan.port, risk.free=r.free)
#' plot(tan.port, col="blue")
#' 
#' # compute tangency portfolio with no short sales
#' tan.port.ns <- tangency.portfolio(er, covmat, r.free, shorts=FALSE)
#' tan.port.ns
#' summary(tan.port.ns, risk.free=r.free)
#' plot(tan.port.ns, col="blue")

tangency.portfolio <-
function(er,cov.mat,risk.free, shorts=TRUE)
{
  # compute tangency portfolio
  #
  # inputs:
  # er				   N x 1 vector of expected returns
  # cov.mat		   N x N return covariance matrix
  # risk.free		 scalar, risk-free rate
  # shorts          logical, allow shorts is TRUE
  #
  # output is portfolio object with the following elements
  # call			  captures function call
  # er				  portfolio expected return
  # sd				  portfolio standard deviation
  # weights		 N x 1 vector of portfolio weights
  call <- match.call()

  #
  # check for valid inputs
  #
  asset.names <- names(er)
  if(risk.free < 0)
    stop("Risk-free rate must be positive")
  er <- as.vector(er)
  cov.mat <- as.matrix(cov.mat)
  N <- length(er)
  if(N != nrow(cov.mat))
    stop("invalid inputs")
  if(any(diag(chol(cov.mat)) <= 0))
    stop("Covariance matrix not positive definite")
  # remark: could use generalized inverse if cov.mat is positive semi-definite

  #
  # compute global minimum variance portfolio
  #
  gmin.port <- globalMin.portfolio(er, cov.mat, shorts=shorts)
  if(gmin.port$er < risk.free)
    stop("Risk-free rate greater than avg return on global minimum variance portfolio")

  # 
  # compute tangency portfolio
  #
  if(shorts==TRUE){
    cov.mat.inv <- solve(cov.mat)
    w.t <- cov.mat.inv %*% (er - risk.free) # tangency portfolio
    w.t <- as.vector(w.t/sum(w.t))	# normalize weights
  } else if(shorts==FALSE){
    Dmat <- 2*cov.mat
    dvec <- rep.int(0, N)
    er.excess <- er - risk.free
    Amat <- cbind(er.excess, diag(1,N))
    bvec <- c(1, rep(0,N))
    result <- solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=1)
    w.t <- round(result$solution/sum(result$solution), 6)
  } else {
    stop("Shorts needs to be logical. For no-shorts, shorts=FALSE.")
  }
    
  names(w.t) <- asset.names
  er.t <- crossprod(w.t,er)
  sd.t <- sqrt(t(w.t) %*% cov.mat %*% w.t)
  tan.port <- list("call" = call,
		   "er" = as.vector(er.t),
		   "sd" = as.vector(sd.t),
		   "weights" = w.t)
  class(tan.port) <- "portfolio"
  return(tan.port)
}
