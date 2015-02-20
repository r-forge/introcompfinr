#' @title Compute tangency portfolio
#' 
#' @author Eric Zivot
#' 
#' @description
#' Compute tangency (maximum Sharpe ratio) portfolio. The portfolio can allow all assets to be
#' shorted or not allow any assets to be shorted.
#' 
#' @details 
#' The tangency portfolio \samp{t} is the portfolio of risky assets with the highest Sharpe's slope
#' and solves the optimization problem: max \eqn{(t(t)\mu-r_f)/(t(t)\Sigma t^{1/2})} s.t.
#' \eqn{t(t)1=1} where \eqn{r_f} denotes the risk-free rate. If short sales are allowed then there
#' is an analytic solution using matrix algebra. If short sales are not allowed then the maximum
#' Sharpe ratio portfolio must be computed numerically.
#' 
#' @param er \samp{N x 1} vector of expected returns
#' @param cov.mat \samp{N x N} return covariance matrix
#' @param risk.free numeric, risk free rate
#' @param shorts logical, if \samp{TRUE} then short sales (negative portfolio weights)
#' are allowed. If \samp{FALSE} then no asset is allowed to be sold short.
#' 
#' @return 
#'  \item{call}{captures function call}
#'  \item{er}{portfolio expected return}
#'  \item{sd}{portfolio standard deviation}
#'  \item{weights}{\samp{N x 1} vector of portfolio weights}
#' 
#' @examples
#' # construct the data
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
#' 
#' @export tangency.portfolio

tangency.portfolio <-
function(er,cov.mat,risk.free, shorts=TRUE)
{
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
    w.t <- as.vector(w.t/sum(w.t))          # normalize weights
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