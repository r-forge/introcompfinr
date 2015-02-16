#' @title Create portfolio object
#' 
#' @author Eric Zivot
#' 
#' @description
#' Construct the portfolio with expected return vector and covariance matrix.
#' 
#' @details 
#' To specify a portfolio, an expected return vector and covariance matrix for the assets under
#' consideration as well as a vector of portfolio weights are needed.
#' 
#' @param er N x 1 vector of expected returns
#' @param cov.mat N x N return covariance matrix
#' @param weigths N x 1 vector of portfolio weights
#' @param object object of class portfolio
#' @param risk.free numeric, risk free rate
#' @param ... controlled variables for \code{plot()}, \code{print()} and \code{summary()}
#' 
#' @return 
#'  \item{call}{captures function call}
#'  \item{er}{portfolio expected return}
#'  \item{sd}{portfolio standard deviation}
#'  \item{weights}{N x 1 vector of portfolio weights}
#' 
#' @examples
#' # Examples from Introduction to Financial Econometrics
#' asset.names = c("MSFT", "NORD", "SBUX")
#' er = c(0.0427, 0.0015, 0.0285)
#' names(er) = asset.names
#' covmat = matrix(c(0.0100, 0.0018, 0.0011,
#'                   0.0018, 0.0109, 0.0026,
#'                   0.0011, 0.0026, 0.0199),
#'                 nrow=3, ncol=3)
#' r.free = 0.005
#' dimnames(covmat) = list(asset.names, asset.names)
#' er
#' covmat
#' r.free
#' 
#' # compute equally weighted portfolio
#' ew = rep(1,3)/3
#' equalWeight.portfolio = getPortfolio(er=er,cov.mat=covmat,weights=ew)
#' class(equalWeight.portfolio)
#' names(equalWeight.portfolio)
#' equalWeight.portfolio
#' summary(equalWeight.portfolio)
#' plot(equalWeight.portfolio, col="blue")

getPortfolio <-
function(er, cov.mat, weights)
{
	# contruct portfolio object
	#
	# inputs:
	# er				   N x 1 vector of expected returns
	# cov.mat  		 N x N covariance matrix of returns
	# weights			 N x 1 vector of portfolio weights
	#
	# output is portfolio object with the following elements
	# call				original function call
	# er				  portfolio expected return
	# sd				  portfolio standard deviation
	# weights			N x 1 vector of portfolio weights
	#
	call <- match.call()
	
	#
	# check for valid inputs
	#
	asset.names <- names(er)
	weights <- as.vector(weights)
	names(weights) = names(er)
  er <- as.vector(er)					# assign names if none exist
	if(length(er) != length(weights))
		stop("dimensions of er and weights do not match")
 	cov.mat <- as.matrix(cov.mat)
	if(length(er) != nrow(cov.mat))
		stop("dimensions of er and cov.mat do not match")
	if(any(diag(chol(cov.mat)) <= 0))
		stop("Covariance matrix not positive definite")
		
	#
	# create portfolio
	#
	er.port <- crossprod(er,weights)
	sd.port <- sqrt(weights %*% cov.mat %*% weights)
	ans <- list("call" = call,
	      "er" = as.vector(er.port),
	      "sd" = as.vector(sd.port),
	      "weights" = weights) 
	class(ans) <- "portfolio"
	return(ans)
}
