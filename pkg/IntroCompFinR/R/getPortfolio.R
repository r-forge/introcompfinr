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
