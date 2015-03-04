#' @title Create four-panel plot of returns
#' 
#' @author Eric Zivot
#' 
#' @description
#' Four-panel plot showing histogram with normal curve overlaid, boxplot, sample ACF, and normal
#' qq-plot. Inspired by the four-panel plot in Statistical Analysis of Financial Data with R by 
#' Rene Carmona.
#' 
#' @param ret Single data object (xts, zoo, matrix, data.frame) of returns. It is assumed that the 
#' column has a name
#' 
#' @export fourPanelPlot

fourPanelPlot <-
function(ret)
{
  ret = PerformanceAnalytics::checkData(ret, "matrix")
  retName = colnames(ret)
  ret.den = density(ret)
  par(mfrow=c(2,2))
  hist(ret, main=paste(retName, " monthly returns", sep=""),
       xlab=retName, probability=T, col="cornflowerblue")
  # overlay normal distribution on smoothed density
  lines(ret.den$x, dnorm(ret.den$x, mean=mean(ret), sd=sd(ret)),
        col="black", lwd=2)
  legend(x="topleft", legend=c("Normal Curve"),
         lty=1, col="black", lwd=2, bty="n")
  boxplot(ret, outchar=T, col="cornflowerblue")
  # autocorrelations
  acf(ret, lwd=2, main="")
  # qq plot
  qqnorm(ret, col="cornflowerblue", pch=16)
  qqline(ret)
  par(mfrow=c(1,1))
}