##' Data Constructor
##'
##' 
##' @param Y the output marix
##' @return an object of class datax
##' @author cayek
##' @export
ExpRdata <- function(...)
{
  res <- list(...)
  class(res) <- c("ExpRdata")
  res
}
