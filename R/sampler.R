##' Samler Constructor
##'
##' 
##' @param ... parameters
##' @return an object of class method
##' @author cayek
##' @export
ExpRsampler <- function(...)
{
  res <- list(...)
  class(res) <- c("ExpRsampler", "ExpRoperand")
  res
}
