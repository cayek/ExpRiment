##' Method Constructor
##'
##' 
##' @param ... parameters
##' @return an object of class method
##' @author cayek
##' @export
ExpRmethod <- function(...)
{
  res <- list(...)
  class(res) <- c("ExpRmethod", "ExpRoperand")
  res
}
