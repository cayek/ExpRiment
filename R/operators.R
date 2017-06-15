##' Concat list
##'
##' @param list1 list 1
##' @param list2 list 2
##' @return a concatenation of both list
##' @author cayek
##' @export
##'
##' @examples
##'
##' list3 <- list(a = 1) + list(b = 2)
##' 
`+.ExpRlistOperand` <- function(list1, list2)
{
  res <- c(list1, list2)
  class(res) <- "ExpRlistOperand"
  res
}

##' Expand list1 with element of list 2
##'
##' @param list1 list 1
##' @param list2 list 2
##' @return expanded list
##' @author cayek
##' @export
##'
##' @examples
##' class(list1) <- "ExpRoperand"
##' list2 <- list(b = 2)
##' class(list2) <- "ExpRoperand"
##' list1 + list2
`*.ExpRoperand` <- function(list1, list2)
{

  params <- base::expand.grid(list2)
  res <- list()
  class(res) <- "ExpRlistOperand"
  ns <- names(params)

  if (length(ns) == 0) {
    res[[1]] <- list1
  } else {
    for (i in 1:nrow(params)) {
      res[[i]] <- list1
      res[[i]][ns] <- params[i,]
    }
  }
  res
}


##' Parameters
##'
##' 
##' @param ... parameters
##' @return an object of class ExpRparam
##' @author cayek
##' @export
param <- function(...)
{
  res <- list(...)
  class(res) <- c("ExpRparam", "ExpRoperand")
  res
}

#' *MOULINE*
#'
#' Run the algorithm of an ExpRoperand
#'
#' @export
ExpRmouline <- function(...){
  UseMethod("ExpRmouline")
}

