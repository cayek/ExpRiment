#' ExpRiment R package
#'
#'
#' @docType package
#'
#' @name ExpRiment
#' @importFrom foreach foreach %:% %do% %dopar%
NULL

##' Main class
##'
##' This class define an experiment.
##'
##' 
##' @param samplers list of sampler
##' @param preprocessors list of preprocessors
##' @param methods list of method
##' @param extractor the extractor
##' @return an ExpR object
##' @author cayek
##' @export
ExpR <- function(rep.nb, samplers, preprocessors, methods, extractor) {
  res <- list(rep.nb = rep.nb,
              samplers = samplers,
              preprocessors = preprocessors,
              methods = methods,
              extractor = extractor)
  class(res) <- c("ExpR")
  res
}

##' Run the experiment
##'
##' @param expr an ExpR class object.
##' @return a tidy tibble with wanted information.
##' @author cayek
ExpRmouline.ExpR <- function(expr) {

  ## sample

  dats <- list()
  dats <-
    foreach(sampler = expr$samplers, .combine = 'c') %dopar%
    {
      aux <- list()
      for (i in 1:(expr$rep.nb)) {
        dat <- ExpRmouline(sampler)
        dat$i <- i
        aux[[i]] <- dat
      }
      return(aux)
    }
  
  ## preprocess

  ## main loop
  expr$df.res <-
    foreach(d = dats, m = expr$methods, .combine = 'rbind') %dopar%
    {
      method <- ExpRmouline(m, d)
      expr$extractor(d, m)
    }

  ## return
  expr
}
