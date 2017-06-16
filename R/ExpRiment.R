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
  message("=== Sampling data.")
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
  message("=== Main loop.")
  expr$df.res <-
    foreach(m = methods, .combine = 'rbind') %:%
    foreach(d = dats, .combine = 'rbind') %dopar%
    {
      m <- ExpRmouline(m, d)
      return(expr$extractor(d, m))
    }

  ## return
  expr
}
