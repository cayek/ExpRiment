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
ExpR <- function(rep.nb.sampler, samplers, preprocessors, rep.nb.method, methods, extractor,
                 sampler.env = NULL) {
  res <- list(rep.nb.sampler = rep.nb.sampler,
              rep.nb.method = rep.nb.method,
              samplers = samplers,
              preprocessors = preprocessors,
              methods = methods,
              sampler.env = sampler.env,
              extractor = extractor)
  class(res) <- c("ExpR")
  res
}

##' Run the experiment
##'
##' @param expr an ExpR class object.
##' @return a tidy tibble with wanted information.
##' @author cayek
##' @export
ExpRmouline.ExpR <- function(expr) {

  ## sample
  message("=== Sampling data.")
  dats <- list()
  dats <-
    foreach(sampler = expr$samplers, .combine = 'c') %dopar%
    {
      aux <- list()
      for (i in 1:(expr$rep.nb.sampler)) {
        dat <- ExpRmouline(sampler)
        aux[[i]] <- list(dat = dat, i = i)
      }
      return(aux)
    }

  ## free memory
  if (!is.null(expr$sampler.env)) {
    sampler.env$empty(sampler.env)
  }

  ## preprocess

  ## main loop
  message("=== Main loop.")
  expr$df.res <-
    foreach(m = methods, .combine = 'rbind') %:%
    foreach(d = dats, .combine = 'rbind') %dopar%
    {
      res <- data.frame()
      for (j in 1:(expr$rep.nb.method)) {
        m <- ExpRmouline(m, d$dat)
        res <- rbind(res,
                     expr$extractor(d$dat, m, rep.sampler = d$i, rep.method = j))
      }
      return(res)
    }
  ## return
  expr
}
