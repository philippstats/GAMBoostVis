#' Plots GAMBoost objects with ggplot2
#' 
#' Same functionality (and almost same code) like in the orginal 
#' FIX_ME_PLEASE: If several plots are created (which is default) 
#' only the last plot is returned
#' NOTE: phi does not need to be specified anymore
#' \code{plot.GAMBoost} function but done with ggplot2.
#' Not tested 
#' @param object [\code{GAMBoost}]\cr including \code{y}
#' @param select [\code{vector(1)}]\cr Indicating the variables which should be plotted.
#' @param at.step [\code{integer(1)}]\cr Numeric value indicating which iteration step of GAMBoost is used.
#' @param print [\code{logical(1)}]\cr Sepcifies if should be printed or not. Default is TRUE.
#' @author Philipp J Roesch
#' @import GAMBoost
#' @importFrom splines spline.des
#' @import ggplot2 
#' @import scales
#' @import gridExtra
#' @export
 
 
ploterp <- function(object, select=NULL,at.step=NULL, print = TRUE) {
  if (class(object) != "GAMBoost") stop("<ploter> only implemented for objects of class 'GAMBoost'")
  #if(GAMBoost$y != )
  # phi - dispersion parameter
  if(object$family$family == "gaussian") phi <- var(object$y)
  if(object$family$family == "binomial" | object$family$family == "poisson") phi <- 1
  if(!object$family$family %in% c("gaussian", "binomial", "poisson")) {
    phi <- 1
    message("distribution family is not specified. <phi> is set to 1.")
  }
  #
  if (length(object$predictors) < 2) {
    cat("No non-paramatric components fitted, i.e. no plots available.\n")
    return(NULL)
  }
  if (is.null(select)) select <- 1:(length(object$predictors)-1) #' reduce one which is the intercept
  if (is.null(at.step)) { 
    at.step <- nrow(object$beta[[1]])
  } else {
    at.step <- at.step + 1
  }
  
  smooth.names <- colnames(object$x)
  if (is.null(smooth.names)) smooth.names <- paste("S",1:(length(object$predictors)-1),sep="")
  
  #set.xlab <- is.null(xlab)
  
  for (i in select) { #' 
    actual.x <- seq(from=object$predictors[[i+1]]$xl,to=object$predictors[[i+1]]$xr,length=100)
    actual.expansion <- splines::spline.des(knots = object$predictors[[i+1]]$knots, 
                                            x = actual.x, 
                                            ord = object$predictors[[i+1]]$bdeg + 1, 
                                            derivs = rep(0,length(actual.x)))$design
    actual.expansion <- actual.expansion %*% rbind(diag(ncol(actual.expansion)-1),rep(-1,ncol(actual.expansion)-1))
    eta <- drop(actual.expansion %*% object$beta[[i+1]][at.step,])
    
    #bands <- GAMBoost:::calc.confidence.bands(object,i,at.step-1,phi = phi)
    
    p_data <- data.frame(actual.x = actual.x, eta = eta)
   # b_data <- as.data.frame(bands)
      
    g <- ggplot() + #environment = environment()
        geom_line(data = p_data, aes(x = actual.x, y = eta)) + xlab(smooth.names[i]) +
        theme(axis.title =  element_text(size = rel(1.5)), 
              axis.text  =  element_text(size = rel(1.5))) #+ 
        #geom_line(data = b_data, aes(x, upper), linetype = "longdash") + 
        #geom_line(data = b_data, aes(x, lower), linetype = "longdash") 

  if(print) print(g)
  }
  return(g) #' FIX_ME_PLEASE: If several plots are created (which is default) only the last plot is returned
}