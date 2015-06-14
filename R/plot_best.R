#' Plots all varibles at once in best step according to \code{criterion}
#' 
#' 
#' @param object [\code{GAMBoost}]\cr including \code{y}
#' @param title [\code{character(1)}]\cr Change title. Default "object: Best AIC at Iteration x"
#' @importFrom gridExtra grid.arrange
#' @author Philipp J Roesch
#' @export
#' 


plot_best <- function(object, title = NULL) {
  #Input checking
  if (class(object) != "GAMBoost") stop("<plot_best> only implemented for objects of class 'GAMBoost'")
  #Plot
  best_step <- which.min(object$AIC)
  n_smooth <- length(object$names.smooth) 
  n_lin <- length(object$names.linear)
  n_var <- n_smooth + n_lin 
  #white <- ceiling(n_var / 3) * 3 - n_var
 
  if(is.null(title)) title <- paste0(deparse(substitute(object)), ": Best ", "AIC", " at Iteration ", 
               best_step - 1)
  plots <- lapply(1:n_smooth, function(x)  ploter(object, select = x, at.step = best_step, print = FALSE))
  do.call(grid.arrange,  c(plots, main = title))
}


