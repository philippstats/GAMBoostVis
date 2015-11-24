#' Plot information criterion for all iterations
#' 
#' Plots Deviance and AIC in one plot and indicates the iteration with the best AIC
#' @param object [\code{GAMBoost}]\cr including \code{y}
#' # @param title title [\code{character(1)}]\cr Change title.
#' @import ggplot2
#' @importFrom cowplot plot_grid
#' @export
#' @author Philipp J Roesch

plot_ICs <- function(object) {
#plot_ICs <- function(object, title = NULL) {
  #
  if (class(object) != "GAMBoost") stop("<plot_ICs> only implemented for objects of class 'GAMBoost'")
  #
  #if(is.null(title)) title <- paste0(deparse(substitute(object)), ": Information Criterions")
  stepno <- object$stepno
  data <- data.frame(stepno = object$stepno, deviance = object$deviance, AIC = object$AIC)
  plot_d <- ggplot(data = data) + 
    geom_line(aes(x = 0:stepno[1], y = deviance), size = rel(1.5)) + xlab("Iteration") + ylab("Deviance") + 
    theme(axis.title =  element_text(size = rel(1.5)),   axis.text  =  element_text(size = rel(1.5))) + 
    scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x)))
  plot_a <- ggplot() + 
    geom_line(data = data, aes(x = 0:stepno[1], y = AIC), size = rel(1.5)) + xlab("Iteration") + ylab("AIC") +
    geom_vline(xintercept = which.min(object$AIC) - 1, colour = "#56B1F7", size = 2, linetype = "solid") +
    theme(axis.title =  element_text(size = rel(1.5)),   axis.text  =  element_text(size = rel(1.5)))
  plot_grid(plot_d, plot_a, ncol = 2)
}
