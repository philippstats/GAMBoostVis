#' Plots frequency of selected variables at specified step
#' 
#' xx
#' @param object [\code{GAMBoost}]\cr including \code{y}
#' @param at.step [\code{numeric(1)}]\cr specifies iteration step
#' @import ggplot2
#' @author Philipp J Roesch
#' @export

plot_freq <- function(object, at.step = NULL) {
  if (is.null(at.step)) at.step <- object$stepno + 1
  if ((at.step - 1) > object$stepno) stop("<at.step> cannot be larger than <object$stepno>")
  var_names <- c(object$names.smooth, object$names.linear)
  data <- data.frame(step = at.step, 
                    iter_sel = factor(object$selected[1:(at.step - 1)], 
                                     levels = 1:length(var_names), 
                                     labels = var_names))

  ggplot(data) + geom_bar(aes(iter_sel)) + 
    xlab("") + ylab("Frequency") +
    theme(axis.title =  element_text(size = rel(1.5)),   
          axis.text  =  element_text(size = rel(1.5)))
}

