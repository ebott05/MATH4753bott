#' Notched Boxplot
#'
#' @param data a data table
#' @param var the variable in question
#' @param xlab x label, defaults to column name
#' @param main title of the plot
#' @param col purple
#'
#' @returns a notched boxplot
#' @export
#'
#' @examples
#' my_notchplot(spruce, BHDiameter)
my_notchplot <- function(data, var, xlab = NULL, main = NULL, col = "magenta") {
  var_name <- deparse(substitute(var))
  x <- data[[var_name]]
  if (!is.numeric(x)) stop("Selected column must be numeric.")

  if (is.null(xlab)) xlab <- var_name
  if (is.null(main)) main <- paste("Box and Whisker Plot of", var_name)

  boxplot(x,
          horizontal = TRUE,
          notch = TRUE,
          col = col,
          xlab = xlab,
          main = main)
}
