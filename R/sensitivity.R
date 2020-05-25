#' Sensitivity
#'
#' Measure sensitivity of a classifier
#'
#' This function calculates sensitivity given predictions, responses, and a
#' prediction threshold.
#'
#' sensitivity = TP / P
#'
#' where TP is the number of True Positives (correctly identified positive
#' responses) and P is the total number of positive responses.
#'
#'
#' @export
#'
#' @examples
#' # observed (binary) values
#' resp <- c(1, 0, 1, 1, 0, 1,
#'         0, 1, 1, 1, 0, 0)
#'
#' # predicted values
#' pred <- c(0.75, 0.7, 0.63,
#'           0.7, 0.4, 0.52,
#'          0.6, 0.9, 0.3, 0.2,
#'          0.4, 0.3)
#'
#' sensitivity(threshold = 0.5, responses = resp, predictions = pred)
#'
#'
#'
#'
#' @param threshold a numeric threshold.  Values of predictions greater than this threshold will be predicted to be positive
#' @param responses a vector of binary responses (TRUE/FALSE or 1/0) to be predicted
#' @param predictions a vector of numeric predicted values that can be converted to binary predictions using threshold
#' @param na.rm if TRUE, NA values will be removed before computing sensitivity
#' @return the sensitivity when prediction responses using predictions at the given threshold

sensitivity <- function(threshold, responses, predictions, na.rm = FALSE) {
  # check format of inputs
  if(na.rm) x <- x[!is.na(x)]
  if(!is.numeric(predictions)) stop("predictions must be numeric")
  if(!is.logical(responses)) {
    responses <- as.logical(as.numeric(as.character(responses)))}

  P = sum(responses) # get number of positive cases
  # convert numeric predictions to binary values using threshold
  preds <- predictions > threshold
  # find out how many positive cases were predicted as positive
  TP <-length(which(preds == T & responses == T))
  sens <- TP/P # calculate sensitivity
  sens
}
