% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/specificity.R
\name{specificity}
\alias{specificity}
\title{Specificity}
\usage{
specificity(threshold, responses, predictions, na.rm = FALSE)
}
\arguments{
\item{threshold}{a numeric threshold.  Values of predictions greater than this threshold will be predicted to be positive}

\item{responses}{a vector of binary responses (TRUE/FALSE or 1/0) to be predicted}

\item{predictions}{a vector of numeric predicted values that can be converted to binary predictions using threshold}

\item{na.rm}{if TRUE, NA values will be removed before computing sensitivity}
}
\value{
the specificity when prediction responses using predictions at the given threshold
}
\description{
Measure specificity of a classifier
}
\details{
This function calculates specificity given predictions, responses, and a
prediction threshold.

specificity = TN / N

where TN is the number of True Negatives (correctly rejected negative
responses) and N is the total number of negative responses.
}
\examples{
# observed (binary) values
resp <- c(1, 0, 1, 1, 0, 1,
        0, 1, 1, 1, 0, 0)

# predicted values
pred <- c(0.75, 0.7, 0.63,
          0.7, 0.4, 0.52,
         0.6, 0.9, 0.3, 0.2,
         0.4, 0.3)

specificity(threshold = 0.5, responses = resp, predictions = pred)




}
