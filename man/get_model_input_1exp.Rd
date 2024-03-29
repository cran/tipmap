% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_model_input_1exp.R
\name{get_model_input_1exp}
\alias{get_model_input_1exp}
\title{Transform cumulative probabilities to fit beta distributions}
\usage{
get_model_input_1exp(cum_probs, w = NULL)
}
\arguments{
\item{cum_probs}{Numeric vector, containing cumulative probabilities of weights for one expert, as elicited through the roulette method. Each element of the vector represents one bin in the grid.}

\item{w}{Numeric vector, upper interval limit of bin (defaults to \code{1:length(cum_probs) / length(cum_probs)}).}
}
\value{
Dataframe to be used as input to fit beta distributions by \code{\link{fit_beta_1exp}}.
}
\description{
Transform cumulative probabilities to fit beta distributions
}
\examples{
chips <- c(0, 2, 3, 2, 1, 1, 1, 0, 0, 0)
x <- get_cum_probs_1exp(chips)
print(x)
y <- get_model_input_1exp(x)
print(y)

}
\seealso{
\code{\link{get_cum_probs_1exp}} and \code{\link{fit_beta_1exp}}.
}
