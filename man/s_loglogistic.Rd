% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loglogistic_class.R
\name{s_loglogistic}
\alias{s_loglogistic}
\title{Factory of SURVIVAL objects with Log Logistic distributions}
\usage{
s_loglogistic(...)
}
\arguments{
\item{...}{Parameters to define the distribution. See the Parameters for details}
}
\value{
a SURVIVAL object of the log-logistic distribution family. See the
documentation of \code{s_factory} for the methods available for SURVIVAL objects
}
\description{
Creates a SURVIVAL object with a Log Logistic distribution.
}
\section{Parameters}{


To create an exponential survival object the following
options are available:

\emph{\code{scale}} and \emph{\code{shape}} to specify the canonical parameters of the distribution, or

\emph{\code{surv}}, \emph{\code{t}} and \emph{\code{shape}} for the proportion surviving (no events) at time t and the shape parameter, or

\emph{\code{fail}}, \emph{\code{t}} and \emph{\code{shape}} for the proportion failing (events) at time t and the shape parameter or

\emph{\code{intercept}} and \emph{\code{scale}} for the parameters returned by \code{survreg(.., dist = "loglogistic")} models.

The parameters should be spell correctly as partial matching is not available
}

\examples{
s_loglogistic(scale = 2,shape = 2)
s_loglogistic(surv = 0.6, t= 12, shape = 0.5)
s_loglogistic(fail = 0.4, t = 12, shape =0.5)
s_loglogistic(intercept = 0.4, scale = 0.5)
}
