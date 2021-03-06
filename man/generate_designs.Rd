% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_designs.R
\name{generate_designs}
\alias{generate_designs}
\title{Generate obfuscation designs}
\usage{
generate_designs(design_opt_input = list())
}
\arguments{
\item{design_opt_input}{A list of user supplied design options.}
}
\value{
A list of matrices with rules and actions
}
\description{
The function takes the list of design options \code{design_opt_input} and 
generates one or more obfuscation designs subject to the specified 
restrictions. A full specification of all the options available can be found
in the manual along with detailed examples of different designs. At a minimum
the user must supply the number of rules and actions, i.e. the dimensions of
the design problem.
}
\examples{
design_opt_input <- list(rules = 4,
                         actions = 5)

generate_designs(design_opt_input)

}
