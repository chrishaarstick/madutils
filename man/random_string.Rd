% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/random_string.R
\name{random_string}
\alias{random_string}
\title{Random String Generator}
\usage{
random_string(prefix = "")
}
\arguments{
\item{prefix}{optional string to append to the beginning of random string}
}
\value{
single string value
}
\description{
Creates a random string with optional prefix. Useful for unique identifier
strings
}
\examples{
random_string("rando")
}
