% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Example-class.R
\docType{class}
\name{Example-class}
\alias{Example-class}
\alias{.Example}
\alias{Example}
\title{A class to represent S4 examples}
\usage{
Example(x = list())
}
\arguments{
\item{x}{Either a 'list' or a 'SimpleList' object (defaults to an empty
\code{list})}
}
\description{
This class is a simple example of an S4 class.
It inherits from the \link[S4Vectors:SimpleList-class]{SimpleList} class.
}
\examples{
showClass("Example")

library(S4Vectors)

Example()
Example(list(a = 1, b = 2))
Example(SimpleList(a = 1, b = 2))
}
