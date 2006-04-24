\name{listCustoms}
\alias{listCustoms}

\title{ List all custom types for a method }
\description{
  List all custom types for a method.
}
\usage{
listCustoms(method, class)
}

\arguments{
  \item{method}{ The method name }
  \item{class}{ The class name }
}
\value{
  A vector with character strings with custom methods names.
}
\author{ Philippe Grosjean <phgrosjean@sciviews.org> }
\note{ This function is only useful for special generic functions with type argument like \code{view}, or \code{copy}. These functions offer a mechanism to easily add custom types, and the present function list them all. }

\seealso{ \code{\link{listMethods}} }
\examples{
	listCustoms("view", "data.frame") # None, except if you defined some of it!
}
\keyword{ utilities }
