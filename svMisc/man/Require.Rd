\name{Require}
\alias{Require}
\title{A Require Package Function with a GUI (for Windows only)}
\description{
  A Require Package Function with a GUI (for Windows only).
}
\usage{
  Require(package, bundle = NULL, quietly = FALSE, warn.conflicts = TRUE,
      keep.source = getOption("keep.source.pkgs"),
      character.only = FALSE, version, save = TRUE,gui=getOption("guiRequire"))
}

\arguments{
  \item{package}{the name of a package, given as a \link{name} or
    literal character string, or a character string, depending on
    whether \code{character.only} is \code{FALSE} (default) or
    \code{TRUE}).}
  \item{bundle}{a character string specifying the name of the bundle where the
    required package can be found. This is useful to specify, otherwise, \code{Require()}
	will look for the package name in the repository, while only the bundle name can
	be found there!}
  \item{quietly}{a logical.  If \code{TRUE}, no message confirming
    package loading is printed.}
  \item{warn.conflicts}{logical.  If \code{TRUE}, warnings are printed about
          code{conflicts} from attaching the new package, unless that
          package contains an object \code{.conflicts.OK}.}
  \item{keep.source}{logical.  If \code{TRUE}, functions \dQuote{keep their
    source} including comments, see argument \code{keep.source} to
    \code{\link{options}}.}
  \item{character.only}{a logical indicating whether \code{package} or
    \code{help} can be assumed to be character strings.}
  \item{version}{A character string denoting a version number of the
     package to be loaded.  If no version is given, a suitable default
     is chosen.}
  \item{save}{logical or environment.  If \code{TRUE}, a call to
    \code{require} from the source for a package will save the name of
    the required package in the variable \code{".required"}, allowing
    function \code{\link{detach}} to warn if a required package is
    detached. See section \sQuote{Packages that require other packages} below.}
  \item{gui}{logical.  If \code{FALSE}, this function behaves just like the 
    \link{require} function.  If \code{TRUE}, then if a package fails to load,
    the user will be prompted with a message box and dialog asking whether they
    would like to install the package.   If the user chooses to install the package, 
    they will be asked which repository or local file they would like to install it from.}
}

\author{James Wettenhall (\email{wettenhall@wehi.edu.au}), adaptation by Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\examples{
  \dontrun{
  options(guiRequire = TRUE)
  # For packages which are already installed, Require works in the same way as require.
  Require(base)

  # For packages which are not installed, Require asks the user whether they would like to install the package.
  # Require(foo)
  }
}

\keyword{misc}
\keyword{utilities}
