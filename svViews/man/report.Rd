\name{report}
\alias{report}
\alias{reportGraph}

\title{ Send a View to the report }
\description{
  Given a SciViews-compatible reporting application (like SciViews R report) is parameterized as the default application to use, this function generates and append a View to its current report.
}
\usage{
    report(x, objname = deparse(substitute(x)), reptype = "html", application = getOption("reporter"), ...)
    reportGraph(device = dev.cur(), application = getOption("reporter"), multiformat = FALSE, dir = file.path(tempdir(), "svGraph"), width = 480, height = 480, pointsize = 12, bg = "transparent", ...)
}

\arguments{
  \item{x}{ An object }
  \item{objname}{ A name for this object (its own name, by default) }
  \item{reptype}{ The type of report. By default, it is \code{"html"} format }
  \item{device}{ The device where the graph should be appended to the report }
  \item{application}{ The reporter application }
  \item{multiformat}{ Do we use multiple formats (PNG + PDF, currently), or not (PNG only, default value) }
  \item{dir}{ The directory where to place resulting files }
  \item{width}{ The width of the generated image, in pixels }
  \item{height}{ The height of the generated image, in pixels }
  \item{pointsize}{ The pointsize (font size) to use }
  \item{bg}{ The color of the background (transparent, by default) }
  \item{\dots}{ Further argument to use to generate the View or the graph }
}

\details{
  \code{ReportGraph()} prepares graph outputs, so that they are suitable to be incorporated in a report. For an HTML report, it is a PNG image (plus a PDF one, if \code{multiformat = TRUE}).
}

\value{
  Returns invisibly \code{TRUE} for \code{report()} and the path to the created PNG file for \code{reportGraph}.
}

\author{ Eric Lecoutre & Philippe Grosjean}

\seealso{ \code{\link{view}} }
\examples{
    \dontrun{
        data(iris)
        report(iris)
    }
}
\keyword{print}
\keyword{interface}
