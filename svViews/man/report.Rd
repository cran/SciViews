\name{report}
\alias{report}
\alias{reportGraph}

\title{ Send a View to the report }
\description{
  Given a SciViews-compatible reporting application (like SciViews R Report or
  Microsoft Word) is parameterized as the default application to use, this function
  generates and append a View to the current report.
}
\usage{
    report(x, objname = deparse(substitute(x)), reptype = getOption("report"),
      application = getOption("reporter"), bookmark = "<End>", ...)
    reportGraph(device = dev.cur(), reptype = getOption("report"),
      application = getOption("reporter"), bookmark = "<End>", multiformat = FALSE,
      dir = file.path(tempdir(), "svGraph"), width = 480, height = 480,
      pointsize = 12, bg = "transparent", ...)
}

\arguments{
  \item{x}{ An object }
  \item{objname}{ A name for this object (its own name, by default) }
  \item{reptype}{ The type of report. By default, it is \code{"html"} format. An alternative
    is \code{"doc"} for a Microsoft Word report }
  \item{device}{ The device where the graph should be appended to the report }
  \item{application}{ The reporter application, in case of \code{reptype == "html"} }
  \item{bookmark}{ A bookmark to select in a Word document before pasting the data. If \code{"<End>"}
    (by default), the end of the document is selected (data is appended). If \code{NULL}, then
    a GoTo dialog box is first displayed in Word to select a location before pasting. If
    a string is given, it must match a valid bookmark in the active Word document, otherwise
    an error will be issued. }
  \item{multiformat}{ Do we use multiple formats (PNG + PDF, currently), or not (PNG only, default value) }
  \item{dir}{ The directory where to place resulting files }
  \item{width}{ The width of the generated image, in pixels }
  \item{height}{ The height of the generated image, in pixels }
  \item{pointsize}{ The pointsize (font size) to use }
  \item{bg}{ The color of the background (transparent, by default) }
  \item{\dots}{ Further argument to use to generate the View or the graph }
}

\details{
  \code{ReportGraph()} prepares graph outputs, so that they are suitable to be incorporated in a report.
    For an HTML report, it is a PNG image (plus a PDF one, if \code{multiformat = TRUE}).
    You can specify an option to tell which type of report is used by default:
    define \code{options(report = "html")} (used by default) or \code{options(report = "doc")}.
    For an \code{"html"} type, you need also to give the path to a compatible application
    like SciViews R Report. You can indicate it with \code{options(reporter = "path_to_the_app")}
}

\value{
  Returns invisibly \code{TRUE} for \code{report()} and the path to the created
  PNG/EMF file for \code{reportGraph}.
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
