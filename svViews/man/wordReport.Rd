\name{wordReport}
\alias{WordOpen}
\alias{WordActivate}
\alias{WordExit}
\alias{WordGoto}
\alias{WordGotoEnd}
\alias{WordInsertPara}
\alias{WordInsertPageBreak}
\alias{WordInsertText}
\alias{WordInsertFile}
\alias{WordInsertPictureFile}
\alias{WordExec}

\title{ Manage a report in Microsoft Word from within R }
\description{
  These various functions allow to control Microsoft Word (currently, only through DDE under Windows) to append results to a document from within R.
}
\usage{
    WordOpen(report = NULL, warn = TRUE)
	WordActivate(async = FALSE)
	WordExit()
	WordGoto(bookmark = NULL)
	WordGotoEnd()
	WordInsertPara()
	WordInsertPageBreak()
	WordInsertText(text)
	WordInsertFile(file, keep.bookmark = FALSE)
	WordInsertPictureFile(file, keep.bookmark = FALSE)
	WordExec(command, type = "DDE", async = FALSE)
}

\arguments{
  \item{report}{ The path to a Word .doc file to use as a starting basis for the report. If \code{report == NULL} (by default), Word is just opened with a new empty document, if it was not already running. }
  \item{warn}{ Should the function issue a warning if it cannot find or start Microsoft Word? }
  \item{async}{ Would the command run asynchroneously (returns immediatelly, before the command is fully processed by Word), on not (by default). }
  \item{bookmark}{ A bookmark in the current Word document to select. If \code{bookmark = NULL} (by default), the GoTo dialog box of Word is opened, and you have an opportunity to navigate to a given place in your Word document before continuing (to indicate where to place the next piece of report in your file). If you specify \code{bookmark = ""}, the insertion point is moved to the beginning of the current document. }
  \item{text}{ A text to insert in the Word document. Note that you can use \\n or \\v for forced line breaks insider a paragraph, \\r for going to the next paragraph, \\f for inserting a page break and \\t for a tabulation. }
  \item{file}{ The path to a file containing data or an image to insert in the current Word document. All formats supported by Microsoft Word are accepted and automatically converted (e.g., HTML, RTF, ... and for images, BMP, TIFF, GIF, WMF, EMF, ...) }
  \item{keep.bookmark}{ A flag to indicate that we want to keep the currently selected bookmark. When using \code{WordInsertFile()} or \code{WordInsertPictureFile()} with this option set to \code{FALSE}, any selected bookmark is deleted. With this option set to \code{TRUE}, the bookmark is preserved, but note that it requires a non-empty selection in the active Word document! }
  \item{command}{ A command to execute in Word }
  \item{type}{ The type of communication to use with Word. Currently, only DDE is valid. }
}

\details{
  These are various functions to control Microsoft Word: open it, open or activate a given document, insert text, pictures, or other items, and select a position in the document. All functions return invisibly \code{TRUE} in case of success and \code{FALSE} otherwise. Note that \code{WordGoto(\"my bookmark\")} will fail if there is no bookmark called \"my bookmark\" in the active document... and R will not be advised of the error, that is, the function still returns \code{TRUE} in this case! This is because there is no feedback mechanism from Word to R implemented yet! \code{WordExec()} is the low-level function that allows to pass directly WordBasic commands to Word. It is not normally used directly, but it can be useful for commands not included in the other functions. Look at the WordBasic help and the code of the other functions to learn how you should enter these commands...
}

\value{
  Returns invisibly \code{TRUE} for \code{report()} and the path to the created PNG file for \code{reportGraph()}.
}

\author{ Philippe Grosjean <phgrosjean@sciviews.org> }

\seealso{ \code{\link{report}} }
\examples{
    \dontrun{
        WordOpen()	# Open Miscrosoft Word with a new empty document (or activate current doc)
		WordGotoEnd() # Move insertion point to the end of that document
		# Insert some text in it
		WordInsertText("Some \ttext...\nOther text after line feed in same para\rOther para\fOther page")
		# Insert a new paragraph
		WordInsertPara()

		# Open an existing Word document (it must exist there, of course!)
		WordOpen("c:/temp/test.doc")
		WordGoto() # Display the GoTo dialog box in Word, and allow to navigate to a given place or bookmark manually
		WordInsertPictureFile("c:/temp/test.emf")	# Insert a picture there (he file must exist, of course!)
		WordGoto("my bookmark") # Go to the "my bookmark" bookmark (it must exist, otherwise an error is issued in Word!)
		WordInsertFile("c:/temp/test.html", "testit") # Insert a piece of HTML there (the file must exist, of course!)
		WordExit()	# Close word, possibly prompting to save all unsaved documents before.
    }
}
\keyword{print}
\keyword{interface}
