"view.ts" <-
function(x, type = "summary", objname = deparse(substitute(x)), file = guiViewsFile(), CSSFile = guiViewsCSS(), command = "", browse = TRUE, ...) {

	"viewSummary" <- function(x, objname, type, file, CSSFile, command, usefactorcol = (sum(sapply(x, is.factor)) > 0), colorspan = 2, ...) {
        #Done in NAMESPACE
        #require(svMisc)
        #Require(R2HTML)
		viewHTMLinit(objname = objname, type = type, file = file, CSSFile = CSSFile, command = command)
		HTML(summary(x), file = file, append = TRUE)
		cat("<center><table border=0 cellpadding=0 cellspacing=0 width=700><td align=right valign=top width=200>", file = file, append = TRUE)
		cat("<p align=right><font size=+1><b>", objname,"</b></font><br>",file = file, append = TRUE)
		cat("<font size=+1>Time Serie</font><br>", file = file, append = TRUE)
		cat("Start:", deparse(tsp(x)[1]), file = file, append = TRUE)
		cat("<br>End:", deparse(tsp(x)[2]), file = file, append = TRUE)
		cat("<br>Frequency:", deparse(tsp(x)[3]), file = file, append = TRUE)
		cat("</td><td width=10>&nbsp;</td><td valign=top width=500>", file = file, append = TRUE)
		tmpfile <- paste(tempfile(), "png", sep = ".")
		png(tmpfile, width = 500, height = 400)
		oma.saved <- par("oma")
		par(oma = rep.int(0, 4))
		par(oma = oma.saved)
		o.par <- par(mar = rep.int(0, 4))
		on.exit(par(o.par))
		mytitle <- function(txt) {
			x <- mean(range(par('usr')[1:2]))
			y <- par('usr')[4] - 0.05 * (par('usr')[4] - par('usr')[3])
			text(x, y, txt, pos = 1)
		}
		myl <- layout(matrix(c(1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 7), ncol = 2),
		height = c(6, 4, 2, 2, 4, 6)/24, width = c(3, 2)/5)
		plot(x, axes = FALSE, type = "n")	
		a <- time(x)
		i1 <- floor(min(a))
		i2 <- ceiling(max(a))
		y1 <- par('usr')[3]
		y2 <- par('usr')[4]
		for (i in seq(from = i1, to = i2 - 1, by = 2 * colorspan)) {
		    polygon( c(i, i + colorspan, i + colorspan, i), c(y1, y1, y2, y2), col = '#FFF5CC', border = NA)
		}
		axis(side = 1)
		k <- round(quantile(1:NROW(x), c(0.1, 0.25, 0.5, 0.7)))
		col <- rainbow(length(k))
		for (i in 1:length(k)) {
		  	z <- filter(x, rep(1, k[i]) / k[i])
		  	lines(z, col = col[i])
		}
		lines(x, lwd = 2)
		abline(lm(x ~ time(x)), col = 'red', lty = 2)
		mytitle(objname)
		axis(side = 4)
		box()
		acf(x, axes = FALSE)
		mytitle("ACF")
		box()
		pacf(x, axes = FALSE, main = "pACF")
		mytitle("pACF")
		box()
		spectrum(x, axes = FALSE, ...)
		box()
		xNA = x[!is.na(x)]
		histx <- hist(xNA, plot = FALSE)	 	
		rx <- range(histx$breaks)
		rx <- c(rx[1] - diff(rx) / 2, rx[2] + diff(rx) / 2)
		seqHy <- histx$density
		seqNx <- seq(rx[1], rx[2], length = 1000)
		seqNy <- dnorm(seqNx, mean(x, na.rm = TRUE), sqrt(var(x, na.rm = TRUE)))
		densx <- density(xNA, from = rx[1], to = rx[2])
		seqDx <- densx$x
		seqDy <- densx$y
		ymin <- 0
		ymax <- max(seqDy, seqHy, seqNy, na.rm = TRUE)
		xmin <- min(seqNx, seqDx, na.rm = TRUE)
		xmax <- max(seqNx, seqDx, na.rm = TRUE)
		hist(x, xlim = c(xmin, xmax), ylim = c(ymin, ymax), prob = TRUE, main = "", col = "#FFF1D2", axes = FALSE)
		axis(side = 1)
		lines(seqDx, seqDy, col = "darkblue", lwd = 2)
		lines(seqNx, seqNy, col = "darkred", lwd = 2)
		box()	
		plot(c(0, 1), c(0.2, 1), xlim = c(xmin, xmax), ylim = c(0.2, 1), axes = FALSE, main = "", type = "n")
		boxplot(xNA, horizontal = TRUE, axes = FALSE, add = TRUE, at = 0.5, col = "#C8EBFF")
		box()
		qqnorm(xNA, main = "", axes = FALSE)
		qqline(xNA,col = "darkred")
		box()
		dev.off()
		cat("<img src=", tmpfile, " border=0 width=500 height=400>", file = file, append = TRUE)
		if (usefactorcol) cat("<br><i>Scatterplot is colorised according to factor:</i>", dimnames(x[ , selfac, drop = FALSE][ , whichfactor, drop = FALSE])[[2]][1], file = file, append = TRUE)
		cat("</td></table></center><br>", file = file, append = TRUE)
		return(file)
	}


	"viewPrint" <- function(x, objname, type, file, CSSFile, command, ...) {
        # Done in NAMESPACE
        #require(svMisc)
        #Require(R2HTML)
		viewHTMLinit(objname = objname, type = type, file = file, CSSFile = CSSFile, command = command)
		HTML(x, file = file)
		return(file)
	}

	objname <- objname
    # Done in NAMESPACE
    #require(svMisc)
	typelist <- unique(c("summary", "print", listCustoms("view", "ts")))
	if (command == "") command <- guiViewsCmd(type = type, typelist = typelist, command = match.call(), file = file)
	# Compute the expression
	xexp <- try(if (inherits(x, "expression")) x else NULL, silent = TRUE)
	if (inherits(xexp, "try-error") || is.null(xexp)) {
		xexp <- substitute(x)
		if (is.character(xexp)) # To make sure that non conventional names will be correctly evaluated, we use backticks!
			xexp <- parse(text = paste("`", xexp, "`", sep = ""))
		xexp <- as.expression(xexp)
	}
	# Do we have to pass this to a custom function as 'view.data.frame..type()'?
	custom <- paste("view.ts..", type, sep = "")
	if (exists(custom, mode = "function")) {
		res <- get(custom, mode = "function")(expr = xexp, envir = envir, objname = objname, file = file, CSSFile = CSSFile, command = command, browse = browse, ...)
	} else {	
		# Process the command in the standard function 
		x <- eval(xexp, envir = .GlobalEnv)
		res <- switch(type,
			"typelist"= typelist,
			"summary" = viewSummary(x, objname, type, file, CSSFile, command, ...),
			"print"   = viewPrint(x, objname, type, file, CSSFile, command, ...),
			view.default(x = xexp, type = type, file = file, CSSFile = CSSFile, objname = objname, command = command, browse = browse, ...))
	}
	# Add browsing here
	if (browse == TRUE && type != "typelist") guiViewsDisplay(res)
	if (type == "typelist" || browse == FALSE) return(res) else invisible(res)
}
