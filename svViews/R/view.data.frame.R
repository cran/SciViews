"view.data.frame" <-
function(x, type = "summary", objname = deparse(substitute(x)), file = guiViewsFile(), CSSFile = guiViewsCSS(), command = "", browse = TRUE, ...) {

	"viewSummary" <- function(x, objname, type, file, CSSFile, command, usefactorcol = (sum(sapply(x, is.factor)) > 0), whichfactor = 1, ...) {
        # Done in NAMESPACE
        #require(svMisc)
        #Require(R2HTML)
		viewHTMLinit(objname = objname, type = type, file = file, CSSFile = CSSFile, command = command)
		HTML(summary(x), file = file, append = TRUE)
		nmissing <- sum(is.na(x))
		pmissing <- round(100 * nmissing / (nrow(x) * ncol(x)))
		if (nmissing > 0) cat(paste("<p><font color=darkred>Missing values in data.frame: ", nmissing, " (", pmissing, "%)</font></p><br>", sep = ""), file = file, append = TRUE)
		nam <- names(x)
		selnum <- sapply(x, is.numeric)
		nnum <- sum(selnum)
		selfac <- sapply(x, is.factor)
		nfac <- sum(selfac)
		if (nfac < 1) usefactorcol <- FALSE
		if (nnum > 0) {
			# PhG: restrict to first three variables
			if (nnum > 3) nnum <- 3
			datanum <- x[, selnum, drop = FALSE]
			for (i in 1:nnum) {
				nam  <- names(datanum)
				varx <- datanum[[i]]
				cat("<center><table border=0 cellpadding=0 cellspacing=0 width=700><td align=right valign=top width=200>", file = file, append = TRUE)
				cat("<p align=right><font size=+1><b>", nam[i], "</b></font><br>", file = file, append = TRUE)
				cat("<font size=+1>Numeric</font><br>", file = file, append = TRUE)
				cat("Range:", paste(range(varx,na.rm = TRUE), collapse = "-"), "<br><br>", file = file, append = TRUE)
				nmissing <- sum(is.na(varx))
				pmissing <- round(100 * nmissing / (nrow(varx) * ncol(varx)))
                if (nmissing > 0) cat(paste("Missing values:<br><font size=+1> ", nmissing, " (", pmissing, "%)</font><br>", sep = ""), file = file, append = TRUE)
				cat("</td><td width=10>&nbsp;</td><td valign=top width=500>", file = file, append = TRUE)
				tmpfile <- paste(tempfile(), "png", sep = ".")
				png(tmpfile, width = 500, height = 400)
				oma.saved <- par("oma")
				par(oma = rep.int(0, 4))
				par(oma = oma.saved)
				o.par <- par(mar = rep.int(0, 4))
				on.exit(par(o.par))
				myl <- layout(matrix(c(1, 1, 2, 3, 4, 5, 5, 5), ncol = 2), height = c(200, 100, 60, 40) / 500, width = c(300, 200) / 500)
				histx <- hist(varx[!is.na(varx)], plot = FALSE)	 	
				rx <- range(histx$breaks)
				rx <- c(rx[1] - diff(rx) / 2, rx[2] + diff(rx) / 2)
				seqHy <- histx$density
				seqNx <- seq(rx[1], rx[2], length = 1000)
				seqNy <- dnorm(seqNx, mean(varx, na.rm = TRUE), sqrt(var(varx, na.rm = TRUE)))
				densx <- density(varx[!is.na(varx)], from = rx[1], to = rx[2])
				seqDx <- densx$x
				seqDy <- densx$y
				ymin <- 0
				ymax <- max(seqDy, seqHy, seqNy, na.rm = TRUE)
				xmin <- min(seqNx, seqDx, na.rm = TRUE)
				xmax <- max(seqNx, seqDx, na.rm = TRUE)
				hist(varx, xlim = c(xmin, xmax), ylim = c(ymin, ymax), prob = TRUE, main = "", col = "#FFF1D2")
				lines(seqDx, seqDy, col = "darkblue", lwd = 2)
				lines(seqNx, seqNy, col = "darkred", lwd = 2)
				box()	
				plot(c(0, 1), c(0.2, 1), xlim = c(xmin, xmax), ylim = c(0.2, 1), axes = FALSE, main = "", type = "n")
				boxplot(varx[!is.na(varx)], horizontal = TRUE, axes = FALSE, add = TRUE, at = 0.5, col = "#C8EBFF")
				box()
				plot(c(0, 1), c(0, 2), xlim = c(xmin, xmax), ylim = c(0, 2), axes = FALSE, main = "", type = "n")
				stripchart(varx[!is.na(varx)], "jitter", add = TRUE)
				box()	
				qqnorm(varx[!is.na(varx)], main = "", axes = FALSE)
				qqline(varx[!is.na(varx)], col = "darkred")
				box()
				plot(1:length(varx), varx, main = "", axes = FALSE, type = "n")
				xmean <- mean(varx, na.rm = TRUE)
				if (length(varx) > 1) {
					sdx <- sqrt(var(varx, na.rm = TRUE))
					polygon(x = c(1, length(varx), length(varx), 1), y = c(xmean - 1.96 * sdx, xmean - 1.96 * sdx, xmean + 1.96 * sdx, xmean + 1.96 * sdx), density = -1, col = "#FFF5CC", border = NA)
					polygon(x = c(1, length(varx), length(varx), 1), y = c(xmean - 0.67 * sdx, xmean - 0.67 * sdx, xmean + 0.67 * sdx, xmean + 0.67 * sdx), density = -1, col = "#FFFEE8", border = NA)
				}
				abline(h = xmean, col = "darkblue")
				if (usefactorcol) {
					points(1:length(varx), varx, lty = 1, type = "b", pch = 3, col = c("black", rainbow(nlevels(x[, selfac, drop = FALSE][ , whichfactor]))[as.numeric(x[ , selfac, drop = FALSE][ , whichfactor])][-1]))
				} else {
					points(1:length(varx), varx, lty = 1, type = "b", pch = 3)
				}
				box()
				dev.off()
				cat("<img src=", tmpfile, " border=0 width=500 height=400>", file = file, append = TRUE)
				if (usefactorcol) cat("<br><i>Scatterplot is colorised according to factor:</i>", dimnames(x[ , selfac, drop = FALSE][ , whichfactor, drop = FALSE])[[2]][1], file = file, append = TRUE)
				cat("</td></table></center><br>", file = file, append = TRUE)
			}
		}
		if (nfac > 0) {
			# PhG: restrict to first two factors
			if (nfac > 2) nfac <- 2
			datafac <- x[ , selfac, drop = FALSE]
			nam <- names(datafac)
			for (i in 1:nfac) {
				ff <- datafac[[i]]
				nmissing <- sum(is.na(ff))
				pmissing <- round(100 * nmissing / (nrow(ff) * ncol(ff)))
                datanumff <- datanum[!is.na(ff), ]
				ff <- ff[!is.na(ff)]
				cat("<center><table border=0 cellpadding=0 cellspacing=0 width=700><td align=right valign=top width=200>", file = file, append = TRUE)
				cat("<p align=right><font size=+1><b>", nam[i], "</b></font><br>", file = file, append = TRUE)
				cat("<font size=+1>Factor</font><br>", file = file, append = TRUE)
				cat("# levels", nlevels(ff), "<br>", file = file, append = TRUE)
				if (nmissing > 0) cat(paste("<br>Missing values:<br><font size=+1> ", nmissing, " (", pmissing,"%)</font><br>", sep = ""), file = file, append = TRUE)
				cat("</td><td width=10>&nbsp;</td><td valign=top width=500>", file = file, append = TRUE)
				tmpfile <- paste(tempfile(), "png", sep = ".")
				if (nnum > 0) {
					png(tmpfile, width = 500, height = 400)
					oma.saved <- par("oma")
					par(oma = rep.int(0, 4))
					par(oma = oma.saved)
					o.par <- par(mar = rep.int(0, 4))
					on.exit(par(o.par))
					mylayout <- layout(matrix(c(1, 3, 1, 4, 2, 4), ncol = 2, byrow = TRUE), width = c(300, 200) / 500, height=c(3, 1, 2) / 6)
                    # Done in NAMESPACE
                    #require(svMisc)
                    #Require(MASS)
					if (nnum > 2) {
						tmplda <- lda(ff ~ ., data = datanumff)
						# Disciminant analysis result
						bestlda <- tmplda$scaling[ , 1]
						bestlda <- rev(sort(abs(bestlda)))
						v1 <- names(bestlda)[1]
						v2 <- names(bestlda)[2]
					} else {
						v1 <- names(datanumff)[1]
						if (nnum == 2) v2 <- names(datanumff)[2] else v2 <- names(datanumff)[1]
					}
					plot(datanumff[[v1]], datanumff[[v2]], col = rainbow(nlevels(ff))[as.numeric(ff)])
					text(x = max(datanumff[[v1]], na.rm = TRUE), y = min(datanumff[[v2]], na.rm = TRUE), labels = v1, pos = 2)
					text(x = min(datanumff[[v1]], na.rm = TRUE), y = max(datanumff[[v2]], na.rm = TRUE), labels = v2, pos = 2, srt = 90)
					box()
					plot(c(0, nlevels(ff) + 1), range(datanumff[[v1]]), type = "n")
					for (i in 1:nlevels(ff)) boxplot(datanumff[[v1]][ff == levels(ff)[i]], at = i, add = TRUE, col = rainbow(nlevels(ff))[i])		
					tab <- table(ff)
					pie(tab, col = rainbow(nlevels(ff)))
					box()
					dotchart(tab, col = rainbow(nlevels(ff)), lcol = rainbow(nlevels(ff)), lwd = 2, lty = 1)
					axis(side = 3)
					box()	
				} else {
					png(tmpfile, width = 500, height = 400)
					mylayout <- layout(matrix(1:2, ncol = 2))
					pie(ff)
					barplot(ff)
				}
				dev.off()
				cat("<img src=", tmpfile, " border=0 width=500 height=400>", file = file, append = TRUE)
				cat("</td></table></center><br>", file = file, append = TRUE)
			}
		}		
		return(file)
	}
	
	"viewPrint" <- function(x, objname, type, file, CSSFile, command, ...) {
        # Done in NAMESPACE
        #require(svMisc)
        #Require(R2HTML)
		viewHTMLinit(objname = objname, type = type, file = file, CSSFile = CSSFile, command = command)
		objdim <- dim(x)
		# For data.frame / matrices, if object is too big, subset it according to option view.maxsize
		maxsize <- getOption("view.maxsize")
		if (is.null(maxsize) || !is.numeric(maxsize) || length(maxsize) < 2)	# Wrong value for view.maxsize => use default one
			maxsize <- c(50, 15)
		maxsize[maxsize < 1] <- 1
		maxsize <- as.integer(maxsize) # make sure it is postive integers
		if (any(objdim > maxsize[1:2])) {
			HTML(x[1:min(maxsize[1], objdim[1]), 1:min(maxsize[2], objdim[2])], file = file)
			HTML(paste("<i>Some rows/columns have been removed from preview / Object dimensions:", paste(objdim, collapse = "x"), "</i>", sep = ""), file = file)
		} else {
			HTML(x, file = file)
		}
		return(file)
	}
	
	"viewTwoVars" <- function(x, objname, type, file, CSSFile, command, usefactorcol = (sum(sapply(x, is.factor)) > 0), whichfactor = 1, ...) {
		"ellipse" <- function(x, scale = c(1., 1.), centre = c(0., 0.), level = 0.95, t = sqrt(qchisq(level, 2.)), which = c(1., 2.), npoints = 100.) {
			names <- c("x", "y")
			if (is.matrix(x)) {
				xind <- which[1.]
				yind <- which[2.]
				scale <- sqrt(c(x[xind, xind], x[yind, yind]))
				r <- x[xind, yind]/scale[1.]/scale[2.]
				if (!is.null(dimnames(x)[[1.]]))
					names <- dimnames(x)[[1.]][c(xind,yind)]
			} else r <- x
			d <- acos(r)
			a <- seq(0., 2. * pi, len = npoints)
			matrix(c(t * scale[1.] * cos(a + d/2.) + centre[1.], t * scale[2.] * cos(a - d/2.) + centre[2.]), npoints, 2., dimnames = list(NULL, names))
		}				
		
		viewHTMLinit(objname = objname, type = type, file = file, CSSFile = CSSFile, command = command, view.title = FALSE)
		selnum <- sapply(x, is.numeric)
		nnum <- sum(selnum)
		selfac <- sapply(x, is.factor)
		nfac <- sum(selfac)
		datagraph <- x[ , (selnum|selfac), drop = FALSE]
		ngraph <- dim(datagraph)[2]
		if (ngraph > 1) {
			# PhG: restrict to three first graphs
			if (ngraph > 3) ngraph <- 3
			vnames <- dimnames(datagraph)[[2]]
			tmpfile <- paste(tempfile(), "png", sep = ".")
			png(tmpfile, width = 120 * ngraph, height = 120 * ngraph)
			oma.saved <- par("oma")
			par(oma = rep.int(0, 4))
			par(oma = oma.saved)
			o.par <- par(mar = rep.int(0, 4))
			on.exit(par(o.par))
			mylayout <- layout(matrix(1:(ngraph^2), ncol = ngraph, byrow = TRUE))
			for (i in 1:ngraph) {	
				varX <- datagraph[ , i]
				for (j in 1:ngraph) {
					if (i==j) if (is.numeric(varX)) {
						histx <- hist(varX[!is.na(varX)], plot = FALSE)	 	
						rx <- range(histx$breaks)
						rx <- c(rx[1] - diff(rx) / 2, rx[2] + diff(rx) / 2)
						seqHy <- histx$density
						seqNx <- seq(rx[1], rx[2], length = 1000)
						seqNy <- dnorm(seqNx, mean(varX, na.rm = TRUE), sqrt(var(varX, na.rm = TRUE)))
						densx <- density(varX[!is.na(varX)], from = rx[1], to = rx[2])
						seqDx <- densx$x
						seqDy <- densx$y
						ymin <-0
						ymax <- max(seqDy, seqHy, seqNy, na.rm = TRUE)
						xmin <- min(seqNx, seqDx, na.rm = TRUE)
						xmax <- max(seqNx, seqDx, na.rm = TRUE)
						hist(varX, xlim = c(xmin, xmax), ylim = c(ymin, ymax), prob = TRUE, main = "", col = "#FFF1D2", axes = FALSE)
						lines(seqDx, seqDy, col = "darkblue", lwd = 2)
						lines(seqNx, seqNy, col = "darkred", lwd = 2)
						text(xmax, ymax, pos = 2, labels = vnames[i])
					} else pie(table(varX), col = rainbow(length(table(varX))))
					else {
						varY <- datagraph[ , j]
						if (is.numeric(varX)) {
							if (is.numeric(varY)) {
								if (j > i) {
									varXX <-varY
									varY <- varX
									varX <- varXX
								}
								coeff.cor <- cor(varX, varY, use = "pairwise.complete.obs")
								varX <- (varX - mean(varX, na.rm = TRUE)) / sqrt(var(varX, na.rm = TRUE))
								varY <- (varY - mean(varY, na.rm = TRUE)) / sqrt(var(varY, na.rm = TRUE))
								ell <- ellipse(coeff.cor, scale = c(0.9 * sqrt(var(varX)), .9 * sqrt(var(varY))))
								if (usefactorcol) plot(varX, varY, main = "", axes = FALSE, col = rainbow(nlevels(x[ , selfac, drop = FALSE][ , whichfactor]))[as.numeric(x[ , selfac, drop = FALSE][ , whichfactor])]) else plot(varX, varY, main = "", axes = FALSE)
								points(ell, col = "darkblue", type = "l")
								abline(lm(varY ~ varX), col = "darkred")
							} else{ 
								boxplot(split(varX, varY), notch = TRUE, varwidth = TRUE, col = rainbow(nlevels(varY)), axes = FALSE)
							}
						} else{
							if (is.numeric(varY))
								boxplot(split(varY, varX), notch = TRUE, varwidth = TRUE, horizontal = TRUE, col = rainbow(nlevels(varX)), axes = FALSE)
							else mosaicplot(table(varX, varY), main = "", shade = TRUE)
						}
		
					}					
					box()		
				}
		
			}
			dev.off()
			cat("</center><center><img src=", tmpfile, " border=0 width=", 120 * ngraph, " height=", 120 * ngraph, ">", file = file, append = TRUE)
			if (usefactorcol) cat("<br><i>Scatterplot are colorised according to factor:</i>", dimnames(x[ , selfac, drop = FALSE][ , whichfactor, drop = FALSE])[[2]][1], file = file, append = TRUE)
			cat("</center>", file = file, append = TRUE)
		}
		if (nnum > 0) {
			cat("<p><font size=+2>Numeric variables</font></p>\n", file = file, append = TRUE)
			if (nnum > 1) {
				datanum <- x[, selnum]
				cormat <- cor(datanum, use = "pairwise.complete.obs")
				HTML.cormat(cormat, file = file, append = TRUE)
			} else cat(paste("Only one numeric variable: ", dimnames(x[ , selnum, drop = FALSE])[[2]][1]), file = file, append = TRUE)
		}	
		return(file)
	}

	"viewMissing" <- function(x, objname, type, file, CSSFile, command, ...) {
        # Done in NAMESPACE
        #require(svMisc)
        #Require(R2HTML)
		viewHTMLinit(objname = objname, type = type, file = file, CSSFile = CSSFile, command = command)
		nmissing <- sum(is.na(x))
		pmissing <- round(100 * nmissing / (nrow(x) * ncol(x)))
		HTML(paste("Total number of missing values: ", nmissing, " (", pmissing, "%)", sep = ""), file = file)
		if (nmissing > 0) {
			# Some missing values / See in details
			names <- dimnames(x)[[2]]
			for (i in 1:dim(x)[2]) {
				HTML(paste("<li><b>", names[i], "</b><br>", sep = ""), file = file)
				xi <- x[, i]
				nmissing <- sum(is.na(xi))
				pmissing <- round(100 * nmissing / length(xi))
				HTML(paste("&nbsp;&nbsp;&nbsp;Missing values: ", nmissing, " (", pmissing, "%)", sep = ""), file = file)
			}
		}
		return(file)
	}

	"viewSAScode" <- function(xexp, objname, type, file, CSSFile, command, ...){
        # Done in NAMESPACE
        #require(svMisc)
        #Require(R2HTML)
		viewHTMLinit(objname = objname, type = type, file = file, CSSFile = CSSFile, command = command)
		HTMLhr(file = file)
		cat("<xmp class=sascode>", file = file, append = TRUE)
		txt <- export(x = xexp, type = "sascode", file = file, append = TRUE, objname = objname, saslib = "work", sasmember = "", writeit = FALSE)
		cat(paste(txt, collapse = "\n"), file = file, append = TRUE)
		cat("</xmp>", file = file, append = TRUE)
		return(file)
	}

	objname <- objname
    # Done in NAMESPACE
    #require(svMisc)
    typelist <- unique(c("summary", "print", "twovars", "missing", "sascode", listCustoms("view", "data.frame")))
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
	custom <- paste("view.data.frame..", type, sep = "")
	if (exists(custom, mode = "function")) {
		res <- get(custom, mode = "function")(expr = xexp, objname = objname, file = file, CSSFile = CSSFile, command = command, browse = browse, ...)
	} else {	
		# Process the command in the standard function 
		x <- eval(xexp, envir = .GlobalEnv)
		res <- switch(type,
			"typelist"= typelist,
			"summary" = viewSummary(x, objname, type, file, CSSFile, command, ...),
			"twovars" = viewTwoVars(x, objname, type, file, CSSFile, command, ...),
			"missing" = viewMissing(x, objname, type, file, CSSFile, command, ...),
			"print"   = viewPrint(x, objname, type, file, CSSFile, command, ...),
			"sascode" = viewSAScode(xexp, objname, type, file, CSSFile, command, ...),
			view.default(x = xexp, type = type, file = file, CSSFile = CSSFile, objname = objname, command = command, browse = browse, ...))
	}
	# Add browsing here
	if (browse == TRUE && type != "typelist") guiViewsDisplay(res)
	if (type == "typelist" || browse == FALSE) return(res) else invisible(res)
}
