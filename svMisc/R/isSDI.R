"isSDI" <-
function() {
	# This function is specific to Windows, but it is defined everywhere
	# so that we don't have to test the platform before use!
	# Check if Rgui was started in SDI mode (needed by some GUI clients)
	# TO DO: use new features of R 2.0.0dev!
	# I found nothing indicating this in R, so, I have to look myself
	# in the right Rconsole file
	# rem: if this file is modified after start => I would got wrong value!

	# 1) First is it Rgui?
	if (!.Platform$GUI[1] == "Rgui")
        return(FALSE)    # This is not Rgui

    # The code is much simpler, starting form R 2.0.0
    if (compareRVersion("2.0") == 1) { # R >= 2.0.0
        # RGui SDI mode: returns "R Console", in MDI mode: returns "RGui"
        if (getIdentification() == "R Console") return(TRUE) else return(FALSE)
    }

    # Rem: this code will never run, because svMisc is compiled only for R >= 2.0.0
    # It is left there just in case one would like to make svMisc backward compatible!
    # 2) Check parameters
	if (any(commandArgs() == "--sdi"))
		return(TRUE)

	# 3) Look for Rconsole file
	UserDir <- Sys.getenv("R_USER")
	if (UserDir == "") UserDir <- Sys.getenv("HOME")
	if (UserDir == "") UserDir <- paste(Sys.getenv("HOMEDRIVE"),
		Sys.getenv("HOMEPATH"), sep="")
	if (UserDir == "") ConfFile <- "" else
		ConfFile <- paste(UserDir, "Rconsole", sep=.Platform$file.sep)
	# Does it exists
    if (!file.exists(ConfFile)) { # Look for a possible system-wide config file
        ConfFile <- paste(Sys.getenv("R_HOME"), "/etc/Rconsole", sep="")
        if (!file.exists(ConfFile))
			return(FALSE)	# No config file found => default behavious: MDI
	}
	
	# 4) Read the Rconsole file
	Conf <- read.table(ConfFile, sep="------", header = FALSE)
	# Look for a line starting with 'MDI'
	MDIpos <- grep("^MDI", as.vector(Conf[, 1]))
	if (length(MDIpos) == 0)
	    return(FALSE)   # Argument not found => default value (MDI)?
	MDIarg <- as.character(Conf[MDIpos[1], 1])
	MDIvalue <- strsplit(MDIarg, "=")[[1]][2]
	MDIvalue <-  gsub(" ", "", tolower(MDIvalue))
	# If contains "yes" or "1", it is MDI mode, otherwise SDI mode (?)
	if (MDIvalue == "yes") return(FALSE)
	if (MDIvalue == "1") return(FALSE)
	# Should be SDI mode?
	return(TRUE)
}
