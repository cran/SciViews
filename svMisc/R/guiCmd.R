"guiCmd" <-
function(command) {
    # This function sends a command to the GUI client
    # The actual code is a custom function named .guiCmd (usually in TempEnv)
	#require(svMisc)
	CmdFun <- getTemp(".guiCmd", mode = "function")
    if (!is.null(CmdFun)) {
		CmdFun(command)
		return(TRUE)
	} else {
     	# Should not happen => better for debugging purpose!
        cat("Impossible to send the command:", command, "\n")
		return(command)
	}
}

