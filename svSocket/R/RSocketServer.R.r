# Create and manage R socket servers using tcltk

"sendSocket" <-
function(text, socket) {
	# Send the given text to a client through a socket
	tcl("puts", socket, text)
}

"processSocket" <-
function(msg) {
    # This is the default R function that processes a command send by a socket client
	# 'msg' is assumed to be pure R code
	# Process it...
    textcommands <- textConnection(msg)
	results <- tryCatch({
		res <- guiSource(textcommands)
  		res
	}, error = function(e) e,
	finally = close(textcommands))
	return(results)
}

"startSocketServer" <-
function(port = 8888, server.name = "Rserver", procfun = processSocket) {    # OK, could be port = 80 to emulate a simple HTML server
    # This is the main function that starts the server
	# This function implements a basic R socket server on 'port'
 	# SocketServerProc is the R workhorse function that do the computation
 	# The server is written in Tcl. This way it is not blocking R command-line!
 	# It is designed in a way that R can open simultaneously several ports and
 	# accept connection from multiple clients to each of them.
 	# Commands from each port can be processed differently
 	# For security reasons, this socket server currently only accepts local connections

	#require(tcltk) || stop("'tcltk' package required!")

	is.function(procfun) || stop("'procfun' must be a function!")
    # Note: the data send by the client must be read from the Tcl $::sockMsg variable
    # I don't know if a clash could happen here is multiple clients send data at the
    # same time to the R socket server???
	if (!is.numeric(port[1]) || port[1] < 1) stop("'port' must be a positive integer!")
	portnum <- round(port[1])
	port <- as.character(portnum)
	
	if (!is.character(server.name)) stop("'server.name' must be a string!")
	server.name <- server.name[1]
	
	# Check if the port is not open yet
	servers <- getTemp("SocketServers")
	if (port %in% servers)
	    return(TRUE) # This port is alredy open!

	# We need Tcl to be able to call an R functions to process clients' requests
	"tclProcExists" <- function(proc) {
		proc <- as.character(proc[1])
		return(length(as.character(tcl("info", "commands", proc))) == 1)
	}
	
    if (!tclProcExists("SocketServerProc")) {
	    # Create the callback when a client sends data
		"SocketServerProc" <- function() {
			require(tcltk)
			# Note: I don't know how to pass arguments here.
			# So, I use Tcl global variables instead:
			# - the server port from $::sockPort,
			# - the socket client from $::sockClient,
			# - and the message from $::sockMsg
            "tclGetValue_" <- function(name) {
    			# Get the value stored in a plain Tcl variable
    			if (!is.character(name)) stop("'name' must be a character!")

    			# Create a temporary dual variable with tclVar() (name does not matter)
    			Temp <- tclVar(init = "")

    			# Copy the content of the var of interest to it
    			.Tcl(paste("catch {set ", as.character(Temp), " $", name, "}", sep = ""))

    			# Get the content of the temporary variable
    			Res <- tclvalue(Temp) # (Temp will be destroyed when the function exists)
    			return(Res)
			}

			port <- tclGetValue_("::sockPort")
			if (port == "") return(FALSE) # The server is closed
			client <- tclGetValue_("::sockClient")
			if (client == "") return(FALSE) # The socket client is unknown!
			msg <- tclGetValue_("::sockMsg")
			if (msg == "") return(FALSE) # No message!
			
			# Make sure this message is not processed twice
			.Tcl("set ::sockMsg {}")
			
			# Do we have to debug socket transactions
			Debug <- getOption("debug.Socket")
			if (is.null(Debug) || Debug != TRUE) Debug <- FALSE else Debug <- TRUE
			if (Debug) cat(client, " > ", port, ": ", msg, "\n", sep = "")

			# The function that processes the client request is SocketServerProc_<port>
			"TempEnv_" <- function() {
    			pos <-  match("TempEnv", search())
    			if (is.na(pos)) { # Must create it
        			TempEnv <- list()
        			attach(TempEnv, pos = length(search()) - 1)
        			rm(TempEnv)
        			pos <- match("TempEnv", search())
    			}
    			return(pos.to.env(pos))
			}

			"getTemp_" <- function(x, default = NULL, mode="any") {
    			if  (exists(x, envir = TempEnv_(), mode = mode, inherits = FALSE)) {
        			return(get(x, envir = TempEnv_(), mode = mode, inherits = FALSE))
    			} else { # Variable not found, return the default value
        			return(default)
    			}
			}

			proc <- getTemp_(paste("SocketServerProc", port, sep = "_"), mode = "function")
			if (is.null(proc)) return(FALSE) # The server should be closed
			# Call this function
			res <- proc(msg)
			# Return result to the client
			if (res != "") {
			    if (Debug) cat(port, " > ", client, ": ", res, "\n", sep = "")
				tcl("puts", client, res)
			}
			return(TRUE) # The command is processed
		}
		# This is a copy of tclFun from tcltk2, to avoid a Depends: tcltk2
		"tclFun_" <-
			function(f, name = deparse(substitute(f))) {
    		# Register a simple R function (without arguments) as a callback in Tcl,
			# and give it the same name)
    		# Indeed, .Tcl.callback(f) in tcltk package does the job... but it gives
			# cryptic names like R_call 0x13c7168
    		# Done in NAMESPACE
    		#require(tcltk) || stop("Package 'tcltk' is needed!")

    		# Check that 'f' is a function with no arguments (cannot handle them, currently)
    		is.function(f) || stop("'f' must be a function!")
    		is.null(formals(f)) || stop("The function used cannot (yet) have arguments!")
    		# Make sure the name of the function is valid
    		if (!is.character(name)) stop("'name' must be a character string!") else
				name <- make.names(name[1])

    		res <- .Tcl.callback(f)
    		# Make sure this is correct (R_call XXXXXXXX)
    		if (length(grep("R_call ", res) > 0)) # Create a proc with the same name in Tcl
    		.Tcl(paste("proc ", name, " {} {", res, "}", sep = ""))
    		# Return the R_call XXXXXXXX string, as .Tcl.callback() does
    		return(res)
    		# Rem: if you delete the R 'f' function, the Tcl 'f' function still works (?!)
		}
		tclFun_(SocketServerProc)
	}

	# Copy procfun into TempEnv as SocketServerProc_<port>
 	assignTemp(paste("SocketServerProc", port, sep ="_"), procfun)

 	# Create the Tcl function that retrieves data from the socket
	# (command send by the client), call the processing R function
	# and returns result to the client
    cmd <- paste(c(paste("proc  sockHandler_", port, " {sock} {", sep = ""),
                   paste("global Rserver_", port, sep = ""),
                   "if {[eof $sock] || [catch {gets $sock line}]} {",
                   "    # end of file or abnormal connection drop",
                   "    fileevent $sock readable {}",
				   "    close $sock",
				   paste("    #puts \"Close $Rserver_", port, "($sock)\"", sep = ""),
				   paste("    unset Rserver_", port, "($sock)", sep = ""),
				   "} else {",
  			       "#    global sockPort",
  			       "#    global sockClient",
  			       "#    global sockMsg",
				   paste("    set ::sockPort", port),
				   "    set ::sockClient $sock",
				   "    set ::sockMsg $line",
				   "    SocketServerProc    ;# process the command in R",
				   "}\n}"), collapse = "\n")
    # if {[gets $sock line] < 0} {return} # To handle incomplete lines!
    .Tcl(cmd)

    # Create the Tcl function that accepts input from a client (a different one for each server port)
    cmd <- paste(c(paste("proc sockAccept_", port, " {sock addr port} {", sep = ""),
				   paste("global Rserver_", port, sep = ""),
                   "# Configure the socket",
  		           "fconfigure $sock -buffering line -blocking 0",
				   "# Accept only local clients",
				   "if {$addr != \"127.0.0.1\"} {",
				   " #   puts $sock \"Error: Only local clients allowed!\"",
	 			   "    close $sock",
  				   "    return",
  		           "}",
  		           paste("set Rserver_", port, "($sock) [list $addr, $port]", sep = ""),
  		           paste("fileevent $sock readable [list sockHandler_", port, " $sock]", sep = ""),
  				   "puts $sock \"$addr:$port, You are connected to the local R socket server.\"",
  				   "}"), collapse = "\n")
  	.Tcl(cmd)

	# Create the socket server itself in Tcl (a different one for each port)
	.Tcl(paste("set Rserver_", port, "(main) [socket -server sockAccept_", port, " ", port, "]", sep =""))
	#.Tcl(paste("set Rserver_", port, "(port) ", port, sep = ""))
	
	# Add this port in the TempEnv variable 'SocketServers'
	
	assignTemp("SocketServers", sort(unique(c(getTemp("SocketServers"), portnum))))

	return(TRUE) # Humm! Only if it succeed... TO DO: check this!
}

"stopSocketServer" <-
function(port = 8888) {
	# Stop a running socket server
	# Note: it does not close existing connections to clients, but don't process them any more
	
    if (!is.numeric(port[1]) || port[1] < 1) stop("'port' must be a positive integer!")
	portnum <- round(port[1])
	port <- as.character(portnum)

 	# Get the list of running socket servers
	servers <- getTemp("SocketServers")
	if (port %in% servers) { # This port is open
		# Assign it back, with the corresponding port stripped out
		# But if I was the last one, delete the SocketServers variable
		servers <- servers[servers != port]
		if (length(servers) == 0) {
		    rmTemp("SocketServers")
		} else {
			assignTemp("SocketServers", servers[servers != port])
  		}
	
		# Eliminate the processing function from TempEnv
		rmTemp(paste("SocketServerProc", port, sep = "_"))
	
		# Close the socket in order not to reject future client connections
		.Tcl(paste("close $Rserver_", port, "(main)", sep = ""))
	
		# Note: Tcl procs and variables are not eliminated yet because there may be
		# still clients connected!
		return(TRUE)
	} else {
	    return(FALSE) # This port was not allocated to a running R socket server
	}
}

"getSocketServers" <-
function() {
	# Get the list of currently running socket servers
	return(getTemp("SocketServers"))
}

"getSocketClients" <-
function(port = 8888) {
    if (!is.numeric(port[1]) || port[1] < 1) stop("'port' must be a positive integer!")
	portnum <- round(port[1])
	port <- as.character(portnum)

	# Does a server exists on this port?
	servers <- getTemp("SocketServers")
	if (!(port %in% servers))
		return(NULL) # If no R socket server running on this port

	# Get the list of clients currently connected to this server
	clients <- as.character(.Tcl(paste("array names Rserver", port, sep = "_")))
	# Eliminate "main", which is the connection socket
	clients <- clients[clients != "main"]
	
	# Are there client connected?
	if (length(clients) == 0) return(character(0))
	
	# For each client, retrieve its address and port
	addresses <- NULL
	arrayname <- paste("Rserver", port, sep = "_")
	for (i in 1:length(clients)) {
	    client <- as.character(.Tcl(paste("array get", arrayname, clients[i])))
	    addresses[i] <- sub(", ", ":", client[2])
	}
	names(addresses) <- clients
	return(addresses)
}

"getSocketServerName" <-
function(port = 8888) {
    if (!is.numeric(port[1]) || port[1] < 1) stop("'port' must be a positive integer!")
	portnum <- round(port[1])
	port <- as.character(portnum)

	# Return the name of a given R socket server
	servers <- getTemp("SocketServers")
	if (!(port %in% servers))
		return(NULL) # If no R socket server running on this port

	ServerNames <- names(servers)
	return(ServerNames[servers == port])
}


############################################################
# Test of these functions:
#getSocketServers()
#startSocketServer(port = 8888)
#getSocketServers()
#getSocketClients()


# Connect a client then:
#getSocketClients()

# To debug exchanges through the socket:
#options(debug.Socket = TRUE)

# Stop the server with and without clients connected
#stopSocketServer()
#getSocketServers()
#getSocketClients()


