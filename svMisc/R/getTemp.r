"getTemp" <-
function(x, default = NULL, mode="any") {
    if  (exists(x, envir = TempEnv(), mode = mode, inherits = FALSE)) {
        return(get(x, envir = TempEnv(), mode = mode, inherits = FALSE))
    } else { # Variable not found, return the default value
        return(default)
    }
}
