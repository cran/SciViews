"rmTemp" <-
function(x) {
    if (!is.character(x))
        stop("'x' must be a character string, or vector of strings!")
    for (i in 1:length(x))
        try(if (exists(x[i], env = TempEnv())) rm(list = x[i], envir = TempEnv()), silent = TRUE)
}
