reval <- function(x) x()

reval_if <- function(x) if (is.function(x)) x() else x
