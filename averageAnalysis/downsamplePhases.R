downsamplePhases <- function(phases, n) {
    indices <- seq(from=1, to=length(phases), by=n)
    return(phases[indices])
}
