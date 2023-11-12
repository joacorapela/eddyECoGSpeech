getAbsoluteElectrodeNumber <- function(groupNumber, elecNumber) {
    return((groupNumber-1)*64+elecNumber)
}
