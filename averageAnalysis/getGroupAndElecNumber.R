getGroupAndElecNumber <- function(elecNumber) {
    groupNumber <- (elecNumber-1)%/%64+1
    elecNumber <- (elecNumber-1)%%64+1
    return(list(groupNumber=groupNumber, elecNumber=elecNumber))
}
