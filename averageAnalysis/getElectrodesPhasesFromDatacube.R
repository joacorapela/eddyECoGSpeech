getElectrodesPhasesFromDatacube <- function(elecNumbers, datacube) {
    electrodesPhases <- c()
    for(i in 1:length(elecNumbers)) {
        electrodeIndexInArray <-
         getElectrodeIndexInArrayGGPlot(elecNumber=elecNumbers[i])
        electrodesPhases <- cbind(electrodesPhases, 
                                   datacube[electrodeIndexInArray[1],
                                             electrodeIndexInArray[2],])
    }
    return(electrodesPhases)
}
