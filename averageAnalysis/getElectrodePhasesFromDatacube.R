getElectrodePhasesFromDatacube <- function(elecNumbers, phasesDataCube) {
    phases <- c()
    for(i in 1:length(elecNumbers)) {
        electrodeIndexInArray <- getElectrodeIndexInArrayGGPlot(elecNumber=
                                                                 elecNumbers[i])
        phases <- cbind(phases, phasesDataCube[electrodeIndexInArray[1], 
                                                electrodeIndexInArray[2],])
    }
    return(phases)
}
