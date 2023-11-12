
getCVSIndicesOverlappingCWE <- function(cweStart, cweEnd, cvsStarts, cvsEnds) {
    cvsIndices <- which((cweStart<cvsStarts & cvsEnds<cweEnd) |
                         (cweStart<cvsStarts & cvsStarts<cweEnd & 
                          cweEnd<cvsEnds) |
                         (cvsStarts<cweStart & cweEnd<cvsEnds) |
                         (cvsStarts<cweStart & cweStart<cvsEnds & 
                          cvsEnds<cweEnd))
# cweCaseBands <- c()
# if(length(which(cweStart<cvsStarts & cvsEnds<cweEnd))) {
#     cweCaseBands <- c(cweCaseBands, 1)
# }
# if(length(which(cweStart<cvsStarts & cvsStarts<cweEnd & cweEnd<cvsEnds))) {
#     cweCaseBands <- c(cweCaseBands,2)
# }
# if(length(which(cvsStarts<cweStart & cweEnd<cvsEnds))) {
#     cweCaseBands <- c(cweCaseBands,3)
# }
# if(length(which(cvsStarts<cweStart & cweStart<cvsEnds & cvsEnds<cweEnd))) {
#     cweCaseBands <- c(cweCaseBands,4)
# }
# caseBands <- get(x="caseBands", envir=.GlobalEnv)
# caseBands <- c(caseBands, cweCaseBands)
# assign(x="caseBands", value=caseBands, envir=.GlobalEnv)
    return(cvsIndices)
#     return(list(cvsIndices=cvsIndices, cweCaseBands=cweCaseBands))
}
                    
