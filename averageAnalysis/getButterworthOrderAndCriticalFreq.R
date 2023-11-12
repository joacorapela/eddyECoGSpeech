getButterworthOrderAndCriticalFreq <- function(wP, wS, rP, rS) {
    order <- (log(1/rP^2-1)-log(1/rS^2-1))/(2*log(wP/wS))
    criticalAngle <- wP/(1/rP^2-1)^(1/(2*order))
browser()
    return(list(order=order, criticalAngle=criticalAngle))
}
