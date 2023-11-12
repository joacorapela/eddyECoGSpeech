
plotERPWithCI <- function(times, erp, ci, xlim=NULL, 
                                 xlab="Time (sec)", 
                                 ylab=expression(paste("Voltage ~ (", 
                                                       mu, "V)")),
                                 colVLine="black", 
                                 fillRibbon="orange", alphaRibbon=.3) {
    if(is.null(xlim)) {
        xlim <- range(times)
    }

    erp <- erp*1e6
    ci <- ci*1e6
    d <- data.frame(times=times, erp=erp, erpCIL=ci[,1], erpCIU=ci[,2])

    p <- ggplot(data=d, aes(x=times, y=erp))
    p <- p + geom_line()
    p <- p + geom_ribbon(aes(x=times, ymin=erpCIL, ymax=erpCIU), 
                          fill=fillRibbon, alpha=alphaRibbon)
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
    p <- p + xlim(xlim)
    p <- p + geom_vline(xintercept=0, color=colVLine)
    print(p)

#     plot(d$x, d$y, xlab=xlab, ylab=ylab, xlim=xlim, type="l")
#     abline(v=0, col=color)

# browser()
}

