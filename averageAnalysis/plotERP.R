
plotERP <- function(times, erp, cis, xlim=NULL, xlab="Time (ms)", ylab="ERP", color="red") {
    if(is.null(xlim)) {
        xlim <- range(times)
    }

    d <- data.frame(x=times, y=erp)

    p <- ggplot(data=d, aes(x=x, y=y))
    p <- p + geom_line()
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
    p <- p + coord_cartesian(xlim=xlim)
    p <- p + geom_vline(xintercept=0, color=color)
    print(p)

#     plot(d$x, d$y, xlab=xlab, ylab=ylab, xlim=xlim, type="l")
#     abline(v=0, col=color)

# browser()
}

