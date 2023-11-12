
plotOneSetOfResults <- function(results,
                                 erpFigFilename,
                                 itcFigFilename,
                                 erspFigFilename,
                                 xlim,
                                 ylim,
                                 zlimITC,
                                 zlimERSP,
                                 contour,
                                 width,
                                 height,
                                 plotERP=TRUE,
                                 plotITC=TRUE,
                                 plotERSP=TRUE) {
    if(plotERP) {
        trellis.device("postscript", color=TRUE, width=width, height=height,
                       onefile=FALSE, horizontal=FALSE,
                       file=erpFigFilename)
        plotERP(times=results$times, erp=results$erp, xlim=xlim, xlab="Time (sec)")
        dev.off()
    }

    if(plotITC) {
        trellis.device("postscript", color=TRUE, width=width, height=height,
                       onefile=FALSE, horizontal=FALSE,
                       file=itcFigFilename)
        plotTimeFreqRes(timeFreqRes=results$itc, times=results$times, freqs=results$freqs, xlim=xlim, ylim=ylim, zlim=zlimITC, contour=contour, xlab="Time (sec)")
        dev.off()
    }

    if(plotERSP) {
        trellis.device("postscript", color=TRUE, width=width, height=height,
                       onefile=FALSE, horizontal=FALSE,
                       file=erspFigFilename)
        plotTimeFreqRes(timeFreqRes=results$ersp, times=results$times, freqs=results$freqs, xlim=xlim, ylim=ylim, zlim=zlimERSP, contour=contour, xlab="Time (sec)")
        dev.off()
    }
}
