
processAll <- function() {
    xlim <- c(500, 600)
    ht1 <- get(load("results/simulated/htTWFreq0.62WaveLength8.00FromTime0.00ToTime1.00SR762.94Wav234.RData"))
    ht1$ht <- ht1$ht[xlim[1]:xlim[2]]
    ht2 <- get(load("results/simulated/htTWFreq0.62WaveLength8.00FromTime0.00ToTime1.00SR762.94Wav235.RData"))
    ht2$ht <- ht2$ht[xlim[1]:xlim[2]]
    ht3 <- get(load("results/simulated/htTWFreq0.62WaveLength8.00FromTime0.00ToTime1.00SR762.94Wav236.RData"))
    ht3$ht <- ht3$ht[xlim[1]:xlim[2]]
    ht4 <- get(load("results/simulated/htTWFreq0.62WaveLength8.00FromTime0.00ToTime1.00SR762.94Wav237.RData"))
    ht4$ht <- ht4$ht[xlim[1]:xlim[2]]
    ht5 <- get(load("results/simulated/htTWFreq0.62WaveLength8.00FromTime0.00ToTime1.00SR762.94Wav238.RData"))
    ht5$ht <- ht5$ht[xlim[1]:xlim[2]]
    ht6 <- get(load("results/simulated/htTWFreq0.62WaveLength8.00FromTime0.00ToTime1.00SR762.94Wav239.RData"))
    ht6$ht <- ht6$ht[xlim[1]:xlim[2]]
    ht7 <- get(load("results/simulated/htTWFreq0.62WaveLength8.00FromTime0.00ToTime1.00SR762.94Wav240.RData"))
    ht7$ht <- ht7$ht[xlim[1]:xlim[2]]
    # ht8 <- get(load("results/simulated/htTWFreq0.62WaveLength8.00FromTime0.00ToTime1.00SR762.94Wav241.RData"))
    # ht8$ht <- ht8$ht[xlim[1]:xlim[2]]
    matplot(cbind(unwrap(Arg(ht1$ht)), 
                   unwrap(Arg(ht2$ht)),
                   unwrap(Arg(ht3$ht)),
                   unwrap(Arg(ht4$ht)),
                   unwrap(Arg(ht5$ht)),
                   unwrap(Arg(ht6$ht)),
                   unwrap(Arg(ht7$ht))))
                   # unwrap(Arg(ht8$ht))))
    show(sprintf("mean(ht1)=%f", mean(unwrap(Arg(ht1$ht)))))
    show(sprintf("mean(ht2)=%f", mean(unwrap(Arg(ht2$ht)))))
    show(sprintf("mean(ht3)=%f", mean(unwrap(Arg(ht3$ht)))))
    show(sprintf("mean(ht4)=%f", mean(unwrap(Arg(ht4$ht)))))
    show(sprintf("mean(ht5)=%f", mean(unwrap(Arg(ht5$ht)))))
    show(sprintf("mean(ht6)=%f", mean(unwrap(Arg(ht6$ht)))))
    show(sprintf("mean(ht7)=%f", mean(unwrap(Arg(ht7$ht)))))
    # show(sprintf("mean(ht8)=%f", mean(unwrap(Arg(ht8$ht)))))
    browser()
}

processAll()

rm(processAll)
