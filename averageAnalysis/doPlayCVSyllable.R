
processAll <- function() {
    srate <- 10000000
    startTime <- 57990365/srate
    endTime <- 75911030/srate
    audio <- load.wave("data/EC2_B1/Analog/analog1.wav")
    set.audio.driver("/usr/bin/totem")
    play(audio)
    browser()
}

processAll()

rm(processAll)
