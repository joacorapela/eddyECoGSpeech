
robustSkewness <- function(x) {
    require(moments)

    res <- outbox(x=x)
    if(length(res$out.id)>0) {
        answer <- skewness(x[-res$out.id])
    } else {
        answer <- skewness(x)
    }
    return(answer)
}
