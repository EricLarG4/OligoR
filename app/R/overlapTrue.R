overlapTrue <- function (d1, d2 = NULL) 
{
  if (!is.null(ncol(d1)) && ncol(d1) == 2) {
    d2 <- d1[, 2]
    d1 <- d1[, 1]
  }
  if (identical(all.equal(d1[1], d1[length(d1)]), TRUE) && 
      identical(all.equal(d2[1], d2[length(d1)]), TRUE)) {
    d1 <- d1[-1]
    d2 <- d2[-1]
  }
  d1 <- d1/sum(d1)
  d2 <- d2/sum(d2)
  sum(pmin(d1, d2))
}