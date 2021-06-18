distributR <- function(x){

  if(as.vector(x$distrib)=='bi'){

    data.frame(
      theo.1 = peak.positionR(nrPeaks.user=nrPeaks.user,
                              DC=x$DC1,
                              seq=seq,
                              MonoMW=massr$MonoMW) %>%
        mutate(Iso.Pattern = Iso.Pattern * x$ab1),
      theo.2 = peak.positionR(nrPeaks.user=nrPeaks.user,
                              DC=x$DC2,
                              seq=seq,
                              MonoMW=massr$MonoMW) %>%
        mutate(Iso.Pattern = Iso.Pattern * x$ab2)
    ) %>%
      select(-theo.2.mz.th) %>%
      magrittr::set_colnames(c('mz.th', 'iso.1', 'iso.2')) %>%
      mutate(
        iso = iso.1+iso.2,
        centroid.1 = weighted.mean(mz.th, iso.1),
        centroid.2 = weighted.mean(mz.th, iso.2),
        centroid = weighted.mean(mz.th, iso)
      )

  } else {
    data.frame(
      theo.1 = peak.positionR(nrPeaks.user=nrPeaks.user,
                              DC=x$DC1,
                              seq=seq,
                              MonoMW=massr$MonoMW) %>%
        mutate(Iso.Pattern = Iso.Pattern * x$ab1)
    ) %>%
      magrittr::set_colnames(c('mz.th', 'iso.1')) %>%
      mutate(
        iso = iso.1,
        centroid.1 = NA,
        centroid.2 = NA,
        centroid = weighted.mean(mz.th, iso)
      )
  }

}




