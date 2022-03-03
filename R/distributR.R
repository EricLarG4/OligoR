## ---- distributR
#
# distributR <- function(opt.data, seq, massr, nrPeaks.user = 72){
#
#
#   opt.data <- as.data.frame(opt.data)
#
#   massr <- as.data.frame(massr)
#
#   seq <- as.data.frame(seq)
#
#   distrib.group <- opt.data %>%
#     select(distrib) %>%
#     unique()
#
#   if(distrib.group=='bi'){
#
#     data.frame(
#       theo.1 = peak.positionR(nrPeaks.user=nrPeaks.user,
#                               DC=opt.data$DC1,
#                               seq=seq,
#                               MonoMW=massr$MonoMW) %>%
#         mutate(Iso.Pattern = Iso.Pattern * opt.data$ab1),
#       theo.2 = peak.positionR(nrPeaks.user=nrPeaks.user,
#                               DC=opt.data$DC2,
#                               seq=seq,
#                               MonoMW=massr$MonoMW) %>%
#         mutate(Iso.Pattern = Iso.Pattern * opt.data$ab2)
#     )
#     select(-theo.2.mz.th) %>%
#     magrittr::set_colnames(c('mz.th', 'iso.1', 'iso.2')) %>%
#     mutate(
#       iso = iso.1+iso.2,
#       centroid.1 = weighted.mean(mz.th, iso.1),
#       centroid.2 = weighted.mean(mz.th, iso.2),
#       centroid = weighted.mean(mz.th, iso)
#     )
#
#
#   } else {
#
#       data.frame(
#         theo.1 = peak.positionR(nrPeaks.user=nrPeaks.user,
#                                 DC=opt.data$DC1,
#                                 seq=seq,
#                                 MonoMW=massr$MonoMW) %>%
#           mutate(Iso.Pattern = Iso.Pattern * opt.data$ab1)
#       ) %>%
#         magrittr::set_colnames(c('mz.th', 'iso.1')) %>%
#         mutate(
#           iso = iso.1,
#           centroid.1 = NA,
#           centroid.2 = NA,
#           centroid = weighted.mean(mz.th, iso)
#         )
#
#
#   }
# }




distributR <- function(opt.data, seq, massr, peaks = 72){

  if(as.vector(opt.data$distrib)=='bi'){

    data.frame(
      theo.1 = peak.positionR(nrPeaks.user=peaks,
                              DC=opt.data$DC1,
                              seq=seq,
                              MonoMW=massr$MonoMW) %>%
        mutate(Iso.Pattern = Iso.Pattern * opt.data$ab1),
      theo.2 = peak.positionR(nrPeaks.user=peaks,
                              DC=opt.data$DC2,
                              seq=seq,
                              MonoMW=massr$MonoMW) %>%
        mutate(Iso.Pattern = Iso.Pattern * opt.data$ab2)
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
      theo.1 = peak.positionR(nrPeaks.user=peaks,
                              DC=opt.data$DC1,
                              seq=seq,
                              MonoMW=massr$MonoMW) %>%
        mutate(Iso.Pattern = Iso.Pattern * opt.data$ab1)
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
