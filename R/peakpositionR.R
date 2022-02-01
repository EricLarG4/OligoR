## ---- peak.positionR

#' Oligonucleotide isotopic peak distribution calculation by FFT based on J. Proteome Res. 2018 Jan 5; 17(1): 751–758
#'
#' @param nrPeaks.user An integer controlling the number of calculated peaks.
#' @param DC A numeric value defining the percentage of in solution deuterium.
#' @param K41C A numeric value defining the percentage of 41K isotope. Defaults to the natural abundance of 6.730244%.
#' @param seq A list created with the \code{sequenceR} function.
#' @param MonoMW A numeric value, the monoisotopic mass of the oligonucleotide, which can be imported from the \code{massR} function.
#' @return A dataframe containing the isotopic distribution.
#' @examples
#' peak.positionR(nrPeaks.user = 32, DC = 9, K41C = 6.730244 seq = sequencer, MonoMW = massr$MonoMW)


peak.positionR <- function(nrPeaks.user, DC, K41C = 6.730244, seq, MonoMW){



  #Below a different set of isotope abundances are used for the calculation of all isotopic peaks (full distribution), to make the use of FFT easier.

  #Isotopic peak abundance calculation----

  #By default, 32 isotopic peaks are calculated but can be user-defined
  #(more peaks may be necessary for larger and/or heavily deuterated species).

  ##initialization----

  #number of peaks to generate
  nrPeaks <- as.numeric(nrPeaks.user)

  h1 <- rep(0, nrPeaks)
  h2 <- rep(0, nrPeaks)
  c12 <- rep(0, nrPeaks)
  n14 <- rep(0, nrPeaks)
  o16 <- rep(0, nrPeaks)
  p31 <- rep(0, nrPeaks)
  k39 <- rep(0,nrPeaks)


  ##Isotope abundances----

  #load isotope abundances
  if(!exists('listIso')){
    load('data/listIso.Rda')
  }

  h1[1] = 0.999855      #Natural abundances of H isotopes
  h1[2] = 0.000145

  h2[1] = (1-DC/100)    #Calculated abundances of H isotopes for exchangeable sites as a function of DC (user-supplied).
  h2[2] = DC/100

  c12[1] = listIso$C[1]
  c12[2] = listIso$C[2]

  n14[1] = listIso$N[1]
  n14[2] = listIso$N[2]

  o16[1] = listIso$O[1]
  o16[2] = listIso$O[2]
  o16[3] = listIso$O[3]

  p31[1] = 1.0

  k39[1] <- sum(listIso$K)-K41C/100-listIso$K[2]
  k39[2] <- listIso$K[2]
  k39[3] <- K41C/100

  ##Determination of the isotopic pattern----

  #by Fast Fourier Transform
  #(only yields abundance, not the corresponding m/z).
  #Based on J. Proteome Res. 2018 Jan 5; 17(1): 751–758.

  Iso.Pattern <- Re(fft(fft(c12)^seq$nC*fft(h1)^(seq$nH-seq$nX)*fft(n14)^seq$nN * fft(o16)^seq$nO * fft(p31)^seq$nP * fft(k39)^seq$nK * fft(h2)^seq$nX,
                        inverse=TRUE))/length(c12)

  #Isotopic peak m/z calculation----

  #based on the mono-isotopic mass calculated above

  peak.position <- data.frame(
    mz.th = sapply(1:nrPeaks, function(x){(MonoMW+(x-1)*1.0078250321)/seq$z}),
    Iso.Pattern = 1 - (max(Iso.Pattern)-Iso.Pattern)/(max(Iso.Pattern)-min(Iso.Pattern))
  )


}




