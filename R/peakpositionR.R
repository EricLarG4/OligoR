#' Oligonucleotide isotopic peak distribution calculation by FFT based on J. Proteome Res. 2018 Jan 5; 17(1): 751–758
#'
#' @param nrPeaks.user An integer controlling the number of calculated peaks.
#' @param DC A numeric value defining the percentage of in solution deuterium.
#' @param seq A list created with the \code{sequenceR} function.
#' @param MonoMW A numeric value, the monoisotopic mass of the oligonucleotide, which can be imported from the \code{massR} function.
#' @return A dataframe containing the isotopic distribution.
#' @examples
#' peak.positionR(nrPeaks.user = 32, DC = 9, seq = sequencer, MonoMW = massr$MonoMW)


peak.positionR <- function(nrPeaks.user, DC, seq, MonoMW){

  load('Data/listIso.Rda')

  #number of peaks to generate
  nrPeaks <- as.numeric(nrPeaks.user)

  #Below a different set of isotope abundances are used for the calculation of all isotopic peaks (full distribution), to make the use of FFT easier.

  #Isotopic peak abundance calculation. By default, 32 isotopic peaks are calculated but can be user-defined (more peaks may be necessary for larger and/or heavily deuterated species).
  c12 <- rep(0, nrPeaks); h1 <- rep(0, nrPeaks); n14 <- rep(0, nrPeaks);
  o16 <- rep(0, nrPeaks); p31 <- rep(0, nrPeaks); k39 <- rep(0,nrPeaks);
  h2 <- rep(0, nrPeaks);

  #Isotope abundances
  h1[1] = 0.999855;       h1[2] = 0.000145;                            #Natural abundances of H isotopes
  h2[1] = (1-DC/100);     h2[2] = DC/100;                              #Calculated abundances of H isotopes for exchangeable sites as a function of DC (user-supplied).
  c12[1] = listIso$C[1]; c12[2] = listIso$C[2];
  n14[1] = listIso$N[1]; n14[2] = listIso$N[2];
  o16[1] = listIso$O[1]; o16[2] = listIso$O[2]; o16[3] = listIso$O[3];
  p31[1] = 1.0;
  k39[1] = listIso$K[1]; k39[2] = listIso$K[2]; k39[3] = listIso$K[3];

  #Determination of the isotopic pattern by Fast Fourier Transform
  #(only yields abundance, not the corresponding m/z).
  #Based on J. Proteome Res. 2018 Jan 5; 17(1): 751–758.
  Iso_Pattern <- Re(fft(fft(c12)^seq$nC*fft(h1)^(seq$nH-seq$nX)*fft(n14)^seq$nN * fft(o16)^seq$nO * fft(p31)^seq$nP * fft(k39)^seq$nK * fft(h2)^seq$nX,
                        inverse=TRUE))/length(c12)

  #Isotopic peak m/z calculation based on the mono-isotopic mass calculated above.

  nbPeaks <- 1:nrPeaks

  peak.position <- data.frame("nbPeaks1" = unlist(nbPeaks), 'MonoMW1' = MonoMW) %>%
    mutate('mass.th' = MonoMW1 + (nbPeaks1-1)*1.0078250321) %>%
    mutate('mz.th' = mass.th/seq$z) %>%
    dplyr::select(mz.th) %>%
    cbind('Iso.Pattern' = Iso_Pattern) %>%
    mutate(Iso.Pattern = 1 - (max(Iso.Pattern)-Iso.Pattern)/(max(Iso.Pattern)-min(Iso.Pattern)))

}

