## ---- massR

#' Oligonucleotide molecular weight and m/z calculator
#'
#' @param DC A numeric value defining the percentage of in solution deuterium.
#' @param K41C A numeric value defining the percentage of 41K isotope. Defaults to the natural abundance of 6.730244%.
#' @param seq A list created with the \code{sequenceR} function.
#' @return A list with the monoisotopic molecular weight and m/z, and the monoisotopic molecular weight and m/z.
#' @examples
#' massR(DC = 9, K41C = 6.730244 seq = sequencer)

massR <- function(seq, DC, K41C = 6.730244){

  if(!exists('listIso')){
    load('data/listIso.Rda')
  }

  if(!exists('listMass')){
    load('data/listMass.Rda')
  }

  listIso$D <- c(100-DC, DC)/100

  listIso$K <- c(sum(listIso$K)-K41C/100-listIso$K[2],
                 listIso$K[2],
                 K41C/100)

  #Monoisotopic mass
  MonoMW <- seq$nC*listMass$C[1] + seq$nH*listMass$H[1] +
    seq$nN*listMass$N[1] + seq$nO*listMass$O[1] +
    seq$nP*listMass$P[1] + seq$nK*listMass$K[1]

  if (seq$z>0) {
    Monomz <- MonoMW/seq$z
  } else {
    Monomz <- MonoMW
  }

  #Average mass calculation from number of atoms, isotopic masses and abundances.
  #nX is subtracted from nH because nH is the total number of H, including the exchangeable ones.

  AveMW <- seq$nC*(listIso$C[1]*listMass$C[1] + listIso$C[2]*listMass$C[2]) +
    (seq$nH-seq$nX)*(listIso$H[1]*listMass$H[1] + listIso$H[2]*listMass$H[2]) +
    seq$nN*(listIso$N[1]*listMass$N[1] + listIso$N[2]*listMass$N[2]) +
    seq$nO*(listIso$O[1]*listMass$O[1] + listIso$O[2]*listMass$O[2] + listIso$O[3]*listMass$O[3]) +
    seq$nX*(listIso$D[1]*listMass$D[1] + listIso$D[2]*listMass$D[2]) +
    seq$nP*(listIso$P[1]*listMass$P[1]) +
    seq$nK*(listIso$K[1]*listMass$K[1] + listIso$K[2]*listMass$K[2] + listIso$K[3]*listMass$K[3])

  Avemz <- AveMW/seq$z

  output <- list(MonoMW=MonoMW,
                 AveMW=AveMW,
                 Monomz=Monomz,
                 Avemz=Avemz)

  return(output)

}

