## ---- sequenceR

#' Oligonucleotide sequence analysis
#'
#' @param z An integer, defining the absolute value of the charge state.
#' @param K An integer, the number of K+ adducts.
#' @param nX.user.input An integer, the number of exchangeable sites. Use \code{''} to use the presets instead.
#' @param nX.select A string defining the preset to use for the number of exchangeable sites: all sites = \code{'A'}, no phosphates = \code{'B'}, no phosphates nor termini = \code{'C'}
#' @return A list containing the charge state \code{z}, the number of potassium adducts \code{K},
#' the total number of nucleotides \code{nb_nt},
#' the number of each nucleotide \code{nbA}, \code{nbC}, \code{nbG}, \code{nbT},
#' the number of each atom \code{nH}, \code{nC}, \code{nO}, \code{nN}, \code{nP}, \code{nK},
#' the number of exchangeable sites \code{nX},
#' the number of phosphates \code{nb_PO}, and the number of neutralized phosphates \code{nb_POH}
#' @examples
#' sequenceR(z=4, K=2, sequence="TTGGGTGGGTGGGTGGGT", nX.user.input='', nX.select='C')


sequenceR <- function(z, K, sequence, nX.user.input, nX.select, mol = 1, user.atom = list(nC = 0, nH = 0, nN = 0, nO = 0, nP = 0)){

  #strand molecularity----
  mol <- as.numeric(mol)

  #nucleotide extraction from sequence----
  seq2 <- data.frame(number=1:1, string=c(sequence), stringsAsFactors = F)

  nts <- list(
    nbA = str_count(seq2$string, "A"),
    nbT = str_count(seq2$string, "T"),
    nbG = str_count(seq2$string, "G"),
    nbC = str_count(seq2$string, "C")
  )

  #Number of nucleotides per strand----
  nts$nb_nt <- nts$nbA + nts$nbT + nts$nbG + nts$nbC

  #Number of phosphates per strand----
  nts$nb_PO <- nts$nb_nt - 1

  #Total number of nucleotides and phosphates----
  nts <- lapply(nts, function(x){x*mol})

  #Charge and potassium----
  nts$z <- as.numeric(z)
  nts$K <- as.numeric(K)

  #Neutralized phosphate
  #(takes into account charge and potassium adducts that bring positive charges)
  nts$nb_POH <- nts$nb_PO - nts$z - nts$K

  #number of atoms----
  nts$nC <- nts$nbA*10 + nts$nbG*10 + nts$nbC*9 + nts$nbT*10 + user.atom$nC

  #This is the total amount of hydrogen across isotopes (among which nX are exchangeable)
  #Charge taken into account here, so H will not be taken out when calculating m/z
  nts$nH <- nts$nbA*12 + nts$nbG*12 + nts$nbC*12 +nts$nbT*13 + 1*mol - nts$z - nts$K + user.atom$nH

  nts$nO <- nts$nbA*5 + nts$nbG*6 + nts$nbC*6 + nts$nbT*7 - 2*mol  + user.atom$nO

  nts$nN <- nts$nbA*5 + nts$nbG*5 + nts$nbC*3 + nts$nbT*2 + user.atom$nN

  nts$nP <- nts$nb_PO + user.atom$nP

  nts$nK <- nts$K

  #number of exchanging sites----

  #defined by user
  nX.user <-as.numeric(nX.user.input)

  #presets
  if(nX.user.input == ''){
    if(nX.select == 'A'){
      nts$nX <- nts$nb_PO + 2 + nts$nbA*2 + nts$nbT*1 + nts$nbG*3 + nts$nbC*2 - nts$z
    } else {
      if(nX.select == 'B'){
        nts$nX <- 2 + nts$nbA*2 + nts$nbT*1 + nts$nbG*3 + nts$nbC*2
      } else {
        nts$nX <- nts$nbA*2 + nts$nbT*1 + nts$nbG*3 + nts$nbC*2
      }
    }
  } else {
    nts$nX <- nX.user
  }

  return(nts)

}

