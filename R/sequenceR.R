
sequenceR <- function(z, K, sequence, nX.user.input, nX.select){

  z <- as.numeric(z)

  K <- as.numeric(K)

  seq2 <- data.frame(number=1:1, string=c(sequence), stringsAsFactors = F)

  nbA <- str_count(seq2$string, "A")

  nbT <- str_count(seq2$string, "T")

  nbG <- str_count(seq2$string, "G")

  nbC <- str_count(seq2$string, "C")

  #Number of nucleotides
  nb_nt <- nbA + nbT + nbG + nbC

  #Number of phosphates
  nb_PO <- nb_nt - 1

  #Neutralized phosphate
  #(takes into account charge and potassium adducts that bring positive charges)
  nb_POH <- nb_PO - z - K

  #number of atoms
  nC <- nbA*10 + nbG*10 + nbC*9 + nbT*10

  #This is the total amount of hydrogen across isotopes (among which nX are exchangeable)
  #Charge taken into account here, so H will not be taken out when calculating m/z
  nH <- nbA*12 + nbG*12 + nbC*12 + nbT*13 + 1 - z - K

  nO <- nbA*5 + nbG*6 + nbC*6 + nbT*7 - 2

  nN <- nbA*5 + nbG*5 + nbC*3 + nbT*2

  nP <- nb_PO

  nK <- K

  nX.user <-as.numeric(nX.user.input)


  if(nX.user.input == ''){
    if(nX.select == 'A'){
      nX <- nb_PO + 2 + nbA*2 + nbT*1 + nbG*3 + nbC*2 - z
    } else {
      if(nX.select == 'B'){
        nX <- 2 + nbA*2 + nbT*1 + nbG*3 + nbC*2
      } else {
        nX <- nbA*2 + nbT*1 + nbG*3 + nbC*2
      }
    }
  } else {
    nX <- nX.user
  }


  output <- list(z=z, K=K,
                 nbA=nbA, nbT=nbT, nbG=nbG, nbC=nbC,
                 nb_nt=nb_nt, nb_PO=nb_PO, nb_POH=nb_POH,
                 nC=nC, nH=nH, nO=nO, nN=nN, nP=nP, nK=nK,
                 nX = nX
  )

  return(output)

}
