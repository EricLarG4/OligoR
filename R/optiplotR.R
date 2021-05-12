
#to be removed once packaged:
library(tidyverse)
source("R/peakpickR.R")
source("R/sequenceR.R")
source("R/peakpositionR.R")
library(ggthemes)

#optiplotR

optiplotR <- function(par=opt$par, z, raw.data, nX.select, K, nrPeaks.user, sequencer, massr, pp){

  library(DescTools)
  library(tidyverse)

  p.pp <- ggplot() +
    geom_line(data = pp,
              aes(x = mz, y = intensum),
              size = 1, color = 'grey') +
    geom_point(data = pp %>%
                 filter(peak > 0),
               aes(x = mz, y = peak),
               size = 2, color = 'steelblue', alpha = 0.75) +
    geom_line(data = pp %>%
                filter(peak > 0),
              aes(x = mz, y = peak),
              size = 1, color = 'steelblue', alpha = 0.75,
              linetype = 'dashed')

  #theoretical distribution

  if(length(par)>1){ #bimodal distrib

    theo.1 <- peak.positionR(nrPeaks.user=nrPeaks.user,
                             DC=par[1],
                             seq=sequencer,
                             MonoMW=massr$MonoMW) %>%
      mutate(Iso.Pattern = Iso.Pattern * par[3])

    theo.2 <- peak.positionR(nrPeaks.user=nrPeaks.user,
                             DC=par[2],
                             seq=sequencer,
                             MonoMW=massr$MonoMW) %>%
      mutate(Iso.Pattern = Iso.Pattern * par[4])

    theo <-  theo.2 %>%
      rbind(theo.1) %>%
      group_by(mz.th) %>%
      mutate(Iso.Pattern = sum(Iso.Pattern))

  } else { #monomodal distrib

    theo <- peak.positionR(nrPeaks.user=nrPeaks.user,
                           DC=par[1],
                           seq=sequencer,
                           MonoMW=massr$MonoMW)
  }

  p.theo <- p.pp +
    geom_point(data = theo,
               aes(x=mz.th, y=Iso.Pattern),
               size = 2, color = 'tomato', alpha = 0.75) +
    geom_line(data = theo,
              aes(x=mz.th, y=Iso.Pattern),
              size = 1, color = 'tomato', alpha = 0.75) +
    geom_vline(xintercept = weighted.mean(theo$mz.th, theo$Iso.Pattern),
               na.rm = FALSE,
               color = 'tomato',
               size = 1, alpha = 0.75,
               linetype = 'dashed') +
    geom_vline(xintercept = weighted.mean(pp$mz, pp$intensum),
               na.rm = FALSE,
               color = 'steelblue',
               size = 1, alpha = 0.75,
               linetype = 'dashed')

  if (length(par)>1) {
    p.theo <- p.theo +
      geom_point(data = theo.1,
                 aes(x = mz.th, y = Iso.Pattern),
                 size = 2, color = 'lightgoldenrod4', alpha = 0.75) +
      geom_line(data = theo.1,
                aes(x = mz.th, y = Iso.Pattern),
                size = 1, color = 'lightgoldenrod4', alpha = 0.75) +
      geom_point(data = theo.2,
                 aes(x = mz.th, y = Iso.Pattern),
                 size = 2, color = 'seagreen4', alpha = 0.75) +
      geom_line(data = theo.2,
                aes(x = mz.th, y = Iso.Pattern),
                size = 1, color = 'seagreen4', alpha = 0.75) +
      geom_vline(xintercept = weighted.mean(theo.1$mz.th, theo.1$Iso.Pattern),
                 na.rm = FALSE,
                 color = 'lightgoldenrod4',
                 size = 1, alpha = 0.75,
                 linetype = 'dashed') +
      geom_vline(xintercept = weighted.mean(theo.2$mz.th, theo.2$Iso.Pattern),
                 na.rm = FALSE,
                 color = 'seagreen4',
                 size = 1, alpha = 0.75,
                 linetype = 'dashed')
  }

  p.theo <- p.theo +
    theme_pander()

  return(p.theo)
}



#examples====

#example 1: monomodal-----

# significantly deuterated T95-2T, monomodal
raw.data <- readxl::read_excel('inst/extdata/demodata/optim.xlsx') %>%
  mutate(intensum = 1 - (max(intensum)-intensum)/(max(intensum)-min(intensum)))
z <- 4
K <-2
sequence <- "TTGGGTGGGTGGGTGGGT"
nX.select <- 'C'
nX.user.input <- ''
nrPeaks.user <- 48

#previously computed variables in oligor----
#to hide once optimizer implemented
nX <- sequenceR(z=z, K=K,
                sequence=sequence,
                nX.user.input='',
                nX.select=nrPeaks.user)$nX

#peak picking
pp <- peakpickR(raw.data = raw.data,
                neighlim = 5,
                deriv.lim = 10000,
                int.thresh = 0.00000)


#theoretical distrib base
sequencer <- sequenceR(z=z, K=K, sequence=sequence,
                       nX.user.input=nX.user.input,
                       nX.select=nX.select)

#mass calculations
massr <- massR(seq = sequencer, DC=0) #the DC parameter is not actually useful here

opt <- optim(c(10), optimizer,
             z=z, raw.data=ppp, K=K,
             nX.select='C', nrPeaks.user=nrPeaks.user,
             bi=FALSE,
             pp=pp, sequencer=sequencer, massr=massr,
             method = 'L-BFGS-B',
             lower = c(0,0,0,0),
             upper = c(100, 100, 1, 1))

#get NUS
NUS.1 <- opt$par[1]*nX/90 ####NOT SURE ABOUT FORMULA#####

optiplotR(z=z, raw.data=pppp, nX.select=nX.select, K=K, nrPeaks.user=nrPeaks.user,
         sequencer=sequencer, massr=massr, pp=pp)


#example 2: bimodal----

# 23TAG bimodal distribution
raw.data <- readxl::read_excel('inst/extdata/demodata/23TAG-binom-6.xlsx') %>%
  mutate(intensum = 1 - (max(intensum)-intensum)/(max(intensum)-min(intensum))) %>%
  filter(colorscale == 12)
z <- 4
K <-2
sequence <- "TAGGGTTAGGGTTAGGGTTAGGG"
nX.select <- 'C'
nX.user.input <- ''
nrPeaks.user <- 48

#previously computed variables in oligor----
#to hide once optimizer implemented
nX <- sequenceR(z=z, K=K,
                sequence=sequence,
                nX.user.input=nX.user.input,
                nX.select=nrPeaks.user)$nX

#peak picking
pp <- peakpickR(raw.data = raw.data,
                neighlim = 5,
                deriv.lim = 10000,
                int.thresh = 0.00000)


#theoretical distrib base
sequencer <- sequenceR(z=z, K=K, sequence=sequence,
                       nX.user.input=nX.user.input,
                       nX.select=nX.select)

#mass calculations
massr <- massR(seq = sequencer, DC=0) #the DC parameter is not actually useful here

opt <- optim(c(10,25, 0.5, 0.9), optimizer,
             z=z, raw.data=pppp, K=K,
             nX.select=nX.select, nrPeaks.user=nrPeaks.user,
             bi=TRUE,
             pp=pp, sequencer=sequencer, massr=massr,
             method = 'L-BFGS-B',
             lower = c(0,0,0,0),
             upper = c(100, 100, 1, 1))


#get abundance of each population
fraction.1 <- opt$par[3]/(opt$par[3]+opt$par[4])
fraction.2 <- opt$par[4]/(opt$par[3]+opt$par[4])

#get NUS of both population
NUS.1 <- opt$par[1]*nX/90 ####NOT SURE ABOUT FORMULA#####
NUS.2 <- opt$par[2]*nX/90

optiplotR(z=z, raw.data=pppp, nX.select=nX.select, K=K, nrPeaks.user=nrPeaks.user,
         sequencer=sequencer, massr=massr, pp=pp)

