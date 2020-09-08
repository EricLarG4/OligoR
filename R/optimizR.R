
library(tidyverse)
library(ggthemes)

#to be removed once packaged:
source("R/peakpickR.R")
source("R/sequenceR.R")
source("R/peakpositionR.R")

#optimization (minimization) of peakpositionR vs. the peack-picked experimental distribution-------

#Optimizer----
optimizer <- function(par, z, raw.data,
                      sequence, nX.select, K,
                      nrPeaks.user, bi,
                      pp, sequencer, massr){

  library(DescTools)
  library(tidyverse)

  #theoretical distribution (no optim)
  if (isTRUE(bi)) {
    #bimodal distribution
    #first distribution
    theo.1 <- peak.positionR(nrPeaks.user=nrPeaks.user,
                             DC=par[1], #deuteration of the distribution (optimizable)
                             seq=sequencer,
                             MonoMW=massr$MonoMW) %>%
      mutate(Iso.Pattern = Iso.Pattern * par[3]) #scaling to abundance of the distribution (optimizable)

    theo <- peak.positionR(nrPeaks.user=nrPeaks.user, #second distribution
                           DC=par[2], #deuteration of the distribution (optimizable)
                           seq=sequencer,
                           MonoMW=massr$MonoMW) %>%
      mutate(Iso.Pattern = Iso.Pattern * par[4]) %>% #scaling to abundance of the distribution (optimizable)
      rbind(theo.1) %>% #binding to first distribution
      group_by(mz.th) %>% #grouping by mass
      mutate(Iso.Pattern = sum(Iso.Pattern)) #summing intensities of each mass group to output a single series
  } else {
    #monomodal distribution
    theo <- peak.positionR(nrPeaks.user = nrPeaks.user,
                           DC=par[1], #deuteration of the distribution (optimizable)
                           seq = sequencer,
                           MonoMW = massr$MonoMW)
  }

  #merging of optimized theoretical data to experimental data
  #to calculate sum of squares of differences
  merged <- pp %>%
    #mz rouding to bin theoretical and experimental data together
    mutate(rmz = RoundTo(mz, multiple = 1/z, FUN = round)) %>%
    filter(peak > 0) %>% #removes 0 intensity data
    left_join(theo %>% #joining by mass bins (bins width based on charge z)
                mutate(rmz = RoundTo(mz.th, multiple = 1/sequencer$z, FUN = round)),
              by = 'rmz') %>%
    #grouping by timepoint
    group_by(colorscale, Species, filename, min.time, max.time, min.scan, max.scan, min.mz, max.mz, rmz) %>%
    mutate(diff.2 = (Iso.Pattern - intensum)^2) %>% #square of differences
    ungroup() %>%
    summarise(sum.diff.2 = sum(diff.2)) #sum of squares of differences

  output <- as.vector(merged$sum.diff.2)

  #returns a not so randomly picked number if the output is not finite for some reason
  if (is.finite(output)) {
    return(output)
  } else {
    return(42)
  }

}

# plotting------------

optiplot <- function(par=opt$par, z, raw.data, sequence, nX.select, K, nrPeaks.user, sequencer, massr, pp){

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
             sequence=sequence,
             nX.select='C', nrPeaks.user=nrPeaks.user,
             bi=FALSE,
             pp=pp, sequencer=sequencer, massr=massr,
             method = 'L-BFGS-B',
             lower = c(0,0,0,0),
             upper = c(100, 100, 1, 1))

#get NUS
NUS.1 <- opt$par[1]*nX/90 ####NOT SURE ABOUT FORMULA#####

optiplot(z=z, raw.data=pppp, sequence=sequence, nX.select=nX.select, K=K, nrPeaks.user=nrPeaks.user,
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
             sequence=sequence,
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

optiplot(z=z, raw.data=pppp, sequence=sequence, nX.select=nX.select, K=K, nrPeaks.user=nrPeaks.user,
         sequencer=sequencer, massr=massr, pp=pp)
