
library(tidyverse)

#optimization (minimization) of peakpositionR vs. the peack-picked experimental distribution-------


# raw data import----
# significantly deuterated T95-2T, TO REPLACE BY THE ACTUAL RAW DATA
ppp <- readxl::read_excel('inst/extdata/demodata/optim.xlsx') %>%
  mutate(intensum = 1 - (max(intensum)-intensum)/(max(intensum)-min(intensum)))


pppp <- readxl::read_excel('inst/extdata/demodata/23TAG-binom-6.xlsx') %>%
  mutate(intensum = 1 - (max(intensum)-intensum)/(max(intensum)-min(intensum))) %>%
  filter(colorscale == 12)

optimizer <- function(par, z, raw.data, sequence, nX.select, K, nrPeaks.user){

  library(DescTools)
  library(tidyverse)
  source("R/peakpickR.R")
  source("R/sequenceR.R")
  source("R/peakpositionR.R")

  #peak picking----
  pp <- peakpickR(raw.data = raw.data,
                  neighlim = 5,
                  deriv.lim = 10000,
                  int.thresh = 0.00000)


  #theoretical distrib base----
  sequencer <- sequenceR(z=z, K=K, sequence=sequence,
                         nX.user.input='',
                         nX.select=nX.select)

  #mass calculations----
  massr <- massR(seq = sequencer, DC=0) #the DC parameter is not actually useful here


  #theoretical distribution (no optim)
  theo.1 <- peak.positionR(nrPeaks.user=nrPeaks.user,
                           DC=par[1],
                           seq=sequencer,
                           MonoMW=massr$MonoMW) %>%
    mutate(Iso.Pattern = Iso.Pattern * par[3])

  theo <- peak.positionR(nrPeaks.user=nrPeaks.user,
                         DC=par[2],
                         seq=sequencer,
                         MonoMW=massr$MonoMW) %>%
    mutate(Iso.Pattern = Iso.Pattern * par[4]) %>%
    rbind(theo.1) %>%
    group_by(mz.th) %>%
    mutate(Iso.Pattern = sum(Iso.Pattern))


  merged <- pp %>%
    mutate(rmz = RoundTo(mz, multiple = 1/z, FUN = round)) %>% #mz rouding to bin theoretical and experimental data together
    filter(peak > 0) %>%
    left_join(theo %>%
                mutate(rmz = RoundTo(mz.th, multiple = 1/sequencer$z, FUN = round)), #joining/binning
              by = 'rmz') %>%
    group_by(colorscale, Species, filename, min.time, max.time, min.scan, max.scan, min.mz, max.mz, rmz) %>%
    mutate(diff.2 = (Iso.Pattern - intensum)^2) %>% #square of differences
    ungroup()


  merge.sum <- merged %>%
    group_by(colorscale, Species, filename, min.time, max.time, min.scan, max.scan, min.mz, max.mz) %>%
    summarise(sum.diff.2 = sum(diff.2)) #sum of squares of differences

  output <- as.vector(merge.sum$sum.diff.2)

  if (is.finite(output)) {
    return(output)
  } else {
    return(42)
  }

}




#using optim----

opt <- optim(c(10,25, 0.5, 0.9), optimizer,
             z=4, raw.data=pppp, K=2, sequence="TAGGGTTAGGGTTAGGGTTAGGG", nX.select='C', nrPeaks.user=48,
             method = 'L-BFGS-B',
             lower = c(0,0,0,0),
             upper = c(100, 100, 1, 1)) #replace upper[1] et upper[2] par nX as calculated before


#get abundance of each population
fraction.1 <- opt$par[3]/(opt$par[3]+opt$par[4])
fraction.2 <- opt$par[4]/(opt$par[3]+opt$par[4])

#retrieve nX (unecessary in app cause already available)
nX <- sequenceR(z=4, K=2, sequence='TAGGGTTAGGGTTAGGGTTAGGG',
                nX.user.input='',
                nX.select=48)$nX

#get NUS of both population
NUS.1 <- opt$par[1]*nX/90 ####NOT SURE ABOUT FORMULA#####
NUS.2 <- opt$par[2]*nX/90







# verifications------------

optiplot <- function(par=opt$par, z, raw.data, sequence, nX.select, K, nrPeaks.user = 48){

  library(DescTools)
  library(tidyverse)

  #peak picking----
  pp <- peakpickR(raw.data = raw.data,
                  neighlim = 5,
                  deriv.lim = 10000,
                  int.thresh = 0.00000)

  p.pp <- ggplot() +
    geom_line(data = pp,
              aes(x = mz, y = intensum)) +
    geom_point(data = pp %>%
                 filter(peak > 0),
               aes(x = mz, y = peak),
               size = 2, color = 'steelblue', alpha = 0.5)

  #theoretical distrib base----
  sequencer <- sequenceR(z=z, K=K, sequence=sequence,
                         nX.user.input='',
                         nX.select=nX.select)

  #mass calculations----
  massr <- massR(seq = sequencer, DC=0)


  #theoretical distribution (no optim)
  theo.1 <- peak.positionR(nrPeaks.user=nrPeaks.user,
                                  DC=par[1],
                                  seq=sequencer,
                                  MonoMW=massr$MonoMW) %>%
    mutate(Iso.Pattern = Iso.Pattern * par[3])

  theo <- peak.positionR(nrPeaks.user=nrPeaks.user,
                           DC=par[2],
                           seq=sequencer,
                           MonoMW=massr$MonoMW) %>%
    mutate(Iso.Pattern = Iso.Pattern * par[4]) %>%
    rbind(theo.1) %>%
    group_by(mz.th) %>%
    mutate(Iso.Pattern = sum(Iso.Pattern))



  p.theo <- p.pp +
    geom_point(data = theo,
               aes(x=mz.th, y=Iso.Pattern),
               size = 2, color = 'tomato', alpha = 0.5)
  p.theo

  return(p.theo)

}

optiplot(z=4, raw.data=pppp, sequence="TAGGGTTAGGGTTAGGGTTAGGG", nX.select='C', K=2, nrPeaks.user=48)


# optiplot(par=c(12,34,0.3,1),
#          z=4, raw.data=pppp, sequence="TAGGGTTAGGGTTAGGGTTAGGG", nX.select='C', K=2, nrPeaks.user=48)

