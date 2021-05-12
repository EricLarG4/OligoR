


#to be removed once packaged:
# library(tidyverse)
# source("R/peakpickR.R")
# source("R/sequenceR.R")
# source("R/peakpositionR.R")

#optimization (minimization) of peakpositionR vs. the peack-picked experimental distribution

#Optimizer----
optimizer <- function(par, z, raw.data, nX.select, K,
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


