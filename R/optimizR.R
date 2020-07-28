
#optimization (minimization) of peakpositionR vs. the peack-picked experimental distribution

optimizer <- function(xDC){
  library(DescTools)
  z = 4

  #raw data import----
  #significantly deuterated T95-2T, TO REPLACE BY THE ACTUAL RAW DATA
  raw.data <- readxl::read_excel('inst/extdata/demodata/optim.xlsx') %>%
    mutate(intensum = 1 - (max(intensum)-intensum)/(max(intensum)-min(intensum)))

  #peak picking----
  pp <- peakpickR(raw.data = raw.data,
                  neighlim = 5,
                  deriv.lim = 10000,
                  int.thresh = 0.00000)

  # p.pp <- ggplot() +
  #   geom_line(data = pp,
  #             aes(x = mz, y = intensum)) +
  #   geom_point(data = pp %>%
  #                filter(peak > 0),
  #              aes(x = mz, y = peak),
  #              size = 2, color = 'steelblue', alpha = 0.5)

  #theoretical distrib base----
  sequencer <- sequenceR(z=4, K=2, sequence="TTGGGTGGGTGGGTGGGT",
                         nX.user.input='',
                         nX.select='C')

  #mass calculations----
  massr <- massR(seq = sequencer, DC=9)


  #theoretical distribution (no optim)
  theo <- peak.positionR(nrPeaks.user=48,
                         DC=xDC, # THE DC VALUE MUST BE INITIALIZED CORRECTLY FOR BINNING BELOW
                         seq=sequencer,
                         MonoMW=massr$MonoMW)

  # p.theo <- p.pp +
  #   geom_point(data = theo,
  #              aes(x=mz.th, y=Iso.Pattern),
  #              size = 2, color = 'tomato', alpha = 0.5)
  # p.theo


  #optimization
  #the goal is to minimize the sum of square of differences between theo$Iso.Pattern et pp$peak

  #first issue is dealing with different x-axis: done by binning by 1/z (use of the rmz variable). That only works if the DC if initialized correctly.
  # Hence, better to do the rounding later (at least for "manual code execution", during optimization it doesn't matter):

  merged <- pp %>%
    mutate(rmz = RoundTo(mz, multiple = 1/z, FUN = round)) %>%
    filter(peak > 0) %>%
    left_join(theo %>%
                mutate(rmz = RoundTo(mz.th, multiple = 1/sequencer$z, FUN = round)),
              by = 'rmz') %>%
    group_by(colorscale, Species, filename, min.time, max.time, min.scan, max.scan, min.mz, max.mz, rmz) %>%
    mutate(diff.2 = (Iso.Pattern - intensum)^2) %>%
    ungroup()


  merge.sum <- merged %>%
    group_by(colorscale, Species, filename, min.time, max.time, min.scan, max.scan, min.mz, max.mz) %>%
    summarise(sum.diff.2 = sum(diff.2))

  output <- as.vector(merge.sum$sum.diff.2)

  return(output)

}


opt.DC <- optimize(optimizer, xDC, lower = 0, upper = 100, maximum = FALSE)
NUS <- opt.DC$minimum*42/90



