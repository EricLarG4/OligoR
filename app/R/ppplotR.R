## ---- ppplotR

ppplotR <- function(
  pp = pp,
  ncol = NULL,
  rescale = 1
){
  p.pp <- ggplot() +
    geom_line(data = pp,
              aes(x = mz, y = intensum),
              size = 1*rescale, color = 'grey') +
    geom_point(data = pp %>%
                 filter(peak > 0),
               aes(x = mz, y = peak),
               size = 2*rescale, color = 'steelblue', alpha = 0.75) +
    geom_line(data = pp %>%
                filter(peak > 0),
              aes(x = mz, y = peak),
              size = 1*rescale, color = 'steelblue', alpha = 0.75,
              linetype = 'dashed') +
    geom_segment(data = pp %>%
                   select(colorscale, Species, mz, intensum) %>%
                   group_by(colorscale, Species) %>%
                   mutate(centroid = weighted.mean(mz, intensum)) %>%
                   select(-mz, -intensum) %>%
                   unique(),
                 aes(x = centroid, xend = centroid, y = 0, yend = 1),
                 color = 'steelblue',
                 size = 1*rescale, alpha = 0.75,
                 linetype = 'dashed') +
    facet_wrap(~colorscale,
               ncol = ncol) +
    # rescale.theme(rescale) +
    ylab('Normalized intensity') +
    xlab('m/z')

  p.pp
}
