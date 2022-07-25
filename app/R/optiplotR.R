## ---- optiplotR

optiplotR <- function(
  p.pp = p.pp,
  opt = opt,
  binom.NUS = binom.NUS,
  comp.mode = FALSE,
  distrib.select = 'auto',
  alpha = 0.8,
  bi.t.limit = NA,
  # bi.penalty = 1,
  deconv.label = TRUE,
  overall.label = FALSE,
  ncol = NULL,
  rescale = 1
){

  #libraries----
  library(tidyverse)
  library(ggthemes)
  library(ggsci)

  #data filtering----

  ##automatic data filtering based on RSS----
  if(distrib.select == 'auto' & isFALSE(comp.mode)){

    opt <- opt %>%
      filter(
        distrib == 'mono' & p.F >= alpha |
          distrib == 'bi' & p.F < alpha
      )

    binom.NUS <- binom.NUS %>%
      filter(
        distrib == 'mono' & p.F >= alpha |
          distrib == 'bi' & p.F < alpha
      )

  }

  ##manual data filtering based on user input (data frame)----
  if(distrib.select != 'auto' & isFALSE(comp.mode)){

    opt <- opt %>%
      filter(
        distrib == 'mono' & colorscale > bi.t.limit |
          distrib == 'bi' & colorscale <= bi.t.limit
      )

    binom.NUS <- binom.NUS %>%
      filter(
        distrib == 'mono' & colorscale > bi.t.limit |
          distrib == 'bi' & colorscale <= bi.t.limit
      )

  }


  #plot----
  p.opt <- p.pp +
    geom_point(data = opt,
               aes(x = mz.th, y = iso.1),
               size = 2*rescale, color = '#8b2e62', alpha = 0.75) +
    geom_line(data = opt,
              aes(x = mz.th, y = iso.1),
              size = 1*rescale, color = '#8b2e62', alpha = 0.75) +
    geom_point(data = opt,
               aes(x = mz.th, y = iso.2),
               size = 2*rescale, color = 'seagreen4', alpha = 0.75) +
    geom_line(data = opt,
              aes(x = mz.th, y = iso.2),
              size = 1*rescale, color = 'seagreen4', alpha = 0.75)  +
    geom_point(data = opt,
               aes(x = mz.th, y = iso),
               size = 2*rescale, color = 'tomato', alpha = 0.75) +
    geom_line(data = opt,
              aes(x = mz.th, y = iso),
              size = 1*rescale, color = 'tomato', alpha = 0.75) +
    geom_segment(data = opt %>% distinct(., colorscale, Species, distrib, centroid),
                 aes(x = centroid, xend = centroid, y = 0, yend = 1),
                 color = 'tomato',
                 size = 1*rescale, alpha = 0.75,
                 linetype = 'dashed')  +
    geom_segment(data = opt %>% distinct(., colorscale, Species, distrib, centroid.1),
                 aes(x = centroid.1, xend = centroid.1, y = 0, yend = 1),
                 color = '#8b2e62',
                 size = 1*rescale, alpha = 0.75,
                 linetype = 'dashed') +
    geom_segment(data = opt %>% distinct(., colorscale, Species, distrib, centroid.2),
                 aes(x = centroid.2, xend = centroid.2, y = 0, yend = 1),
                 color = 'seagreen4',
                 size = 1*rescale, alpha = 0.75,
                 linetype = 'dashed') +
    scale_y_continuous(breaks = c(0,0.5,1))

  if(isTRUE(deconv.label)){
    p.opt <- p.opt +
      geom_label(
        data = binom.NUS,
        aes(
          x = centroid.1, y = 1.05,
          label = paste0(round(USE.1, 1), " NUS\n(", round(fraction.1*100,1), '%)')
        ),
        size = 3.5*rescale, fontface = 'bold', color = '#8b2e62', alpha = 1
      ) +
      geom_label(
        data = binom.NUS,
        aes(
          x = centroid.2, y = 1.05,
          label = paste0(round(USE.2, 1), " NUS\n(", round(fraction.2*100,1), ')%')
        ),
        size = 3.5*rescale, fontface = 'bold', color = 'seagreen4', alpha = 1
      )
  }

  if(isTRUE(overall.label)){
    p.opt <- p.opt +
      geom_label(
        data = binom.NUS,
        aes(
          x = centroid, y = 1.1,
          label = paste0(round(USE.mean, 1), " NUS")
        ),
        size = 3.5*rescale, fontface = 'bold', color = 'tomato', alpha = 1
      ) 
  }


  ##faceting for comparison mode----
  if(isTRUE(comp.mode)){

    p.opt <- p.opt +
      geom_text(
        data = binom.NUS,
        show.legend = FALSE,
        aes(
          color = RSS,
          label = paste0('RSS: ', signif(RSS, 4), '\np(F-test): ', signif(p.F, 1))
        ),
        x = Inf, y = Inf,
        hjust = 1, vjust = 1,
        size = 3.5*rescale, fontface = 'bold'
      ) +
      scale_color_gradient(low = 'steelblue4', high = 'tomato') +
      facet_grid(distrib ~ round(colorscale,2))

  } else {

    ##faceting for normal mode----
    p.opt <- p.opt +
      geom_text(
        data = binom.NUS,
        show.legend = FALSE,
        aes(
          color = p.F,
          label = paste0('p (F-test): ', signif(p.F, 1))
        ),
        x = Inf, y = Inf,
        hjust = 1, vjust = 1,
        size = 3.5*rescale, fontface = 'bold'
      ) +
      scale_color_gradient(low = 'steelblue4', high = 'tomato') +
      facet_wrap(~round(colorscale,2),
                 ncol = ncol)
  }

}
