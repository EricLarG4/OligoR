

optiplotR <- function(p.pp = p.pp,
                      opt = opt,
                      binom.NUS = binom.NUS,
                      comp.mode = FALSE,
                      distrib.select = 'auto',
                      distribs = NA,
                      bi.penalty = 1){

  #libraries----
  library(tidyverse)
  library(ggthemes)
  library(ggsci)

  #data filtering----

  ##automatic data filtering based on fn.value----
  if(distrib.select == 'auto' & isFALSE(comp.mode)){

    opt <- opt %>%
      #apply penalty for bimodal distribution (default: no penalty)
      mutate(
        fn.value = if_else(
          distrib=='bi',
          bi.penalty*fn.value,
          fn.value
        )
      ) %>%
      #keep smallest fn.value
      group_by(Species, colorscale) %>%
      filter(fn.value == min(fn.value))

    binom.NUS <- binom.NUS %>%
      #apply penalty for bimodal distribution (default: no penalty)
      mutate(
        fn.value = if_else(
          distrib=='bi',
          bi.penalty*fn.value,
          fn.value
        )
      ) %>%
      #keep smallest fn.value
      group_by(Species, colorscale) %>%
      filter(fn.value == min(fn.value))

  }

  ##manual data filtering based on user input (data frame)----
  if(distrib.select != 'auto' & isFALSE(comp.mode)){

    opt <- opt %>%
      #join user input
      left_join(
        ., distribs %>%
          magrittr::set_colnames(c('colorscale', 'choice')),
        by = 'colorscale'
      ) %>%
      #filter based on user choice
      group_by(colorscale) %>%
      filter(distrib == choice)

    binom.NUS <- binom.NUS %>%
      #join user input
      left_join(
        ., distribs %>%
          magrittr::set_colnames(c('colorscale', 'choice')),
        by = 'colorscale'
      ) %>%
      #filter based on user choice
      group_by(colorscale) %>%
      filter(distrib == choice)

  }

  #plot----
  p.opt <- p.pp +
    geom_point(data = opt,
               aes(x = mz.th, y = iso.1),
               size = 2, color = 'lightgoldenrod4', alpha = 0.75) +
    geom_line(data = opt,
              aes(x = mz.th, y = iso.1),
              size = 1, color = 'lightgoldenrod4', alpha = 0.75) +
    geom_point(data = opt,
               aes(x = mz.th, y = iso.2),
               size = 2, color = 'seagreen4', alpha = 0.75) +
    geom_line(data = opt,
              aes(x = mz.th, y = iso.2),
              size = 1, color = 'seagreen4', alpha = 0.75)  +
    geom_point(data = opt,
               aes(x = mz.th, y = iso),
               size = 2, color = 'tomato', alpha = 0.75) +
    geom_line(data = opt,
              aes(x = mz.th, y = iso),
              size = 1, color = 'tomato', alpha = 0.75) +
    geom_segment(data = raw.data %>%
                   group_by(colorscale, Species) %>%
                   mutate(centroid = weighted.mean(mz, intensum)),
                 aes(x = centroid, xend = centroid, y = 0, yend = 1),
                 color = 'steelblue',
                 size = 1, alpha = 0.75,
                 linetype = 'dashed') +
    geom_segment(data = opt,
                 aes(x = centroid, xend = centroid, y = 0, yend = 1),
                 color = 'tomato',
                 size = 1, alpha = 0.75,
                 linetype = 'dashed')  +
    geom_segment(data = opt,
                 aes(x = centroid.1, xend = centroid.1, y = 0, yend = 1),
                 color = 'lightgoldenrod4',
                 size = 1, alpha = 0.75,
                 linetype = 'dashed') +
    geom_segment(data = opt,
                 aes(x = centroid.2, xend = centroid.2, y = 0, yend = 1),
                 color = 'seagreen4',
                 size = 1, alpha = 0.75,
                 linetype = 'dashed') +
    geom_label(
      data = binom.NUS,
      aes(
        x = centroid.1, y = 1.05,
        label = paste0(round(USE.1, 1), " NUS\n(", round(fraction.1*100,1), '%)')
      ),
      size = 5, fontface = 'bold', color = 'lightgoldenrod4', alpha = 1
    ) +
    geom_label(
      data = binom.NUS,
      aes(
        x = centroid.2, y = 1.05,
        label = paste0(round(USE.2, 1), " NUS\n(", round(fraction.2*100,1), ')%')
      ),
      size = 5, fontface = 'bold', color = 'seagreen4', alpha = 1
    ) +
    geom_label(
      data = binom.NUS,
      aes(
        x = centroid, y = 1.1,
        label = paste0(round(USE.mean, 1), " NUS")
      ),
      size = 5, fontface = 'bold', color = 'tomato', alpha = 1
    ) +
    scale_y_continuous(n.breaks = 6)

  ##faceting for comparison mode----
  if(isTRUE(comp.mode)){

    p.opt <- p.opt +
      geom_text(
        data = binom.NUS,
        show.legend = FALSE,
        aes(
          color = fn.value,
          label = paste0('fn.value\n', signif(fn.value, 4))
        ),
        x = Inf, y = Inf,
        hjust = 1, vjust = 1,
        size = 5, fontface = 'bold'
      ) +
      scale_color_gradient(low = 'steelblue4', high = 'tomato') +
      facet_grid(distrib ~ round(colorscale,2))

  } else {

    ##faceting for normal mode----
    p.opt <- p.opt +
      facet_wrap(~round(colorscale,2))
  }

}
