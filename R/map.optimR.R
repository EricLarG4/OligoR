
map.optimR <- function(input.data, method = 'L-BFGS-B', lmm = 10, DC.final=0, DC.init=100, init.par = c(10,35,0.2,0.9)){


  if(method == 'L-BFGS-B'){
    #Methods with bounds----

    ## bimodal fit----
    par.bi <- optim(
      par = init.par,
      optimizer,
      bi = TRUE,
      pp = input.data,
      z=z, K=K,
      nX.select=nX.select,
      nrPeaks.user=nrPeaks.user,
      seq=seq,
      massr=massr,
      method = method, #L-BFGS-B': Byrd et. al. (1995), allows box constraints.
      lower = c(DC.final, DC.final, 0, 0),
      upper = c(DC.init, DC.init, 1, 1),
      control = list(
        trace = 0,
        lmm = lmm
      )
    )

    par.bi <- as.data.frame(par.bi$par) %>%
      data.table::transpose() %>%
      magrittr::set_colnames(c('DC1', 'DC2', 'ab1', 'ab2')) %>%
      mutate(
        convergence = par.bi$convergence,
        fn.value = par.bi$value,
        distrib = 'bi'
      )

    ## monomodal fit----
    par.mono <- optim(
      par = c(init.par[1], 1),
      optimizer,
      bi = FALSE,
      pp = input.data,
      z=z, K=K,
      nX.select=nX.select,
      nrPeaks.user=nrPeaks.user,
      seq=seq,
      massr=massr,
      method = method, #Byrd et. al. (1995), allows box constraints.
      lower = c(DC.final, 0),
      upper = c(DC.init, 1),
      control = list(
        trace = 0,
        lmm = lmm)
    )

    par.mono <- as.data.frame(par.mono$par) %>%
      data.table::transpose() %>%
      magrittr::set_colnames(c('DC1', 'ab1')) %>%
      mutate(
        convergence = par.mono$convergence,
        fn.value = par.mono$value,
        distrib = 'mono'
      )

    return(list(par.mono, par.bi))


  } else {

    #Methods without bounds----

    ## bimodal fit----
    par.bi <- optim(
      par = init.par,
      optimizer,
      bi = TRUE,
      pp = input.data,
      z=z, K=K,
      nX.select=nX.select,
      nrPeaks.user=nrPeaks.user,
      seq=seq,
      massr=massr,
      method = method,
      control = list(trace = 0)
    )

    par.bi <- as.data.frame(par.bi$par) %>%
      data.table::transpose() %>%
      magrittr::set_colnames(c('DC1', 'DC2', 'ab1', 'ab2')) %>%
      mutate(
        convergence = par.bi$convergence,
        fn.value = par.bi$value,
        distrib = 'bi'
      )

    ## monomodal fit----
    par.mono <- optim(
      par = c(init.par[1], 1),
      optimizer,
      bi = FALSE,
      pp = input.data,
      z=z, K=K,
      nX.select=nX.select,
      nrPeaks.user=nrPeaks.user,
      seq=seq,
      massr=massr,
      method = method,
      control = list(trace = 0)
    )

    par.mono <- as.data.frame(par.mono$par) %>%
      data.table::transpose() %>%
      magrittr::set_colnames(c('DC1', 'ab1')) %>%
      mutate(
        convergence = par.mono$convergence,
        fn.value = par.mono$value,
        distrib = 'mono'
      )

    return(list(par.mono, par.bi))


  }

}


