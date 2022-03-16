#Dependencies----

##libraries----

# BiocManager::install("mzR")

librarian::shelf(
  tidyverse, readr, readxl, data.table, DT, tidytable, magrittr, stringr,
  formattable, gnm, DescTools,
  ggpubr, ggrepel, ggthemes, ggpmisc, thematic, zoo,
  BiocManager, V8, mzR
)

##custom functions----

###Theoretical isotopic distributions----
source("R/peakpositionR.R")
source("R/distributR.R")
source("R/sequenceR.R")
source("R/massR.R")

###Peak picking and modeling----
source("R/peakpickR.R")
source("R/ppf.R")
source("R/optimizR.R")
source("R/map.optimR.R")
source("R/optf.R")
source("R/binomNUSf.R")
# source("R/custom theme.R")

## theme----
custom.theme <- theme(
  panel.background = element_blank(),
  strip.background = element_blank(),
  legend.position = 'bottom',
  legend.background = element_blank(),
  legend.box.background = element_blank(),
  legend.key=element_blank(),
  legend.text = element_text(size = 18),
  legend.title = element_text(size = 22),
  axis.text = element_text(size = 18),
  axis.title = element_text(size = 22),
  strip.text = element_text(size = 20),
  axis.line = element_line(size = 0.5)
)

thematic_rmd(
  bg = '#272c30', fg = '#EEE8D5', accent = 'auto',
  sequential = hcl.colors(n = 42, palette = 'viridis')
)



#server---------
server <- function(input, output, session) {

  #OligoR----------

  sequencer <- reactive({
    sequenceR(
      z = input$z,
      K = input$K,
      sequence = input$sequence,
      mol = input$mol,
      nX.user.input = input$nX.user,
      nX.select = input$nX.select
    )
  })

  output$nb_PO <- renderText(sequencer()$nb_PO)

  output$nb_POH <- renderText(sequencer()$nb_POH)

  output$chem.formula <- renderText({
    if (!is.na(sequencer()$nH)) {
      paste(
        tags$b(style="color:tomato", 'Chemical formula: '),
        tags$span(style="color:tomato", "C"), tags$sub(style="color:tomato", sequencer()$nC),
        tags$span(style="color:tomato", "H"), tags$sub(style="color:tomato", sequencer()$nH),
        tags$span(style="color:tomato", "O"), tags$sub(style="color:tomato", sequencer()$nO),
        tags$span(style="color:tomato", "N"), tags$sub(style="color:tomato", sequencer()$nN),
        tags$span(style="color:tomato", "P"), tags$sub(style="color:tomato", sequencer()$nP),
        tags$span(style="color:tomato", "K"), tags$sub(style="color:tomato", sequencer()$nK),
        sep = ''
      )
    }
  })

  output$nX <- renderText(sequencer()$nX)

  #Mass calculations---------

  massr <- reactive({
    massR(seq=sequencer(),
          DC = as.numeric(input$DC),
          K41C = as.numeric(input$K41C))
  })

  oligo.data <- reactive({

    data.frame(
      "Parameters" = c('phosphates', 'neutralized phosphates', 'exchangeable sites', 'monoisotopic mass',
                       'average mass', 'monoisotopic m/z', 'average m/z'),
      "Values" = c(sequencer()$nb_PO, sequencer()$nb_POH, sequencer()$nX, massr()$MonoMW, massr()$AveMW, massr()$Monomz, massr()$Avemz)
    )
  })

  output$oligo.data <- renderDT(server = FALSE, {
    datatable(
      oligo.data(),
      style = "bootstrap",
      extensions = c('Buttons', 'Responsive', 'Scroller'),
      rownames = F,
      escape = T,
      filter = 'top',
      autoHideNavigation = T,
      options = list(
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        autoWidth = F,
        dom = 'Bfrtip', #button position
        buttons = c('copy', 'csv', 'excel') #buttons
      )
    ) %>%
      formatRound(c('Values'), digits = 5, interval = 3, mark = '') %>%
      formatStyle(
        columns = 0:6,
        target = 'row',
        background = '#272c30'
      )

  })


  #FFT-----------
  peak.position <- reactive({
    peak.positionR(nrPeaks.user = as.numeric(input$nrPeaks.user),
                   DC = as.numeric(input$DC),
                   K41C = as.numeric(input$K41C),
                   seq = sequencer(),
                   MonoMW = massr()$MonoMW)
  })

  #peak plotting-----

  output$peak.position <- renderDT(server = FALSE, {
    datatable(
      peak.position(),
      style = "bootstrap",
      extensions = c('Buttons', 'Responsive', 'Scroller'),
      rownames = F,
      escape = T,
      filter = 'top',
      autoHideNavigation = T,
      colnames = c('m/z' = 'mz.th',
                   'Abundance' = 'Iso.Pattern'),
      options = list(
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        pageLength = 6,
        autoWidth = F,
        dom = 'Bfrtip', #button position
        buttons = c('copy', 'csv', 'excel') #buttons
      )
    ) %>%
      formatRound(c('m/z'), digits = 5) %>%
      formatRound(c('Abundance'), digits = 3) %>%
      formatStyle(
        columns = 0:6,
        target = 'row',
        background = '#272c30'
      )


  })

  # output$peaks <- renderDT({
  #   MSsnaps.ref()
  # })

  output$p.hdx.ref <- renderPlot({
    ggplot(data = peak.position(), aes(x = mz.th, y = Iso.Pattern)) +
      geom_line(color = input$col.line.th, size = input$size.line.th) +
      geom_point(color = input$col.dot.th, size = input$size.dot.th) +
      geom_vline(xintercept = massr()$Avemz, linetype = 'dashed', color = input$col.centroid.th, size = input$size.centroid.th) +
      xlab("m/z") +
      ylab('normalized abundance') +
      custom.theme
  })

  p.hdx.ref.vs.exp <- reactive({
    ggplot(data = peak.position(),
           aes(x = mz.th, y = Iso.Pattern)) +
      geom_line(color = input$col.line.th,
                size = input$size.line.th) +
      geom_point(color = input$col.dot.th,
                 size = input$size.dot.th) +
      geom_vline(xintercept = massr()$Avemz,
                 linetype = 'dashed',
                 color = input$col.centroid.th,
                 size = input$size.centroid.th) +
      geom_vline(xintercept = exp.centroid.ref(),
                 linetype = 'dashed',
                 color = input$col.centroid.exp,
                 size = input$size.centroid.th) +
      geom_line(data = MSsnaps.ref(),
                aes(x = mz, y = 1 - (max(intensum)-intensum)/(max(intensum)-min(intensum))),
                inherit.aes = F,
                color = input$col.line.exp,
                size = input$size.line.exp) +
      annotate(
        geom="text", x=Inf, y=0.90,
        label = paste('Reference centroid: ', round(exp.centroid.ref(),5),' m/z\nAccuracy: ', round(centroid.ac(), 0), ' ppm', sep = ""),
        hjust = 1,
        color="#6BC392", size=5, fontface = 2
      ) +
      xlab("m/z") +
      ylab('normalized abundance') +
      custom.theme
  })

  output$p.hdx.ref.vs.exp <- renderPlot({
    p.hdx.ref.vs.exp()
  })

  output$ref.accu.pdf <- downloadHandler(
    filename = function() { paste("stacked spectra", '.pdf', sep='') },
    content = function(file) {
      ggsave(file, plot = p.hdx.ref.vs.exp(), device = "pdf",
             width = 200,
             height = 100,
             units = 'mm',
             dpi = 600)
    }
  )

  output$ref.accu.png <- downloadHandler(
    filename = function() { paste("stacked spectra", '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = p.hdx.ref.vs.exp(), device = "png",
             # width = 25,
             # height = 8,
             # units = 'mm',
             dpi = 300)
    }
  )

  #MS ref snapshots------------
  snaps.ref <- data.frame()

  inputsnap.ref <- reactive({
    if (isTRUE(input$switch69)) {
      inputsnap.ref <- specsumbrsh.ms()
      return(inputsnap.ref)
    } else {
      inputsnap.ref <- specsumtxt()
      return(inputsnap.ref)
    }
  })


  MSsnaps.ref <- eventReactive(input$bttn99, {
    newrow.ref <- data.frame(inputsnap.ref())
    # snaps.ref <<- rbind(snaps.ref, newrow.ref)
  })

  #determination of experimental centroid of the reference
  exp.centroid.ref <- reactive({

    calculation <- MSsnaps.ref() %>%
      mutate(centroid = sum(mz * intensum)/sum(intensum))

    exp.centroid.ref <- calculation$centroid[1]
  })

  #Accuracy of the reference centroid with theory, in ppm
  centroid.ac <- reactive({
    1000000 * abs(exp.centroid.ref()-massr()$Avemz)/massr()$Avemz
  })


  #max file size---------
  options(shiny.maxRequestSize=5000*1024^2)

  mzlimits = c(400,4000) #hard limit on data range

  #data import---------------

  inFile <- reactive({
    input$file1
  })

  ms <- reactive({
    openMSfile(inFile()$datapath)
  })

  hd <- reactive({
    header(ms())
  })

  header.dim <- reactive({
    dim(hd())
  })

  last.scan <- reactive({
    header.dim()[1]
  })

  id <- reactive({
    1:last.scan()
  })

  ret.time <- reactive({
    data.frame(hd()[7])
  })

  sample.name <- "Sample 1"

  inputms.collection <- reactive({
    #Progress bar. Appears upon file import.
    withProgress(message = 'Reading data',
                 detail = 'Please wait', value = 0, {

                   incProgress(amount=1/5)

                   #extraction of ms data, binding of scan number, retention time
                   df.temp <- lapply(id(),function(i) {

                     #Dataframe version - archived
                     # init <- data.frame(peaks(ms(), i))  %>%
                     #   add_column(scan = i) %>%
                     #   add_column(ret.time = ret.time()[i,]/60)

                     init <- data.table(peaks(ms(), i))
                     init[, scan := i]
                     init[, ret.time := ret.time()[i,]/60]

                   })

                   incProgress(amount=2/5)

                   #binding to single datatable
                   filling.df <- data.table::rbindlist(df.temp)

                   incProgress(amount=3/5)

                   #traceability

                   filling.df[,file := inFile()$name]

                   incProgress(amount=4/5)

                   #naming
                   # colnames(filling.df)[1:5] <- c("mz","intensity","scan", "time",'filename')
                   setnames(filling.df, old = c(1:ncol(filling.df)), new = c("mz","intensity","scan", "time",'filename'))

                   return(filling.df)

                   incProgress(amount=5/5)
                 })
  })

  inputms.bin <- reactive({

    withProgress(message = 'Binning in progress',
                 detail = 'Please wait', value = 0, {

                   incProgress(amount=1/2)

                   inputms.bin <- inputms.collection()

                   inputms.bin$mz =  RoundTo(inputms.bin$mz, multiple = input$slider1, FUN = round)

                   return(inputms.bin)

                   incProgress(amount=2/2)
                 })
  })

  inputms <- reactive({

    withProgress(message = 'Filtering in progress',
                 detail = 'Please wait', value = 0, {

                   incProgress(amount=1/2)

                   inputms <- inputms.bin()[mz > input$text11[1] & mz < input$text11[2]]

                   return(inputms)

                   incProgress(amount=2/2)
                 })
  })

  #diagnostics
  # output$inputms69 <- renderDT({
  #   inputms()
  # })


  #TIC------------

  TIC <- reactive({

    TIC <- data.frame(hd()[7]/60, hd()[2], hd()[6])

    colnames(TIC)[1:3] <- c("time","scan","intensity")

    return(TIC)
  })


  output$plot1 <- renderPlot({

    req(input$file1)

    ggplot(data = TIC(), aes(x = time, y = intensity)) +
      geom_line(color = input$col.TIC, size = input$size.line.TIC) +
      xlab("time (min)") +
      custom.theme
  }, bg = "transparent")

  #Selection of MS data from first plot (TIC)-------
  selectedData <- reactive({
    brushedPoints(TIC(), input$plot_brush)
  })

  #textinput-------------
  scanstxt <- reactive({
    input$text1:input$text2
  })

  mztxt <- reactive({
    input$text3:input$text4
  })

  #brushinput-----------
  scansbrsh <- reactive({
    min(selectedData()$scan):max(selectedData()$scan)
  })

  #Selection of defined scans in MS data---------
  selecscanstxt <- reactive({
    inputms() %>%
      filter(scan %in% scanstxt())
  })

  selecscansbrsh <- reactive({
    as.data.table(inputms())[scan %in% scansbrsh()]

  })

  #time management--------

  time.min <- reactive({
    as.numeric(input$time.sec) / 60
  })

  timemin <- reactive({
    paste(
      tags$b(style="color:grey", 'Manual time (min): '),
      tags$b(style="color:grey", round(time.min(), 2)),
      sep = ''
    )
  })

  output$timemin <- renderText({
    timemin()
  })

  #summing of scans-----------

  specsumtxt <- reactive({
    selecscanstxt() %>%
      filter(mz > min(mztxt())) %>%
      filter(mz < max(mztxt())) %>%
      group_by(mz, filename) %>%
      summarise("intensum" = sum(intensity)) %>%
      add_column("mean.time" = mean(selecscanstxt()$time) + as.numeric(input$deadtxt)) %>%
      add_column("Species" = input$sample.id) %>%
      add_column('CFtime' = time.min()) %>%
      add_column('lgd.conc' = as.numeric(input$lgd.conc)) %>%
      add_column('Stoich' = as.numeric(input$Stoich))
  })

  specsumbrsh <- reactive({

    selecscansbrsh <- selecscansbrsh()[, keyby = .(mz, filename),
                                       .(intensum = sum(intensity))][, ':=' (
                                         mean.time = mean(selecscansbrsh()$time) + as.numeric(input$deadtxt),
                                         Species = input$sample.id,
                                         CFtime = time.min(),
                                         lgd.conc = as.numeric(input$lgd.conc),
                                         Stoich = as.numeric(input$Stoich)
                                       )]


  })

  #Definition of initial mz range-------
  ranges <- reactiveValues(x = mzlimits, y = NULL)   #place above to save on calculation time

  #Zoomed MS spectrum from brush----------------
  output$plot3 <- renderPlot({

    req(input$file1)

    ggplot(data = specsumbrsh(), aes(x = mz, y = intensum)) +
      geom_line(color = input$col.MS, size = input$size.line.MS) +
      xlab("m/z") +
      custom.theme +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
  })


  #Zoomed MS spectrum from txt input----------
  output$plot4 <- renderPlot({

    req(input$file1)

    ggplot(data = specsumtxt(), aes(x = mz, y = intensum)) +
      geom_line(color = input$col.MS, size = input$size.line.MS) +
      xlab("m/z") +
      custom.theme +
      coord_cartesian(xlim = c(min(mztxt()), max(mztxt())), expand = FALSE)
  })

  #zoom event on MS plot---------
  observeEvent(input$plot3_brush, {
    brush <- input$plot3_brush
    ranges$x <- c(brush$xmin, brush$xmax)
    ranges$y <- c(brush$ymin, brush$ymax)
  })

  specsumbrsh.ms <- reactive({

    specsumbrsh.ms <- specsumbrsh()[mz > min(ranges$x) & mz < max(ranges$x)]

  })

  # Double click event to reset the zoom
  observeEvent(input$plot3_dblclick, {
    brush <- input$plot3_brush
    ranges$x <- mzlimits
    ranges$y <- NULL
  })

  #Prints selected scans and time range from brush---------
  output$info <- renderText({

    req(input$file1)

    paste(round(min(selectedData()$time), 2), "-", round(max(selectedData()$time), 2), " min",
          sep = "")
  })

  output$info1 <- renderText({

    req(input$file1)

    paste(min(selectedData()$scan), "-", max(selectedData()$scan),
          sep = "")
  })

  #Prints selected scans at bottom of window-------
  output$info2 <- renderText({
    paste(round(min(ranges$x), 2), " - ", round(max(ranges$x), 2), sep = "")
  })

  #Prints selected mz at bottom of window----------
  output$info3 <- renderText({
    paste(round(min(mztxt()), 2), " - ", round(max(mztxt()), 2), sep = "")
  })


  #MS snapshots------------
  snaps <- data.frame()

  inputsnap <- reactive({
    if (isTRUE(input$switch69)) {

      inputsnap <- specsumbrsh.ms()[, ':=' (
        min.time = min(selectedData()$time), #traceability
        max.time = max(selectedData()$time),
        min.scan = min(selectedData()$scan),
        max.scan = max(selectedData()$scan),
        min.mz = min(specsumbrsh.ms()$mz),
        max.mz = max(specsumbrsh.ms()$mz)
      )]

      return(inputsnap)

    } else {
      inputsnap <- specsumtxt() %>%
        add_column(min.time = "NA", #traceability
                   max.time = "NA",
                   min.scan = input$text1,
                   max.scan = input$text2,
                   min.mz = input$text3,
                   max.mz = input$text4)
      return(inputsnap)
    }
  })


  MSsnaps <- eventReactive(input$bttn1, {
    newrow <- data.frame(inputsnap())
    snaps <<- rbind(snaps, newrow)

    #     newrow <- data.table(inputsnap())
    #     snaps <<- rbindlist(list(snaps, newrow))

  })


  #--------------KINETICS--------------

  #Processed data reimport
  kin.old <- reactive({
    if(is.null(input$kin.old))
      return(NULL)

    input$kin.old
  })

  processed.kin <- reactive({

    if(is.null(input$kin.old))
      return(NULL)

    processed.kin <- data.frame(read_excel(kin.old()$datapath,
                                           skip = 1))

    colnames(processed.kin) <- c('filename.x',
                                 'Species.x',
                                 'name',
                                 'mz.range.x',
                                 'scan',
                                 'group',
                                 'time.x',
                                 'corrected.time',
                                 'intensity.x',
                                 'intensity.y',
                                 'corr.int',
                                 'mean.time',
                                 'mean.raw',
                                 'mean.corr')

    processed.kin <- processed.kin %>%
      mutate(group = round(scan/as.numeric(input$text35), 0)) %>%
      group_by(name, group) %>%
      mutate(mean.raw = mean(intensity.x),
             mean.corr = mean(corr.int),
             mean.time = mean(corrected.time)) %>%
      filter(
        mean.time >= as.numeric(input$text33),
        mean.time <= as.numeric(input$text34)
      )

    return(processed.kin)

  })


  snaps42 <- data.frame()

  kin.brsh <- reactive({
    selecscansbrsh()
    # group_by(mz, filename) %>%
    # add_column("Species" = input$sample.id)
  })


  k.spectra <- eventReactive(input$bttn42, {

    k.init <- data.frame(kin.brsh()) %>%
      filter(mz > min(ranges$x)) %>%
      filter(mz < max(ranges$x)) %>%
      add_column("Species" = input$sample.id) %>%
      add_column(mz.range = paste0(round(min(ranges$x),2),"-",round(max(ranges$x), 2)))

    newrow42 <-  data.frame(k.init)

    snaps42 <<- rbind(snaps42, newrow42)
  })


  k.data <- reactive({

    k.spectra() %>%
      group_by(filename, Species, scan, time, mz.range, mz) %>%
      mutate(prod.int = intensity*mz) %>%
      group_by(filename, Species, scan, time, mz.range) %>%
      summarise(
        intensity = sum(intensity),
        centroid = sum(prod.int)/sum(intensity)
      )
  })


  k.norm <- reactive({

    k.standard <- k.data() %>%
      filter(Species == 'Standard')

    k.spl <- k.data() %>%
      filter(Species != 'Standard')

    k.joined <- left_join(k.spl, k.standard, by = "scan")

    if (is.na(k.joined$intensity.y[1])) {
      k.joined$intensity.y <- 1
    }

    k.joined <- k.joined %>%
      filter(Species.x %in% input$Pick1) %>%
      group_by(scan, filename.x, time.x) %>% #filename.x grouping necessary to use proper standard?
      mutate(corr.int = intensity.x/intensity.y) %>%
      mutate(corrected.time = time.x + as.numeric(input$deadtxt)) %>%
      dplyr::select(-Species.y, -filename.y, -time.y, -mz.range.y, -centroid.y) %>%
      mutate(group = round(scan/as.numeric(input$text35), 0)) %>%
      group_by(Species.x, group) %>%
      mutate(mean.raw = mean(intensity.x),
             mean.corr = mean(corr.int),
             mean.time = mean(corrected.time)) %>%
      filter(mean.time >= as.numeric(input$text33)) %>%
      filter(mean.time <= as.numeric(input$text34))

    return(k.joined)

  })

  k.norm.1 <- reactive({
    return(NULL)
  })

  k.norm.1 <- reactive({
    k.norm() %>%
      ungroup() %>%
      mutate(name = case_when(
        Species.x == 'Species 1' ~ input$text36,
        Species.x == 'Species 2' ~ input$text37,
        Species.x == 'Species 3' ~ input$text38,
        Species.x == 'Species 4' ~ input$text39,
        Species.x == 'Species 5' ~ input$text40,
        Species.x == 'Species 6' ~ input$text41,
        Species.x == 'Species 7' ~ input$text42,
        Species.x == 'Species 8' ~ input$text43
      )
      ) %>%
      setcolorder(c(1,2,14,5,3,10,4,9,6,7,8,13,11,12))
  })

  # k.norm.0 <- reactive({
  #   if (is.null(kin.brsh())) {
  #     if (is.null(kin.old)) {
  #       return(NULL)
  #     } else {
  #       return(processed.kin())
  #     }
  #   } else {
  #     if (is.null(kin.old)) {
  #       return(k.norm.1())
  #     } else {
  #       rbind.data.frame(k.norm.1(),processed.kin())
  #     }
  #   }
  # })

  k.norm.0 <- reactive({
    if (is.null(kin.old)) {
      return(k.norm.1())
    } else {
      rbind.data.frame(k.norm.1(),processed.kin())
    }
  })


  # Selection of the y axis
  kin.input <- reactive({
    if (input$kin.input == 'raw') {
      k.norm.0()$mean.raw
    } else {
      if(input$kin.input == 'corrected'){
        k.norm.0()$mean.corr
      } else {
        k.norm.0()$centroid.x
      }
    }
  })

  # generation of a wide table, easier to work with with other softwares.
  # only a few columns kept.
  k.wide <- eventReactive(input$k.wide.bttn,{
    if (input$kin.input == 'raw') {
      k.norm.0()[,c(3,12,13)] %>%
        unique() %>% #removes duplicated lines that appear when averaging scans
        pivot_wider(names_from = name,
                    values_from = mean.raw) %>%
        round(2)
    } else {
      k.norm.0()[,c(3,12,14)] %>%
        unique() %>%
        pivot_wider(names_from = name,
                    values_from = mean.corr) %>%
        round(2)
    }
  })

  output$k.table <-  DT::renderDT(server = FALSE, {
    datatable(data = k.norm.0(),
              style = "bootstrap",
              extensions = c('Buttons', 'Responsive', 'Scroller'),
              selection = 'multiple',
              colnames = c('Species #' = 'Species.x',
                           'Name' = 'name',
                           'Scan' = 'scan',
                           'TIC Time (min)' = 'time.x',
                           'Time (min)' = 'corrected.time',
                           'Mean Time (min)' = 'mean.time',
                           'Raw intensity' = 'intensity.x',
                           'Mean raw intensity' = 'mean.raw',
                           'Standard intensity' = 'intensity.y',
                           'Corrected intensity' = 'corr.int',
                           'Mean corrected intensity' = 'mean.corr',
                           'Filename' = 'filename.x',
                           'Scan group' = 'group',
                           'm/z range' = 'mz.range.x',
                           'Centroid' = 'centroid.x'),
              editable = T,
              rownames = F,
              escape = T,
              filter = 'top',
              autoHideNavigation = T,
              plugins = 'natural',
              options = list(
                deferRender = TRUE,
                scrollY = 200,
                scroller = TRUE,
                autoWidth = F,
                dom = 'Bfrtip', #button position
                buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
                columnDefs = list(list(visible=FALSE, targets=c(0,4,5,6,7,8,9,10)))
              )
    ) %>%
      formatRound(c('TIC Time (min)', 'Time (min)', 'Raw intensity',
                    "Standard intensity", "Corrected intensity",
                    "Mean raw intensity", "Mean Time (min)", 'Mean corrected intensity'),
                  digits = 2)
  })

  output$k.wide <-  DT::renderDT(server = FALSE, {
    datatable(data = k.wide(),
              style = "bootstrap",
              extensions = c('Buttons', 'Responsive', 'Scroller'),
              selection = 'multiple',
              colnames = c('Mean Time (min)' = 'mean.time'),
              editable = T,
              rownames = F,
              escape = T,
              filter = 'top',
              autoHideNavigation = T,
              plugins = 'natural',
              options = list(
                deferRender = TRUE,
                scrollY = 200,
                scroller = TRUE,
                autoWidth = F,
                dom = 'Bfrtip', #button position
                buttons = c('copy', 'csv', 'excel')
              )
    )
  })

  output$k.spectra <-  DT::renderDT(server = FALSE, {
    datatable(data = k.spectra(),
              style = "bootstrap",
              callback = callback,
              extensions = c('Buttons', 'Responsive', 'Scroller'),
              selection = 'multiple',
              # colnames = c('Mean Time (min)' = 'mean.time'),
              editable = T,
              rownames = F,
              escape = T,
              filter = 'top',
              autoHideNavigation = T,
              plugins = 'natural',
              options = list(
                dom = 'B<"dwnld">frtip',
                deferRender = TRUE,
                scrollY = 200,
                scroller = TRUE,
                autoWidth = F,
                dom = 'Bfrtip', #button position
                buttons = list('copy')
              )
    )
  })

  output$download1 <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(k.spectra(), file)
    }
  )

  output$k.plot <- renderPlot({
    k.plot <- ggplot(data = k.norm.0(), aes(x = mean.time, y = kin.input(),
                                            color = name)) +
      geom_point(size = input$size.dot.kin, alpha = input$transp.kin) +
      scale_color_manual(name = "Species",
                         values = c(input$col.dot.kin1, input$col.dot.kin2, input$col.dot.kin3, input$col.dot.kin4, input$col.dot.kin5, input$col.dot.kin6, input$col.dot.kin7, input$col.dot.kin8)) +
      xlab("time (min)") +
      theme(panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black", size = 0.75),
            axis.ticks.length=unit(0.1, "in"),
            axis.ticks = element_line(size = 0.75),
            axis.text = element_text(colour="black", size = 16, color = "black", angle = 0),
            axis.title = element_text(size=18,face="bold"),
            plot.margin = margin(25, 0.5, 0.5, 0.5),
            legend.box = "vertical",
            legend.title = element_text(size=18, face="bold"),
            legend.key = element_rect(fill = "white"),
            legend.text = element_text(size=16, face="bold"))  +
      coord_cartesian(expand = T)

    if(input$kin.input == 'centroid'){
      k.plot <- k.plot + ylab("centroid (m/z)")
    } else {
      k.plot <- k.plot + ylab("intensity")
    }

    return(k.plot)

  })

  output$k.plot.ui <- renderUI({
    plotOutput("k.plot",
               width = as.numeric(input$k.plot.w),
               height = as.numeric(input$k.plot.h)
    )
  })

  #########TITRATION###############

  #import processed data

  titr.old <- reactive({
    if(is.null(input$titr.old))
      return(NULL)

    input$titr.old
  })

  processed.titr <- reactive({

    if(is.null(input$titr.old))
      return(NULL)

    processed.titr <- data.frame(read_excel(titr.old()$datapath,
                                            col_names = F, skip = 2))

    colnames(processed.titr) <- c('Species',
                                  'lgd.conc',
                                  'filename',
                                  'min.time',
                                  'max.time',
                                  'min.scan',
                                  'max.scan',
                                  'min.mz',
                                  'max.mz',
                                  'Stoich',
                                  'intensity',
                                  'rel.intensity',
                                  'uncorr.C')

    return(processed.titr)

  })


  lgd.conc <- reactive({
    as.numeric(input$lgd.conc)
  })

  MSsnaps24 <- eventReactive(input$bttn24, {
    newrow <- data.frame(inputsnap())
    snaps <<- rbind(snaps, newrow)
  })

  eq.raw <- reactive({

    if (is.null(input$file1)) {
      return(processed.titr())
    } else {
      eq.raw <- MSsnaps24() %>%
        group_by(Species, lgd.conc, filename, min.time, max.time, min.scan, max.scan, min.mz, max.mz, Stoich) %>% #replace CFtime by concentration
        summarise(intensity = sum(intensum)) %>%
        group_by(lgd.conc) %>%
        mutate(rel.intensity = ifelse(Species == 'Standard',
                                      '',
                                      intensity/sum(intensity[Species != "Standard"]))
        ) %>%
        #the relative intensity does not take into account the SI intensity
        mutate(rel.intensity = as.numeric(rel.intensity)) %>%
        group_by(Species, Stoich) %>%
        mutate(uncorr.C = rel.intensity*Mtot())



      if (is.null(titr.old)) {
        return(eq.raw)
      } else {
        rbind.data.frame(eq.raw, processed.titr())
      }
    }


  })


  output$eq.raw <- DT::renderDT(server = FALSE, {

    if (is.null(eq.raw())) {
      return(NULL)
    } else {

      datatable(data = eq.raw(),
                style = "bootstrap",
                extensions = c('Buttons', 'Responsive', 'Scroller'),
                selection = 'multiple',
                colnames = c(
                  '[Ligand] (µM)' = 'lgd.conc',
                  'File name' = 'filename',
                  'Ligand stoichiometry' = 'Stoich',
                  'Raw intensity' = 'intensity',
                  'Relative intensity' = 'rel.intensity',
                  'Start TIC time' = 'min.time',
                  'End TIC time' = 'max.time',
                  'Start scan' = 'min.scan',
                  'End scan' = 'max.scan',
                  'Start m/z' = 'min.mz',
                  'End m/z'= 'max.mz',
                  'C (µM, uncorrected)' = 'uncorr.C'
                ),
                editable = T,
                rownames = F,
                escape = T,
                filter = 'top',
                autoHideNavigation = T,
                plugins = 'natural',
                options = list(
                  deferRender = TRUE,
                  scrollY = 200,
                  scroller = TRUE,
                  autoWidth = F,
                  dom = 'Bfrtip', #button position
                  buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
                  columnDefs = list(list(visible=FALSE, targets=c(3, 4, 5, 6, 7, 8, 2)))
                )
      ) %>%
        formatRound(c('Raw intensity', 'Relative intensity', 'C (µM, uncorrected)'), digits = 2)
    }
  })

  #User inputs
  Mtot <- reactive({ #total target concentration
    as.numeric(input$Mtot)
  })

  Std <- reactive({ #SI concentration
    as.numeric(input$Std)
  })

  R <- eventReactive(input$bttn55,{

    # Calculation of the intensity ratio vs IS (by experiment = by lgd.conc)
    I <- eq.raw() %>%
      group_by(lgd.conc) %>%
      mutate(I.ratio = intensity/intensity[Species == "Standard"]) %>%
      filter(Species != "Standard") %>% # ratio should be = 1; useless for remainder of script
      dplyr::select(I.ratio, Stoich, lgd.conc) # selection of relevant columns

    #Ligand concentrations matrix
    Lgd <- I %>%
      dplyr::select(lgd.conc) %>%
      distinct(lgd.conc, .keep_all = TRUE) %>%
      as.matrix()


    ### At this point, are the rows order by concentration and stoichio? #####

    # pivoting from long to wide format to work with matrices after
    # allocate a 0 intensity to absent species
    # remove the stoich column
    I <- I %>%
      pivot_wider(values_from = "I.ratio", names_from = c("lgd.conc")) %>%
      mutate_all(~replace(., is.na(.), 0))

    Stoich <- as.matrix(I$Stoich)

    I <- I %>%
      dplyr::select(-Stoich)

    #switching to matrix and transposing to follow the formatting of Anal. Chem.2009,81,6708–6715
    I <- t(as.matrix(I))



    C <- matrix(ncol = 1, nrow = nrow(I))

    for (nrowI in 1:nrow(I)) {
      C[nrowI] <- Mtot()/Std()
    }

    #I.R = C with R the response factors

    #Computes the Moore-Penrose generalized inverse of matrix I
    I.inv = MPinv(I)
    # I.inv

    #Response factors relative to the IS.
    R <- I.inv%*%C

    return(R)

  })

  corr.C <- eventReactive(input$bttn55,{

    # Calculation of the intensity ratio vs IS (by experiment = by lgd.conc)
    I <- eq.raw() %>%
      group_by(lgd.conc) %>%
      mutate(I.ratio = intensity/intensity[Species == "Standard"]) %>%
      filter(Species != "Standard") %>% # ratio should be = 1; useless for remainder of script
      dplyr::select(I.ratio, Stoich, lgd.conc) # selection of relevant columns

    #Ligand concentrations matrix
    Lgd <- I %>%
      dplyr::select(lgd.conc) %>%
      distinct(lgd.conc, .keep_all = TRUE) %>%
      as.matrix()


    ### At this point, are the rows order by concentration and stoichio? #####

    # pivoting from long to wide format to work with matrices after
    # allocate a 0 intensity to absent species
    # remove the stoich column
    I <- I %>%
      pivot_wider(values_from = "I.ratio", names_from = c("lgd.conc")) %>%
      mutate_all(~replace(., is.na(.), 0))

    Stoich <- as.matrix(I$Stoich)

    I <- I %>%
      dplyr::select(-Stoich)

    #switching to matrix and transposing to follow the formatting of Anal. Chem.2009,81,6708–6715
    I <- t(as.matrix(I))

    C <- matrix(ncol = 1, nrow = nrow(I))

    for (nrowI in 1:nrow(I)) {
      C[nrowI] <- Mtot()/Std()
    }

    #I.R = C with R the response factors

    #Computes the Moore-Penrose generalized inverse of matrix I
    I.inv = MPinv(I)
    I.inv

    #Response factors relative to the IS.
    R <- I.inv%*%C

    # Generalization


    #RI has the same dimensions as I
    RI <- matrix(ncol = ncol(I), nrow = nrow(I))
    #RI is obtained by distributing each row of R onto the same numbered column of I
    for (nrowR in 1:nrow(R)) {
      for (nrowI in 1:nrow(I)) {
        RI[nrowI,nrowR] <- R[nrowR] * I[nrowI,nrowR]
      }
    }


    #Final matrix of corrected concentrations
    #Same dimensions as I
    corr.C <- matrix(ncol = ncol(I), nrow = nrow(I))
    #Product of Mtot on RI, divided by the sum of RI across species for a given experiment
    for (nrowI in 1:nrow(I)) {
      for (ncolI in 1:ncol(I)) {
        corr.C[nrowI,ncolI] <- Mtot()*RI[nrowI,ncolI]/sum(RI[nrowI,])
      }
    }

    corr.C.colnames <- eq.raw() %>%
      select(Stoich) %>%
      unique() %>%
      mutate(Stoich = case_when(
        Stoich == 0 ~ 'M',
        Stoich == 1 ~ 'ML',
        Stoich == 2 ~ 'ML2'
      )) %>%
      as.list() %>%
      unlist()

    corr.C <- data.frame(corr.C) %>%
      arrange(desc(X1)) %>%
      magrittr::set_colnames(corr.C.colnames)

    return(corr.C)

  })


  output$Rf <- renderDT({

    eq.raw() %>%
      filter(Species != 'Standard') %>%
      group_by(Stoich) %>%
      filter(row_number() == 1) %>%
      add_column(Rf = R())

  })

  output$corr.C <- renderDT({

    datatable(data = corr.C(),
              style = "bootstrap",
              extensions = c('Buttons', 'Responsive', 'Scroller'),
              selection = 'multiple',
              editable = T,
              rownames = F,
              escape = T,
              filter = 'top',
              autoHideNavigation = T,
              plugins = 'natural',
              options = list(
                deferRender = TRUE,
                scrollY = 200,
                scroller = TRUE,
                autoWidth = F,
                dom = 'Bfrtip', #button position
                buttons = c('copy', 'csv', 'excel', 'colvis')
              )
    )


  })


  # output$eq.raw <- DT::renderDT({
  #
  #   datatable(data = eq.raw(),
  #             extensions = c('Buttons', 'Responsive', 'Scroller'),
  #             selection = 'multiple',
  #             colnames = c(
  #               '[Ligand] (µM)' = 'lgd.conc',
  #               'File name' = 'filename',
  #               'Ligand stoichiometry' = 'Stoich',
  #               'Raw intensity' = 'intensity',
  #               'Relative intensity' = 'rel.intensity',
  #               'Start TIC time' = 'min.time',
  #               'End TIC time' = 'max.time',
  #               'Start scan' = 'min.scan',
  #               'End scan' = 'max.scan',
  #               'Start m/z' = 'min.mz',
  #               'End m/z'= 'max.mz'
  #             ),
  #             editable = T,
  #             rownames = F,
  #             escape = T,
  #             filter = 'top',
  #             autoHideNavigation = T,
  #             plugins = 'natural',
  #             options = list(
  #               deferRender = TRUE,
  #               scrollY = 200,
  #               scroller = TRUE,
  #               autoWidth = F,
  #               dom = 'Bfrtip', #button position
  #               buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
  #               columnDefs = list(list(visible=FALSE, targets=c(3, 4, 5, 6, 7, 8, 2)))
  #             )
  #   ) %>%
  #     formatRound(c('Raw intensity', 'Relative intensity'), digits = 2)
  # })



  #MS STACKING AND HDX DECONVOLUTION----

  ##snaps plotting---------

  #snaps scaling
  MSsnaps1 <- reactive({
    if (isTRUE(input$manu1)) {
      MSsnaps() %>%
        mutate(colorscale = MSsnaps()$CFtime) %>% #creates a variable to scale the color and position the spectrum in the stack based on manual or TIC time
        group_by(colorscale, Species, filename, min.time, max.time, min.scan, max.scan, min.mz, max.mz) %>%
        mutate(intensum = 1-(max(intensum)-intensum)/(max(intensum)-min(intensum))) #Normalization of each spectrum (per time point, per sample)
    } else {
      MSsnaps() %>%
        mutate(colorscale = MSsnaps()$mean.time) %>%
        group_by(colorscale, Species, filename, min.time, max.time, min.scan, max.scan, min.mz, max.mz) %>%
        mutate(intensum = 1-(max(intensum)-intensum)/(max(intensum)-min(intensum)))
    }
  })

  #variable to select a common or independent x axis. Useful to compare across samples with a fixed axis or across adducts/charge states when not fixed
  common.scale <- reactive({
    if (isTRUE(input$com.scale)) {
      return('fixed')
    } else {
      return('free_x')
    }
  })

  #variables to define if time indicated by a label or a color guide
  time.label <- reactive({
    if (isTRUE(input$t.indic)) {
      return(element_blank())
    } else {
      return(element_text(size = 16, color = "black", face = "bold", angle = 0))
    }
  })

  color.guide <- reactive({
    if (isTRUE(input$t.indic)) {
      return('right')
    } else {
      return('none')
    }
  })


  output$plot5 <- renderPlot({
    Plot5()
  })

  output$plot5.ui <- renderUI({
    plotOutput("plot5",
               width = as.numeric(input$plot5.w),
               height = as.numeric(input$plot5.h)
    )
  })

  output$plot5bi <- renderPlot({
    Plot5bi()
  }
  )

  output$plot5bi.ui <- renderUI({
    plotOutput("plot5bi",
               width = as.numeric(input$plot5.w),
               height = as.numeric(input$plot5.h)
    )
  })

  ###plot stacked spectra----
  Plot5 <- reactive({
    ggplot(data = MSsnaps1(), aes(x = mz, y = intensum,
                                  color = colorscale)) +
      geom_line(size = 1) +
      scale_color_gradient(name = 't (min)', low=input$col.snap1, high=input$col.snap2,
                           guide=guide_colourbar(reverse = TRUE, barheight = 20, barwidth = 3, ticks.linewidth = 2),
                           # breaks = breaks,
                           trans = input$trans.user) +
      xlab("m/z") +
      facet_grid(colorscale ~ Species,
                 scales = common.scale()
      ) +
      custom.theme  +
      theme(strip.text = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "right") +
      coord_cartesian(expand = FALSE)
  })


  ##Peak picking----

  MSsnaps.pp.0 <- reactive({

    ppf(raw = as.data.frame(MSsnaps1()),
        neigh = input$neighlim,
        deriv = input$deriv.lim,
        thresh = input$int.thresh
    )

  })

  ### Import already peak-picked data----

  ####upload already processed data----
  exported.snaps <- reactive({
    if(is.null(input$exported.snaps))
      return(NULL)

    input$exported.snaps
  })

  exported.snaps.import <- reactive({

    if(is.null(input$exported.snaps)) {
      return(NULL)
    } else {
      readxl::read_excel(
        exported.snaps()$datapath
      ) %>%
        rename(
          "colorscale" = "Time (min)",
          "mz" = "m/z",
          "filename" = "Filename",
          "intensum" = "Intensity",
          "mean.time" = "TIC time (min)",
          "CFtime" = "Manual time (min)",
          "peak" = "Peak picked",
          "min.time" = "start time",
          "max.time" = "end time",
          "min.scan" = "start scan",
          "max.scan" = "end scan",
          'min.mz' = "start m/z",
          "max.mz" = "end m/z"
        ) %>%
        as.data.frame()
    }
  })

  MSsnaps.pp <- reactive({
    if(is.null(exported.snaps)) {
      return(MSsnaps.pp.0())
    } else {
      if(is.null(input$file1)) {
        return(exported.snaps.import())
      } else{
        exported.snaps.import() %>%
          rbind(MSsnaps.pp.0() %>%
                  select(filename, Species, colorscale, mean.time, CFtime, mz, intensum, peak,
                         min.time, max.time, min.scan, max.scan, min.mz, max.mz)
          )
      }
    }
  })


  ### Plot peak-picked data----
  Plot5bi <- reactive({
    ggplot(
      data = MSsnaps.pp(),
      aes(
        x = mz,
        color = colorscale
      )
    ) +
      geom_line(
        aes(y = intensum),
        size = 1
      ) +
      geom_point(
        data = MSsnaps.pp() %>%
          filter(peak > 0),
        aes(y = intensum),
        color = 'green',
        size = 5,
        alpha = 0.5
      ) +
      xlab("m/z") +
      facet_grid(colorscale ~ Species,
                 scales = common.scale()
      ) +
      scale_color_gradient(
        name = 't (min)',
        low=input$col.snap1, high=input$col.snap2,
        guide=guide_colourbar(reverse = TRUE, barheight = 20, barwidth = 3, ticks.linewidth = 2),
        trans = input$trans.user
      ) +
      custom.theme +
      theme(strip.text = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "right") +
      coord_cartesian(expand = TRUE)
  })


  ## Optimization----

  ### Least-square minimization----
  opt.0 <- eventReactive(input$optibtn, {

    withProgress(
      message = 'Optimizing data',
      detail = 'Please wait', value = 0, {

        incProgress(amount=1/2)

        opt.0 <- MSsnaps.pp() %>%
          as.data.frame() %>%
          group_by(Species, colorscale) %>%
          nest() %>%
          mutate(
            par = map(
              data,
              ~map.optimR(
                input.data = .,
                method = 'L-BFGS-B',
                lmm = 10, #number of BFGS updates
                seq = sequencer(),
                massr = massr(),
                init.par = c(10,0.2), #initial parameters
                DC.final = min(input$DC.pp), #lower threshold for deuterium content
                DC.init = max(input$DC.pp),  #higher threshold for deuterium content
                peaks = input$nrPeaks.user.pp #number of isotopic peaks to calculate
              )
            )
          ) %>%
          as.data.frame() %>%
          unnest(par) %>%
          unnest(par)

        incProgress(amount=2/2)

        return(opt.0)

      })

  })

  ### Generation of optimized distributions----

  #### All distributions----
  opt.distrib <- reactive({

    withProgress(
      message = 'Generating optimized distributions',
      detail = 'Please wait', value = 0, {

        incProgress(amount=1/3)

        opt <- opt.f(opt.0(), seq = sequencer(), mass = massr(), peaks = input$nrPeaks.user.pp)

        incProgress(amount=2/3)

        opt <- opt %>%
          select(Species, colorscale, distrib, mz.th, iso, iso.1, iso.2, everything())

        return(opt)

      })
  })

  ##### Import already optimized data----

  exported.opt <- reactive({
    if(is.null(input$exported.opt))
      return(NULL)

    input$exported.opt
  })

  exported.opt.import <- reactive({

    if(is.null(input$exported.opt)) {
      return(NULL)
    } else {
      readxl::read_excel(
        exported.opt()$datapath
      ) %>%
        rename(
          "colorscale" = "Time (min)",
          "mz.th" = "m/z",
          "distrib" = "Modality",
          'iso' = "Global isotopic abundance",
          'iso.1' = "Isotopic abundance 1",
          'iso.2' = "Isotopic abundance 2"
        ) %>%
        as.data.frame()
    }
  })

  opt <- reactive({
    if(is.null(exported.opt)) {
      return(opt.distrib())
    } else {
      if(is.null(input$file1)) {
        return(exported.opt.import())
      } else{
        exported.opt.import() %>%
          rbind(opt.distrib())
      }
    }
  })

  #### Filtered distributions----

  alpha <- reactive({as.numeric(input$alpha)})

  bi.t.limit <- reactive({as.numeric(input$b.t.limit)})

  opt.filter <- reactive({

    ##automatic data filtering based on RSS----
    if(isTRUE(input$model.selection)){
      opt.filter <- opt() %>%
        filter(
          distrib == 'mono' & p.F >= alpha() |
            distrib == 'bi' & p.F < alpha()
        )
    } else{
      ##manual data filtering based on user input (data frame)----
      opt.filter <- opt() %>%
        filter(
          distrib == 'mono' & colorscale > bi.t.limit() |
            distrib == 'bi' & colorscale <= bi.t.limit()
        )
    }

    return(opt.filter)
  })


  ### Derived values----

  #### All values----
  binom.NUS <- reactive({
    binom.NUS.f(
      opt(),
      sequencer(),
      DC.init = max(input$DC.pp),
      DC.final = min(input$DC.pp),
      ref = as.numeric(input$user.hdx.ref)
    )
  })

  ### Filtered values----

  binom.filter <- reactive({

    ##automatic data filtering based on RSS----
    if(isTRUE(input$model.selection)){
      binom.filter <- binom.NUS() %>%
        filter(
          distrib == 'mono' & p.F >= alpha() |
            distrib == 'bi' & p.F < alpha()
        )
    } else {
      ##manual data filtering based on user input (data frame)----
      binom.filter <- binom.NUS() %>%
        filter(
          distrib == 'mono' & colorscale > bi.t.limit() |
            distrib == 'bi' & colorscale <= bi.t.limit()
        )
    }

    return(binom.filter)
  })


  ## Peak picking and modeling table outputs----
  output$MSsnaps.pp.table <- DT::renderDT(server = FALSE, {
    datatable(
      data = MSsnaps.pp() %>%
        select(c(filename, Species, colorscale, mean.time, CFtime, mz, intensum, peak,
                 min.time, max.time, min.scan, max.scan, min.mz, max.mz)),
      style = "bootstrap",
      extensions = c('Buttons', 'Responsive', 'Scroller'),
      selection = 'multiple',
      colnames = c(
        "Time (min)" = "colorscale",
        "m/z" = "mz",
        "Filename" = "filename",
        "Intensity" = "intensum",
        "TIC time (min)" = "mean.time",
        "Manual time (min)" = "CFtime",
        "Peak picked" = "peak",
        "start time" = "min.time",
        "end time" = "max.time",
        "start scan" = "min.scan",
        "end scan" = "max.scan",
        "start m/z" = 'min.mz',
        "end m/z" = "max.mz"
      ),
      editable = T,
      rownames = F,
      escape = T,
      filter = 'top',
      autoHideNavigation = T,
      plugins = 'natural',
      options = list(
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        autoWidth = F,
        dom = 'Bfrtip', #button position
        buttons = list(
          list(extend='copy'),
          list(extend='csv',
               title=NULL,
               filename="Peak-picked data"),
          list(extend='excel',
               title=NULL,
               filename="Peak-picked data"),
          list(extend='colvis')
        ),
        columnDefs = list(list(visible=FALSE, targets=c(0,3,4,7:12)))
      )
    ) %>%
      formatStyle(
        0:12,
        target = 'row',
        backgroundColor = styleEqual(c(0,1), c('#272c30', '#272c30'))
      )
  })

  output$opt.table <- DT::renderDT(server = FALSE, {
    datatable(
      data = opt(),
      style = "bootstrap",
      extensions = c('Buttons', 'Responsive', 'Scroller'),
      selection = 'multiple',
      colnames = c(
        "Time (min)" = "colorscale",
        "Modality" = "distrib",
        "m/z" = "mz.th",
        "Global isotopic abundance" = 'iso',
        "Isotopic abundance 1" = 'iso.1',
        "Isotopic abundance 2" = 'iso.2'
      ),
      editable = T,
      rownames = F,
      escape = T,
      filter = 'top',
      autoHideNavigation = T,
      plugins = 'natural',
      options = list(
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        autoWidth = F,
        dom = 'Bfrtip', #button position
        buttons = list(
          list(extend='copy'),
          list(extend='csv',
               title=NULL,
               filename="Optimized distributions"),
          list(extend='excel',
               title=NULL,
               filename="Optimized distributions"),
          list(extend='colvis')
        ),
        columnDefs = list(list(visible=FALSE, targets=c(7:(ncol(opt())-1))))
      )
    ) %>%
      formatStyle(
        0:7,
        target = 'row',
        backgroundColor = styleEqual(c(0,1), c('#272c30', '#272c30'))
      )
  })

  output$opt.stat <- DT::renderDT(server = FALSE, {
    datatable(
      data = opt() %>%
        select(
          Species, colorscale, distrib,
          convergence,
          DC1, DC2,
          ab1, ab2,
          n, df,
          p, F.test, p.F, MSE, RSS
        ) %>%
        unique() %>%
        #mutate error codes to messages
        mutate(
          convergence = case_when(
            convergence == 0 ~ 'yes',
            convergence == 1 ~ 'max iterations',
            convergence == 10 ~ 'degeneracy',
            convergence == 51 ~ 'warning',
            convergence == 52 ~ 'error'
          )
        ),
      style = "bootstrap",
      extensions = c('Buttons', 'Responsive', 'Scroller'),
      selection = 'multiple',
      colnames = c(
        "Time (min)" = "colorscale",
        "Modality" = "distrib",
        "DC 1" = "DC1",
        "DC 2" = "DC2",
        "Abundance 1" = "ab1",
        "Abundance 2" = "ab2",
        "Number of points" = "n",
        "Degrees of freedom" = "df",
        "Convergence" = "convergence"
      ),
      editable = T,
      rownames = F,
      escape = T,
      filter = 'top',
      autoHideNavigation = T,
      plugins = 'natural',
      options = list(
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        autoWidth = F,
        dom = 'Bfrtip', #button position
        buttons = list(
          list(extend='copy'),
          list(extend='csv',
               title=NULL,
               filename="Optimization results"),
          list(extend='excel',
               title=NULL,
               filename="Optimization results"),
          list(extend='colvis')
        ),
        columnDefs = list(list(visible=FALSE, targets=c(4:7)))
      )
    ) %>%
      formatStyle(
        0:15,
        target = 'row',
        backgroundColor = styleEqual(c(0,1), c('#272c30', '#272c30'))
      )
  })

  output$binom.NUS.table <- DT::renderDT(server = TRUE, {
    datatable(
      data = binom.NUS() %>%
        select(
          Species, colorscale, distrib,
          DC1, DC2, DC.mean,
          centroid, centroid.1, centroid.2,
          USE.1, USE.2, USE.mean, delta.USE,
          ab1, ab2,
          fraction.1, fraction.2
        ),
      style = "bootstrap",
      extensions = c('Buttons', 'Responsive', 'Scroller'),
      selection = 'multiple',
      colnames = c(
        "Time (min)" = "colorscale",
        "Modality" = "distrib",
        "DC 1" = "DC1",
        "DC 2" = "DC2",
        "DC mean" = "DC.mean",
        "NUS 1" = "USE.1",
        "NUS 2" = "USE.2",
        "NUS mean" = "USE.mean",
        "Delta NUS" = "delta.USE",
        "Global centroid" = "centroid",
        "Centroid 1" = "centroid.1",
        "Centroid 2" = "centroid.2",
        "Abundance 1" = "ab1",
        "Abundance 2" = "ab2",
        "Fraction 1" = "fraction.1",
        "Fraction 2" = "fraction.2"
      ),
      editable = T,
      rownames = F,
      escape = T,
      filter = 'top',
      autoHideNavigation = T,
      plugins = 'natural',
      options = list(
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        autoWidth = F,
        dom = 'Bfrtip', #button position
        buttons = list(
          list(extend='copy'),
          list(extend='csv',
               title=NULL,
               filename="Derived values"),
          list(extend='excel',
               title=NULL,
               filename="Derived values"),
          list(extend='colvis')
        ),
        columnDefs = list(list(visible=FALSE, targets=c(3:8, 13,14)))
      )
    ) %>%
      formatStyle(
        0:17,
        target = 'row',
        backgroundColor = styleEqual(c(0,1), c('#272c30', '#272c30'))
      )
  })

  output$opt.filter.table <- DT::renderDT(server = TRUE, {
    datatable(
      data = opt.filter() %>%
        select(
          Species, colorscale, distrib,
          mz.th, iso, iso.1, iso.2
        ),
      style = "bootstrap",
      extensions = c('Buttons', 'Responsive', 'Scroller'),
      selection = 'multiple',
      colnames = c(
        "Time (min)" = "colorscale",
        "Modality" = "distrib",
        "m/z" = "mz.th",
        "Global isotopic abundance" = 'iso',
        "Isotopic abundance 1" = 'iso.1',
        "Isotopic abundance 2" = 'iso.2'
      ),
      editable = T,
      rownames = F,
      escape = T,
      filter = 'top',
      autoHideNavigation = T,
      plugins = 'natural',
      options = list(
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        autoWidth = F,
        dom = 'Bfrtip', #button position
        buttons = list(
          list(extend='copy'),
          list(extend='csv',
               title=NULL,
               filename="Filtered optimized distributions"),
          list(extend='excel',
               title=NULL,
               filename="Filtered optimized distributions"),
          list(extend='colvis')
        )
      )
    ) %>%
      formatStyle(
        0:7,
        target = 'row',
        backgroundColor = styleEqual(c(0,1), c('#272c30', '#272c30'))
      )
  })

  output$binom.filter.table <- DT::renderDT(server = TRUE, {
    datatable(
      data = binom.filter()%>%
        select(
          Species, colorscale, distrib,
          DC1, DC2, DC.mean,
          centroid, centroid.1, centroid.2,
          USE.1, USE.2, USE.mean, delta.USE,
          ab1, ab2,
          fraction.1, fraction.2
        ),
      style = "bootstrap",
      extensions = c('Buttons', 'Responsive', 'Scroller'),
      selection = 'multiple',
      colnames = c(
        "Time (min)" = "colorscale",
        "Modality" = "distrib",
        "DC 1" = "DC1",
        "DC 2" = "DC2",
        "DC mean" = "DC.mean",
        "NUS 1" = "USE.1",
        "NUS 2" = "USE.2",
        "NUS mean" = "USE.mean",
        "Delta NUS" = "delta.USE",
        "Global centroid" = "centroid",
        "Centroid 1" = "centroid.1",
        "Centroid 2" = "centroid.2",
        "Abundance 1" = "ab1",
        "Abundance 2" = "ab2",
        "Fraction 1" = "fraction.1",
        "Fraction 2" = "fraction.2"
      ),
      editable = T,
      rownames = F,
      escape = T,
      filter = 'top',
      autoHideNavigation = T,
      plugins = 'natural',
      options = list(
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        autoWidth = F,
        dom = 'Bfrtip', #button position
        buttons = list(
          list(extend='copy'),
          list(extend='csv',
               title=NULL,
               filename="Filtered derived values"),
          list(extend='excel',
               title=NULL,
               filename="Filtered derived values"),
          list(extend='colvis')
        ),
        columnDefs = list(list(visible=FALSE, targets=c(3:8, 13,14)))
      )
    ) %>%
      formatStyle(
        0:17,
        target = 'row',
        backgroundColor = styleEqual(c(0,1), c('#272c30', '#272c30'))
      )
  })

  ## Modeling plot outputs-------

  output$optiplot <- renderPlot({

    Plot5bi() +
      geom_point(data = opt.filter(),
                 aes(x = mz.th, y = iso.1),
                 size = 5, color = '#8b2e62', alpha = 0.75) +
      geom_line(data = opt.filter(),
                aes(x = mz.th, y = iso.1),
                size = 1, color = '#8b2e62', alpha = 0.75) +
      geom_point(data = opt.filter(),
                 aes(x = mz.th, y = iso.2),
                 size = 5, color = 'seagreen4', alpha = 0.75) +
      geom_line(data = opt.filter(),
                aes(x = mz.th, y = iso.2),
                size = 1, color = 'seagreen4', alpha = 0.75)  +
      geom_point(data = opt.filter(),
                 aes(x = mz.th, y = iso),
                 size = 5, color = 'tomato', alpha = 0.75) +
      geom_line(data = opt.filter(),
                aes(x = mz.th, y = iso),
                size = 1, color = 'tomato', alpha = 0.75) +
      geom_segment(data = opt.filter() %>% distinct(., colorscale, Species, distrib, centroid),
                   aes(x = centroid, xend = centroid, y = 0, yend = 1),
                   color = 'tomato',
                   size = 1, alpha = 0.75,
                   linetype = 'dashed')  +
      geom_segment(data = opt.filter() %>% distinct(., colorscale, Species, distrib, centroid.1),
                   aes(x = centroid.1, xend = centroid.1, y = 0, yend = 1),
                   color = '#8b2e62',
                   size = 1, alpha = 0.75,
                   linetype = 'dashed') +
      geom_segment(data = opt.filter() %>% distinct(., colorscale, Species, distrib, centroid.2),
                   aes(x = centroid.2, xend = centroid.2, y = 0, yend = 1),
                   color = 'seagreen4',
                   size = 1, alpha = 0.75,
                   linetype = 'dashed')

  })


  output$optiplot.ui <- renderUI({
    plotOutput("optiplot",
               width = as.numeric(input$plot5.w),
               height = as.numeric(input$plot5.h)
    )
  })


  ##download spectra----------
  output$dwnspec <- downloadHandler(
    filename = function() { paste("stacked spectra", '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = Plot5(), device = "png",
             width = as.numeric(input$plot5.w),
             height = as.numeric(input$plot5.h),
             units = 'mm')
    }
  )

  output$dwnspec.pdf <- downloadHandler(
    filename = function() { paste("stacked spectra", '.pdf', sep='') },
    content = function(file) {
      ggsave(file, plot = Plot5(), device = "pdf",
             width = as.numeric(input$plot5.w),
             height = as.numeric(input$plot5.h),
             units = 'mm')
    }
  )

  ##CENTROIDS AND NUS--------

  ###centroid calculation----
  centroids <- reactive({

    req(MSsnaps.pp())

    MSsnaps.pp() %>%
      # group_by(mean.time, Species) %>%
      group_by(mean.time, Species, CFtime, filename, min.time, max.time, min.scan, max.scan, min.mz, max.mz) %>%
      summarise(centroid = weighted.mean(mz, intensum)) #calculation of centroids
  })

  ####hot table----
  NUS.init0 <- reactive({

    req(centroids())

    NUS.init0 <- data.frame(Species = unique(centroids()$Species),
                            Name = unique(centroids()$Species),
                            Reference = rep(massr()$Avemz, length(unique(centroids()$Species))),
                            Charge = rep(as.numeric(input$z), length(unique(centroids()$Species))),
                            D.initial = rep(max(input$DC.pp), length(unique(centroids()$Species))),
                            D.final = rep(min(input$DC.pp), length(unique(centroids()$Species)))
    )
  })

  NUS.change <- reactive({
    req(input$hotable2)
    req(NUS.init0())
    as.data.frame(hot.to.df(input$hotable2))
  })

  output$hotable2 <- renderHotable({
    req(centroids())
    NUS.init0()
  }, readOnly = F)

  ###NUS calculation----
  NUS <- reactive({
    centroids() %>%
      left_join(NUS.change(), by = "Species") %>%
      group_by(mean.time, Species, CFtime, filename, min.time, max.time, min.scan, max.scan, min.mz, max.mz) %>%
      mutate(#NUS calculation
        NUS = (centroid - Reference)*Charge/((D.initial - D.final)/100*(2.013553-1.007825)),
        mean.time.s = mean.time * 60,
        CFtime.s = CFtime * 60) %>% #creation of a time column in seconds
      dplyr::select(Species, Name, mean.time, mean.time.s, CFtime, CFtime.s, centroid, NUS,
                    Reference, Charge, filename, min.time, max.time, min.scan, max.scan, min.mz, max.mz)
  })

  ####time scaling----
  centroidscaled.init <- reactive({
    if (isTRUE(input$manu2)) {
      NUS() %>%
        add_column(timescale = NUS()$CFtime)
    } else {
      NUS() %>%
        add_column(timescale = NUS()$mean.time)
    }
  })

  centroidscaled <- reactive({
    if (isTRUE(input$manu2)) {
      centroidscaled.init() %>%
        mutate(timescale = CFtime)
    } else {
      centroidscaled.init() %>%
        mutate(timescale = mean.time)
    }
  })

  output$centroids <- DT::renderDT(server = FALSE, {
    datatable(data = centroidscaled() %>%
                select(
                  filename, min.time, max.time, min.scan, max.scan,
                  Species, Name, timescale, mean.time, mean.time.s, CFtime, CFtime.s,
                  centroid, Reference, Charge, NUS
                ),
              style = "bootstrap",
              extensions = c('Buttons', 'Responsive', 'Scroller'),
              selection = 'multiple',
              colnames = c(
                'TIC Time (min)' = 'mean.time',
                'TIC Time (s)' = 'mean.time.s',
                'Centroid' = 'centroid',
                "Manual time (min)" = 'CFtime',
                'Manual time (s)' = 'CFtime.s',
                'Filename' = 'filename',
                'Start TIC time' = 'min.time',
                'End TIC time' = 'max.time',
                'Start scan' = 'min.scan',
                'End scan' = 'max.scan',
                'Start m/z' = 'min.mz',
                'End m/z'= 'max.mz',
                'Species number' = 'Species',
                'Species name' = 'Name',
                "Time (min)" = "timescale"
              ),
              editable = F,
              rownames = F,
              escape = T,
              filter = 'top',
              autoHideNavigation = T,
              plugins = 'natural',
              options = list(
                deferRender = TRUE,
                scrollY = 200,
                scroller = TRUE,
                autoWidth = F,
                dom = 'Bfrtip', #button position
                buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
                columnDefs = list(list(visible=FALSE, targets=c(0:4, 8:14)))
              )
    ) %>%
      formatRound(c('TIC Time (min)', 'Centroid', 'NUS', "TIC Time (s)"), digits = 2) %>%
      formatStyle(
        columns = 0:17,
        target = 'row',
        background = '#272c30'
      )
  })

  #Select all lines
  centroids_proxy <- DT::dataTableProxy("centroids")

  observeEvent(input$centroids_sel, {
    if (isTRUE(input$centroids_sel)) {
      DT::selectRows(centroids_proxy, input$centroids_rows_all)
    } else {
      DT::selectRows(centroids_proxy, NULL)
    }
  })
  output$selected_rows <- renderPrint(print(input$centroids_rows_selected))

  output$plot6 <- renderPlot({

    req(centroidscaled()) #computes only once centroidscaled() is populated

    #data selection
    s = input$centroids_rows_selected
    selected.points <- centroidscaled()[ s,]

    ggplot(data = centroidscaled(), aes(x = centroidscaled()$timescale, y = centroidscaled()$centroid)) +
      geom_point(color = input$col.kin, size = input$size.kin) +
      geom_point(data = selected.points, size = input$size.kin,
                 aes(x = timescale, y = centroid, color = Name), inherit.aes = F) +
      scale_color_manual(values = c(input$col.kin.high1, input$col.kin.high2, input$col.kin.high3,input$col.kin.high4)) +
      xlab("time (min)") +
      ylab("centroid (m/z)") +
      custom.theme
  })

  ##HDX fit initialization----
  hdx.fit.init <- reactive({
    as.data.frame(hot.to.df(input$hotable3))
  })

  output$hotable3 <- renderHotable({

    s = input$centroids_rows_selected
    selected.points <- centroidscaled()[ s,]

    #automated N1, N2, NUS0 initialization
    parm.init <- selected.points %>%
      select(Name, NUS) %>% #gets names and NUS
      group_by(Name) %>%
      filter(NUS == max(NUS) | NUS == min(NUS)) %>% #calculates min and max NUS for each name
      mutate(#calculates initial values for number of sites and offset
        N1 = max(NUS)/2, #assumes same number of sites for both exponentials (may be quite inacurate)
        N2 = max(NUS)/2,
        NUS0 = min(NUS) #offset should be spot on
      ) %>%
      ungroup() %>%
      select(Name, N1, N2, NUS0) %>% #discard unnecessary data
      unique() #keep a single row per name


    #automated k1 and k2 initialization
    lm.prep <- selected.points %>%
      select(Name, NUS, timescale) %>% #get names, NUS, and timescale
      group_by(Name) %>%
      mutate(tr = NUS - min(NUS)) %>%
      filter(tr > 0) %>% #removes non strictly positive values before calculating log
      mutate(tr = log(tr)) #calculates neperian log

    lm.results <- data.frame() #initializes data frame to gather linear fit results

    for (i in unique(lm.prep$Name)) {
      selected.species <- lm.prep %>%
        filter(Name == i) #loops across Names

      #linear fit of tr = f(timescale)
      fit <- lm(data = selected.species,
                formula = tr ~ timescale)

      #k1, k2 initial values estimated from slope of log(NUS) = f(timescale)
      buffer <- data.frame(k1 = -coef(fit)[2] * 10/3,
                           k2 = -coef(fit)[2] / 3,
                           Name = i) #adds Name to join to other parameters below

      lm.results <- rbind(lm.results, buffer) #added to the result data frame
    }

    parm.init <- left_join(parm.init, lm.results) %>%
      select(Name, NUS0, k1, N1, k2, N2) #reorders of columns

    lm.prep <- NULL #empties lm.prep
    lm.results <- NULL #empties lm.results

    return(parm.init)

  },
  readOnly = F)

  ##HDX fit plot----
  plot7 <- reactive({

    req(centroidscaled()) #computes only once centroidscaled() is populated

    #data selection
    s = input$centroids_rows_selected
    selected.points <- centroidscaled()[ s,]

    #fitting (for labeling only, fit lines are calculated directly in the plot)
    if (isTRUE(input$fit.hdx)) { #calculates fit labeling is requested by user
      fit.list <- data.frame()

      for (i in unique(selected.points$Name)) {

        selected.species <- selected.points %>%
          filter(Name == i) #select one species per loop iteration

        start.df <- hdx.fit.init() %>%
          filter(Name == i) #select corresponding fit parameter start value

        #fit start value vector
        start <- c(N1 = as.numeric(start.df$N1), k1=as.numeric(start.df$k1),
                   N2=as.numeric(start.df$N2), k2=as.numeric(start.df$k2),
                   NUS0=as.numeric(start.df$NUS0))

        #non linear fitting
        fit <- nls(data = selected.species ,
                   formula = NUS~NUS0+N1*exp(-k1*timescale)+N2*exp(-k2*timescale),
                   start = start,
                   control = nls.control(maxiter = 100000, warnOnly = T))

        #result collection
        buffer <- data.frame(label = paste0('k1 = ', round(coef(fit)[2], 5), ' (N = ', round(coef(fit)[1], 1), '), ',
                                            'k2 = ', round(coef(fit)[4], 5), ' (N = ', round(coef(fit)[3], 1), '), ',
                                            'NUS0 = ', round(coef(fit)[5], 1)),
                             species.name = i,
                             position.x = min(selected.species$timescale),
                             position.y = max(selected.species$NUS))

        fit.list <- rbind(fit.list, buffer)
      }

      buffer <- NULL #empties buffer
      start.df <- NULL #empties start.df
      selected.species <- NULL #empties selected.species

    }

    #plot
    p7 <- ggplot(data = centroidscaled(), aes(x = centroidscaled()$timescale, y = NUS()$NUS)) +
      geom_point(data = selected.points,
                 size = input$size.kin, aes(x = timescale, y = NUS, color = Name),
                 alpha = as.numeric(input$trans.kin),
                 inherit.aes = F) +
      xlab("time (min)") +
      ylab("NUS") +
      scale_color_manual(values = c(input$col.kin.high1, input$col.kin.high2, input$col.kin.high3,input$col.kin.high4)) +
      custom.theme +
      coord_cartesian(expand = T)

    if (isTRUE(input$fit.hdx)) { #displays fit lines and labeling is requested by user
      p7 <- p7 + geom_line(stat = "smooth", #This instead of geom_smooth to be able to apply alpha on fit line
                           method = "nls",
                           data = selected.points,
                           formula=y~NUS0+N1*exp(-k1*x)+N2*exp(-k2*x),
                           method.args = list(start = start,
                                              nls.control(maxiter = 100000, warnOnly = T)),
                           se = F,
                           inherit.aes = T,
                           aes(x = timescale, y = NUS, color = Name),
                           alpha = 0.5,
                           size = 1) +
        geom_text_repel(data = fit.list,
                        inherit.aes = F,
                        aes(x = position.x,
                            y = position.y,
                            colour = species.name,
                            label = label),
                        show.legend = F,
                        force = 3,
                        point.padding = 2,
                        alpha = 0.9,
                        size = 5,
                        fontface = "bold"
        )
    }

    return(p7)

  })

  ###HDX optim kinetics----

  output$optim.nus.plot <- renderPlot({
    binom.filter() %>%
      ungroup() %>%
      select(Species, colorscale, USE.1, USE.2, USE.mean) %>%
      pivot_longer(
        cols = 3:ncol(.),
        values_to = "NUS",
        names_to = "Population"
      ) %>%
      mutate(
        Population = case_when(
          Population == "USE.mean" ~ "overall",
          Population == "USE.1" ~ "high exchange",
          Population == "USE.2" ~ "low exchange"
        )
      ) %>%
      ggplot(
        aes(x = colorscale, y = NUS, color = Population, shape = Species)
      ) +
      geom_point(size = 4) +
      custom.theme +
      labs(x = "time (min)")
  })

  output$optim.ab.plot <- renderPlot({
    binom.filter() %>%
      ungroup() %>%
      select(Species, colorscale, fraction.1, fraction.2) %>%
      pivot_longer(
        cols = 3:ncol(.),
        values_to = "fraction",
        names_to = "Population"
      ) %>%
      mutate(
        Population = case_when(
          Population == "fraction.1" ~ "high exchange",
          Population == "fraction.2" ~ "low exchange"
        )
      ) %>%
      ggplot(
        aes(x = colorscale, y = fraction, color = Population, shape = Species)
      ) +
      geom_point(size = 4) +
      custom.theme +
      labs(x = "time (min)")
  })



  #Download kinetics plots-----------
  Plot6 <- reactive({
    ggplot(data = centroids(), aes(x = centroids()$mean.time, y = centroids()$centroid)) +
      geom_point(color = "steelblue", size = 3) +
      custom.theme
  })

  output$dwnplot <- downloadHandler(
    filename = function() { paste("kinetics-raw", '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = Plot6(), device = "png")
    }
  )

  output$plot7 <- renderPlot({
    plot7()
  })

  output$dwnplot2 <- downloadHandler(
    filename = function() { paste("kinetics-NUS", '.png', sep='') },
    content = function(file) {
      gggsave(file, plot = plot7(), device = "png",
              width = 200,
              height = 125,
              units = 'mm',
              dpi = 600)
    }
  )

  output$dwnplot3 <- downloadHandler(
    filename = function() { paste("kinetics-NUS", '.pdf', sep='') },
    content = function(file) {
      ggsave(file, plot = plot7(), device = "pdf",
             width = 200,
             height = 125,
             units = 'mm',
             dpi = 600)
    }
  )

  #PALETTE MODS-------
  palette.modifier <- function(plot = NULL){
    if (input$select.melting.palette.fam == 'd3') {
      plot <- plot + scale_color_d3(palette = input$select.melting.palette,
                                    labels = tm.init.change()$legend)
    } else {
      if (input$select.melting.palette.fam == "Brewer - qualitative") {
        plot <- plot + scale_color_brewer(palette = input$select.melting.palette,
                                          labels = tm.init.change()$legend)
      } else{
        if (input$select.melting.palette.fam == "Brewer - sequential") {
          plot <- plot + scale_color_brewer(palette = input$select.melting.palette,
                                            labels = tm.init.change()$legend)
        } else {
          if (input$select.melting.palette.fam == "Brewer - diverging") {
            plot <- plot + scale_color_brewer(palette = input$select.melting.palette,
                                              labels = tm.init.change()$legend)
          } else {
            if (input$select.melting.palette.fam == "NPG") {
              plot <- plot + scale_color_npg(labels = tm.init.change()$legend)
            } else {
              if (input$select.melting.palette.fam == "AAAS") {
                plot <- plot + scale_color_aaas(labels = tm.init.change()$legend)
              } else {
                if (input$select.melting.palette.fam == "NEJM") {
                  plot <- plot + scale_color_nejm(labels = tm.init.change()$legend)
                } else {
                  if (input$select.melting.palette.fam == "Lancet") {
                    plot <- plot + scale_color_lancet(labels = tm.init.change()$legend)
                  } else {
                    if (input$select.melting.palette.fam == "JAMA") {
                      plot <- plot + scale_color_jama(labels = tm.init.change()$legend)
                    } else {
                      if (input$select.melting.palette.fam == "JCO") {
                        plot <- plot + scale_color_jco(labels = tm.init.change()$legend)
                      } else {
                        if (input$select.melting.palette.fam == "UCSCGB") {
                          plot <- plot + scale_color_ucscgb(labels = tm.init.change()$legend)
                        } else {
                          if (input$select.melting.palette.fam == "LocusZoom") {
                            plot <- plot + scale_color_locuszoom(labels = tm.init.change()$legend)
                          } else {
                            if (input$select.melting.palette.fam == "IGV") {
                              plot <- plot + scale_color_igv(palette = input$select.melting.palette,
                                                             labels = tm.init.change()$legend)
                            } else {
                              if (input$select.melting.palette.fam == "UChicago") {
                                plot <- plot + scale_color_uchicago(palette = input$select.melting.palette,
                                                                    labels = tm.init.change()$legend)
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  #Palette family selector
  output$select.melting.palette.fam <- renderUI({
    pickerInput("select.melting.palette.fam",
                label = "Choose palette family",
                choices = list("d3",
                               "Brewer - qualitative", "Brewer - diverging", "Brewer - sequential",
                               "AAAS", "IGV", "JAMA", "JCO", "Lancet", 'LocusZoom', 'NEJM', "NPG",
                               "UChicago", "UCSCGB"),
                multiple = F
    )
  })

  #Palette subcategory selector
  output$select.melting.palette <- renderUI({

    if (input$select.melting.palette.fam == 'd3') {
      pickerInput("select.melting.palette",
                  label = "Choose palette",
                  choices = list("d3a" = "category20",
                                 "d3b" = "category20b",
                                 "d3c" = "category20c"),
                  multiple = F
      )
    } else {
      if (input$select.melting.palette.fam == 'Brewer - qualitative') {
        pickerInput("select.melting.palette",
                    label = "Choose palette",
                    choices = list("Accent", "Dark2", "Paired",
                                   "Pastel1", "Pastel2", "Set1",
                                   "Set2", "Set3"),
                    multiple = F
        )
      } else {
        if (input$select.melting.palette.fam == 'Brewer - diverging') {
          pickerInput("select.melting.palette",
                      label = "Choose palette",
                      choices = list("BrBG", 'PiYG', 'PRGn',
                                     'PuOr', 'RdBu', 'RdGy',
                                     'RdYlBu', 'RdYlGn', 'Spectral'),
                      multiple = F
          )
        } else {
          if (input$select.melting.palette.fam == 'Brewer - sequential') {
            pickerInput("select.melting.palette",
                        label = "Choose palette",
                        choices = list('Blues', 'BuGn', 'BuPu', 'GnBu',
                                       'Greens', 'Greys', 'Oranges', 'OrRd',
                                       'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu',
                                       'Reds', 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd'),
                        multiple = F
            )
          } else {
            if (input$select.melting.palette.fam == 'IGV') {
              pickerInput("select.melting.palette",
                          label = "Choose palette",
                          choices = list("default", "alternating"),
                          multiple = F
              )
            } else {
              if (input$select.melting.palette.fam == 'UChicago') {
                pickerInput("select.melting.palette",
                            label = "Choose palette",
                            choices = list("default", "light", "dark"),
                            multiple = F
                )
              } else {
                return(NULL)
              }
            }
          }
        }
      }
    }

  })






  # #output options-----------
  outputOptions(output, "plot5", suspendWhenHidden = FALSE)
  outputOptions(output, "plot5bi", suspendWhenHidden = FALSE)
  # outputOptions(output, "k.plot", suspendWhenHidden = FALSE)
  # outputOptions(output, "eq.raw", suspendWhenHidden = FALSE)


}
