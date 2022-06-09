
#max file size---------
options(shiny.maxRequestSize=5000*1024^2)

#Dependencies----

##libraries----

# BiocManager::install("mzR")

librarian::shelf(
  tidyverse, readr, readxl, data.table, DT, tidytable, magrittr, stringr,
  formattable, gnm, DescTools,
  ggpubr, ggrepel, ggthemes, ggpmisc, thematic, zoo,
  BiocManager, V8, mzR,
  bslib
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


#server---------
server <- function(input, output, session) {

  #0. Theme-----

  ## 0.1. Custom theme for figures----

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

  ## 0.2. Dark/light switch----
  theme.dark <- observeEvent(input$theme, {
    if(isTRUE(input$theme)){
      thematic_shiny(
        bg = '#272c30', fg = '#EEE8D5', accent = 'auto',
        sequential = hcl.colors(n = 42, palette = 'viridis')
      )
    } else {
      thematic_shiny(
        bg = 'white', fg = 'black', accent = 'auto',
        sequential = hcl.colors(n = 42, palette = 'viridis')
      )
    }
  })

  output$theme.dark <- renderUI({
    prettySwitch(
      inputId = 'theme',
      label = "Figure dark mode",
      value = TRUE,
      fill = TRUE,
      status = 'success'
    )
  })


  #1. OligoRef----------

  ## 1.1. Sequencing----

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
        tags$b(style="color:#EEE8D5", 'Chemical formula: '),
        tags$span(style="color:#EEE8D5", "C"), tags$sub(style="color:#EEE8D5", sequencer()$nC),
        tags$span(style="font-weight: bold; color:#EEE8D5", "H"), tags$sub(style="font-weight: bold; color:#EEE8D5", sequencer()$nH),
        tags$span(style="color:#EEE8D5", "O"), tags$sub(style="color:#EEE8D5", sequencer()$nO),
        tags$span(style="color:#EEE8D5", "N"), tags$sub(style="color:#EEE8D5", sequencer()$nN),
        tags$span(style="color:#EEE8D5", "P"), tags$sub(style="color:#EEE8D5", sequencer()$nP),
        tags$span(style="color:#EEE8D5", "K"), tags$sub(style="color:#EEE8D5", sequencer()$nK),
        sep = ''
      )
    }
  })

  output$ex.sites <- renderText({
    if (!is.na(sequencer()$nH)) {
      paste(
        tags$span(style="font-style: italic; color:#d5dbee", "with "),
        tags$span(style="font-style: italic; font-weight: bold; color:#d5dbee", sequencer()$nX),
        tags$span(style="font-style: italic; color:#d5dbee", " exchangeable H"),
        sep = ''
      )
    }
  })

  output$nX <- renderText(sequencer()$nX)

  ## 1.2. Mass calculations---------

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
      extensions = c('Buttons', 'Responsive', 'Scroller', 'ColReorder', 'RowReorder'),
      rownames = F,
      escape = T,
      filter = 'top',
      autoHideNavigation = T,
      options = list(
        colReorder = TRUE, rowReorder = TRUE,
        deferRender = TRUE,
        scrollY = 300,
        scroller = TRUE,
        autoWidth = F,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      )
    ) %>%
      formatRound(c('Values'), digits = 5, interval = 3, mark = '') %>%
      formatStyle(
        columns = 0:6,
        target = 'row',
        background = '#272c30'
      )

  })

  output$charge.series <- renderDT(server = FALSE,{
    datatable(
      data.frame(
        z = 1:sequencer()$nb_PO
      ) %>%
        mutate(
          "Average m/z" = round(
            (massr()$AveMW-z*1.00811)/z,
            5
          ),
          "Monoisotopic m/z" = round(
            (massr()$MonoMW-z*1.00811)/z,
            5
          )
        ),
      style = "bootstrap",
      extensions = c('Buttons', 'Responsive', 'Scroller', 'ColReorder', 'RowReorder'),
      rownames = F,
      escape = T,
      filter = 'top',
      autoHideNavigation = T,
      options = list(
        colReorder = TRUE, rowReorder = TRUE,
        deferRender = TRUE,
        scrollY = 300,
        scroller = TRUE,
        autoWidth = F,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      )
    ) %>%
      formatStyle(
        columns = 0:2,
        target = 'row',
        background = '#272c30'
      )
  })

  ## 1.3. Peak position calculation-----------
  peak.position <- reactive({
    peak.positionR(nrPeaks.user = as.numeric(input$nrPeaks.user),
                   DC = as.numeric(input$DC),
                   K41C = as.numeric(input$K41C),
                   seq = sequencer(),
                   MonoMW = massr()$MonoMW)
  })

  ## 1.4. Plotting-----

  ### 1.4.1. Theoretical distibutions----
  output$peak.position <- renderDT(server = FALSE, {
    datatable(
      peak.position(),
      style = "bootstrap",
      extensions = c('Buttons', 'Responsive', 'Scroller', 'ColReorder', 'RowReorder'),
      rownames = F,
      escape = T,
      filter = 'top',
      autoHideNavigation = T,
      colnames = c('m/z' = 'mz.th',
                   'Abundance' = 'Iso.Pattern'),
      options = list(
        colReorder = TRUE, rowReorder = TRUE,
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        pageLength = 6,
        autoWidth = F,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
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

  ### 1.4.2. Experimental HDX reference----

  #### 1.4.2.1 Calculations----

  ##### 1.4.2.2 Experimental centroid----
  exp.centroid.ref <- reactive({

    calculation <- MSsnaps.ref() %>%
      mutate(centroid = sum(mz * intensum)/sum(intensum))

    exp.centroid.ref <- calculation$centroid[1]
  })

  ##### 1.4.2.3. Accuracy of the reference centroid----
  centroid.ac <- reactive({
    1000000 * abs(exp.centroid.ref()-massr()$Avemz)/massr()$Avemz
  })

  #### 1.4.2.4. Plot----

  p.hdx.ref <- reactive({
    ggplot(data = peak.position(), aes(x = mz.th, y = Iso.Pattern)) +
      geom_line(color = input$col.line.th, size = input$size.line.th) +
      geom_point(color = input$col.dot.th, size = input$size.dot.th) +
      geom_vline(xintercept = massr()$Avemz, linetype = 'dashed', color = input$col.centroid.th, size = input$size.centroid.th) +
      xlab("m/z") +
      ylab('normalized abundance') +
      custom.theme
  })

  output$p.hdx.ref <- renderPlot({
    p.hdx.ref()
  })

  output$plot.ref.ui <- renderUI({
    plotOutput(
      "p.hdx.ref",
      width = as.numeric(input$plot.ref.w),
      height = as.numeric(input$plot.ref.h)
    )
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

  output$p.hdx.ref.vs.exp.ui <- renderUI({
    plotOutput(
      "p.hdx.ref.vs.exp",
      width = as.numeric(input$plot.ref.w),
      height = as.numeric(input$plot.ref.h)
    )
  })

  ### 1.4.3. Downloads----

  #### 1.4.3.1 Theoretical distribution----
  output$ref.pdf <- downloadHandler(
    filename = function() { paste("Theorerical isotopic distribution", '.pdf', sep='') },
    content = function(file) {
      ggsave(
        file,
        plot = p.hdx.ref(),
        device = "pdf",
        width = 29.7,
        height = 29.7*input$plot.ref.h/input$plot.ref.w,
        units = 'cm',
        bg = NULL
      )
    }
  )

  output$ref.png <- downloadHandler(
    filename = function() { paste("Theorerical isotopic distribution", '.png', sep='') },
    content = function(file) {
      ggsave(
        file,
        plot = p.hdx.ref(),
        device = "png",
        width = 29.7,
        height = 29.7*input$plot.ref.h/input$plot.ref.w,
        units = 'cm',
        bg = NULL
      )
    }
  )

  #### 1.4.3.2 Theoretical + experimental distributions----
  output$ref.accu.pdf <- downloadHandler(
    filename = function() { paste("Reference HDX", '.pdf', sep='') },
    content = function(file) {
      ggsave(
        file,
        plot = p.hdx.ref.vs.exp(),
        device = "pdf",
        width = 29.7,
        height = 29.7*input$plot.ref.h/input$plot.ref.w,
        units = 'cm',
        bg = NULL
      )
    }
  )

  output$ref.accu.png <- downloadHandler(
    filename = function() { paste("Reference HDX", '.png', sep='') },
    content = function(file) {
      ggsave(
        file,
        plot = p.hdx.ref.vs.exp(),
        device = "png",
        width = 29.7,
        height = 29.7*input$plot.ref.h/input$plot.ref.w,
        units = 'cm',
        bg = NULL
      )
    }
  )

  #2. MSxploR----

  ##2.1. data import---------------

  mzlimits <- c(400,4000) #hard-coded limit on data range

  inFile <- reactive({
    input$mzml.file
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

  ##2.2 TIC------------

  TIC <- reactive({

    TIC <- data.frame(hd()[7]/60, hd()[2], hd()[6])

    colnames(TIC)[1:3] <- c("time","scan","intensity")

    return(TIC)
  })


  output$plot1 <- renderPlot({

    req(input$mzml.file)

    ggplot(data = TIC(), aes(x = time, y = intensity)) +
      geom_line(color = input$col.TIC, size = input$size.line.TIC) +
      xlab("time (min)") +
      custom.theme
  }, bg = "transparent")

  ###2.2.1 Selection of MS data from TIC-------
  selectedData <- reactive({
    brushedPoints(TIC(), input$plot_brush)
  })

  #### brushinput
  scansbrsh <- reactive({
    min(selectedData()$scan):max(selectedData()$scan)
  })

  ###Selection of brushed scans
  selecscansbrsh <- reactive({
    as.data.table(inputms())[scan %in% scansbrsh()]

  })

  ###2.2.2. Time management--------

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

  ##2.3 MS Spectrum----

  ###2.3.1 Scan summing-----------

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

  ###2.3.2. Print selection info---------

  ####2.3.2.1 Time----
  output$info <- renderText({

    req(input$mzml.file)

    paste(round(min(selectedData()$time), 2), "-", round(max(selectedData()$time), 2), " min",
          sep = "")
  })

  ####2.3.2.2 Scans----
  output$info1 <- renderText({

    req(input$mzml.file)

    paste(min(selectedData()$scan), "-", max(selectedData()$scan),
          sep = "")
  })


  ### 2.3.3. Zoom----

  #### 2.3.3.1. Definition of initial mz range-------
  ranges <- reactiveValues(x = mzlimits, y = NULL)

  #### 2.3.3.2. Zoom event---------
  observeEvent(input$plot3_brush, {
    brush <- input$plot3_brush
    ranges$x <- c(brush$xmin, brush$xmax)
    ranges$y <- c(brush$ymin, brush$ymax)
  })

  specsumbrsh.ms <- reactive({

    specsumbrsh.ms <- specsumbrsh()[mz > min(ranges$x) & mz < max(ranges$x)]

  })

  # Zoom reset
  observeEvent(input$plot3_dblclick, {
    brush <- input$plot3_brush
    ranges$x <- mzlimits
    ranges$y <- NULL
  })


  # m/z range print info
  output$info2 <- renderText({
    paste(round(min(ranges$x), 2), " - ", round(max(ranges$x), 2), sep = "")
  })

  ### 2.3.4. Plot----

  plot3 <- reactive({

    req(input$mzml.file)

    ggplot(
      data = specsumbrsh(),
      aes(x = mz, y = intensum)
    ) +
      geom_line(
        color = input$col.MS,
        size = input$size.line.MS
      ) +
      labs(
        x = "m/z",
        y = "intensity"
      ) +
      custom.theme +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
  })

  output$plot3 <- renderPlot({
    plot3()
  })


  ### 2.3.5. Download spectra-------

  output$plot3.pdf <- downloadHandler(
    filename = function() { paste("Mass spectrum", '.pdf', sep='') },
    content = function(file) {
      ggsave(
        file,
        plot = plot3(),
        device = "pdf",
        width = 29.7,
        height = 21,
        units = 'cm',
        bg = NULL
      )
    }
  )

  output$plot3.png <- downloadHandler(
    filename = function() { paste("Mass spectrum", '.png', sep='') },
    content = function(file) {
      ggsave(
        file,
        plot = plot3(),
        device = "png",
        width = 29.7,
        height = 21,
        units = 'cm',
        bg = NULL
      )
    }
  )

  ##2.3. MS snapshots------------

  ###2.3.1. MS ref snapshots------------

  MSsnaps.ref <- reactive({
    newrow.ref <- data.frame(specsumbrsh.ms())
  }) %>%
    bindEvent(input$bttn99)

  ###2.3.2. MS time point snapshots------------

  #### 2.3.2.1 snapping----
  snaps <- data.frame()

  inputsnap <- reactive({

    inputsnap <- specsumbrsh.ms()[, ':=' (
      min.time = min(selectedData()$time), #traceability
      max.time = max(selectedData()$time),
      min.scan = min(selectedData()$scan),
      max.scan = max(selectedData()$scan),
      min.mz = min(specsumbrsh.ms()$mz),
      max.mz = max(specsumbrsh.ms()$mz)
    )]

    return(inputsnap)

  })

  MSsnaps <- reactive({

    newrow <- data.frame(inputsnap())
    snaps <<- rbind(snaps, newrow)

  }) %>%
    bindEvent(input$bttn1)


  #### 2.3.2.2. Output table----
  output$MSsnap <- renderDT(server = TRUE,{
    datatable(
      MSsnaps() %>%
        select(filename, Species, mean.time, CFtime, mz, intensum,
               min.time, max.time, min.scan, max.scan),
      style = "bootstrap",
      extensions = c('Buttons', 'Responsive', 'Scroller', 'ColReorder', 'RowReorder'),
      selection = 'multiple',
      colnames = c(
        'File name' = 'filename',
        'Intensity' = 'intensum',
        'Start TIC time' = 'min.time',
        'End TIC time' = 'max.time',
        'Start scan' = 'min.scan',
        'End scan' = 'max.scan',
        'm/z' = "mz",
        'Manual time (min)' = 'CFtime',
        'TIC time (min)' = 'mean.time'
      ),
      editable = T,
      rownames = F,
      escape = T,
      filter = 'top',
      autoHideNavigation = T,
      plugins = 'natural',
      options = list(
        colReorder = TRUE, rowReorder = TRUE,
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        autoWidth = F,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'colvis'),
        columnDefs = list(list(visible=FALSE, targets=c(0,6:9)))
      )
    ) %>%
      formatStyle(
        columns = 0:9,
        target = 'row',
        background = '#272c30'
      )
  })

  ### 2.3.3. Titration snapshots----

  #### 2.3.3.1 snapping----

  snaps.target <- data.frame()

  snaps.std <- data.frame()

  MSsnaps.target <- reactive({
    newrow <- data.frame(inputsnap())
    snaps.target <<- rbind(snaps.target, newrow)
  }) %>%
    bindEvent(input$bttn.target)

  MSsnaps.std <- reactive({
    newrow <- data.frame(inputsnap())
    snaps.std <<- rbind(snaps.std, newrow)
  }) %>%
    bindEvent(input$bttn.std)

  #### 2.3.3.2. Output tables----

  output$target <- renderDT(server = TRUE,{
    datatable(
      MSsnaps.target() %>%
        select(filename, Species, lgd.conc, Stoich, mz, intensum,
               min.time, max.time, min.scan, max.scan, min.mz, max.mz),
      style = "bootstrap",
      extensions = c('Buttons', 'Responsive', 'Scroller', 'ColReorder', 'RowReorder'),
      selection = 'multiple',
      colnames = c(
        '[Ligand] (µM)' = 'lgd.conc',
        'File name' = 'filename',
        'Ligand stoichiometry' = 'Stoich',
        'Intensity' = 'intensum',
        'Start TIC time' = 'min.time',
        'End TIC time' = 'max.time',
        'Start scan' = 'min.scan',
        'End scan' = 'max.scan',
        'Start m/z' = 'min.mz',
        'End m/z'= 'max.mz',
        'm/z' = 'mz'
      ),
      editable = T,
      rownames = F,
      escape = T,
      filter = 'top',
      autoHideNavigation = T,
      plugins = 'natural',
      options = list(
        colReorder = TRUE, rowReorder = TRUE,
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        autoWidth = F,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'colvis'),
        columnDefs = list(list(visible=FALSE, targets=c(0,6:11)))
      )
    ) %>%
      formatStyle(
        columns = 0:11,
        target = 'row',
        background = '#272c30'
      )
  })

  output$standard <- renderDT(server = TRUE,{
    datatable(
      MSsnaps.std() %>%
        select(filename, Species, lgd.conc, Stoich, mz, intensum,
               min.time, max.time, min.scan, max.scan, min.mz, max.mz),
      style = "bootstrap",
      extensions = c('Buttons', 'Responsive', 'Scroller', 'ColReorder', 'RowReorder'),
      selection = 'multiple',
      colnames = c(
        '[Ligand] (µM)' = 'lgd.conc',
        'File name' = 'filename',
        'Ligand stoichiometry' = 'Stoich',
        'Intensity' = 'intensum',
        'Start TIC time' = 'min.time',
        'End TIC time' = 'max.time',
        'Start scan' = 'min.scan',
        'End scan' = 'max.scan',
        'Start m/z' = 'min.mz',
        'End m/z'= 'max.mz',
        'm/z' = 'mz'
      ),
      editable = T,
      rownames = F,
      escape = T,
      filter = 'top',
      autoHideNavigation = T,
      plugins = 'natural',
      options = list(
        colReorder = TRUE, rowReorder = TRUE,
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        autoWidth = F,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'colvis'),
        columnDefs = list(list(visible=FALSE, targets=c(0,6:11)))
      )
    ) %>%
      formatStyle(
        columns = 0:11,
        target = 'row',
        background = '#272c30'
      )
  })

  #3. MSstackR----

  ## 3.1. Time scaling and intensity normalization----
  MSsnaps1 <- reactive({
    if (isTRUE(input$timescale)) {
      MSsnaps() %>%
        mutate(time.scale = MSsnaps()$CFtime) %>% #creates a variable to scale the color and position the spectrum in the stack based on manual or TIC time
        group_by(time.scale, Species, filename, min.time, max.time, min.scan, max.scan, min.mz, max.mz) %>%
        mutate(intensum = 1-(max(intensum)-intensum)/(max(intensum)-min(intensum))) #Normalization of each spectrum (per time point, per sample)
    } else {
      MSsnaps() %>%
        mutate(time.scale = MSsnaps()$mean.time) %>%
        group_by(time.scale, Species, filename, min.time, max.time, min.scan, max.scan, min.mz, max.mz) %>%
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


  ## 3.2. Peak picking----

  ### 3.2.1. Current data----

  MSsnaps.pp.0 <- reactive({

    ppf(raw = as.data.frame(MSsnaps1()),
        neigh = input$neighlim,
        deriv = input$deriv.lim,
        thresh = input$int.thresh
    )

  })

  ### 3.2.2. Imported data----

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
          "time.scale" = "Time (min)",
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

  ### 3.2.3. Merge current and imported data----
  MSsnaps.pp <- reactive({
    if(is.null(exported.snaps)) {
      return(MSsnaps.pp.0())
    } else {
      if(is.null(input$mzml.file)) {
        return(exported.snaps.import())
      } else{
        exported.snaps.import() %>%
          rbind(MSsnaps.pp.0() %>%
                  select(filename, Species, time.scale, mean.time, CFtime, mz, intensum, peak,
                         min.time, max.time, min.scan, max.scan, min.mz, max.mz)
          )
      }
    }
  })

  ### 3.2.4. Table output----
  output$MSsnaps.pp.table <- DT::renderDT(server = FALSE, {
    datatable(
      data = MSsnaps.pp() %>%
        select(c(filename, Species, time.scale, mean.time, CFtime, mz, intensum, peak,
                 min.time, max.time, min.scan, max.scan, min.mz, max.mz)),
      style = "bootstrap",
      extensions = c('Buttons', 'Responsive', 'Scroller', 'ColReorder', 'RowReorder'),
      selection = 'multiple',
      colnames = c(
        "Time (min)" = "time.scale",
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
        colReorder = TRUE, rowReorder = TRUE,
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        autoWidth = F,
        dom = 'Bfrtip',
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

  ### 3.2.5. Plot output----
  plot.pp <- reactive({
    ggplot(
      data = MSsnaps.pp(),
      aes(
        x = mz,
        color = time.scale
      )
    ) +
      geom_line(
        aes(y = intensum),
        size = input$size.line.snap
      ) +
      geom_point(
        data = MSsnaps.pp() %>%
          filter(peak > 0),
        aes(y = intensum),
        color = '#CC79A7',
        size = input$size.dot.pp,
        alpha = 0.5
      ) +
      xlab("m/z") +
      facet_grid(signif(time.scale, 3) ~ Species,
                 scales = common.scale()
      ) +
      scale_color_gradient(
        name = 't (min)',
        low=input$col.snap1, high=input$col.snap2,
        trans = input$trans.user
      ) +
      custom.theme +
      theme(
        # strip.text = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none"
      ) +
      coord_cartesian(expand = TRUE)
  })


  output$plot.pp <- renderPlot({
    plot.pp()
  }
  )

  output$plot.pp.ui <- renderUI({
    plotOutput("plot.pp",
               width = as.numeric(input$plot.snaps.w),
               height = as.numeric(input$plot.snaps.h)
    )
  })

  ### 3.2.6. Download spectra------

  output$plot.pp.pdf <- downloadHandler(
    filename = function() { paste("Peak picking", '.pdf', sep='') },
    content = function(file) {
      ggsave(
        file,
        plot = plot.pp(),
        device = "pdf",
        width = 29.7,
        height = 29.7*input$plot.snaps.h/input$plot.snaps.w,
        units = 'cm',
        bg = NULL
      )
    }
  )

  output$plot.pp.png <- downloadHandler(
    filename = function() { paste("Peak picking", '.png', sep='') },
    content = function(file) {
      ggsave(
        file,
        plot = plot.pp(),
        device = "png",
        width = 29.7,
        height = 29.7*input$plot.snaps.h/input$plot.snaps.w,
        units = 'cm',
        bg = NULL
      )
    }
  )


  ## 3.3. Optimization----

  ### 3.3.1. Least-square minimization----
  opt.0 <- reactive({

    withProgress(
      message = 'Optimizing data',
      detail = 'Please wait', value = 0, {

        incProgress(amount=1/2)

        opt.0 <- MSsnaps.pp() %>%
          as.data.frame() %>%
          group_by(Species, time.scale) %>%
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

  }) %>%
    bindEvent(input$optibtn)

  ### 3.3.2. Generation of optimized distributions----

  #### 3.3.2.1. All distributions----
  opt.distrib <- reactive({

    withProgress(
      message = 'Generating optimized distributions',
      detail = 'Please wait', value = 0, {

        incProgress(amount=1/3)

        opt <- opt.f(opt.0(), seq = sequencer(), mass = massr(), peaks = input$nrPeaks.user.pp)

        incProgress(amount=2/3)

        opt <- opt %>%
          select(Species, time.scale, distrib, mz.th, iso, iso.1, iso.2, everything())

        return(opt)

      })
  })

  ##### 3.3.2.1.1. Import already optimized data----

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
          "time.scale" = "Time (min)",
          "mz.th" = "m/z",
          "distrib" = "Modality",
          'iso' = "Global isotopic abundance",
          'iso.1' = "Isotopic abundance 1",
          'iso.2' = "Isotopic abundance 2"
        ) %>%
        as.data.frame()
    }
  })

  ##### 3.3.2.1.2.  binding to current data----
  opt <- reactive({
    if(is.null(exported.opt)) {
      return(opt.distrib())
    } else {
      if(is.null(input$mzml.file)) {
        return(exported.opt.import())
      } else{
        exported.opt.import() %>%
          rbind(opt.distrib())
      }
    }
  })

  #### 3.3.2.3. Filtered distributions----

  alpha <- reactive({as.numeric(input$alpha)})

  bi.t.limit <- reactive({as.numeric(input$b.t.limit)})

  opt.filter <- reactive({

    ##automatic data filtering based on RSS
    if(isTRUE(input$model.selection)){
      opt.filter <- opt() %>%
        filter(
          distrib == 'mono' & p.F >= alpha() |
            distrib == 'bi' & p.F < alpha()
        )
    } else{
      ##manual data filtering based on user input (data frame)
      opt.filter <- opt() %>%
        filter(
          distrib == 'mono' & time.scale > bi.t.limit() |
            distrib == 'bi' & time.scale <= bi.t.limit()
        )
    }

    return(opt.filter)
  })


  ### 3.3.3. Derived values----

  #### 3.3.3.1. All values----
  binom.NUS <- reactive({
    binom.NUS.f(
      opt(),
      sequencer(),
      DC.init = max(input$DC.pp),
      DC.final = min(input$DC.pp),
      ref = as.numeric(input$user.hdx.ref)
    )
  })

  #### 3.3.3.2. Filtered values----

  binom.filter <- reactive({

    ##automatic data filtering based on RSS
    if(isTRUE(input$model.selection)){
      binom.filter <- binom.NUS() %>%
        filter(
          distrib == 'mono' & p.F >= alpha() |
            distrib == 'bi' & p.F < alpha()
        )
    } else {
      ##manual data filtering based on user input (data frame)
      binom.filter <- binom.NUS() %>%
        filter(
          distrib == 'mono' & time.scale > bi.t.limit() |
            distrib == 'bi' & time.scale <= bi.t.limit()
        )
    }

    return(binom.filter)
  })


  ## 3.4. Modeling table outputs----

  ### 3.4.1. Optimized distributions----

  output$opt.table <- DT::renderDT(server = FALSE, {
    datatable(
      data = opt(),
      style = "bootstrap",
      extensions = c('Buttons', 'Responsive', 'Scroller', 'ColReorder', 'RowReorder'),
      selection = 'multiple',
      colnames = c(
        "Time (min)" = "time.scale",
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
        colReorder = TRUE, rowReorder = TRUE,
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        autoWidth = F,
        dom = 'Bfrtip',
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
        0:22,
        target = 'row',
        backgroundColor = styleEqual(c(0,1), c('#272c30', '#272c30'))
      )
  })

  ### 3.4.2. Optimization results----
  output$opt.stat <- DT::renderDT(server = FALSE, {
    datatable(
      data = opt() %>%
        select(
          Species, time.scale, distrib,
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
      extensions = c('Buttons', 'Responsive', 'Scroller', 'ColReorder', 'RowReorder'),
      selection = 'multiple',
      colnames = c(
        "Time (min)" = "time.scale",
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
        colReorder = TRUE, rowReorder = TRUE,
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        autoWidth = F,
        dom = 'Bfrtip',
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
        columnDefs = list(list(visible=FALSE, targets=c(4:9)))
      )
    ) %>%
      formatStyle(
        0:14,
        target = 'row',
        backgroundColor = styleEqual(c(0,1), c('#272c30', '#272c30'))
      )
  })

  ### 3.4.3. Derived values----
  output$binom.NUS.table <- DT::renderDT(server = TRUE, {
    datatable(
      data = binom.NUS() %>%
        select(
          Species, time.scale, distrib,
          DC1, DC2, DC.mean,
          centroid, centroid.1, centroid.2,
          USE.1, USE.2, USE.mean, delta.USE,
          ab1, ab2,
          fraction.1, fraction.2
        ),
      style = "bootstrap",
      extensions = c('Buttons', 'Responsive', 'Scroller', 'ColReorder', 'RowReorder'),
      selection = 'multiple',
      colnames = c(
        "Time (min)" = "time.scale",
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
        colReorder = TRUE, rowReorder = TRUE,
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        autoWidth = F,
        dom = 'Bfrtip',
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

  ### 3.4.4. Optimization results----
  output$opt.filter.table <- DT::renderDT(server = TRUE, {
    datatable(
      data = opt.filter() %>%
        select(
          Species, time.scale, distrib,
          mz.th, iso, iso.1, iso.2
        ),
      style = "bootstrap",
      extensions = c('Buttons', 'Responsive', 'Scroller', 'ColReorder', 'RowReorder'),
      selection = 'multiple',
      colnames = c(
        "Time (min)" = "time.scale",
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
        colReorder = TRUE, rowReorder = TRUE,
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        autoWidth = F,
        dom = 'Bfrtip',
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

  ### 3.4.5. Filtered derived results----
  output$binom.filter.table <- DT::renderDT(server = TRUE, {
    datatable(
      data = binom.filter()%>%
        select(
          Species, time.scale, distrib,
          DC1, DC2, DC.mean,
          centroid, centroid.1, centroid.2,
          USE.1, USE.2, USE.mean, delta.USE,
          ab1, ab2,
          fraction.1, fraction.2
        ),
      style = "bootstrap",
      extensions = c('Buttons', 'Responsive', 'Scroller', 'ColReorder', 'RowReorder'),
      selection = 'multiple',
      colnames = c(
        "Time (min)" = "time.scale",
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
        colReorder = TRUE, rowReorder = TRUE,
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        autoWidth = F,
        dom = 'Bfrtip',
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
        0:16,
        target = 'row',
        backgroundColor = styleEqual(c(0,1), c('#272c30', '#272c30'))
      )
  })

  ## 3.5. Optimized  plot outputs-------

  optiplot <- reactive({

    if(!is.null(input$mzml.file)){
      optiplot <- plot.pp()
    } else {
      if(!is.null(input$exported.snaps)){
        optiplot <- plot.pp()
      } else {

        optiplot <- ggplot() +
          facet_grid(
            signif(time.scale, 3) ~ Species,
            scales = common.scale()
          ) +
          custom.theme +
          theme(strip.text = element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.line.y = element_blank(),
                axis.ticks.y = element_blank(),
                legend.position = "right") +
          coord_cartesian(expand = TRUE)
      }
    }

    opt.filter <- opt.filter() %>%
      group_by(time.scale, Species) %>%
      arrange(mz.th) %>%
      slice_head(n = input$nrPeaks.user.pp)


    optiplot <- optiplot +
      geom_point(data = opt.filter,
                 aes(x = mz.th, y = iso.1),
                 size = input$size.dot.opt, color = '#E69F00', alpha = 0.75) +
      geom_line(data = opt.filter,
                aes(x = mz.th, y = iso.1),
                size = input$size.line.opt, color = '#E69F00', alpha = 0.75) +
      geom_point(data = opt.filter,
                 aes(x = mz.th, y = iso.2),
                 size = input$size.dot.opt, color = '#009E73', alpha = 0.75) +
      geom_line(data = opt.filter,
                aes(x = mz.th, y = iso.2),
                size = input$size.line.opt, color = '#009E73', alpha = 0.75)  +
      geom_point(data = opt.filter,
                 aes(x = mz.th, y = iso),
                 size = input$size.dot.opt, color = '#0072B2', alpha = 0.75) +
      geom_line(data = opt.filter,
                aes(x = mz.th, y = iso),
                size = input$size.line.opt, color = '#0072B2', alpha = 0.75) +
      geom_segment(data = opt.filter() %>% distinct(., time.scale, Species, distrib, centroid),
                   aes(x = centroid, xend = centroid, y = 0, yend = 1),
                   color = '#0072B2',
                   size = input$size.line.cent, alpha = 0.75,
                   linetype = 'dashed')  +
      geom_segment(data = opt.filter() %>% distinct(., time.scale, Species, distrib, centroid.1),
                   aes(x = centroid.1, xend = centroid.1, y = 0, yend = 1),
                   color = '#E69F00',
                   size = input$size.line.cent, alpha = 0.75,
                   linetype = 'dashed') +
      geom_segment(data = opt.filter() %>% distinct(., time.scale, Species, distrib, centroid.2),
                   aes(x = centroid.2, xend = centroid.2, y = 0, yend = 1),
                   color = '#009E73',
                   size = input$size.line.cent, alpha = 0.75,
                   linetype = 'dashed')

    if (is.null(input$mzml.file)&is.null(input$exported.snaps)&is.null(input$exported.opt)) {
      return(NULL)
    } else {
      return(optiplot)
    }
  })

  output$optiplot <- renderPlot({
    optiplot()
  })


  output$optiplot.ui <- renderUI({
    plotOutput("optiplot",
               width = as.numeric(input$plot.snaps.w),
               height = as.numeric(input$plot.snaps.h)
    )
  })


  ## 3.6. Download spectra----------

  output$optiplot.pdf <- downloadHandler(
    filename = function() { paste("Deconvoluted spectra", '.pdf', sep='') },
    content = function(file) {
      ggsave(
        file,
        plot = optiplot(),
        device = "pdf",
        width = 29.7,
        height = 29.7*input$plot.snaps.h/input$plot.snaps.w,
        units = 'cm',
        bg = NULL
      )
    }
  )

  output$optiplot.png <- downloadHandler(
    filename = function() { paste("Deconvoluted spectra", '.png', sep='') },
    content = function(file) {
      ggsave(
        file,
        plot = optiplot(),
        device = "png",
        width = 29.7,
        height = 29.7*input$plot.snaps.h/input$plot.snaps.w,
        units = 'cm',
        bg = NULL
      )
    }
  )

  ## 4. HDXplotR--------

  ###4.1. Centroid calculation----

  #### 4.1.1. Calculation----
  centroids <- reactive({

    req(MSsnaps.pp())

    MSsnaps.pp() %>%
      # group_by(mean.time, Species) %>%
      group_by(time.scale, mean.time, Species, CFtime, filename, min.time, max.time, min.scan, max.scan, min.mz, max.mz) %>%
      summarise(centroid = weighted.mean(mz, intensum)) #calculation of centroids
  })

  #### 4.1.2. Species naming----
  NUS.init0 <- reactive({

    data.frame(Species = paste("Species", c(1:8)),
               Name = paste("Species", c(1:8)),
               Reference = rep(massr()$Avemz, 8),
               Charge = rep(as.numeric(input$z), 8),
               D.initial = rep(max(input$DC.pp), 8),
               D.final = rep(min(input$DC.pp), 8)
    )


  })

  #### 4.1.3. Hot table for user input----
  NUS.change <- reactive({
    req(input$hotable2)
    req(NUS.init0())
    as.data.frame(hot.to.df(input$hotable2))
  })

  output$hotable2 <- renderHotable({
    # req(centroids())
    NUS.init0()
  }, readOnly = F)

  #### 4.1.4. Apparent centroid plot----

  p.app.cent <- reactive({

    req(NUS()) #computes only once centroidscaled() is populated

    #data selection
    s = input$NUS_rows_selected
    selected.points <- NUS()[ s,]

    p <- ggplot(
      data = centroids(),
      aes(x = centroids()$time.scale, y = centroids()$centroid)
    ) +
      geom_point(
        color = input$col.kin, size = input$size.kin
      ) +
      geom_point(
        data = selected.points,
        aes(x = time.scale, y = centroid, color = Name),
        inherit.aes = F,
        size = input$size.kin
      ) +
      scale_color_manual(
        values = c(
          input$col.kin.high1, input$col.kin.high2, input$col.kin.high3,input$col.kin.high4,
          input$col.kin.high5, input$col.kin.high6, input$col.kin.high7,input$col.kin.high8
        )
      ) +
      xlab("time (min)") +
      ylab("centroid (m/z)") +
      custom.theme

    return(p)
  })

  output$p.app.cent <- renderPlot({p.app.cent()})

  output$p.app.cent.ui <- renderUI({
    plotOutput("p.app.cent",
               width = as.numeric(input$plot.hdx.w),
               height = as.numeric(input$plot.hdx.h)
    )
  })


  ### 4.2 NUS calculation----

  #### 4.2.1 Experiment switch----

  #switch initialization
  rt.cf.switch <- reactiveVal(0)

  #positive value for CF experiments
  observeEvent(input$bttn1, {
    new.rt.cf.switch <- 1
    rt.cf.switch(new.rt.cf.switch)
  })

  #negative value for RT experiments
  observeEvent(input$bttn42, {
    new.rt.cf.switch <- - 1
    rt.cf.switch(new.rt.cf.switch)
  })


  #### 4.2.2 Apparent NUS----

  NUS <- reactive({

    #uses RT or CF data depending on switch value
    if(rt.cf.switch()>=0){
      NUS <- centroids()
    } else {
      NUS <- k.norm.0() %>%
        select(filename, Species,
               mean.time, mean.centroid) %>%
        unique() %>%
        set_colnames(c('filename', 'Species', 'time.scale', 'centroid')) %>%
        mutate(
          min.time = NA_real_,
          max.time = NA_real_,
          min.scan = NA_real_,
          max.scan = NA_real_,
          min.mz = NA_real_,
          max.mz = NA_real_,
          mean.time = time.scale,
          CFtime = NA_real_,
          CFtime.s = NA_real_
        )
    }

    NUS %>%
      left_join(NUS.change(), by = "Species") %>%
      group_by(
        time.scale, mean.time, Species, CFtime, filename,
        min.time, max.time, min.scan, max.scan, min.mz, max.mz
      ) %>%
      mutate(#NUS calculation
        NUS = (centroid - Reference)*Charge/((D.initial - D.final)/100*(2.013553-1.007825)),
        mean.time.s = mean.time * 60,
        CFtime.s = CFtime * 60) %>% #creation of a time column in seconds
      dplyr::select(
        Species, Name,
        time.scale, mean.time, mean.time.s, CFtime, CFtime.s,
        centroid, NUS, Reference, Charge,
        filename, min.time, max.time, min.scan, max.scan, min.mz, max.mz
      )


  }) %>%
    bindEvent(input$bttn.kin.hdx.export)


  output$NUS <- DT::renderDT(server = FALSE, {

    datatable(data = NUS() %>%
                select(
                  filename, min.time, max.time, min.scan, max.scan,
                  Species, Name, time.scale, mean.time, mean.time.s, CFtime, CFtime.s,
                  centroid, Reference, Charge, NUS
                ),
              style = "bootstrap",
              extensions = c('Buttons', 'Responsive', 'Scroller', 'ColReorder', 'RowReorder'),
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
                "Time (min)" = "time.scale"
              ),
              editable = F,
              rownames = F,
              escape = T,
              filter = 'top',
              autoHideNavigation = T,
              plugins = 'natural',
              options = list(
                colReorder = TRUE, rowReorder = TRUE,
                deferRender = TRUE,
                scrollY = 200,
                scroller = TRUE,
                autoWidth = F,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'colvis'),
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
  NUS.proxy <- DT::dataTableProxy("NUS")

  observeEvent(input$NUS.sel, {
    if (isTRUE(input$NUS.sel)) {
      DT::selectRows(NUS.proxy, input$NUS_rows_all)
    } else {
      DT::selectRows(NUS.proxy, NULL)
    }
  })


  #### 4.2.3 Deconvoluted NUS----

  ##### 4.2.3.1. Optimized NUS plot----

  optim.nus.plot <- reactive({
    binom.filter() %>%
      ungroup() %>%
      select(Species, time.scale, USE.1, USE.2, USE.mean) %>%
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
      left_join(
        NUS.change() %>%
          select(Species, Name), by = "Species"
      ) %>%
      ggplot(
        aes(
          x = time.scale, y = NUS,
          color = Population, shape = Name
        )
      ) +
      geom_point(size = input$size.kin) +
      custom.theme +
      labs(x = "time (min)")
  })

  ##### 4.2.3.2. Optimized abundance plot----
  optim.ab.plot <- reactive({
    binom.filter() %>%
      ungroup() %>%
      select(Species, time.scale, fraction.1, fraction.2) %>%
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
      left_join(NUS.change() %>% select(Species, Name), by = "Species") %>%
      ggplot(
        aes(x = time.scale, y = fraction, color = Population, shape = Name)
      ) +
      geom_point(size = input$size.kin) +
      custom.theme +
      labs(x = "time (min)")
  })

  ### 4.3 Non-linear fit----

  #### 4.3.1. Apparent NUS----

  ##### 4.3.1.1 Initialisation----

  hdx.fit.app.init <- reactive({

    #data selection
    s = input$NUS_rows_selected
    selected.points <- NUS()[ s,]


    selected.points %>%
      mutate(time.scale.s = time.scale*60) %>%
      select(Species, time.scale.s, NUS) %>%
      group_by(Species) %>%
      #initialization of offset and amplitude from raw data and linearization
      mutate(
        init.y0 = min(NUS),
        init.A = max(NUS)-min(NUS),
        log.NUS = log(NUS)
      ) %>%
      nest() %>%
      #initialisation of an apparent rate constant by linear fit
      mutate(
        init.k = map(
          data,
          ~summary(
            lm(
              formula = log.NUS ~ time.scale.s,
              data = .
            )
          )$coefficient[2]*(-1)
        )
      ) %>%
      unnest(c(init.k, data)) %>%
      select(-log.NUS) %>%
      group_by(Species, init.y0, init.k, init.A) %>%
      nest() %>%
      ##### Monoexponential fitting
      mutate(
        init.fit = map(
          data,
          ~summary(
            nls(
              formula = NUS~y0+A*exp(-k*time.scale.s),
              data = .,
              start = list(
                y0 = init.y0,
                k = init.k,
                A = init.A
              ),
              control = nls.control(maxiter = 99999)
            )
          )$coefficient %>%
            as.data.frame() %>%
            mutate(param = rownames(.)) %>%
            select(param, Estimate)
        )
      ) %>%
      unnest(init.fit) %>%
      pivot_wider(
        names_from = param,
        values_from = Estimate
      )
  })


  ##### 4.3.1.2 User input----
  output$hotable3 <- renderHotable({

    hdx.fit.app.init() %>%
      group_by(Species) %>%
      mutate(
        init.k1 = 1.1*k,
        init.k2 = 0.9*k,
        init.A1 = 1.1*A/2,
        init.A2 = 0.9*A/2
      ) %>%
      select(Species, y0, init.k1, init.k2, init.A1, init.A2) %>%
      set_colnames(c("Species", "y0", "k1", "k2", "A1", "A2"))
  },
  readOnly = F
  )


  hdx.fit.app.hot <- reactive({
    as.data.frame(hot.to.df(input$hotable3)) %>%
      set_colnames(c("Species", "y0", "init.k1", "init.k2", "init.A1", "init.A2"))
  })


  ##### 4.3.1.3 Biexponential fitting----
  hdx.fit.app <- reactive({
    hdx.fit.app.init() %>%
      #replace init. parameters by those of hotable
      #remains unchanged if user did not alter values
      select(Species, data) %>%
      left_join(
        hdx.fit.app.hot(),
        by = "Species"
      ) %>%
      mutate(
        nls.fit = map(
          data,
          ~ summary(
            nls(
              formula = NUS ~ y0 + A1*exp(-k1*time.scale.s) + A2*exp(-k2*time.scale.s),
              data = .,
              control = nls.control(maxiter = 99999),
              start = list(
                y0 = y0,
                k1 = init.k1,
                k2 = init.k2,
                A1 = init.A1,
                A2 = init.A2
              )
            )
          )$parameters %>%
            as.data.frame() %>%
            mutate(param = rownames(.))
        )
      ) %>%
      ungroup() %>%
      select(Species, data, nls.fit) %>%
      unnest(nls.fit)
  })

  #####4.3.1.4. Table----

  output$hdx.fit.app <- renderDT({
    datatable(
      data = hdx.fit.app() %>%
        left_join(NUS.change() %>%
                    select(Species, Name)
        ) %>%
        select(Species, Name, param, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`),
      style = "bootstrap",
      extensions = c('Buttons', 'Responsive', 'Scroller', 'ColReorder', 'RowReorder'),
      selection = 'multiple',
      colnames = c(
        "Parameter" = "param",
        "Standard Error" =  "Std. Error"
      ),
      editable = F,
      rownames = F,
      escape = T,
      filter = 'top',
      autoHideNavigation = T,
      plugins = 'natural',
      options = list(
        colReorder = TRUE, rowReorder = TRUE,
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        autoWidth = F,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'colvis'),
        columnDefs = list(list(visible=FALSE, targets=c(0,5,6)))
      )
    ) %>%
      formatStyle(
        columns = 0:7,
        target = 'row',
        background = '#272c30'
      ) %>%
      formatSignif(
        .,
        columns = 4:7,
        digits = 3
      )
  })

  #####4.3.1.5. Plot----

  p.hdx.fit.app <- reactive({

    if(isTRUE(input$fit.hdx)) {

      label <- hdx.fit.app() %>%
        unnest(data) %>%
        left_join(NUS.change() %>%
                    select(Species, Name)
        ) %>%
        select(Species, Name, param, Estimate, time.scale.s, NUS) %>%
        pivot_wider(
          names_from = param,
          values_from = Estimate
        ) %>%
        mutate(
          label = paste0(
            "y0 = ", signif(y0, 2),
            ", k1 = ", signif(k1, 2), ", k2 = ", signif(k2, 2),
            ", N1 = ", signif(A1, 2), ", N2 = ", signif(A2, 2)
          ),
          x = (mean(time.scale.s)),
          y = max(NUS)
        ) %>%
        select(Species, Name, label, x, y) %>%
        unique()

      hdx.fit.app() %>%
        left_join(NUS.change() %>%
                    select(Species, Name)
        ) %>%
        select(Species, Name, data, param, Estimate) %>%
        pivot_wider(
          names_from = param,
          values_from = Estimate
        ) %>%
        unnest(data) %>%
        group_by(Species, Name) %>%
        mutate(
          NUS.fit = y0 + A1*exp(-k1*time.scale.s) + A2*exp(-k2*time.scale.s)
        ) %>%
        select(-c(y0, k1, k2, A1, A2)) %>%
        ggplot(
          data = .,
          aes(time.scale.s, color = Name)
        ) +
        geom_point(
          aes(y = NUS),
          size = input$size.kin
        ) +
        geom_line(
          aes(y = NUS.fit),
          size = input$size.line.kin
        ) +
        geom_text_repel(
          data = label,
          aes(x = 1.1*x, y = y, label = label),
          fontface = "bold",
          size = input$size.txt.kin,
          show.legend = FALSE
        ) +
        custom.theme +
        scale_color_manual(
          values = c(
            input$col.kin.high1, input$col.kin.high2, input$col.kin.high3,input$col.kin.high4,
            input$col.kin.high5, input$col.kin.high6, input$col.kin.high7,input$col.kin.high8
          )
        ) +
        labs(
          x = "time (s)"
        )
    } else {
      #data selection
      s = input$NUS_rows_selected

      NUS()[ s,] %>%
        ggplot(aes(time.scale, NUS, color = Name)) +
        geom_point(size = input$size.kin) +
        custom.theme +
        scale_color_manual(
          values = c(
            input$col.kin.high1, input$col.kin.high2, input$col.kin.high3,input$col.kin.high4,
            input$col.kin.high5, input$col.kin.high6, input$col.kin.high7,input$col.kin.high8
          )
        ) +
        labs(x = "time (min)")

    }

  })

  output$p.hdx.fit.app <- renderPlot({p.hdx.fit.app()})

  output$p.hdx.fit.app.ui <- renderUI({
    plotOutput("p.hdx.fit.app",
               width = as.numeric(input$plot.hdx.w),
               height = as.numeric(input$plot.hdx.h)
    )
  })

  #### 4.3.2. Deconvoluted NUS----

  ##### 4.3.2.1 Initialisation----

  hdx.fit.opt.init <- reactive({

    binom.filter() %>%
      filter(distrib == 'bi') %>%
      mutate(time.scale.s = time.scale*60) %>%
      select(Species, time.scale.s, USE.1, USE.2, USE.mean) %>%
      group_by(Species) %>%
      #initialization of offset and amplitude from raw data and linearization
      mutate(
        init.y0.1 = min(USE.1),
        init.y0.2 = min(USE.2),
        init.y0.mean = min(USE.mean),
        init.A.1 = max(USE.1)-min(USE.1),
        init.A.2 = max(USE.2)-min(USE.2),
        init.A.mean = max(USE.mean)-min(USE.mean),
        log.NUS.mean = log(USE.mean)
      ) %>%
      nest() %>%
      #initialisation of an apparent rate constant by linear fit
      mutate(
        init.k = map(
          data,
          ~summary(
            lm(
              formula = log.NUS.mean ~ time.scale.s,
              data = .
            )
          )$coefficient[2]*(-1)
        )
      ) %>%
      unnest(c(init.k, data)) %>%
      select(-c(log.NUS.mean)) %>%
      group_by(
        Species,
        init.y0.1, init.y0.2, init.y0.mean,
        init.k,
        init.A.1, init.A.2, init.A.mean
      ) %>%
      nest() %>%
      mutate(
        nls.fit.mean = map(
          data,
          ~summary(
            nls(
              formula = USE.mean~y0+A*exp(-k*time.scale.s),
              data = .,
              start = list(
                y0 = init.y0.mean,
                k = init.k,
                A = init.A.mean
              ),
              control = nls.control(maxiter = 99999)
            )
          )$coefficient %>%
            as.data.frame() %>%
            mutate(param = rownames(.)) %>%
            select(param, Estimate) %>%
            set_colnames(c('param', 'estimate.mean'))
        ),
        nls.fit.1 = map(
          data,
          ~summary(
            nls(
              formula = USE.1~y0+A*exp(-k*time.scale.s),
              data = .,
              start = list(
                y0 = init.y0.1,
                k = init.k,
                A = init.A.1
              ),
              control = nls.control(maxiter = 99999)
            )
          )$coefficient %>%
            as.data.frame() %>%
            select(Estimate) %>%
            set_colnames(c('estimate.1'))
        ),
        nls.fit.2 = map(
          data,
          ~summary(
            nls(
              formula = USE.2~y0+A*exp(-k*time.scale.s),
              data = .,
              start = list(
                y0 = init.y0.2,
                k = init.k,
                A = init.A.2
              ),
              control = nls.control(maxiter = 99999)
            )
          )$coefficient %>%
            as.data.frame() %>%
            select(Estimate) %>%
            set_colnames(c('estimate.2'))
        )
      ) %>%
      unnest(c(nls.fit.mean, nls.fit.1, nls.fit.2))

  })

  ##### 4.3.2.2 User input----
  output$hotable4 <- renderHotable({

    hdx.fit.opt.init() %>%
      ungroup() %>%
      select(Species, param, estimate.mean, estimate.1, estimate.2) %>%
      unique() %>%
      pivot_wider(
        names_from = "param",
        values_from = c("estimate.mean", "estimate.1", "estimate.2")
      ) %>%
      group_by(Species) %>%
      mutate(
        init.k1.mean = estimate.mean_k,
        init.k2.mean = 0.1*estimate.mean_k,
        init.k1.1 = estimate.1_k,
        init.k2.1 = 0.1*estimate.1_k,
        init.k1.2 = estimate.2_k,
        init.k2.2 = 0.1*estimate.2_k,
        init.A1.mean = 1.1*estimate.mean_A/2,
        init.A2.mean = 0.9*estimate.mean_A/2,
        init.A1.1 = 1.1*estimate.1_A/2,
        init.A2.1 = 0.9*estimate.1_A/2,
        init.A1.2 = 1.1*estimate.2_A/2,
        init.A2.2 = 0.9*estimate.2_A/2,
        init.y0.mean = estimate.mean_y0,
        init.y0.1 = estimate.1_y0,
        init.y0.2 = estimate.2_y0
      ) %>%
      select(
        Species,
        init.k1.mean, init.k2.mean, init.k1.1, init.k2.1, init.k1.2, init.k2.2,
        init.A1.mean, init.A2.mean, init.A1.1, init.A2.1, init.A1.2, init.A2.2,
        init.y0.mean, init.y0.1, init.y0.2
      ) %>%
      set_colnames(
        c("Species",
          "k1 (overall)", "k2 (overall)", "k1 (high exchange)", "k2 (high exchange)", "k1 (low exchange)", "k2 (low exchange)",
          "A1 (overall)", "A2 (overall)", "A1 (high exchange)","A2 (high exchange)", "A1 (low exchange)", "A2 (low exchange)",
          "y0 (overall)", "y0 (high exchange)", "y0 (low exchange)"
        )
      )
  },
  readOnly = F
  )

  hdx.fit.opt.hot <- reactive({
    as.data.frame(hot.to.df(input$hotable4)) %>%
      set_colnames(
        c(
          'Species',
          'init.k1.mean', 'init.k2.mean', 'init.k1.1', 'init.k2.1', 'init.k1.2', 'init.k2.2',
          'init.A1.mean', 'init.A2.mean', 'init.A1.1', 'init.A2.1', 'init.A1.2', 'init.A2.2',
          'init.y0.mean', 'init.y0.1', 'init.y0.2'
        )
      )
  })

  ##### 4.3.2.3. Biexponential fitting----

  hdx.fit.opt <- reactive({
    hdx.fit.opt.init() %>%
      ungroup() %>%
      select(Species, data) %>%
      left_join(
        hdx.fit.opt.hot(),
        by = "Species"
      ) %>%
      unnest(data) %>%
      group_by(
        Species,
        init.k1.mean, init.k2.mean, init.k1.1, init.k2.1, init.k1.2, init.k2.2,
        init.A1.mean, init.A2.mean, init.A1.1, init.A2.1, init.A1.2, init.A2.2,
        init.y0.mean, init.y0.1, init.y0.2
      ) %>%
      nest() %>%
      mutate(
        nls.fit.mean = map(
          data,
          ~summary(
            nls(
              formula = USE.mean ~ y0 + A1*exp(-k1*time.scale.s) + A2*exp(-k2*time.scale.s),
              data = .,
              start = list(
                y0 = init.y0.mean,
                k1 = init.k1.mean,
                k2 = init.k2.mean,
                A1 = init.A1.mean,
                A2 = init.A2.mean
              ),
              control = nls.control(maxiter = 99999, warnOnly = TRUE)
            )
          )$coefficient %>%
            as.data.frame() %>%
            mutate(param = rownames(.)) %>%
            select(param, Estimate) %>%
            set_colnames(c('param', 'estimate.mean'))
        ),
        nls.fit.1 = map(
          data,
          ~summary(
            nls(
              formula = USE.1 ~ y0 + A1*exp(-k1*time.scale.s) + A2*exp(-k2*time.scale.s),
              data = .,
              start = list(
                y0 = init.y0.1,
                k1 = init.k1.1,
                k2 = init.k2.1,
                A1 = init.A1.1,
                A2 = init.A2.1
              ),
              control = nls.control(maxiter = 99999, warnOnly = TRUE)
            )
          )$coefficient %>%
            as.data.frame() %>%
            select(Estimate) %>%
            set_colnames(c('estimate.1'))
        ),
        nls.fit.2 = map(
          data,
          ~summary(
            nls(
              formula = USE.2 ~ y0 + A1*exp(-k1*time.scale.s) + A2*exp(-k2*time.scale.s),
              data = .,
              start = list(
                y0 = init.y0.2,
                k1 = init.k1.2,
                k2 = init.k2.2,
                A1 = init.A1.2,
                A2 = init.A2.2
              ),
              control = nls.control(maxiter = 99999, warnOnly = TRUE)
            )
          )$coefficient %>%
            as.data.frame() %>%
            select(Estimate) %>%
            set_colnames(c('estimate.2'))
        )
      ) %>%
      unnest(c(nls.fit.mean, nls.fit.1, nls.fit.2)) %>%
      ungroup() %>%
      select(Species, data, param, estimate.mean, estimate.1, estimate.2)

  })


  ##### 4.3.3.4. Table----

  output$hdx.fit.opt <- renderDT({

    pre.fit <- hdx.fit.opt.init() %>%
      ungroup() %>%
      select(Species, data) %>%
      left_join(
        hdx.fit.opt.hot(),
        by = "Species"
      ) %>%
      unnest(data) %>%
      group_by(
        Species,
        init.k1.mean, init.k2.mean, init.k1.1, init.k2.1, init.k1.2, init.k2.2,
        init.A1.mean, init.A2.mean, init.A1.1, init.A2.1, init.A1.2, init.A2.2,
        init.y0.mean, init.y0.1, init.y0.2
      ) %>%
      nest()

    pre.fit %>%
      mutate(
        nls.fit.mean = map(
          data,
          ~summary(
            nls(
              formula = USE.mean ~ y0 + A1*exp(-k1*time.scale.s) + A2*exp(-k2*time.scale.s),
              data = .,
              start = list(
                y0 = init.y0.mean,
                k1 = init.k1.mean,
                k2 = init.k2.mean,
                A1 = init.A1.mean,
                A2 = init.A2.mean
              ),
              control = nls.control(maxiter = 99999, warnOnly = TRUE)
            )
          )$coefficient %>%
            as.data.frame() %>%
            mutate(Parameter = paste0(rownames(.), " (overall)")) %>%
            select(Parameter, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`)
        )
      ) %>%
      ungroup() %>%
      select(Species, data, nls.fit.mean) %>%
      unnest(c(nls.fit.mean)) %>%
      rbind(
        pre.fit %>%
          mutate(
            nls.fit.1 = map(
              data,
              ~summary(
                nls(
                  formula = USE.1 ~ y0 + A1*exp(-k1*time.scale.s) + A2*exp(-k2*time.scale.s),
                  data = .,
                  start = list(
                    y0 = init.y0.1,
                    k1 = init.k1.1,
                    k2 = init.k2.1,
                    A1 = init.A1.1,
                    A2 = init.A2.1
                  ),
                  control = nls.control(maxiter = 99999, warnOnly = TRUE)
                )
              )$coefficient %>%
                as.data.frame() %>%
                mutate(Parameter = paste0(rownames(.), " (high exchange)")) %>%
                select(Parameter, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`)
            )
          ) %>%
          ungroup() %>%
          select(Species, data, nls.fit.1) %>%
          unnest(c(nls.fit.1))
      ) %>%
      rbind(
        pre.fit %>% mutate(
          nls.fit.2 = map(
            data,
            ~summary(
              nls(
                formula = USE.2 ~ y0 + A1*exp(-k1*time.scale.s) + A2*exp(-k2*time.scale.s),
                data = .,
                start = list(
                  y0 = init.y0.2,
                  k1 = init.k1.2,
                  k2 = init.k2.2,
                  A1 = init.A1.2,
                  A2 = init.A2.2
                ),
                control = nls.control(maxiter = 99999, warnOnly = TRUE)
              )
            )$coefficient %>%
              as.data.frame() %>%
              mutate(Parameter = paste0(rownames(.), " (low exchange)")) %>%
              select(Parameter, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`)
          )
        ) %>%
          ungroup() %>%
          select(Species, data, nls.fit.2) %>%
          unnest(nls.fit.2)
      ) %>%
      ungroup() %>%
      left_join(
        NUS.change() %>%
          select(Species, Name)
      ) %>%
      select(
        Species, Name,
        Parameter, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`
      ) %>%
      datatable(
        data = .,
        style = "bootstrap",
        extensions = c('Buttons', 'Responsive', 'Scroller', 'ColReorder', 'RowReorder'),
        selection = 'multiple',
        colnames = c(
          "Standard Error" =  "Std. Error"
        ),
        editable = F,
        rownames = F,
        escape = T,
        filter = 'top',
        autoHideNavigation = T,
        plugins = 'natural',
        options = list(
          colReorder = TRUE, rowReorder = TRUE,
          deferRender = TRUE,
          scrollY = 200,
          scroller = TRUE,
          autoWidth = F,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'colvis'),
          columnDefs = list(list(visible=FALSE, targets=c(0,5,6)))
        )
      ) %>%
      formatStyle(
        columns = 0:7,
        target = 'row',
        background = '#272c30'
      ) %>%
      formatSignif(
        .,
        columns = 4:7,
        digits = 3
      )
  })

  #####4.3.2.5. Plot----

  p.hdx.fit.opt <- reactive({

    if(isTRUE(input$fit.hdx)) {

      fit.calc <- hdx.fit.opt() %>%
        pivot_wider(
          names_from = "param",
          values_from = c("estimate.mean", "estimate.1", "estimate.2")
        ) %>%
        unnest(data) %>%
        group_by(Species) %>%
        mutate(
          NUS.fit.mean = estimate.mean_y0 + estimate.mean_A1*exp(-estimate.mean_k1*time.scale.s) + estimate.mean_A2*exp(-estimate.mean_k2*time.scale.s),
          NUS.fit.1 = estimate.1_y0 + estimate.1_A1*exp(-estimate.1_k1*time.scale.s) + estimate.1_A2*exp(-estimate.1_k2*time.scale.s),
          NUS.fit.2 = estimate.2_y0 + estimate.2_A1*exp(-estimate.2_k1*time.scale.s) + estimate.2_A2*exp(-estimate.2_k2*time.scale.s)
        )


      fit.calc %>%
        pivot_longer(
          cols = c("USE.1", "USE.2", "USE.mean"),
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
        select(Species, time.scale.s, Population, NUS) %>%
        left_join(
          fit.calc %>%
            select(Species, time.scale.s, NUS.fit.mean, NUS.fit.1, NUS.fit.2) %>%
            pivot_longer(
              cols = c("NUS.fit.mean", "NUS.fit.1", "NUS.fit.2"),
              values_to = "NUS.fit",
              names_to = "Population"
            ) %>%
            mutate(
              Population = case_when(
                Population == "NUS.fit.mean" ~ "overall",
                Population == "NUS.fit.1" ~ "high exchange",
                Population == "NUS.fit.2" ~ "low exchange"
              )
            ),
          by = c("Species", "time.scale.s", "Population")
        ) %>%
        left_join(
          NUS.change() %>%
            select(Species, Name), by = "Species"
        ) %>%
        ggplot(
          data = .,
          aes(x = time.scale.s, color = Population, shape = Name)
        ) +
        geom_point(
          aes(y = NUS),
          size = input$size.kin
        ) +
        geom_line(
          aes(y = NUS.fit),
          size = input$size.line.kin
        ) +
        custom.theme +
        labs(x = "time (s)")

    } else {
      optim.nus.plot()
    }

  })

  output$p.hdx.fit.opt <- renderPlot({p.hdx.fit.opt()})

  output$p.hdx.fit.opt.ui <- renderUI({
    plotOutput("p.hdx.fit.opt",
               width = as.numeric(input$plot.hdx.w),
               height = as.numeric(input$plot.hdx.h)
    )
  })


  #### 4.3.3. Deconvoluted populations----

  ##### 4.3.3.1 Initialisation----

  pop.fit.opt.init <- reactive({
    binom.filter() %>%
      filter(distrib == 'bi') %>%
      mutate(time.scale.s = time.scale*60) %>%
      select(Species, time.scale.s, fraction.1, fraction.2, USE.mean) %>%
      group_by(Species) %>%
      #initialization of offset and amplitude from raw data and linearization
      mutate(
        init.y0.1 = max(fraction.1),
        init.y0.2 = 1 - init.y0.1,
        init.A = max(fraction.1)-min(fraction.1),
        log.fraction.1 = log(fraction.1)
      ) %>%
      group_by(Species, init.y0.1, init.y0.2, init.A) %>%
      nest() %>%
      #initialisation of an apparent rate constant by linear fit
      mutate(
        init.k = map(
          data,
          ~summary(
            lm(
              formula = log.fraction.1 ~ time.scale.s,
              data = .
            )
          )$coefficient[2]
        )
      ) %>%
      unnest(c(init.k))
  })

  ##### 4.3.3.2 User input----
  output$hotable5 <- renderHotable({

    pop.fit.opt.init() %>%
      select(-data) %>%
      set_colnames(c(
        "Species",
        "y0 (high exchange)", "y0 (low exchange)",
        "A",
        "k2"
      ))
  },
  readOnly = F
  )


  pop.fit.opt.hot <- reactive({
    as.data.frame(hot.to.df(input$hotable5)) %>%
      set_colnames(c("Species", "init.y0.1", "init.y0.2", "init.A", "init.k"))
  })


  ##### 4.3.2.3 Monoexponential fitting----
  pop.fit.opt <- reactive({

    pop.fit.opt.init() %>%
      ungroup() %>%
      #replace init. parameters by those of hotable
      #remains unchanged if user did not alter values
      select(Species, data) %>%
      left_join(
        pop.fit.opt.hot(),
        by = "Species"
      ) %>%
      unnest() %>%
      group_by(
        Species,
        init.y0.1, init.y0.2,
        init.k,
        init.A
      ) %>%
      nest() %>%
      mutate(
        nls.fit.1 = map(
          data,
          ~summary(
            nls(
              formula = fraction.1~y0-A*exp(-k*time.scale.s),
              data = .,
              start = list(
                y0 = init.y0.1,
                k = init.k,
                A = init.A
              ),
              control = nls.control(maxiter = 99999, warnOnly = TRUE)
            )
          )$coefficient %>%
            as.data.frame() %>%
            mutate(param = rownames(.)) %>%
            select(param, Estimate) %>%
            set_colnames(c('param', 'estimate.1'))
        ),
        nls.fit.2 = map(
          data,
          ~summary(
            nls(
              formula = fraction.2~y0+A*exp(-k*time.scale.s),
              data = .,
              start = list(
                y0 = init.y0.2,
                k = init.k,
                A = init.A
              ),
              control = nls.control(maxiter = 99999, warnOnly = TRUE)
            )
          )$coefficient %>%
            as.data.frame() %>%
            select(Estimate) %>%
            set_colnames(c('estimate.2'))
        )
      ) %>%
      select(Species, data, nls.fit.1, nls.fit.2) %>%
      unnest(c(nls.fit.1, nls.fit.2))

  })

  ##### 4.3.3.4. Table----

  output$pop.fit.opt <- renderDT({

    pre.fit <- pop.fit.opt.init() %>%
      ungroup() %>%
      #replace init. parameters by those of hotable
      #remains unchanged if user did not alter values
      select(Species, data) %>%
      left_join(
        pop.fit.opt.hot(),
        by = "Species"
      ) %>%
      unnest() %>%
      group_by(
        Species,
        init.y0.1, init.y0.2,
        init.k,
        init.A
      ) %>%
      nest()

    pre.fit %>%
      mutate(
        nls.fit.1 = map(
          data,
          ~summary(
            nls(
              formula = fraction.1~y0-A*exp(-k*time.scale.s),
              data = .,
              start = list(
                y0 = init.y0.1,
                k = init.k,
                A = init.A
              ),
              control = nls.control(maxiter = 99999, warnOnly = TRUE)
            )
          )$coefficient %>%
            as.data.frame() %>%
            mutate(Parameter = paste0(rownames(.), " (high exchange)")) %>%
            select(Parameter, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`)
        )
      ) %>%
      ungroup() %>%
      select(Species, data, nls.fit.1) %>%
      unnest(c(nls.fit.1)) %>%
      rbind(
        pre.fit %>%
          mutate(
            nls.fit.2 = map(
              data,
              ~summary(
                nls(
                  formula = fraction.2~y0+A*exp(-k*time.scale.s),
                  data = .,
                  start = list(
                    y0 = init.y0.2,
                    k = init.k,
                    A = init.A
                  ),
                  control = nls.control(maxiter = 99999, warnOnly = TRUE)
                )
              )$coefficient %>%
                as.data.frame() %>%
                mutate(Parameter = paste0(rownames(.), " (low exchange)")) %>%
                select(Parameter, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`)
            )
          ) %>%
          ungroup() %>%
          select(Species, data, nls.fit.2) %>%
          unnest(c(nls.fit.2))
      ) %>%
      ungroup() %>%
      left_join(
        NUS.change() %>%
          select(Species, Name)
      ) %>%
      select(
        Species, Name,
        Parameter, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`
      ) %>%
      datatable(
        data = .,
        style = "bootstrap",
        extensions = c('Buttons', 'Responsive', 'Scroller', 'ColReorder', 'RowReorder'),
        selection = 'multiple',
        colnames = c(
          "Standard Error" =  "Std. Error"
        ),
        editable = F,
        rownames = F,
        escape = T,
        filter = 'top',
        autoHideNavigation = T,
        plugins = 'natural',
        options = list(
          colReorder = TRUE, rowReorder = TRUE,
          deferRender = TRUE,
          scrollY = 200,
          scroller = TRUE,
          autoWidth = F,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'colvis'),
          columnDefs = list(list(visible=FALSE, targets=c(0,5,6)))
        )
      ) %>%
      formatStyle(
        columns = 0:7,
        target = 'row',
        background = '#272c30'
      ) %>%
      formatSignif(
        .,
        columns = 4:7,
        digits = 3
      )

  })

  #####4.3.3.5. Plot----

  p.pop.fit.opt <- reactive({

    if(isTRUE(input$fit.hdx)) {

      fit.calc <- pop.fit.opt() %>%
        pivot_wider(
          names_from = "param",
          values_from = c("estimate.1", "estimate.2")
        ) %>%
        unnest(data) %>%
        group_by(Species) %>%
        mutate(
          frac.fit.1 = estimate.1_y0 - estimate.1_A*exp(-estimate.1_k*time.scale.s),
          frac.fit.2 = estimate.2_y0 + estimate.2_A*exp(-estimate.2_k*time.scale.s)
        )


      fit.calc %>%
        pivot_longer(
          cols = c("fraction.1", "fraction.2"),
          values_to = "fraction",
          names_to = "Population"
        ) %>%
        mutate(
          Population = case_when(
            Population == "fraction.1" ~ "high exchange",
            Population == "fraction.2" ~ "low exchange"
          )
        ) %>%
        select(Species, time.scale.s, Population, fraction) %>%
        left_join(
          fit.calc %>%
            select(Species, time.scale.s, frac.fit.1, frac.fit.2) %>%
            pivot_longer(
              cols = c("frac.fit.1", "frac.fit.2"),
              values_to = "frac.fit",
              names_to = "Population"
            ) %>%
            mutate(
              Population = case_when(
                Population == "frac.fit.1" ~ "high exchange",
                Population == "frac.fit.2" ~ "low exchange"
              )
            ),
          by = c("Species", "time.scale.s", "Population")
        ) %>%
        left_join(
          NUS.change() %>%
            select(Species, Name), by = "Species"
        ) %>%
        ggplot(
          data = .,
          aes(x = time.scale.s, color = Population, shape = Name)
        ) +
        geom_point(
          aes(y = fraction),
          size = input$size.kin
        ) +
        geom_line(
          aes(y = frac.fit),
          size = input$size.line.kin
        ) +
        custom.theme +
        labs(x = "time (s)")

    } else {
      optim.ab.plot()
    }

  })

  output$p.pop.fit.opt <- renderPlot({p.pop.fit.opt()})

  output$p.pop.fit.opt.ui <- renderUI({
    plotOutput("p.pop.fit.opt",
               width = as.numeric(input$plot.hdx.w),
               height = as.numeric(input$plot.hdx.h)
    )
  })


  ## 4.4. Download plots----------

  output$centroids.pdf <- downloadHandler(
    filename = function() { paste("Apparent centroids", '.pdf', sep='') },
    content = function(file) {
      ggsave(
        file,
        plot = p.app.cent(),
        device = "pdf",
        width = 29.7,
        height = 29.7*input$plot.hdx.h/input$plot.hdx.w,
        units = 'cm',
        bg = NULL
      )
    }
  )

  output$centroids.png <- downloadHandler(
    filename = function() { paste("Apparent centroids", '.png', sep='') },
    content = function(file) {
      ggsave(
        file,
        plot = p.app.cent(),
        device = "png",
        width = 29.7,
        height = 29.7*input$plot.hdx.h/input$plot.hdx.w,
        units = 'cm',
        bg = NULL
      )
    }
  )

  output$NUS.pdf <- downloadHandler(
    filename = function() { paste("Apparent exchange plots", '.pdf', sep='') },
    content = function(file) {
      ggsave(
        file,
        plot = p.hdx.fit.app(),
        device = "pdf",
        width = 29.7,
        height = 29.7*input$plot.hdx.h/input$plot.hdx.w,
        units = 'cm',
        bg = NULL
      )
    }
  )

  output$NUS.png <- downloadHandler(
    filename = function() { paste("Apparent exchange plots", '.png', sep='') },
    content = function(file) {
      ggsave(
        file,
        plot = p.hdx.fit.app(),
        device = "png",
        width = 29.7,
        height = 29.7*input$plot.hdx.h/input$plot.hdx.w,
        units = 'cm',
        bg = NULL
      )
    }
  )

  output$deconvoluted.NUS.pdf <- downloadHandler(
    filename = function() { paste("Deconvoluted exchange plot", '.pdf', sep='') },
    content = function(file) {
      ggsave(
        file,
        plot = p.hdx.fit.opt(),
        device = "pdf",
        width = 29.7,
        height = 29.7*input$plot.hdx.h/input$plot.hdx.w,
        units = 'cm',
        bg = NULL
      )
    }
  )

  output$deconvoluted.NUS.png <- downloadHandler(
    filename = function() { paste("Deconvoluted exchange plot", '.png', sep='') },
    content = function(file) {
      ggsave(
        file,
        plot = p.hdx.fit.opt(),
        device = "png",
        width = 29.7,
        height = 29.7*input$plot.hdx.h/input$plot.hdx.w,
        units = 'cm',
        bg = NULL
      )
    }
  )

  output$deconvoluted.pop.pdf <- downloadHandler(
    filename = function() { paste("Deconvoluted population abundances", '.pdf', sep='') },
    content = function(file) {
      ggsave(
        file,
        plot = p.pop.fit.opt(),
        device = "pdf",
        width = 29.7,
        height = 29.7*input$plot.hdx.h/input$plot.hdx.w,
        units = 'cm',
        bg = NULL
      )
    }
  )

  output$deconvoluted.pop.png <- downloadHandler(
    filename = function() { paste("Deconvoluted population abundances", '.png', sep='') },
    content = function(file) {
      ggsave(
        file,
        plot = p.pop.fit.opt(),
        device = "png",
        width = 29.7,
        height = 29.7*input$plot.hdx.h/input$plot.hdx.w,
        units = 'cm',
        bg = NULL
      )
    }
  )


  ##5. TimeR--------------

  ### 5.1. Process new data----

  #### 5.1.1 Snap data----
  snaps.kin <- data.frame()

  kin.brsh <- reactive({
    selecscansbrsh()
  })

  k.spectra <- reactive({

    k.init <- data.frame(kin.brsh()) %>%
      filter(
        mz > min(ranges$x),
        mz < max(ranges$x)
      ) %>%
      mutate(
        Species = input$sample.id,
        mz.range = paste0(round(min(ranges$x),2),"-",round(max(ranges$x), 2))
      )

    snaps.kin <<- rbind(snaps.kin, data.frame(k.init))

  }) %>%
    bindEvent(input$bttn42)


  #### 5.1.2. Data processing----

  k.norm <- reactive({

    k.data <- k.spectra() %>%
      group_by(filename, Species, scan, time, mz) %>%
      mutate(prod.int = intensity*mz) %>%
      group_by(filename, Species, scan, time) %>%
      summarise(
        intensity = sum(intensity),
        centroid = sum(prod.int)/sum(intensity)
      )

    writexl::write_xlsx(
      x = k.data,
      path = "kdata.xlsx"
    )

    k.standard <- k.data %>%
      filter(Species == 'Standard') %>%
      mutate(std.intensity = intensity) %>%
      ungroup() %>%
      select(filename, scan, time, std.intensity)

    writexl::write_xlsx(
      x = k.standard,
      path = "k.standard.xlsx"
    )

    if(nrow(k.standard)>0){
      k.joined <- k.data %>%
        filter(Species != 'Standard') %>%
        left_join(
          k.standard,
          by = c("filename", "scan", "time")
        )
    } else {
      k.joined <- k.data %>%
        mutate(std.intensity = 1)
    }

    k.joined <- k.joined %>%
      group_by(scan, Species, time) %>%
      mutate(
        corr.int = intensity/std.intensity,
        corrected.time = time + as.numeric(input$deadtxt),
        group = round(scan/as.numeric(input$ave.scan), 0)
      ) %>%
      group_by(Species, group) %>%
      mutate(
        mean.raw = mean(intensity),
        mean.corr = mean(corr.int),
        mean.time = mean(corrected.time),
        mean.centroid = mean(centroid)
      ) %>%
      filter(
        mean.time >= as.numeric(input$text33),
        mean.time <= as.numeric(input$text34)
      ) %>%
      ungroup() %>%
      mutate(
        name = case_when(
          Species == 'Species 1' ~ input$text36,
          Species == 'Species 2' ~ input$text37,
          Species == 'Species 3' ~ input$text38,
          Species == 'Species 4' ~ input$text39,
          Species == 'Species 5' ~ input$text40,
          Species == 'Species 6' ~ input$text41,
          Species == 'Species 7' ~ input$text42,
          Species == 'Species 8' ~ input$text43
        )
      ) %>%
      setcolorder(c(1,2,14,5,3,10,4,9,6,7,8,13,11,12))

    return(k.joined)

  })

  ### 5.2. Processed data re-import-----

  #### 5.2.1. File path----
  kin.old <- reactive({
    if(is.null(input$kin.old))
      return(NULL)

    input$kin.old
  })

  #### 5.2.2 Conversion to live format----
  processed.kin <- reactive({

    if(is.null(input$kin.old))
      return(NULL)

    processed.kin <- data.frame(
      read_excel(kin.old()$datapath,
                 skip = 0)
    )

    colnames(processed.kin) <- c(
      'name', 'filename', 'Species',
      'mean.centroid', 'intensity',
      'scan', 'group',
      'time', 'corrected.time',
      'centroid','std.intensity',  'corr.int',
      'mean.time', 'mean.raw', 'mean.corr'
    )

    processed.kin <- processed.kin %>%
      mutate(group = round(scan/as.numeric(input$ave.scan), 0)) %>%
      group_by(name, group) %>%
      mutate(mean.raw = mean(intensity),
             mean.corr = mean(corr.int),
             mean.time = mean(corrected.time)) %>%
      filter(
        mean.time >= as.numeric(input$text33),
        mean.time <= as.numeric(input$text34)
      )

    return(processed.kin)

  })

  #### 5.2.3. Merging----
  k.norm.0 <- reactive({
    if (is.null(kin.old)) {
      return(
        k.norm() %>%
          filter(Species %in% input$Pick1)
      )
    } else {
      if(is.null(input$mzml.file)){
        processed.kin() %>%
          filter(Species %in% input$Pick1)
      } else {
        rbind.data.frame(k.norm(),processed.kin()) %>%
          filter(Species %in% input$Pick1)
      }
    }
  })


  #### 5.4.Selection of y data----
  kin.input <- reactive({
    if (input$kin.input == 'raw') {
      if(isFALSE(input$kin.norm)){
        k.norm.0()$mean.raw
      } else {
        norm <- k.norm.0() %>%
          group_by(scan, time) %>%
          mutate(norm.int = mean.raw/sum(mean.raw)) %>%
          ungroup()

        return(norm$norm.int)
      }
    } else {
      if(input$kin.input == 'corrected'){
        if(isFALSE(input$kin.norm)){
          k.norm.0()$mean.corr
        } else {
          norm <- k.norm.0() %>%
            group_by(scan, time) %>%
            mutate(norm.int = mean.corr/sum(mean.corr)) %>%
            ungroup()

          return(norm$norm.int)
        }
      } else {
        k.norm.0()$mean.centroid
      }
    }
  })

  #### 5.5. Tables----

  #### 5.5.1 Kinetics data----

  output$k.table <-  DT::renderDT(server = FALSE, {
    datatable(data = k.norm.0() %>%
                relocate(name),
              style = "bootstrap",
              extensions = c('Buttons', 'Responsive', 'Scroller', 'ColReorder', 'RowReorder'),
              selection = 'multiple',
              colnames = c('Species #' = 'Species',
                           'Name' = 'name',
                           'Scan' = 'scan',
                           'TIC Time (min)' = 'time',
                           'Time (min)' = 'corrected.time',
                           'Mean Time (min)' = 'mean.time',
                           'Raw intensity' = 'intensity',
                           'Mean raw intensity' = 'mean.raw',
                           'Standard intensity' = 'std.intensity',
                           'Corrected intensity' = 'corr.int',
                           'Mean corrected intensity' = 'mean.corr',
                           'Filename' = 'filename',
                           'Scan group' = 'group',
                           'Centroid' = 'centroid',
                           'Mean centroid' = 'mean.centroid'),
              editable = T,
              rownames = F,
              escape = T,
              filter = 'top',
              autoHideNavigation = T,
              plugins = 'natural',
              options = list(
                colReorder = TRUE, rowReorder = TRUE,
                deferRender = TRUE,
                scrollY = 200,
                scroller = TRUE,
                autoWidth = F,
                dom = 'Bfrtip',
                buttons = list(
                  list(extend='copy'),
                  list(extend='csv',
                       title=NULL,
                       filename="Kinetics data"),
                  list(extend='excel',
                       title=NULL,
                       filename="Kinetics data"),
                  list(extend='colvis')
                ),
                columnDefs = list(list(visible=FALSE, targets=c(1,5:10,12)))
              )
    ) %>%
      formatStyle(
        columns = 0:14,
        target = 'row',
        background = '#272c30'
      ) %>%
      formatRound(c('TIC Time (min)', 'Time (min)', 'Raw intensity',
                    "Standard intensity", "Corrected intensity",
                    "Mean raw intensity", "Mean Time (min)", 'Mean corrected intensity'),
                  digits = 2)
  })


  #### 5.5.1 Spectral data----

  output$k.spectra <-  DT::renderDT(server = FALSE, {
    datatable(data = k.spectra() %>%
                select(-mz.range),
              style = "bootstrap",
              extensions = c('Buttons', 'Responsive', 'Scroller', 'ColReorder', 'RowReorder'),
              selection = 'multiple',
              colnames = c(
                "m/z" = "mz",
                "species" = "Species",
                "time (min)" = "time"
              ),
              editable = T,
              rownames = F,
              escape = T,
              filter = 'top',
              autoHideNavigation = T,
              plugins = 'natural',
              options = list(
                colReorder = TRUE, rowReorder = TRUE,
                deferRender = TRUE,
                scrollY = 200,
                scroller = TRUE,
                autoWidth = F,
                dom = 'Bfrtip',
                buttons = list(
                  list(extend='copy'),
                  list(extend='csv',
                       title=NULL,
                       filename="Spectra from kinetics"),
                  list(extend='excel',
                       title=NULL,
                       filename="Spectra from kinetics"),
                  list(extend='colvis')
                )
              )
    ) %>%
      formatStyle(
        columns = 0:15,
        target = 'row',
        background = '#272c30'
      )
  })


  #### 5.6 Plot----

  k.plot <- reactive({

    k.plot <- ggplot(
      data = k.norm.0(),
      aes(
        x = mean.time, y = kin.input(),
        color = name
      )
    ) +
      geom_point(
        size = input$size.dot.kin,
        alpha = input$transp.kin
      ) +
      scale_color_manual(
        name = "Species",
        values = c(
          input$col.dot.kin1, input$col.dot.kin2,
          input$col.dot.kin3, input$col.dot.kin4,
          input$col.dot.kin5, input$col.dot.kin6,
          input$col.dot.kin7, input$col.dot.kin8
        )
      ) +
      xlab("time (min)") +
      custom.theme


    if (input$kin.input == 'raw') {
      if(isFALSE(input$kin.norm)){
        k.plot <- k.plot + ylab("intensity")
      } else {
        k.plot <- k.plot + ylab("normalized intensity")
      }
    } else {
      if(input$kin.input == 'corrected'){
        if(isFALSE(input$kin.norm)){
          k.plot <- k.plot + ylab("corrected intensity")
        } else {
          k.plot <- k.plot + ylab("normalized corrected intensity")
        }
      } else {
        k.plot <- k.plot + ylab("centroid (m/z)")
      }
    }

    return(k.plot)

  })


  output$k.plot <- renderPlot({k.plot()})

  output$k.plot.ui <- renderUI({
    plotOutput(
      "k.plot",
      width = as.numeric(input$k.plot.w),
      height = as.numeric(input$k.plot.h)
    )
  })

  ## 5.7. Download plot----

  output$timer.pdf <- downloadHandler(
    filename = function() { paste("Kinetics plot", '.pdf', sep='') },
    content = function(file) {
      ggsave(
        file,
        plot = k.plot(),
        device = "pdf",
        width = 29.7,
        height = 29.7*input$k.plot.h/input$k.plot.w,
        units = 'cm',
        bg = NULL
      )
    }
  )

  output$timer.png <- downloadHandler(
    filename = function() { paste("Kinetics plot", '.png', sep='') },
    content = function(file) {
      ggsave(
        file,
        plot = k.plot(),
        device = "png",
        width = 29.7,
        height = 29.7*input$k.plot.h/input$k.plot.w,
        units = 'cm',
        bg = NULL
      )
    }
  )



  # 6. TitratR-----------

  ##6.1 Experimental conditions----

  lgd.conc <- reactive({
    as.numeric(input$lgd.conc)
  })

  Mtot <- reactive({ #total target concentration
    as.numeric(input$Mtot)
  })

  Std <- reactive({ #SI concentration
    as.numeric(input$Std)
  })



  ## 6.2 Data processing----

  ### 6.2.1 Calculations----

  eq.raw.target <- reactive({

    MSsnaps.target() %>%
      group_by(
        Species, lgd.conc, Stoich, filename,
        min.time, max.time, min.scan, max.scan
      ) %>%
      summarise(intensity = sum(intensum))
  })

  eq.raw.std <- reactive({

    MSsnaps.std() %>%
      group_by(
        lgd.conc, filename
      ) %>%
      summarise(std.intensity = sum(intensum)) %>%
      ungroup() %>%
      select(filename, lgd.conc, std.intensity)
  })

  eq.raw <- reactive({

    if(is.null(input$titr.old)){
      eq.raw.target() %>%
        left_join(
          eq.raw.std()
        ) %>%
        group_by(Species, lgd.conc) %>%
        mutate(
          #the relative intensity does not take into account the SI intensity
          rel.intensity = intensity/sum(intensity),
          uncorr.C = rel.intensity*Mtot()
          # uncorr.C.lgd = uncorr.C*Stoich,
          # free.lgd = lgd.conc-sum(uncorr.C.lgd)
        )
    } else {
      processed.titr()
    }

  }) %>%
    bindEvent(input$IS)

  ### 6.2.2 Tables----

  output$eq.raw.target <- DT::renderDT(server = FALSE, {

    datatable(
      data = eq.raw.target(),
      style = "bootstrap",
      extensions = c('Buttons', 'Responsive', 'Scroller', 'ColReorder', 'RowReorder'),
      selection = 'multiple',
      colnames = c(
        '[Ligand] (µM)' = 'lgd.conc',
        'File name' = 'filename',
        'Ligand stoichiometry' = 'Stoich',
        'Raw intensity' = 'intensity',
        'Start TIC time' = 'min.time',
        'End TIC time' = 'max.time',
        'Start scan' = 'min.scan',
        'End scan' = 'max.scan'
      ),
      editable = T,
      rownames = F,
      escape = T,
      filter = 'top',
      autoHideNavigation = T,
      plugins = 'natural',
      options = list(
        colReorder = TRUE, rowReorder = TRUE,
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        autoWidth = F,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'colvis'),
        columnDefs = list(list(visible=FALSE, targets=c(3:7)))
      )
    ) %>%
      # formatRound(
      #   c('Raw intensity', 'Relative intensity', 'C (µM, uncorrected)'),
      #   digits = 2
      # ) %>%
      formatStyle(
        columns = 0:8,
        target = 'row',
        background = '#272c30'
      )

  })

  output$eq.raw.std <- DT::renderDT(server = FALSE, {

    datatable(
      data = eq.raw.std(),
      style = "bootstrap",
      extensions = c('Buttons', 'Responsive', 'Scroller', 'ColReorder', 'RowReorder'),
      selection = 'multiple',
      colnames = c(
        '[Ligand] (µM)' = 'lgd.conc',
        'File name' = 'filename',
        "Standard intensity" = 'std.intensity'
      ),
      editable = T,
      rownames = F,
      escape = T,
      filter = 'top',
      autoHideNavigation = T,
      plugins = 'natural',
      options = list(
        colReorder = TRUE, rowReorder = TRUE,
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        autoWidth = F,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'colvis'),
        columnDefs = list(list(visible=FALSE, targets=c(0)))
      )
    ) %>%
      formatStyle(
        columns = 0:2,
        target = 'row',
        background = '#272c30'
      )

  })

  output$eq.raw <- DT::renderDT(server = FALSE, {

    datatable(
      data = eq.raw(),
      style = "bootstrap",
      extensions = c('Buttons', 'Responsive', 'Scroller', 'ColReorder', 'RowReorder'),
      selection = 'multiple',
      colnames = c(
        '[Ligand] (µM)' = 'lgd.conc',
        'File name' = 'filename',
        'Ligand stoichiometry' = 'Stoich',
        'Raw intensity' = 'intensity',
        'Start TIC time' = 'min.time',
        'End TIC time' = 'max.time',
        'Start scan' = 'min.scan',
        'End scan' = 'max.scan',
        "Standard intensity" = 'std.intensity',
        "Relative intensity" = "rel.intensity",
        'Concentration (µM, uncorrected)' = "uncorr.C"
      ),
      editable = T,
      rownames = F,
      escape = T,
      filter = 'top',
      autoHideNavigation = T,
      plugins = 'natural',
      options = list(
        colReorder = TRUE, rowReorder = TRUE,
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        autoWidth = F,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'colvis'),
        columnDefs = list(list(visible=FALSE, targets=c(0, 3:7)))
      )
    ) %>%
      formatStyle(
        columns = 0:11,
        target = 'row',
        background = '#272c30'
      )

  })


  ##6.3 import processed data----

  titr.old <- reactive({
    if(is.null(input$titr.old)){
      return(NULL)
    } else {
      return(input$titr.old)
    }

  })

  processed.titr <- reactive({

    if(is.null(input$titr.old)){
      return(NULL)
    } else {
      return(
        read_excel(
          titr.old()$datapath,
          skip = 1
        ) %>%
          set_colnames(
            c('Species', 'lgd.conc', 'Stoich', 'filename',
              'min.time', 'max.time', 'min.scan', 'max.scan',
              'intensity', 'std.intensity',
              'rel.intensity', 'uncorr.C')
          )
      )
    }

  })

  ## 6.4 Internal standardization----

  ### 6.4.1 Response factors----

  Rf <- reactive({

    eq.raw <- eq.raw() %>%
      arrange(lgd.conc, Stoich)

    # Calculation of the intensity ratio vs IS (by experiment = by lgd.conc)
    I <- eq.raw %>%
      mutate(I.ratio = intensity/std.intensity) %>% #VERIFY----
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

    #Normalisation to 1 for unbound DNA
    R <- R/R[1,1]

    return(R)

  }) %>%
    bindEvent(input$bttn55)

  output$Rf <- renderDT(server = FALSE,{
    datatable(
      Rf()%>%
        as.data.frame() %>%
        mutate(Stoich = unique(eq.raw()$Stoich)) %>%
        mutate(species = case_when(
          Stoich == 0 ~ 'M',
          Stoich == 1 ~ 'ML',
          Stoich == 2 ~ 'ML2'
        )) %>%
        select(species, V1) %>%
        set_colnames(c("Species", "Response factor")),
      style = "bootstrap",
      extensions = c('Buttons', 'Responsive', 'Scroller', 'ColReorder', 'RowReorder'),
      selection = 'multiple',
      editable = T,
      rownames = F,
      escape = T,
      filter = 'top',
      autoHideNavigation = T,
      plugins = 'natural',
      options = list(
        colReorder = TRUE, rowReorder = TRUE,
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        autoWidth = F,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'colvis')
      )
    ) %>%
      formatStyle(
        columns = 0:2,
        target = 'row',
        background = '#272c30'
      ) %>%
      formatSignif(
        .,
        columns = 2,
        digits = 3
      )
  })

  ### 6.4.2 Corrected concentrations----

  corr.C <- reactive({

    eq.raw <- eq.raw() %>%
      arrange(lgd.conc, Stoich)

    R <- Rf()

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

    corr.C.colnames <- eq.raw %>%
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

  }) %>%
    bindEvent(input$bttn.Ccor)


  output$corr.C <- renderDT({

    datatable(data = corr.C(),
              style = "bootstrap",
              extensions = c('Buttons', 'Responsive', 'Scroller', 'ColReorder', 'RowReorder'),
              selection = 'multiple',
              editable = T,
              rownames = F,
              escape = T,
              filter = 'top',
              autoHideNavigation = T,
              plugins = 'natural',
              options = list(
                colReorder = TRUE, rowReorder = TRUE,
                deferRender = TRUE,
                scrollY = 200,
                scroller = TRUE,
                autoWidth = F,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'colvis')
              )
    ) %>%
      formatStyle(
        columns = 0:2,
        target = 'row',
        background = '#272c30'
      )

  })







  #output options-----------
  # outputOptions(output, "plot.snaps", suspendWhenHidden = FALSE)
  # outputOptions(output, "plot.snaps.ui", suspendWhenHidden = FALSE)
  outputOptions(output, "plot.pp", suspendWhenHidden = FALSE)
  outputOptions(output, "k.plot", suspendWhenHidden = FALSE)
  # outputOptions(output, "eq.raw.target", suspendWhenHidden = FALSE)
  # outputOptions(output, "eq.raw.std", suspendWhenHidden = FALSE)


}
