#dependencies----
# if (require(devtools)) install.packages("devtools")#if not already installed
# devtools::install_github("AnalytixWare/ShinySky")

packages <- c("librarian")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}


library(shinysky)

librarian::shelf(
  shiny, shinydashboard, shinydashboardPlus, shinyBS, shinyWidgets,
  DT, colourpicker, DavidBarke/QWUtils
)

# library(shiny)
# library(shinydashboard)
# library(shinydashboardPlus)
# library(shinyWidgets)
# library(shinyBS) #useful?
# library(shinysky) #useful?   devtools::install_github("AnalytixWare/ShinySky")
#
# library(DT)
#
# library(colourpicker)
#
# # install.packages("remotes")
# # remotes::install_github("DavidBarke/QWUtils")
# library(QWUtils) #useful?


ui <- dashboardPage(
  skin = "midnight",
  # includeCSS("www/ui.css"),
  title = "OligoR 0.9",
  scrollToTop = TRUE,
  header = dashboardHeader(
    title = "DNA-HDX/MS"
  ),
  #sidebar-------------
  dashboardSidebar(
    # useShinyjs(),
    # setSliderColor(c("tomato"), c(1)),
    #sidebar MSxplorR-------------
    conditionalPanel(
      condition = "input.tabs == 'MSxploR'",
      box(
        width = 12,
        title = "Import MS data",
        status = 'info',
        collapsible = T,
        enable_dropdown = T,
        dropdown_icon = 'upload',
        fileInput(
          'file1',
          'Select mzML or mzXML file'
        )
      ),
      box(
        width = 12,
        title = "m/z range narrowing",
        status = 'info',
        solidHeader = F,
        collapsible = T,
        collapsed = F,
        sliderInput(
          inputId = "text11",
          label = NULL,
          min = 400, max = 4000,
          value = c(800, 2500),
          step = 50
        )
      ),
      box(
        width = 12,
        title = "m/z binning width",
        status = 'danger',
        collapsible = T,
        collapsed = TRUE,
        sliderInput(
          inputId = "slider1",
          label = NULL,
          min = 0.001, max = 0.025, value = 0.005, step = 0.001,
          round = -3,
          ticks = TRUE,
          animate = FALSE,
          sep = " ",
          dragRange = TRUE
        )
      ),
      box(width = 12,
          title = "Data selection",
          status = 'warning',
          collapsible = T,
          collapsed = F,
          switchInput(inputId = 'switch69',
                      label = 'Combine method',
                      value = T,
                      onLabel = 'Brush',
                      offLabel = 'Text',
                      onStatus = 'danger',
                      offStatus = 'info',
                      size = 'normal',
                      width = 12),
          textInput(inputId = 'deadtxt',
                    label = "Dead time (min)",
                    value = 0),
          selectInput(
            inputId = 'sample.id',
            label = 'Species',
            choices = c('Species 1', 'Species 2', 'Species 3', 'Species 4', 'Species 5', 'Species 6', 'Species 7', 'Species 8', 'Standard', 'Target')
          )
      ),
      box(width = 12,
          title = "HDX",
          status = 'warning',
          solidHeader = F,
          collapsible = T,
          collapsed = F,
          actionBttn(inputId = "bttn99",
                     label = "Select reference",
                     icon = icon('check-circle', class = 'regular'),
                     style = "simple",
                     color = "danger",
                     size = "sm",
                     block = F,
                     no_outline = TRUE),
          actionBttn(inputId = "bttn1",
                     label = "Select timepoint",
                     icon = icon('check-circle', class = 'solid'),
                     style = "simple",
                     color = "warning",
                     size = "sm",
                     block = F,
                     no_outline = TRUE),
          textInput(inputId = 'time.sec',
                    label = "Manual time (s)",
                    value = 0),
          htmlOutput('timemin')
      ),
      box(width = 12,
          title = "Kinetics",
          status = 'teal',
          solidHeader = F,
          collapsible = T,
          collapsed = T,
          actionBttn(inputId = "bttn42",
                     label = "Select species",
                     icon = icon('check-circle', class = 'solid'),
                     style = "simple",
                     color = "royal",
                     size = "sm",
                     block = F,
                     no_outline = TRUE)
      ),
      box(width = 12,
          title = "Titration",
          status = 'maroon',
          solidHeader = F,
          collapsible = T,
          collapsed = T,
          # textInput(inputId = 'tgt.conc',
          #           label = "Target concentration (µM)",
          #           value = 0,
          #           width = 12),
          textInput(inputId = 'Stoich',
                    label = "Ligand stoichiometry",
                    value = 0),
          textInput(inputId = 'lgd.conc',
                    label = "Ligand concentration (µM)",
                    value = 0),
          actionBttn(inputId = "bttn24",
                     label = "Select datapoint",
                     icon = icon('check-circle', class = 'solid'),
                     style = "simple",
                     color = "primary",
                     size = "sm",
                     block = F,
                     no_outline = TRUE)
      ),
      box(
        width = 12,
        title = "Scan range selection",
        status = 'success',
        solidHeader = F,
        collapsible = T,
        collapsed = TRUE,
        splitLayout(
          cellWidths = "50%",
          textInput(inputId = "text1", "start", "1"),
          textInput(inputId = "text2", "end", "1000"))
      ),
      box(
        width = 12,
        title = "m/z range selection",
        status = 'success',
        solidHeader = F,
        collapsible = T,
        collapsed = TRUE,
        splitLayout(
          cellWidths = "50%",
          textInput(inputId = "text3", "start", "1000"),
          textInput(inputId = "text4", "end", "2500"))
      )
    ),
    #sidebar HDXplotR-------------
    conditionalPanel(
      condition = "input.tabs == 'HDXplotR'",
      box(
        width = 12,
        title = "Import processed data",
        status = 'primary',
        solidHeader = F,
        collapsible = T,
        enable_dropdown = T,
        dropdown_icon = 'upload',
        fileInput(
          'file.old',
          'Select Excel file'
        )
      ),
      box(
        title = 'Time scale',
        id = 'timescale2',
        collapsible = T,
        collapsed = F,
        solidHeader = F,
        width = 12,
        switchInput(
          label = 'Time scale',
          inputId = 'manu2',
          value = TRUE,
          width = 12,
          onLabel = 'Manual',
          offLabel = 'TIC',
          size = 'normal',
          onStatus = 'danger',
          offStatus = 'info'
        )
      ),
      box(
        width = 12,
        title = "Fitting initialization",
        status = "maroon",
        solidHeader = F,
        collapsible = T,
        collapsed = F,
        switchInput(inputId = "fit.hdx", #toggles baseline on/off
                    label = "fit HDX plots",
                    onLabel = 'fit',
                    offLabel = 'no fit',
                    value = FALSE,
                    size = 'normal',
                    width = 'auto')
      ),
      box(
        title = "Downloads",
        id = "NUSdown",
        collapsible = T,
        solidHeader = F,
        width = 12,
        downloadBttn(
          outputId = "dwnplot",
          label = "Raw plot",
          style = "material-flat",
          size = 'sm'
        ),
        br(),
        downloadBttn(
          outputId = "dwnplot2",
          label = "NUS plot png",
          style = "material-flat",
          size = 'sm'
        ),
        downloadBttn(
          outputId = "dwnplot3",
          label = "NUS plot pdf",
          style = "material-flat",
          size = 'sm'
        )
      )
    ),
    #sidebar MSstackR-------------
    conditionalPanel(
      condition = "input.tabs == 'MSstackR'",
      box(
        title = 'Time scale',
        id = 'timescale1',
        collapsible = T,
        collapsed = F,
        solidHeader = F,
        width = 12,
        switchInput(
          label = 'Time scale',
          inputId = 'manu1',
          value = TRUE,
          width = 12,
          onLabel = 'Manual',
          offLabel = 'TIC',
          size = 'normal',
          onStatus = 'danger',
          offStatus = 'info'
        )
      ),
      box(
        title = "Downloads",
        id = "NUSdown",
        collapsible = T,
        solidHeader = F,
        width = 12,
        downloadBttn(
          outputId = "dwnspec",
          label = "png",
          style = "material-flat",
          size = 'sm'
        ),
        downloadBttn(
          outputId = "dwnspec.pdf",
          label = "pdf",
          style = "material-flat",
          size = 'sm'
        )
      ),
      box(
        id = "peak.picking",
        title = "Peak Picking",
        status = 'danger',
        solidHeader = F,
        collapsible = T,
        collapsed = F,
        width = 12,
        sliderInput(
          inputId = 'neighlim',
          label = 'neighbour limit',
          min = 0,
          max = 100,
          step = 1,
          value = 5,
          ticks = FALSE
        ),
        sliderInput(
          inputId = 'deriv.lim',
          label = 'derivative limit',
          min = 0,
          max = 100000,
          step = 1000,
          value = 10000,
          ticks = FALSE
        ),
        sliderInput(
          inputId = 'int.thresh',
          label = 'intensity limit',
          min = 0,
          max = 0.1,
          step = 0.001,
          value = 0.01,
          ticks = FALSE
        )
      ),
      box(
        id = "optim",
        title = "Optimization",
        status = "info",
        solidHeader = FALSE,
        collapsible = TRUE,
        width = 12,
        sliderInput(
          inputId = "nrPeaks.user.pp",
          label = 'Number of peaks',
          value =72,
          min = 8, max = 128,
          step = 4,
          ticks = FALSE
        ),
        sliderInput(
          inputId = "DC.pp",
          label = 'D%',
          value = c(9,90),
          min = 0, max = 100,
          step = 0.5,
          ticks = FALSE
        ),
        textInput(
          inputId = "user.hdx.ref",
          label = "Exchanged reference m/z (0 = auto)",
          value = 0
        ),
        prettyToggle(
          inputId = "model.selection",
          label_on = "F-test-based model selection",
          label_off = "Manual model selection",
          value = TRUE,
          shape = "round",
          width = "100%",
          bigger = TRUE,
          animation = "pulse"
        ),
        textInput(
          inputId = "alpha",
          label = "alpha for automated model selection",
          value = 0.8
        ),
        textInput(
          inputId = "b.t.limit",
          label = "manual bimodal time threshold",
          value = -1
        ),
        actionBttn(
          inputId = "optibtn",
          label = "Optimization",
          icon = icon('check-circle', class = 'solid'),
          style = "simple",
          color = "primary",
          size = "sm",
          block = F,
          no_outline = TRUE
        )
      )
    ),
    #sidebar KineticR-------------
    conditionalPanel(
      condition = "input.tabs =='KineticR'",
      box(
        width = 12,
        title = "Import results",
        status = 'primary',
        solidHeader = F,
        collapsible = T,
        enable_dropdown = T,
        dropdown_icon = 'upload',
        fileInput(
          'kin.old',
          'Select .xlsx file'
        )
      ),
      box(
        width = 12,
        title = "Data processing",
        status = 'primary',
        solidHeader = F,
        collapsible = T,
        collapsed = F,
        splitLayout(
          textInput(
            inputId = "text33",
            label = "Start (min)",
            value = 0,
            width = 12
          ),
          textInput(
            inputId = "text34",
            label = "End (min)",
            value = 999,
            width = 12
          )
        ),
        sliderInput(
          inputId = "text35",
          label = "Scans to average",
          min = 1,
          max = 25,
          step = 1,
          value = 1
        )
      ),
      box(
        width = 12,
        title = "Species",
        status = 'danger',
        solidHeader = F,
        collapsible = T,
        collapsed = F,
        splitLayout(
          textInput(
            inputId = "text36",
            label = "Species 1",
            value = "Species 1",
            width = 12
          ),
          textInput(
            inputId = "text37",
            label = "Species 2",
            value = "Species 2",
            width = 12
          )
        ),
        splitLayout(
          textInput(
            inputId = "text38",
            label = "Species 3",
            value = "Species 3",
            width = 12
          ),
          textInput(
            inputId = "text39",
            label = "Species 4",
            value = "Species 4",
            width = 12
          )
        ),
        splitLayout(
          textInput(
            inputId = "text40",
            label = "Species 5",
            value = "Species 5",
            width = 12
          ),
          textInput(
            inputId = "text41",
            label = "Species 6",
            value = "Species 6",
            width = 12
          )
        ),
        splitLayout(
          textInput(
            inputId = "text42",
            label = "Species 7",
            value = "Species 7",
            width = 12
          ),
          textInput(
            inputId = "text43",
            label = "Species 8",
            value = "Species 8",
            width = 12
          )
        )
      )
    ),
    #sidebar TitR-------------
    conditionalPanel(
      condition = "input.tabs =='TitR'",
      box(
        width = 12,
        title = "Import processed data",
        status = 'primary',
        solidHeader = F,
        collapsible = T,
        enable_dropdown = T,
        dropdown_icon = 'upload',
        fileInput(
          'titr.old',
          'Select csv file'
        )
      ),
      textInput(
        inputId = "Mtot",
        label = "Total target concentration (µM)",
        value = 10,
        width = 12
      ),textInput(
        inputId = "Std",
        label = "IS concentration (µM)",
        value = 10,
        width = 12
      ),
      actionBttn(inputId = "bttn55",
                 label = "Process response factors",
                 icon = icon('check-circle', class = 'solid'),
                 style = "simple",
                 color = "primary",
                 size = "sm",
                 block = F,
                 no_outline = TRUE)
    ),
    #sidebar OligoRef-------------
    conditionalPanel(
      condition = "input.tabs == 'OligoRef'",
      box(id = "boxseq",
          title = "Analyte information",
          status = "primary",
          solidHeader = F,
          collapsible = T,
          width = 12,
          textInput(
            inputId = "sequence",
            label = "Sequence",
            value = "TTGTGGTGGGTGGGTGGGT"
          ),
          textInput(
            inputId = 'mol',
            label = 'Molecularity',
            value = "1"
          ),
          splitLayout(
            textInput(
              inputId = "z",
              label = "Charge",
              value = "4"
            ),
            textInput(
              inputId = "K",
              label = "Potassium",
              value = "2"
            )
          ),
          br(),
          htmlOutput('chem.formula'),
          br(),
          textInput(
            inputId = "K41C",
            label = 'K41%',
            value = 6.730244
          ),
          br(),
          sliderInput(
            inputId = "DC",
            label = 'D%',
            value = 9,
            min = 0, max = 100,
            step = 0.5
          ),
          sliderInput(
            inputId = "nrPeaks.user",
            label = "Number of peaks",
            value = 72,
            min = 8, max = 128,
            step = 8
          ),
          br(),
          textInput(
            inputId = "nX.user",
            label = 'Exchangeable sites',
            placeholder = 'defaults to preset'
          ),
          selectInput("nX.select",
                      label = 'presets',
                      choices = list("all sites" = 'A', "no phosphates" = 'B', "no phosphates nor termini" = 'C'),
                      selected = "C")
      ),
      downloadBttn(
        outputId = "ref.accu.png",
        label = "png",
        style = "material-flat",
        size = 'sm'
      ),
      downloadBttn(
        outputId = "ref.accu.pdf",
        label = "pdf",
        style = "material-flat",
        size = 'sm'
      )
    ),
    verbatimTextOutput("value")
    # extendShinyjs(text = jscode, functions = c('shinyjs.collapse'))
  ),
  #body--------------
  dashboardBody(
    # useShinyjs(),
    # extendShinyjs(text = jscode, functions = c("shinyjs.collapse")),
    tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: pink !important;}')),
    tags$style(
      type="text/css", #hides error messages
      # ".shiny-output-warning { visibility: hidden; }",
      # ".shiny-output-warning:before { visibility: hidden; }",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"
    ),
    navbarPage('Navigation',
               id = 'tabs',
               #panel oligoRef------------
               tabPanel("OligoRef",
                        icon = icon("dna"),
                        fluidRow(
                          box(id = 'Oligoutput69',
                              title = 'Analyte characteristics',
                              collapsible = T,
                              collapsed = T,
                              width = 6,
                              dataTableOutput("oligo.data")
                          ),
                          box(id = 'Oligoutput10',
                              title = 'Theoretical distribution data',
                              collapsible = T,
                              collapsed = T,
                              width = 6,
                              dataTableOutput("peak.position")
                          ),
                          box(id = 'Oligoutput7',
                              title = 'Theoretical distribution',
                              width = 6,
                              collapsible = TRUE,
                              plotOutput('p.hdx.ref')
                          ),
                          box(id = 'Oligoutput7-2',
                              title = 'Reference accuracy',
                              width = 6,
                              collapsible = T,
                              plotOutput('p.hdx.ref.vs.exp')
                          )
                        ),
                        absolutePanel(
                          top = 300, right = 40, width = 200,
                          draggable = TRUE,
                          wellPanel(h3("Customisation"),
                                    colourInput("col.dot.th", "Theory  dot colour", "tomato"),
                                    colourInput("col.line.th", "Theory line colour", "tomato"),
                                    colourInput("col.centroid.th", "Theory centroid colour", "tomato"),
                                    colourInput("col.line.exp", "Reference line colour", "#BDD1E3"),
                                    colourInput("col.centroid.exp", "Centroid line colour", "#BDD1E3"),
                                    sliderInput('size.dot.th', 'Theory dot size',
                                                min=0, max=10, value=4,
                                                step=0.25, round=0),
                                    sliderInput('size.line.th', 'Theory line size',
                                                min=0, max=5, value=1,
                                                step=0.25, round=0),
                                    sliderInput('size.line.exp', 'Reference line size',
                                                min=0, max=5, value=1,
                                                step=0.25, round=0),
                                    sliderInput('size.centroid.th', 'Centroid line size',
                                                min=0, max=5, value=1,
                                                step=0.25, round=0)
                          ),
                          style = "opacity: 0.9"
                        )
               ),
               #panel MSxploR------------
               tabPanel("MSxploR",
                        icon = icon('drafting-compass'),
                        fluidRow(
                          box(id = "box1",
                              width = 10,
                              title = "TIC",
                              p("Brush to select scans, resize edges and drag as desired"),
                              status = 'info',
                              collapsible = TRUE,
                              plotOutput("plot1",
                                         brush = brushOpts(id = "plot_brush",
                                                           fill = "#fff5e7", stroke = "#fff5e7",
                                                           direction = "x"),
                                         height = 200)),
                          box(
                            title = "Selected time range",
                            width = 2,
                            status = 'info',
                            collapsible = T,
                            textOutput("info", inline = F)
                          ),
                          box(
                            title = "Selected scans",
                            width = 2,
                            status = 'info',
                            collapsible = T,
                            textOutput("info1", inline = F)
                          )
                        ),
                        fluidRow(
                          box(id = "box3",
                              width = 10,
                              title = "MS spectrum", p("Brush to zoom, double click to reset zoom"),
                              status = "danger",
                              collapsible = TRUE,
                              plotOutput("plot3",
                                         height = 400,
                                         dblclick = "plot3_dblclick",
                                         brush = brushOpts(
                                           id = "plot3_brush",
                                           fill = "#fff5e7", stroke = "#fff5e7", direction = "xy",
                                           resetOnNew = TRUE
                                         ))),
                          box(
                            title = "Selected m/z",
                            width = 2,
                            status = "danger",
                            closable = F,
                            collapsible = T,
                            textOutput("info2", inline = F)
                          )
                        ),
                        fluidRow(
                          box(
                            id = "box4",
                            width = 10,
                            title = "MS spectrum (text input)", p("select scans and zoom from text input in sidebar"),
                            collapsible = TRUE,
                            collapsed = TRUE,
                            plotOutput("plot4",
                                       height = 400,
                            ))
                        ),
                        # DT::dataTableOutput(outputId = 'inputms69'),
                        absolutePanel(
                          bottom = 200, right = 40, width = 200,
                          draggable = TRUE,
                          wellPanel(h3("Customisation"),
                                    colourInput("col.TIC", "TIC colour", "steelblue"),
                                    colourInput("col.MS", "Spectrum colour", "steelblue"),
                                    sliderInput('size.line.TIC', 'TIC line size',
                                                min=0.25, max=5, value=1,
                                                step=0.25, round=0),
                                    sliderInput('size.line.MS', 'Spectrum line size',
                                                min=0.25, max=5, value=1,
                                                step=0.25, round=0)
                          ),
                          style = "opacity: 0.9"
                        )
               ),
               #panel MSstackR--------------
               tabPanel("MSstackR",
                        icon = icon('layer-group'),
                        fluidRow(
                          box(
                            title = "Stacked spectra",
                            width =6,
                            status = "danger",
                            collapsible = T,
                            height = 1000,
                            # tableOutput("MSsnaps"), #diagnostics for snapshots
                            uiOutput("plot5.ui")
                          ),
                          box(
                            title = "Peak picking",
                            width = 6,
                            status = "danger",
                            collapsible = T,
                            collapsed = F,
                            height = 1000,
                            uiOutput("plot5bi.ui")
                          ),
                          box(
                            title = "Optimized data",
                            width = 6,
                            status = "info",
                            collapsible = TRUE,
                            collapsed = FALSE,
                            height = 1000,
                            uiOutput("optiplot.ui")
                          ),
                          box(
                            title = "Peak-picked data",
                            width = 6,
                            status = "info",
                            collapsible = T,
                            collapsed = T,
                            height = 1000,
                            DTOutput('MSsnaps.pp.table')
                          ),
                          box(
                            title = "Optimized distributions",
                            width = 6,
                            status = "success",
                            collapsible = T,
                            collapsed = T,
                            height = 1000,
                            DTOutput('opt.table')
                          ),
                          box(
                            title = "Optimization results",
                            width = 6,
                            status = "success",
                            collapsible = T,
                            collapsed = T,
                            height = 1000,
                            DTOutput('opt.stat')
                          ),
                          box(
                            title = "Derived values",
                            width = 6,
                            status = "success",
                            collapsible = T,
                            collapsed = T,
                            height = 1000,
                            DTOutput('binom.NUS.table')
                          ),
                          box(
                            title = "Filtered optimized distributions",
                            width = 6,
                            status = "danger",
                            collapsible = T,
                            collapsed = T,
                            height = 1000,
                            DTOutput('opt.filter.table')
                          ),
                          box(
                            title = "Filtered derived values",
                            width = 6,
                            status = "danger",
                            collapsible = T,
                            collapsed = T,
                            height = 1000,
                            DTOutput('binom.filter.table')
                          )
                        ),
                        absolutePanel(
                          bottom = 200, right = 40, width = 200,
                          draggable = TRUE,
                          wellPanel(
                            h3("Customisation"),
                            colourInput("col.snap1", "Gradient start", "tomato"),
                            colourInput("col.snap2", "Gradient end", "steelblue4"),
                            selectInput("trans.user", 'color guide',
                                        choices = list("identity", "log10"),
                                        selected = 'identity'),
                            sliderInput('plot5.w', 'Plot width',
                                        min=100, max=2000, value=500,
                                        step=20, round=0),
                            sliderInput('plot5.h', 'Plot height',
                                        min=100, max=3000, value= 700,
                                        step=20, round=0),
                            switchInput(inputId = "com.scale",
                                        label = 'm/z axis',
                                        value = TRUE,
                                        width = 12,
                                        onLabel = 'fixed',
                                        offLabel = 'free',
                                        onStatus = 'danger',
                                        offStatus = 'info',
                                        size = 'normal'),
                            switchInput(inputId = "t.indic",
                                        label = 'scale',
                                        value = TRUE,
                                        width = 12,
                                        onLabel = 'color guide',
                                        offLabel = 'text',
                                        onStatus = 'danger',
                                        offStatus = 'info',
                                        size = 'normal')
                            # sliderInput('plot5.ncol', 'number of columns',
                            #             min=1, max=5, value=1,
                            #             step=1, round=0)
                          ),
                          style = "opacity: 0.9"
                        )
               ),
               #panel HDXplotR----------
               tabPanel("HDXplotR",
                        icon = icon('stopwatch'),
                        fluidRow(
                          column(12,
                                 collapsible_tabBox(
                                   id = 'kinetic.hdx',
                                   title = 'HDX kinetics',
                                   width = 12,
                                   tabPanel(
                                     title = 'NUS calculation',
                                     icon = icon('calculation'),
                                     hotable("hotable2"),
                                   ),
                                   tabPanel(
                                     title = 'Kinetics data',
                                     icon = icon('table'),
                                     DTOutput("centroids"),
                                     checkboxInput(
                                       inputId = "centroids_sel",
                                       label = "select all",
                                       value = FALSE
                                     )
                                   ),
                                   tabPanel(
                                     title = 'Fit initialization',
                                     icon = icon("edit"),
                                     hotable('hotable3')
                                   )
                                 )
                          ),
                          box(
                            title = "Raw exchange data",
                            width = 6,
                            status = "primary",
                            collapsible = T,
                            plotOutput("plot6")
                          ),
                          box(
                            title = "Exchange plot: from raw data",
                            width = 6,
                            status = "primary",
                            collapsible = T,
                            plotOutput("plot7")
                          ),
                          box(
                            title = "Exchange plot: from optimization",
                            width = 6,
                            status = "danger",
                            collapsible = TRUE,
                            plotOutput("optim.nus.plot")
                          ),
                          box(
                            title = "Population abundances: from optimization",
                            width = 6,
                            status = "danger",
                            collapsible = TRUE,
                            plotOutput("optim.ab.plot")
                          )
                        ),
                        absolutePanel(
                          bottom = 200, right = 40, width = 200,
                          draggable = TRUE,
                          wellPanel(h3("Customisation"),
                                    colourInput("col.kin", "Unselected points", "#777F85"),
                                    colourInput("col.kin.high1", "Series 1", "#1f77b4"),
                                    colourInput("col.kin.high2", "Series 2", "#ff7f0e"),
                                    colourInput("col.kin.high3", "Series 3", "#2ca02c"),
                                    colourInput("col.kin.high4", "Series 4", "#d62728"),
                                    sliderInput('size.kin', 'Dot Size',
                                                min=1, max=10, value=3,
                                                step=0.5, round=0),
                                    sliderInput('trans.kin', 'Opacity',
                                                min=0, max=1, value=0.9,
                                                step=0.05)
                          ),
                          style = "opacity: 0.9"
                        )
               ),
               #panel timR--------
               tabPanel("KineticR",
                        icon = icon('clock'),
                        fluidRow(
                          box(
                            title = "Kinetics data",
                            footer = "To be able to reimport the data, save as Excel. The number of scans to average and time range can be reprocessed. Caution: scans excluded will not be exported.",
                            width = 12,
                            status = "success",
                            solidHeader = T,
                            collapsible = T,
                            DTOutput("k.table")
                          ),
                          box(
                            title = "Kinetics plot",
                            width = 12,
                            status = "danger",
                            solidHeader = T,
                            collapsible = T,
                            div(style="display: inline-block;vertical-align:top; width: 50px;",
                                dropdownButton(
                                  circle = TRUE,
                                  status = "danger",
                                  icon = icon("filter"),
                                  size = 'sm',
                                  width = '400px',
                                  tooltip = tooltipOptions(title = "Click to see change input"),
                                  tags$h4("Standardization"),
                                  prettyRadioButtons(
                                    inputId = "kin.input",
                                    label = "Intensity",
                                    choices = c("Raw" = 'raw', "Corrected" = 'corrected', "Centroids" = 'centroid'),
                                    icon = icon("check"),
                                    inline = T,
                                    bigger = TRUE,
                                    status = "info",
                                    animation = "jelly"
                                  ),
                                  tags$h4("Data filtering"),
                                  checkboxGroupButtons(
                                    inputId = "Pick1",
                                    label = "Select species",
                                    choices = c("Species 1", "Species 2", "Species 3", "Species 4", "Species 5", "Species 6", "Species 7", "Species 8"),
                                    selected = c("Species 1", "Species 2", "Species 3", "Species 4", "Species 5", "Species 6", "Species 7", "Species 8"),
                                    size = 'sm',
                                    status = "primary",
                                    direction = 'horizontal',
                                    checkIcon = list(
                                      yes = icon("ok", lib = "glyphicon"),
                                      no = icon("remove", lib = "glyphicon"))
                                  )
                                )),
                            div(style="display: inline-block;vertical-align:top; width: 50px;",
                                dropdownButton(
                                  tags$h4("Dots"),
                                  circle = TRUE,
                                  status = "primary",
                                  icon = icon("fill-drip"),
                                  size = 'sm',
                                  tooltip = tooltipOptions(title = "Click to change appearance"),
                                  sliderInput('size.dot.kin', 'Size',
                                              min=0, max=10, value=4,
                                              step=0.25, round=0),
                                  sliderInput('transp.kin', 'Opacity',
                                              min=0, max=1, value=0.9,
                                              step=0.05),
                                  tags$h4("Colors"),
                                  colourInput("col.dot.kin1", "Series 1", "#1f77b4"),
                                  colourInput("col.dot.kin2", "Series 2", "#ff7f0e"),
                                  colourInput("col.dot.kin3", "Series 3", "#2ca02c"),
                                  colourInput("col.dot.kin4", "Series 4", "#d62728"),
                                  colourInput("col.dot.kin5", "Series 5", "#9467bd"),
                                  colourInput("col.dot.kin6", "Series 6", "#8c564b"),
                                  colourInput("col.dot.kin7", "Series 7", "#e377c2"),
                                  colourInput("col.dot.kin8", "Series 8", "#7f7f7f")
                                )),
                            div(style="display: inline-block;vertical-align:top; width: 50px;",
                                dropdownButton(
                                  tags$h4("Dimensions"),
                                  circle = TRUE,
                                  status = "success",
                                  icon = icon("ruler-combined"),
                                  size = 'sm',
                                  tooltip = tooltipOptions(title = "Click to change dimensions"),
                                  sliderInput('k.plot.w', 'Plot width',
                                              min=100, max=2000, value=1000,
                                              step=20, round=0),
                                  sliderInput('k.plot.h', 'Plot height',
                                              min=100, max=3000, value= 500,
                                              step=20, round=0)
                                )),
                            uiOutput("k.plot.ui")
                          ),
                          box(
                            title = 'Data for post-processing',
                            footer = 'Select data in plot filter dropdown menu. Create/update the table by clicking on "Generate table".',
                            solidHeader = T,
                            status = 'primary',
                            collapsible = T,
                            collapsed = T,
                            actionBttn(inputId = "k.wide.bttn",
                                       label = "Generate table",
                                       icon = icon('table', class = 'solid'),
                                       style = "simple",
                                       color = "primary",
                                       size = "sm",
                                       block = T,
                                       no_outline = TRUE),
                            tags$h4(''),
                            DTOutput('k.wide')
                          ),
                          box(
                            title = 'Raw spectrum data',
                            solidHeader = T,
                            status = 'primary',
                            collapsible = T,
                            collapsed = T,
                            downloadButton("download1", ""), # no label: this button will be hidden
                            numericInput("nrows", "Number of rows", 10),
                            DTOutput('k.spectra')
                          )
                        )
               ),
               #panel titR------
               tabPanel("TitR",
                        icon = icon('chart-line'),
                        fluidRow(
                          box(
                            title = "Equilibrium data",
                            width = 12,
                            status = "info",
                            solidHeader = T,
                            collapsible = T,
                            DTOutput('eq.raw')
                          ),
                          box(
                            title = "Relative response factors",
                            width = 12,
                            status = "info",
                            solidHeader = T,
                            collapsible = T,
                            DTOutput('Rf')
                          ),
                          box(
                            title = "Corrected concentrations",
                            width = 12,
                            status = "info",
                            solidHeader = T,
                            collapsible = T,
                            DTOutput('corr.C')
                          )
                        )
               )
    )
  )
)
