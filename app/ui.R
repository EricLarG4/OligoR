#dependencies----
# if (require(devtools)) install.packages("devtools")#if not already installed
# devtools::install_github("AnalytixWare/ShinySky")

packages <- c("librarian")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

devtools::install_github("AnalytixWare/ShinySky", force = FALSE)
library(shinysky)

librarian::shelf(
  shiny, shinydashboard, shinydashboardPlus, shinyBS, shinyWidgets, bslib,
  DT, colourpicker, DavidBarke/QWUtils
)


ui <- dashboardPage(
  skin = "midnight",
  # includeCSS("www/ui.css"),
  title = "OligoR 0.9",
  scrollToTop = TRUE,
  header = dashboardHeader(
    title = "OligoR 0.9"
  ),
  #sidebar-------------
  dashboardSidebar(
    tags$head(
      tags$style(
        HTML('
          .panel-title {
          color: white !important;
          font-weight: bold;
          }
          .panel-body {background-color: #272c30 !important;}
          .panel-heading {background-color: steelblue !important;}
          .irs-min {
          background-color: forestgreen !important;
          color: white !important;
          }
          .irs-max {
          background-color: tomato !important;
          color: white !important;
          }
          .bootstrap-switch-handle-on.bootstrap-switch-primary {
          background: "black"; !important
          }
        ')
      )
    ),
    # chooseSliderSkin("Flat", color = "steelblue"),
    ##sidebar OligoRef-------------
    conditionalPanel(
      condition = "input.tabs == 'OligoRef'",
      br(),
      box(id = "boxseq",
          title = "Analyte information",
          status = "primary",
          solidHeader = F,
          collapsible = T,
          width = 12,
          textInput(
            inputId = "sequence",
            label = "Sequence",
            value = "TAGGGTTAGGGTTAGGGTTAGGG"
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
            )
          ),
          column(
            width = 12,
            strong("Additional atoms"),
            splitLayout(
              textInput(
                inputId = "user.nC",
                label = "C",
                value = 0
              ),
              textInput(
                inputId = "user.nH",
                label = "H",
                value = 0
              )
            ),
            splitLayout(
              textInput(
                inputId = "user.nN",
                label = "N",
                value = 0
              ),
              textInput(
                inputId = "user.nO",
                label = "O",
                value = 0
              )
            ),
            splitLayout(
              textInput(
                inputId = "user.nP",
                label = "P",
                value = 0
              ),
              textInput(
                inputId = "K",
                label = "K+",
                value = "2"
              )
            ),
            htmlOutput('chem.formula'),
            htmlOutput('ex.sites')
          )
      ),
      box(
        title = "Exchangeable sites",
        status = "warning",
        solidHeader = FALSE,
        collapsible = TRUE,
        width = 12,
        textInput(
          inputId = "nX.user",
          label = 'Custom',
          placeholder = 'defaults to preset'
        ),
        selectInput(
          "nX.select",
          label = 'Presets',
          choices = list(
            "nucleobases only" = 'C',
            "nucleobases + termini" = 'B',
            "all sites" = 'A'
          ),
          selected = "C"
        )
      ),
      box(
        title = "Isotopic abundance",
        status = "danger",
        solidHeader = FALSE,
        collapsible = TRUE,
        width = 12,
        sliderInput(
          inputId = "DC",
          label = 'D%',
          value = 9,
          min = 0, max = 100,
          step = 0.5,
          ticks = FALSE
        ),
        sliderInput(
          inputId = "nrPeaks.user",
          label = "Number of peaks",
          value = 72,
          min = 8, max = 128,
          step = 8,
          ticks = FALSE
        ),
        textInput(
          inputId = "K41C",
          label = 'K41%',
          value = 6.730244
        )
      )
    ),
    ##sidebar MSxplorR-------------
    conditionalPanel(
      condition = "input.tabs == 'MSxploR'",
      br(),
      box(
        width = 12,
        title = "Import MS data",
        status = 'primary',
        collapsible = T,
        enable_dropdown = T,
        dropdown_icon = 'upload',
        fileInput(
          'mzml.file',
          'Select mzML or mzXML file'
        )
      ),
      box(
        width = 12,
        title = "m/z range",
        status = 'primary',
        solidHeader = F,
        collapsible = T,
        collapsed = F,
        sliderInput(
          inputId = "text11",
          label = NULL,
          ticks = FALSE,
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
                     color = "default",
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
          status = 'success',
          solidHeader = FALSE,
          collapsible = TRUE,
          collapsed = FALSE,
          actionBttn(inputId = "bttn42",
                     label = "Select species",
                     icon = icon('check-circle', class = 'solid'),
                     style = "simple",
                     color = "success",
                     size = "sm",
                     block = F,
                     no_outline = TRUE)
      ),
      box(width = 12,
          title = "Titration",
          status = 'danger',
          solidHeader = FALSE,
          collapsible = TRUE,
          collapsed = FALSE,
          textInput(inputId = 'Stoich',
                    label = "Ligand stoichiometry",
                    value = 0),
          textInput(inputId = 'lgd.conc',
                    label = "Ligand concentration (µM)",
                    value = 0),
          actionBttn(
            inputId = "bttn.target",
            label = "Select target",
            icon = icon('check-circle', class = 'solid'),
            style = "simple",
            color = "danger",
            size = "sm",
            block = F,
            no_outline = TRUE
          ),
          actionBttn(
            inputId = "bttn.std",
            label = "Select standard",
            icon = icon('check-circle', class = 'solid'),
            style = "simple",
            color = "default",
            size = "sm",
            block = F,
            no_outline = TRUE
          )
      )
    ),
    ##sidebar TimeR-------------
    conditionalPanel(
      condition = "input.tabs =='TimeR'",
      br(),
      box(
        width = 12,
        title = "Import kinetics data",
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
        textInput(
          inputId = "text33",
          label = "Start (min)",
          value = 0
        ),
        textInput(
          inputId = "text34",
          label = "End (min)",
          value = 999
        ),
        sliderInput(
          inputId = "ave.scan",
          label = "Scans to average",
          min = 1,
          max = 25,
          step = 1,
          value = 1,
          ticks = FALSE
        )
      ),
      box(
        width = 12,
        title = "Standardization",
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
        prettyToggle(
          inputId = 'kin.norm',
          label_off = 'absolute intensity',
          label_on = 'relative intensity',
          value = FALSE
        )
      ),
      box(
        width = 12,
        title = "Species",
        status = 'danger',
        solidHeader = FALSE,
        collapsible = TRUE,
        collapsed = FALSE,
        checkboxGroupButtons(
          inputId = "Pick1",
          label = "Select species",
          choices = c("Species 1", "Species 2", "Species 3", "Species 4", "Species 5", "Species 6", "Species 7", "Species 8"),
          selected = c("Species 1", "Species 2", "Species 3", "Species 4", "Species 5", "Species 6", "Species 7", "Species 8"),
          size = 'xs',
          status = "primary",
          direction = 'horizontal',
          checkIcon = list(
            yes = icon("ok", lib = "glyphicon"),
            no = icon("remove", lib = "glyphicon"))
        ),
        br(),
        strong("Name Species"),
        br(),
        textInput(
          inputId = "text36",
          label = "Species 1",
          value = "Species 1"
        ),
        textInput(
          inputId = "text37",
          label = "Species 2",
          value = "Species 2"
        ),
        textInput(
          inputId = "text38",
          label = "Species 3",
          value = "Species 3"
        ),
        textInput(
          inputId = "text39",
          label = "Species 4",
          value = "Species 4"
        ),
        textInput(
          inputId = "text40",
          label = "Species 5",
          value = "Species 5"
        ),
        textInput(
          inputId = "text41",
          label = "Species 6",
          value = "Species 6"
        ),
        textInput(
          inputId = "text42",
          label = "Species 7",
          value = "Species 7"
        ),
        textInput(
          inputId = "text43",
          label = "Species 8",
          value = "Species 8"
        )
      )
    ),
    ##sidebar HDXplotR-------------
    conditionalPanel(
      condition = "input.tabs == 'HDXplotR'",
      br(),
      box(
        width = 12,
        title = "Import HDX kinetics",
        status = "primary",
        solidHeader = FALSE,
        collapsible = TRUE,
        actionBttn(
          inputId = "bttn.kin.hdx.export",
          label = "Import data",
          icon = icon('file-import', class = 'solid'),
          style = "simple",
          color = "danger",
          size = "sm",
          block = FALSE,
          no_outline = TRUE
        )
      ),
      box(
        width = 12,
        title = "Fitting",
        status = "maroon",
        solidHeader = F,
        collapsible = T,
        collapsed = F,
        prettyToggle(
          inputId = "fit.hdx",
          label_on = "Fit",
          label_off = "No fit",
          value = FALSE,
          shape = "round",
          width = "100%",
          bigger = TRUE,
          animation = "pulse"
        )
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
        )
      )
    ),
    ##sidebar MSstackR-------------
    conditionalPanel(
      condition = "input.tabs == 'MSstackR'",
      br(),
      box(
        width = 12,
        title = "Import data",
        status = 'primary',
        solidHeader = F,
        collapsible = T,
        enable_dropdown = T,
        dropdown_icon = 'upload',
        fileInput(
          'exported.snaps',
          'Peak-picked file'
        ),
        fileInput(
          'exported.opt',
          'Optimized distribution file'
        )
      ),
      box(
        title = 'Time scale',
        collapsible = T,
        collapsed = F,
        solidHeader = F,
        width = 12,
        status = "warning",
        prettyToggle(
          inputId = "timescale",
          label_on = "Manual",
          label_off = "TIC",
          value = TRUE,
          shape = "round",
          width = "100%",
          bigger = TRUE,
          animation = "pulse"
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
        status = "success",
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
    ##sidebar TitratR-------------
    conditionalPanel(
      condition = "input.tabs =='TitratR'",
      br(),
      box(
        width = 12,
        title = "Import data",
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
        label = "Target concentration (µM)",
        value = 10
      ),textInput(
        inputId = "Std",
        label = "Internal std concentration (µM)",
        value = 2
      ),
      actionBttn(
        inputId = "IS",
        label = "Associate internal std",
        icon = icon('puzzle-piece', class = 'solid'),
        color = "warning",
        size = "sm",
        style = "simple"
      ),
      actionBttn(
        inputId = "bttn55",
        label = "Process response factors",
        icon = icon('microchip', class = 'solid'),
        style = "simple",
        color = "danger",
        size = "sm",
        block = F,
        no_outline = TRUE
      ),
      actionBttn(
        inputId = "bttn.Ccor",
        label = "Calculate concentrations",
        icon = icon('microchip', class = 'solid'),
        style = "simple",
        color = "danger",
        size = "sm",
        block = F,
        no_outline = TRUE
      )
    ),
    verbatimTextOutput("value")
  ),
  #body--------------
  dashboardBody(
    tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: pink !important;}')),
    tags$style(
      type="text/css", #hides error messages
      # ".shiny-output-warning { visibility: hidden; }",
      # ".shiny-output-warning:before { visibility: hidden; }",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"
    ),
    tags$head(
      tags$style(
        HTML(
          '
          .handsontable th, .handsontable td {
            background-color: #272c30 !important;
            color: #bec5cb !important;
          }
          '
        )
      )
    ),
    navbarPage(
      # theme = bs_theme(
      #   version = 4,
      #   bootswatch = "darkly"
      # ),
      title = 'OligoR',
      id = 'tabs',
      collapsible = TRUE,
      position = "fixed-top",
      inverse = TRUE,
      ##panel oligoRef------------
      tabPanel(
        "OligoRef",
        icon = icon("dna"),
        fluidRow(
          box(id = 'Oligoutput69',
              title = 'Analyte characteristics',
              collapsible = T,
              collapsed = T,
              width = 3,
              status = "primary",
              dataTableOutput("oligo.data")
          ),
          box(id = 'charge series',
              title = 'Electrospray series',
              collapsible = T,
              collapsed = T,
              width = 3,
              status = "primary",
              p("m/z corresponding to natural isotopic abundances"),
              dataTableOutput("charge.series")
          ),
          box(id = 'Oligoutput10',
              title = 'Theoretical distribution data',
              collapsible = T,
              collapsed = T,
              width = 6,
              status = "warning",
              dataTableOutput("peak.position")
          ),
          box(
            id = 'Oligoutput7',
            title = 'Theoretical distribution',
            width = 6,
            collapsible = TRUE,
            status = "danger",
            uiOutput('plot.ref.ui'),
            sidebar = boxSidebar(
              id = "dwn.sidebar.1",
              width = 25,
              startOpen = FALSE,
              icon = icon("download"),
              downloadBttn(
                outputId = "ref.png",
                label = "Save as png",
                style = "simple",
                size = 'sm'
              ),
              br(),
              br(),
              downloadBttn(
                outputId = "ref.pdf",
                label = "Save as pdf",
                style = "simple",
                size = 'sm'
              )
            )
          ),
          box(
            id = 'Oligoutput7-2',
            title = 'Reference accuracy',
            width = 6,
            collapsible = T,
            status = "success",
            uiOutput('p.hdx.ref.vs.exp.ui'),
            sidebar = boxSidebar(
              id = "dwn.sidebar.2",
              width = 25,
              startOpen = FALSE,
              icon = icon("download"),
              downloadBttn(
                outputId = "ref.accu.png",
                label = "Save as png",
                style = "simple",
                size = 'sm'
              ),
              br(),
              br(),
              downloadBttn(
                outputId = "ref.accu.pdf",
                label = "Save as pdf",
                style = "simple",
                size = 'sm'
              )
            )
          )
        ),
        absolutePanel(
          top = 150, right = 40, width = 300,
          draggable = TRUE,
          fixed = TRUE,
          bsCollapse(
            open = "plop.1",
            bsCollapsePanel(
              "Customisation",
              colourInput("col.dot.th", "Theory  dot colour", "tomato"),
              colourInput("col.line.th", "Theory line colour", "tomato"),
              colourInput("col.centroid.th", "Theory centroid colour", "tomato"),
              colourInput("col.line.exp", "Reference line colour", "#BDD1E3"),
              colourInput("col.centroid.exp", "Centroid line colour", "#BDD1E3"),
              splitLayout(
                sliderInput(
                  'size.dot.th', 'Theory dot size',
                  min=0, max=10, value=4,
                  step=0.25, round=0,
                  tick = FALSE
                ),
                sliderInput(
                  'size.line.th', 'Theory line size',
                  min=0, max=5, value=1,
                  step=0.25, round=0,
                  tick = FALSE
                )
              ),
              splitLayout(
                sliderInput(
                  'size.line.exp', 'Reference line size',
                  min=0, max=5, value=1,
                  step=0.25, round=0,
                  tick = FALSE
                ),
                sliderInput(
                  'size.centroid.th', 'Centroid line size',
                  min=0, max=5, value=1,
                  step=0.25, round=0,
                  tick = FALSE
                )
              ),
              splitLayout(
                sliderInput(
                  'plot.ref.w', 'Plot width',
                  min=100, max=2000, value=750,
                  step=20, round=0,
                  ticks = FALSE
                ),
                sliderInput(
                  'plot.ref.h', 'Plot height',
                  min=100, max=3000, value= 500,
                  step=20, round=0,
                  ticks = FALSE
                )
              )
            )
          ),
          style = "opacity: 0.9"
        )
      ),
      ##panel MSxploR------------
      tabPanel(
        "MSxploR",
        icon = icon('drafting-compass'),
        fluidRow(
          box(
            id = "box1",
            width = 10,
            title = "TIC",
            p("Brush to select scans, resize edges and drag as desired"),
            status = 'primary',
            collapsible = TRUE,
            uiOutput("plot1.ui"),
            sidebar = boxSidebar(
              id = "dwn.sidebar.tic",
              width = 25,
              startOpen = FALSE,
              icon = icon("download"),
              downloadBttn(
                outputId = "plot1.png",
                label = "Save as png",
                style = "simple",
                size = 'sm'
              ),
              br(),
              br(),
              downloadBttn(
                outputId = "plot1.pdf",
                label = "Save as pdf",
                style = "simple",
                size = 'sm'
              )
            )
          ),
          box(
            title = "Selected time range",
            width = 2,
            status = 'primary',
            collapsible = T,
            textOutput("info", inline = F)
          ),
          box(
            title = "Selected scans",
            width = 2,
            status = 'primary',
            collapsible = T,
            textOutput("info1", inline = F)
          )
        ),
        fluidRow(
          box(
            id = "box3",
            width = 10,
            title = "MS spectrum",
            p("Brush to zoom, double click to reset zoom"),
            status = "danger",
            collapsible = TRUE,
            uiOutput("plot3.ui"),
            sidebar = boxSidebar(
              id = "dwn.sidebar.3",
              width = 25,
              startOpen = FALSE,
              icon = icon("download"),
              downloadBttn(
                outputId = "plot3.png",
                label = "Save as png",
                style = "simple",
                size = 'sm'
              ),
              br(),
              br(),
              downloadBttn(
                outputId = "plot3.pdf",
                label = "Save as pdf",
                style = "simple",
                size = 'sm'
              )
            )
          ),
          box(
            title = "Selected m/z",
            width = 2,
            status = "danger",
            collapsible = T,
            textOutput("info2", inline = F)
          )
        ),
        fluidRow(
          box(
            title = "HDX timepoint snap",
            width = 4,
            status = "warning",
            collapsible = TRUE,
            DTOutput("MSsnap")
          ),
          box(
            title = "Target snap",
            width = 4,
            status = "success",
            collapsible = TRUE,
            DTOutput("target")
          ),
          box(
            title = "Standard snap",
            width = 4,
            status = "success",
            collapsible = TRUE,
            DTOutput("standard")
          )
        ),
        absolutePanel(
          top = 150, right = 40, width = 200,
          draggable = TRUE,
          fixed = TRUE,
          bsCollapse(
            open = "plop.2",
            bsCollapsePanel(
              "Customisation",
              colourInput(
                "col.TIC", "TIC colour", "steelblue"
              ),
              colourInput(
                "col.MS", "Spectrum colour", "steelblue"
              ),
              sliderInput(
                'size.line.TIC', 'TIC line size',
                min=0.25, max=5, value=1,
                step=0.25, round=0,
                ticks = FALSE
              ),
              sliderInput(
                'size.line.MS', 'Spectrum line size',
                min=0.25, max=5, value=1,
                step=0.25, round=0,
                ticks = FALSE
              ),
              splitLayout(
                sliderInput(
                  'plot.tic.w', 'TIC width',
                  min=100, max=2000, value=1500,
                  step=20, round=0,
                  ticks = FALSE
                ),
                sliderInput(
                  'plot.tic.h', 'TIC height',
                  min=100, max=3000, value= 150,
                  step=10, round=0,
                  ticks = FALSE
                )
              ),
              splitLayout(
                sliderInput(
                  'plot.xplor.w', 'Plot width',
                  min=100, max=2000, value=1500,
                  step=20, round=0,
                  ticks = FALSE
                ),
                sliderInput(
                  'plot.xplor.h', 'Plot height',
                  min=100, max=3000, value= 600,
                  step=20, round=0,
                  ticks = FALSE
                )
              )
            )
          ),
          style = "opacity: 0.9"
        )
      ),
      ##panel MSstackR--------------
      tabPanel(
        "MSstackR",
        icon = icon('layer-group'),
        fluidRow(
          box(
            title = "Peak picking",
            width = 6,
            status = "danger",
            collapsible = T,
            collapsed = F,
            height = 1000,
            uiOutput("plot.pp.ui"),
            sidebar = boxSidebar(
              id = "dwn.sidebar.4",
              width = 25,
              startOpen = FALSE,
              icon = icon("download"),
              downloadBttn(
                outputId = "plot.pp.png",
                label = "Save as png",
                style = "simple",
                size = 'sm'
              ),
              br(),
              br(),
              downloadBttn(
                outputId = "plot.pp.pdf",
                label = "Save as pdf",
                style = "simple",
                size = 'sm'
              )
            )
          ),
          box(
            title = "Optimized data",
            width = 6,
            status = "success",
            collapsible = TRUE,
            collapsed = FALSE,
            height = 1000,
            uiOutput("optiplot.ui"),
            sidebar = boxSidebar(
              id = "dwn.sidebar.5",
              width = 25,
              startOpen = FALSE,
              icon = icon("download"),
              downloadBttn(
                outputId = "optiplot.png",
                label = "Save as png",
                style = "simple",
                size = 'sm'
              ),
              br(),
              br(),
              downloadBttn(
                outputId = "optiplot.pdf",
                label = "Save as pdf",
                style = "simple",
                size = 'sm'
              )
            )
          ),
          box(
            title = "Peak-picked data",
            width = 6,
            status = "danger",
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
            status = "warning",
            collapsible = T,
            collapsed = T,
            height = 1000,
            DTOutput('opt.filter.table')
          ),
          box(
            title = "Filtered derived values",
            width = 6,
            status = "warning",
            collapsible = T,
            collapsed = T,
            height = 1000,
            DTOutput('binom.filter.table')
          )
        ),
        absolutePanel(
          top = 150, right = 40, width = 350,
          draggable = TRUE,
          fixed = TRUE,
          bsCollapse(
            open = "plop.3",
            bsCollapsePanel(
              "Customisation",
              colourInput("col.snap1", "Gradient start", "#d0d0d0"),
              colourInput("col.snap2", "Gradient end", "#a7a7a7"),
              selectInput("trans.user", 'Color guide',
                          choices = list("identity", "log10"),
                          selected = 'identity'),
              prettyToggle(
                inputId = "com.scale",
                label_on = "fixed m/z axis",
                label_off = "free m/z axis",
                value = TRUE,
                shape = "round",
                width = "100%",
                bigger = TRUE,
                animation = "pulse"
              ),
              splitLayout(
                sliderInput(
                  'size.line.snap', 'MS line size',
                  min=0, max=5, value=1,
                  step=0.25, round=0,
                  tick = FALSE
                ),
                sliderInput(
                  'size.line.opt', 'Model line size',
                  min=0, max=5, value=1,
                  step=0.25, round=0,
                  tick = FALSE
                ),
                sliderInput(
                  'size.line.cent', 'Centroid line size',
                  min=0, max=5, value=1,
                  step=0.25, round=0,
                  tick = FALSE
                )
              ),
              splitLayout(
                sliderInput(
                  'size.dot.pp', 'Peak-picking dot size',
                  min=0, max=10, value=4,
                  step=0.25, round=0,
                  tick = FALSE
                ),
                sliderInput(
                  'size.dot.opt', 'Model dot size',
                  min=0, max=10, value=4,
                  step=0.25, round=0,
                  tick = FALSE
                )
              ),
              splitLayout(
                sliderInput(
                  'plot.snaps.w', 'Plot width',
                  min=100, max=2000, value=750,
                  step=20, round=0,
                  ticks = FALSE
                ),
                sliderInput(
                  'plot.snaps.h', 'Plot height',
                  min=100, max=3000, value= 750,
                  step=20, round=0,
                  ticks = FALSE
                )
              )
            )
          ),
          style = "opacity: 0.9"
        )
      ),
      ##panel TimeR--------
      tabPanel(
        "TimeR",
        icon = icon('clock'),
        fluidRow(
          box(
            title = "Kinetics plot",
            width = 12,
            status = "danger",
            solidHeader = F,
            collapsible = T,
            uiOutput("k.plot.ui"),
            sidebar = boxSidebar(
              id = "dwn.sidebar.10",
              width = 25,
              startOpen = FALSE,
              icon = icon("download"),
              downloadBttn(
                outputId = "timer.png",
                label = "Save as png",
                style = "simple",
                size = 'sm'
              ),
              br(),
              br(),
              downloadBttn(
                outputId = "timer.pdf",
                label = "Save as pdf",
                style = "simple",
                size = 'sm'
              )
            )
          ),
          box(
            title = "Kinetics data",
            footer = "To be able to reimport the data, save as Excel. The number of scans to average and time range can be reprocessed. Caution: scans excluded will not be exported.",
            width = 12,
            status = "success",
            solidHeader = FALSE,
            collapsible = TRUE,
            collapsed = TRUE,
            DTOutput("k.table")
          ),
          box(
            title = 'Raw spectrum data',
            solidHeader = FALSE,
            status = 'primary',
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            DTOutput('k.spectra')
          )
        ),
        absolutePanel(
          top = 150, right = 40, width = 300,
          draggable = TRUE,
          fixed = TRUE,
          bsCollapse(
            open = "plop.5",
            bsCollapsePanel(
              "Customisation",
              flowLayout(
                colourInput("col.dot.kin1", "Series 1", "#E69F00", allowTransparent = TRUE),
                colourInput("col.dot.kin2", "Series 2", "#56B4E9", allowTransparent = TRUE),
                colourInput("col.dot.kin3", "Series 3", "#009E73", allowTransparent = TRUE),
                colourInput("col.dot.kin4", "Series 4", "#F0E442", allowTransparent = TRUE),
                colourInput("col.dot.kin5", "Series 5", "#0072B2", allowTransparent = TRUE),
                colourInput("col.dot.kin6", "Series 6", "#D55E00", allowTransparent = TRUE),
                colourInput("col.dot.kin7", "Series 7", "#CC79A7", allowTransparent = TRUE),
                colourInput("col.dot.kin8", "Series 8", "#777F85", allowTransparent = TRUE)
              ),
              splitLayout(
                sliderInput(
                  'size.dot.kin', 'Size',
                  min=0, max=10, value=4,
                  step=0.25, round=0,
                  ticks = FALSE
                ),
                sliderInput(
                  'transp.kin', 'Opacity',
                  min=0, max=1, value=0.9,
                  step=0.05,
                  ticks = FALSE
                )
              ),
              splitLayout(
                sliderInput(
                  'k.plot.w',
                  'Plot width',
                  min=100, max=2000, value=1000,
                  step=20, round=0,
                  ticks = FALSE
                ),
                sliderInput(
                  'k.plot.h',
                  'Plot height',
                  min=100, max=3000, value= 500,
                  step=20, round=0,
                  ticks = FALSE
                )
              )
            )
          ),
          style = "opacity: 0.9"
        )
      ),
      ##panel HDXplotR----------
      tabPanel(
        "HDXplotR",
        icon = icon('stopwatch'),
        fluidRow(
          box(
            title = 'Kinetics data',
            width = 6,
            collapsible = TRUE,
            status = "success",
            p("Select the data points to be processed by
                              clicking on the rows, or select them all with
                              the checkbox below."),
            checkboxInput(
              inputId = "NUS.sel",
              label = "select all",
              value = FALSE
            ),
            DTOutput("NUS")
          ),
          box(
            title = 'User input',
            width = 6,
            collapsible = TRUE,
            status = "success",
            h4("NUS calculation"),
            hotable("hotable2"),
            br(),
            h4("Fit Initialization"),
            hotable('hotable3'),
            h4("Deconvoluted fit initialization"),
            strong('NUS'),
            hotable('hotable4'),
            br(),
            strong('Population abundance'),
            hotable('hotable5')
          )
        ),
        fluidRow(
          box(
            title = "Apparent centroids",
            width = 6,
            status = "primary",
            collapsible = T,
            uiOutput("p.app.cent.ui"),
            sidebar = boxSidebar(
              id = "dwn.sidebar.6",
              width = 25,
              startOpen = FALSE,
              icon = icon("download"),
              downloadBttn(
                outputId = "centroids.png",
                label = "Save as png",
                style = "simple",
                size = 'sm'
              ),
              br(),
              br(),
              downloadBttn(
                outputId = "centroids.pdf",
                label = "Save as pdf",
                style = "simple",
                size = 'sm'
              )
            )
          ),
          box(
            title = "Apparent exchange plot",
            width = 6,
            status = "primary",
            collapsible = T,
            uiOutput("p.hdx.fit.app.ui"),
            sidebar = boxSidebar(
              id = "dwn.sidebar.7",
              width = 25,
              startOpen = FALSE,
              icon = icon("download"),
              downloadBttn(
                outputId = "NUS.png",
                label = "Save as png",
                style = "simple",
                size = 'sm'
              ),
              br(),
              br(),
              downloadBttn(
                outputId = "NUS.pdf",
                label = "Save as pdf",
                style = "simple",
                size = 'sm'
              )
            )
          ),
          box(
            title = 'Fit results',
            DTOutput("hdx.fit.app"),
            width = 6,
            collapsible = TRUE,
            collapsed = TRUE,
            status = "primary"
          ),
          box(
            title = 'Deconvoluted fit results',
            strong('NUS'),
            br(),
            DTOutput("hdx.fit.opt"),
            br(),
            strong('Populations'),
            br(),
            DTOutput("pop.fit.opt"),
            width = 6,
            collapsible = TRUE,
            collapsed = TRUE,
            status = "danger"
          ),
          box(
            title = "Deconvoluted exchange plot",
            width = 6,
            status = "danger",
            collapsible = TRUE,
            uiOutput("p.hdx.fit.opt.ui"),
            sidebar = boxSidebar(
              id = "dwn.sidebar.8",
              width = 25,
              startOpen = FALSE,
              icon = icon("download"),
              downloadBttn(
                outputId = "deconvoluted.NUS.png",
                label = "Save as png",
                style = "simple",
                size = 'sm'
              ),
              br(),
              br(),
              downloadBttn(
                outputId = "deconvoluted.NUS.pdf",
                label = "Save as pdf",
                style = "simple",
                size = 'sm'
              )
            )
          ),
          box(
            title = "Deconvoluted population abundances",
            width = 6,
            status = "danger",
            collapsible = TRUE,
            uiOutput("p.pop.fit.opt.ui"),
            sidebar = boxSidebar(
              id = "dwn.sidebar.9",
              width = 25,
              startOpen = FALSE,
              icon = icon("download"),
              downloadBttn(
                outputId = "deconvoluted.pop.png",
                label = "Save as png",
                style = "simple",
                size = 'sm'
              ),
              br(),
              br(),
              downloadBttn(
                outputId = "deconvoluted.pop.pdf",
                label = "Save as pdf",
                style = "simple",
                size = 'sm'
              )
            )
          )
        ),
        absolutePanel(
          top = 150, right = 40, width = 300,
          draggable = TRUE,
          fixed = TRUE,
          bsCollapse(
            open = "plop.4",
            bsCollapsePanel(
              "Customisation",
              colourInput("col.kin", "Unselected points", "#777F85", allowTransparent = TRUE),
              colourInput("col.kin.high1", "Series 1", "#E69F00", allowTransparent = TRUE),
              colourInput("col.kin.high2", "Series 2", "#56B4E9", allowTransparent = TRUE),
              colourInput("col.kin.high3", "Series 3", "#009E73", allowTransparent = TRUE),
              colourInput("col.kin.high4", "Series 4", "#F0E442", allowTransparent = TRUE),
              colourInput("col.kin.high5", "Series 5", "#0072B2", allowTransparent = TRUE),
              colourInput("col.kin.high6", "Series 6", "#D55E00", allowTransparent = TRUE),
              colourInput("col.kin.high7", "Series 7", "#CC79A7", allowTransparent = TRUE),
              colourInput("col.kin.high8", "Series 8", "black", allowTransparent = TRUE),
              splitLayout(
                sliderInput(
                  'size.kin', 'Dot Size',
                  min=1, max=10, value=3,
                  step=0.5, round=0,
                  ticks = FALSE
                ),
                sliderInput(
                  'size.line.kin', 'Line size',
                  min=0, max=5, value=1,
                  step=0.25, round=0,
                  tick = FALSE
                ),
                sliderInput(
                  'size.txt.kin', 'Text size',
                  min=1, max=10, value=5,
                  step=0.25, round=0,
                  tick = FALSE
                )
              ),
              splitLayout(
                sliderInput(
                  'plot.hdx.w', 'Plot width',
                  min=100, max=2000, value=1000,
                  step=20, round=0,
                  ticks = FALSE
                ),
                sliderInput(
                  'plot.hdx.h', 'Plot height',
                  min=100, max=3000, value= 600,
                  step=20, round=0,
                  ticks = FALSE
                )
              )
            )
          ),
          style = "opacity: 0.9"
        )
      ),
      ##panel TitratR------
      tabPanel(
        "TitratR",
        icon = icon('chart-line'),
        fluidRow(
          box(
            title = "Equilibrium data",
            width = 8,
            status = "primary",
            solidHeader = FALSE,
            collapsible = TRUE,
            DTOutput('eq.raw.target')
          ),
          box(
            title = "Standard data",
            width = 4,
            status = "primary",
            solidHeader = FALSE,
            collapsible = TRUE,
            DTOutput('eq.raw.std')
          ),
          box(
            title = "Standardized data",
            width = 6,
            status = "warning",
            solidHeader = FALSE,
            collapsible = TRUE,
            DTOutput('eq.raw')
          ),
          box(
            title = "Relative response factors",
            width = 3,
            status = "danger",
            solidHeader = FALSE,
            collapsible = TRUE,
            DTOutput('Rf')
          ),
          box(
            title = "Corrected concentrations",
            width = 3,
            status = "danger",
            solidHeader = FALSE,
            collapsible = TRUE,
            DTOutput('corr.C')
          )
        )
      ),
      ##footer----
      footer = list(
        p("Refresh plot after switching to trigger the change"),
        box(
          title = "Figure theme",
          width = 2,
          uiOutput('theme.dark'),
          splitLayout(
            sliderInput(
              inputId = "lgd.ttl.scl",
              label = "Legend title",
              value = 22,
              min = 0, max = 40,
              step = 0.25,
              ticks = FALSE
            ),
            sliderInput(
              inputId = "lgd.txt.scl",
              label = "Legend text",
              value = 18,
              min = 0, max = 40,
              step = 0.25,
              ticks = FALSE
            )
          ),
          splitLayout(
            sliderInput(
              inputId = "axs.ttl.scl",
              label = "Axis title",
              value = 22,
              min = 0, max = 40,
              step = 0.25,
              ticks = FALSE
            ),
            sliderInput(
              inputId = "axs.txt.scl",
              label = "Axis text",
              value = 18,
              min = 0, max = 40,
              step = 0.25,
              ticks = FALSE
            )
          ),
          sliderInput(
            inputId = "strp.txt.scl",
            label = "Strip text",
            value = 18,
            min = 0, max = 40,
            step = 0.25,
            ticks = FALSE
          ),
          sliderInput(
            inputId = "axs.ln.scl",
            label = "Axis line",
            value = 0.75,
            min = 0, max = 5,
            step = 0.25,
            ticks = FALSE
          )
        ),
        box(
          title = "Figure export",
          width = 1,
          sliderInput(
            inputId = "export.scl",
            label = "Export scaling",
            value = 1.3,
            min = 0, max = 5,
            step = 0.1,
            ticks = FALSE
          )
        )
      )
    )
  )
)
