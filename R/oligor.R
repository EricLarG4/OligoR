#' Software suite for oligonucleotide native MS data treatment
#'
#' @return Launches the shiny app.
#' @examples
#' oligor()


oligor <- function(){

  #libraries----
  library(BiocManager)
  options(repos = BiocManager::repositories())
  library(data.table)
  library(readr)
  library(tidyverse)
  library(DescTools)
  library(ggpubr)
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  # library(devtools)
  # install_github("nik01010/dashboardthemes")
  library(dashboardthemes)
  library(shinyjs)
  library(shinyWidgets)
  library(colourpicker)
  library(DT)
  library(stringr)
  library(formattable)
  library(ggrepel)
  library(waiter)
  library(V8)
  # if (!requireNamespace("BiocManager", quietly = TRUE))
  #   install.packages("BiocManager")
  # BiocManager::install("mzR")
  library(mzR)
  library(data.table)
  library(readxl)
  library(gnm)
  # if (require(devtools)) install.packages("devtools")#if not already installed
  # devtools::install_github("AnalytixWare/ShinySky")
  library(shinysky)
  library(ggthemes)
  library(ggsci)
  library(ggpmisc)
  library(zoo)
  # install.packages("remotes")
  # remotes::install_github("DavidBarke/QWUtils")
  library(QWUtils)
  library(plotly)
  library(shinyBS)

  # Ui change functions ----
  uiChangeThemeDropdown <- function(dropDownLabel = "Change Theme", defaultTheme = "grey_light")
  {
    changeThemeChoices <- c(
      "Blue gradient" = "blue_gradient",
      "BoE website" = "boe_website",
      "Grey light" = "grey_light",
      "Grey dark" = "grey_dark",
      "OneNote" = "onenote",
      "Poor man's Flatly" = "poor_mans_flatly",
      "Purple gradient" = "purple_gradient"
    )

    ns <- NS("moduleChangeTheme")
    dropdown <- tagList(
      selectizeInput
      (
        inputId = ns("dbxChangeTheme"),
        label = dropDownLabel,
        choices = changeThemeChoices,
        selected = defaultTheme
      )
    )

    return(dropdown)
  }

  uiChangeThemeOutput <- function()
  {
    ns <- NS("moduleChangeTheme")
    themeOutput <- tagList(
      uiOutput(ns("uiChangeTheme"))
    )

    return(themeOutput)
  }


  # Server ui change functions ----
  serverChangeTheme <- function(input, output, session)
  {
    observeEvent(
      input$dbxChangeTheme,
      {
        output$uiChangeTheme <- renderUI({
          shinyDashboardThemes(theme = input$dbxChangeTheme)
        })
      }
    )
  }


  #JS----
  jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

#ui------------------
ui <- dashboardPagePlus(
  sidebar_fullCollapse = TRUE,
  #header--------------
  dashboardHeaderPlus(
    title = "OligoR 0.48",
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "palette"
  ),
  #sidebar-------------
  dashboardSidebar(
    useShinyjs(),
    # setSliderColor(c("tomato"), c(1)),
    conditionalPanel(
      condition = "input.tabs == 'MSxploR'",
      boxPlus(
        width = "100%",
        title = "Import MS data",
        status = 'info',
        solidHeader = F,
        collapsible = T,
        enable_dropdown = T,
        dropdown_icon = 'upload',
        fileInput(
          'file1',
          'Select mzML or mzXML file'
        )
      ),
      boxPlus(
        width = "100%",
        title = "m/z range narrowing",
        # enable_label = T,
        # label_text = "narrow the m/z range for faster processing",
        status = 'primary',
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
      boxPlus(
        width = "100%",
        title = "m/z binning width",
        status = 'primary',
        solidHeader = F,
        collapsible = T,
        collapsed = TRUE,
        sliderInput(
          inputId = "slider1",
          label = NULL,
          min = 0.001, max = 0.025, value = 0.005, step = 0.001,
          round = -3,
          ticks = TRUE,
          animate = FALSE,
          width = "100%",
          sep = " ",
          dragRange = TRUE
        )
      ),
      boxPlus(width = "100%",
              title = "Data selection",
              status = 'danger',
              solidHeader = F,
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
                          width = '100%'),
              textInput(inputId = 'deadtxt',
                        label = "Dead time (min)",
                        value = 0,
                        width = "100%"),
              selectInput(
                inputId = 'sample.id',
                label = 'Species',
                choices = c('Species 1', 'Species 2', 'Species 3', 'Species 4', 'Species 5', 'Species 6', 'Species 7', 'Species 8', 'Standard', 'Target')
              )
      ),
      boxPlus(width = "100%",
              title = "HDX",
              status = 'warning',
              solidHeader = F,
              collapsible = T,
              collapsed = T,
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
                        value = 0,
                        width = "100%"),
              htmlOutput('timemin')
      ),
      boxPlus(width = "100%",
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
      boxPlus(width = "100%",
              title = "Titration",
              status = 'primary',
              solidHeader = F,
              collapsible = T,
              collapsed = T,
              # textInput(inputId = 'tgt.conc',
              #           label = "Target concentration (µM)",
              #           value = 0,
              #           width = "100%"),
              textInput(inputId = 'Stoich',
                        label = "Ligand stoichiometry",
                        value = 0,
                        width = "100%"),
              textInput(inputId = 'lgd.conc',
                        label = "Ligand concentration (µM)",
                        value = 0,
                        width = "100%"),
              actionBttn(inputId = "bttn24",
                         label = "Select datapoint",
                         icon = icon('check-circle', class = 'solid'),
                         style = "simple",
                         color = "primary",
                         size = "sm",
                         block = F,
                         no_outline = TRUE)
      ),
      boxPlus(
        width = "100%",
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
      boxPlus(
        width = "100%",
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
    conditionalPanel(
      condition = "input.tabs == 'HDXplotR'",
      boxPlus(
        width = "100%",
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
      boxPlus(
        title = 'Time scale',
        id = 'timescale2',
        collapsible = T,
        collapsed = F,
        solidHeader = F,
        width = '100%',
        switchInput(
          label = 'Time scale',
          inputId = 'manu2',
          value = TRUE,
          width = '100%',
          onLabel = 'Manual',
          offLabel = 'TIC',
          size = 'normal',
          onStatus = 'danger',
          offStatus = 'info'
        )
      ),
      boxPlus(
        width = "100%",
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
      boxPlus(
        title = "Downloads",
        id = "NUSdown",
        collapsible = T,
        solidHeader = F,
        width = '100%',
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
    conditionalPanel(
      condition = "input.tabs == 'MSstackR'",
      boxPlus(
        title = 'Time scale',
        id = 'timescale1',
        collapsible = T,
        collapsed = F,
        solidHeader = F,
        width = '100%',
        switchInput(
          label = 'Time scale',
          inputId = 'manu1',
          value = TRUE,
          width = '100%',
          onLabel = 'Manual',
          offLabel = 'TIC',
          size = 'normal',
          onStatus = 'danger',
          offStatus = 'info'
        )
      ),
      boxPlus(
        title = "Downloads",
        id = "NUSdown",
        collapsible = T,
        solidHeader = F,
        width = '100%',
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
      )
    ),
    conditionalPanel(
      condition = "input.tabs =='KineticR'",
      boxPlus(
        width = "100%",
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
      boxPlus(
        width = "100%",
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
            width = "100%"
          ),
          textInput(
            inputId = "text34",
            label = "End (min)",
            value = 999,
            width = "100%"
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
      boxPlus(
        width = "100%",
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
            width = "100%"
          ),
          textInput(
            inputId = "text37",
            label = "Species 2",
            value = "Species 2",
            width = "100%"
          )
        ),
        splitLayout(
          textInput(
            inputId = "text38",
            label = "Species 3",
            value = "Species 3",
            width = "100%"
          ),
          textInput(
            inputId = "text39",
            label = "Species 4",
            value = "Species 4",
            width = "100%"
          )
        ),
        splitLayout(
          textInput(
            inputId = "text40",
            label = "Species 5",
            value = "Species 5",
            width = "100%"
          ),
          textInput(
            inputId = "text41",
            label = "Species 6",
            value = "Species 6",
            width = "100%"
          )
        ),
        splitLayout(
          textInput(
            inputId = "text42",
            label = "Species 7",
            value = "Species 7",
            width = "100%"
          ),
          textInput(
            inputId = "text43",
            label = "Species 8",
            value = "Species 8",
            width = "100%"
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input.tabs =='TitR'",
      boxPlus(
        width = "100%",
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
        width = "100%"
      ),textInput(
        inputId = "Std",
        label = "IS concentration (µM)",
        value = 10,
        width = "100%"
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
    conditionalPanel(
      condition = "input.tabs == 'OligoRef'",
      boxPlus(id = "boxseq",
              title = "Analyte information",
              status = "primary",
              solidHeader = F,
              collapsible = T,
              width = '100%',
              textInput(
                inputId = "sequence",
                label = "Sequence",
                value = "",
                width = "100%"
              ),
              splitLayout(
                textInput(
                  inputId = "z",
                  label = "Charge",
                  value = "",
                  width = "100%"
                ),
                textInput(
                  inputId = "K",
                  label = "Potassium",
                  value = "",
                  width = "100%"
                )
              ),
              br(),
              htmlOutput('chem.formula'),
              br(),
              sliderInput(
                inputId = "DC",
                label = 'D%',
                value = 0,
                width = "100%",
                min = 0, max = 100,
                step = 0.5
              ),
              br(),
              textInput(
                inputId = "nX.user",
                label = 'Exchangeable sites',
                placeholder = 'defaults to preset',
                width = '100%'
              ),
              selectInput("nX.select", label = 'presets',
                          width = "100%",
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
    conditionalPanel(
      condition = "input.tabs == 'meltR'",
      boxPlus(
        width = "100%",
        title = "Import melting data",
        status = 'info',
        solidHeader = F,
        collapsible = T,
        fileInput('melting.input',
                  'Select Excel file'),
        switchInput(inputId = "melt.blank", #toggles baseline on/off
                    label = "Blank",
                    onLabel = 'subtract',
                    offLabel = 'ignore',
                    value = TRUE,
                    size = 'normal',
                    width = 'auto')
      ),
      boxPlus(
        width = "100%",
        title = "Derivative",
        status = 'primary',
        solidHeader = F,
        collapsible = T,
        sliderInput("melt.deriv.smooth.width",
                    "Smooth window",
                    min = 1,
                    max = 20,
                    value = 5,
                    step = 1),
        actionBttn(inputId = "bttn.deriv.melt", #initiates fit
                   label = "Plot derivatives",
                   icon = icon('calculator', class = 'regular'),
                   style = "simple",
                   color = "primary",
                   size = "sm",
                   block = F,
                   no_outline = TRUE)
      ),
      boxPlus(
        width = "100%",
        title = "Fitting",
        status = 'danger',
        solidHeader = F,
        collapsible = T,
        sliderInput("nb.it.melt.fit",
                    "Max iterations",
                    min = 500,
                    max = 100000,
                    value = 5000,
                    step = 500),
        actionBttn(inputId = "bttn.init.melt", #initiates fit
                   label = "Initialize fitting",
                   icon = icon('sign-out-alt', class = 'regular'),
                   style = "simple",
                   color = "warning",
                   size = "sm",
                   block = F,
                   no_outline = TRUE),
        actionBttn(inputId = "bttn.fit.melt", #initiates fit
                   label = "Launch fitting",
                   icon = icon('sign-in-alt', class = 'regular'),
                   style = "simple",
                   color = "danger",
                   size = "sm",
                   block = F,
                   no_outline = TRUE),
        switchInput(inputId = "toggle.baseline", #toggles baseline on/off
                    label = "toggle baselines",
                    value = TRUE),
        sliderInput("temp.therm",
                    "Temperature (K) for DeltaG", #Temperature for deltaG calculation
                    min = 273,
                    max = 373,
                    value = 273.15 + 22)
      ),
      boxPlus(
        title = "Download figures",
        id = "melt.dl",
        collapsible = T,
        solidHeader = F,
        width = '100%',
        downloadBttn(
          outputId = "dwn.melt.fit",
          label = "Fit data",
          style = "material-flat",
          size = 'xs'
        ),
        downloadBttn(
          outputId = "dwn.melt.model",
          label = "Model data",
          style = "material-flat",
          size = 'xs'
        ),
        downloadBttn(
          outputId = "dwn.melt.folded",
          label = "Folded fraction",
          style = "material-flat",
          size = 'xs'
        ),
        downloadBttn(
          outputId = "dwn.melt.Tm",
          label = "Tm summary",
          style = "material-flat",
          size = 'xs'
        )
      )
    ),
    verbatimTextOutput("value"),
    extendShinyjs(text = jscode)
  ),
  rightsidebar = rightSidebar(
    rightSidebarTabContent(
      id = 'rsb',
      active = T,
      title = 'Theme',
      uiChangeThemeDropdown(dropDownLabel = 'select an app theme', defaultTheme = 'grey_light')
    )
  ),
  #body--------------
  dashboardBody(
    use_waiter(include_js = FALSE), # do not include js
    show_waiter_on_load(spin_fading_circles()), # place at the bottom
    useShinyjs(),
    uiChangeThemeOutput(),
    extendShinyjs(text = jscode),
    tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: pink !important;}')),
    tags$style(type="text/css", #hides error messages
               # ".shiny-output-warning { visibility: hidden; }",
               # ".shiny-output-warning:before { visibility: hidden; }",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    navbarPage('Navigation',
               id = 'tabs',
               #panel meltR---------
               tabPanel("meltR",
                        icon = icon("thermometer-half"),
                        fluidRow(
                          column(12,
                                 collapsible_tabBox(
                                   title = 'Input data',
                                   id = 'tabbox.1',
                                   width = 6,
                                   selected = NULL,
                                   side = 'left',
                                   tabPanel(
                                     title = 'Input data',
                                     plotOutput("p.melt.filtered"),
                                     icon = icon('server', class = 'regular'),
                                   ),
                                   tabPanel(
                                     title = 'Filtered data table',
                                     DTOutput("melt.filtered")
                                   ),
                                   tabPanel(
                                     title = 'Derivative plot',
                                     plotOutput("p.melt.derivative"),
                                     icon = icon('calculator', class = 'regular'),
                                   )
                                 ),
                                 collapsible_tabBox(
                                   title = 'Fit',
                                   id = 'tabbox.2',
                                   width = 6,
                                   selected = NULL,
                                   side = 'left',
                                   tabPanel(
                                     title = 'Approximate Tm',
                                     DTOutput("melt.derivative"),
                                     width = 6,
                                     icon = icon('thermometer-half')
                                   ),
                                   tabPanel(
                                     title = 'Fit initialization',
                                     hotable("hotable1"),
                                     width = 6,
                                     icon = icon('sign-out-alt')
                                   ),
                                   tabPanel(
                                     title = 'Fit result',
                                     width = 6,
                                     plotOutput("p.raw.melt.fit"),
                                     icon = icon('sign-out-alt')
                                   )
                                 )
                          ),
                          column(12,
                                 collapsible_tabBox(
                                   title = 'Fit results',
                                   id = 'tabbox.3',
                                   width = 6,
                                   selected = NULL,
                                   side = 'left',
                                   tabPanel(
                                     title = 'Folded fraction',
                                     width = 6,
                                     plotOutput("p.folded.melt.fit")
                                   ),
                                   tabPanel(
                                     title = 'Modeled folded fraction',
                                     width = 6,
                                     plotlyOutput("p.folded.modeled")
                                   )
                                 ),
                                 collapsible_tabBox(
                                   title = 'Melting temperatures',
                                   id = 'tabbox.4',
                                   width = 6,
                                   selected = NULL,
                                   side = 'left',
                                   tabPanel(
                                     title = 'Table',
                                     width = 6,
                                     DTOutput("fit.melt.result.summary")
                                   ),
                                   tabPanel(
                                     title = 'Plot',
                                     width = 6,
                                     plotOutput("fit.melt.result.plot")
                                   )
                                 )
                          ),
                          boxPlus(id = 'fit.output',
                                  title = 'Fit output',
                                  collapsible = T,
                                  collapsed = T,
                                  width = 12,
                                  DTOutput("nlfit.melt.results")
                          )
                        ),
                        absolutePanel(
                          id = "filter.melt",
                          # class = "panel panel-default",
                          top = 250, right = 600,
                          width = 200, height = 'auto',
                          draggable = TRUE, fixed = TRUE,
                          bsCollapse(id = 'bsCollapseMelt',
                                     open = 'Filter',
                                     bsCollapsePanel(
                                       'Filter',
                                       sliderInput("slider.therm",
                                                   "Filter temperatures", #Temperature for deltaG calculation
                                                   min = 270,
                                                   max = 375,
                                                   step = 0.5,
                                                   value = c(276, 370)),
                                       uiOutput("select.melting.oligo"),
                                       uiOutput("select.melting.ramp"),
                                       uiOutput("select.melting.comment"),
                                       uiOutput("select.melting.rep"),
                                       uiOutput("select.melting.id"),
                                       switchInput(inputId = "melt.merge.replicates", #initiates fit
                                                   label = "merge replicates",
                                                   value = F),
                                       sliderInput('slider.melt.rounder',
                                                   "group temperature",
                                                   min = 1,
                                                   max = 1.5,
                                                   value = 1,
                                                   step = 0.01),
                                       style = 'success'
                                     )
                          ),
                          style = "opacity: 0.9"
                        ),
                        absolutePanel(
                          id = "custom.melt",
                          # class = "panel panel-default",
                          top = 125, right = 100,
                          width = 200, height = 'auto',
                          draggable = TRUE, fixed = TRUE,
                          bsCollapse(id = 'bsCollapseTest',
                                     open = 'Customisation',
                                     bsCollapsePanel(
                                       'Customisation',
                                       uiOutput('select.melting.palette.fam'),
                                       uiOutput('select.melting.palette'),
                                       sliderInput('size.dot.melt', 'Dot size',
                                                   min=0, max=10, value=4,
                                                   step=0.25, round=0),
                                       sliderInput('alpha.dot.melt', 'Dot transparency',
                                                   min = 0, max = 1, value = 0.7,
                                                   step=0.05, round=0),
                                       sliderInput('size.line.melt', 'line size',
                                                   min=0, max=5, value=1,
                                                   step=0.1, round=0),
                                       sliderInput('alpha.line.melt', 'line transparency',
                                                   min = 0, max = 1, value = 1,
                                                   step=0.05, round=0),
                                       sliderInput('size.baseline.melt', 'baseline size',
                                                   min=0, max=5, value=1,
                                                   step=0.1, round=0),
                                       sliderInput('alpha.baseline.melt', 'baseline transparency',
                                                   min = 0, max = 1, value = 0.75,
                                                   step=0.05, round=0),
                                       style = 'primary'
                                     )
                          ),
                          style = "opacity: 0.9"
                        )
               ),
               #panel oligoRef------------
               tabPanel("OligoRef",
                        icon = icon("dna"),
                        fluidRow(
                          boxPlus(id = 'Oligoutput69',
                                  title = 'Analyte characteristics',
                                  collapsible = T,
                                  collapsed = T,
                                  width = 6,
                                  dataTableOutput("oligo.data")
                          ),
                          boxPlus(id = 'Oligoutput10',
                                  title = 'Theoretical distribution data',
                                  collapsible = T,
                                  collapsed = T,
                                  width = 6,
                                  dataTableOutput("peak.position")
                          ),
                          boxPlus(id = 'Oligoutput7',
                                  title = 'Theoretical distribution',
                                  width = 6,
                                  collapsible = T,
                                  enable_sidebar = TRUE,
                                  sidebar_width = 25,
                                  sidebar_start_open = F,
                                  sidebar_content = tagList(
                                    sliderInput(
                                      inputId = "nrPeaks.user",
                                      label = HTML('<p style="color:white;">Number of peaks </p>'),
                                      value =72,
                                      width = "100%",
                                      min = 8, max = 128,
                                      step = 8
                                    )
                                  ),
                                  plotOutput('plot99')
                          ),
                          boxPlus(id = 'Oligoutput7-2',
                                  title = 'Reference accuracy',
                                  width = 6,
                                  collapsible = T,
                                  enable_sidebar = F,
                                  sidebar_width = 25,
                                  sidebar_start_open = F,
                                  sidebar_content = tagList(
                                    sliderInput(
                                      inputId = "nrPeaks.user",
                                      label = HTML('<p style="color:white;">Number of peaks </p>'),
                                      value =72,
                                      width = "100%",
                                      min = 8, max = 128,
                                      step = 8
                                    )
                                  ),
                                  plotOutput('plot98')
                          )
                        ),
                        absolutePanel(
                          top = 300, right = 40, width = 200,
                          draggable = TRUE,
                          wellPanel(h3("Customisation"),
                                    colourInput("col.dot.th", "Theory  dot colour", "tomato"),
                                    colourInput("col.line.th", "Theory line colour", "tomato"),
                                    colourInput("col.centroid.th", "Theory centroid colour", "tomato"),
                                    colourInput("col.line.exp", "Reference line colour", "steelblue"),
                                    colourInput("col.centroid.exp", "Centroid line colour", "steelblue"),
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
                          boxPlus(id = "box1",
                                  width = 10,
                                  title = "TIC", p("Brush to select scans, resize edges and drag as desired"),
                                  status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  plotOutput("plot1",
                                             brush = brushOpts(id = "plot_brush", fill = "tomato", stroke = "tomato", direction = "x"),
                                             height = 200)),
                          boxPlus(
                            title = "Selected time range",
                            width = 2,
                            status = "primary",
                            closable = F,
                            solidHeader = F,
                            collapsible = T,
                            textOutput("info", inline = F)
                          ),
                          boxPlus(
                            title = "Selected scans",
                            width = 2,
                            status = "primary",
                            closable = F,
                            solidHeader = F,
                            collapsible = T,
                            textOutput("info1", inline = F)
                          )
                        ),
                        fluidRow(
                          boxPlus(id = "box3",
                                  width = 10,
                                  title = "MS spectrum", p("Brush to zoom, double click to reset zoom"),
                                  status = "danger", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  plotOutput("plot3",
                                             height = 400,
                                             dblclick = "plot3_dblclick",
                                             brush = brushOpts(
                                               id = "plot3_brush",
                                               fill = "tomato", stroke = "tomato", direction = "xy",
                                               resetOnNew = TRUE
                                             ))),
                          boxPlus(
                            title = "Selected m/z",
                            width = 2,
                            status = "danger",
                            closable = F,
                            solidHeader = F,
                            collapsible = T,
                            textOutput("info2", inline = F)
                          )
                        ),
                        fluidRow(
                          boxPlus(
                            id = "box4",
                            width = 10,
                            title = "MS spectrum (text input)", p("select scans and zoom from text input in sidebar"),
                            status = "success",
                            solidHeader = TRUE,
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
               #panel spectra--------------
               tabPanel("MSstackR",
                        icon = icon('layer-group'),
                        fluidRow(
                          boxPlus(
                            title = "Stacked spectra",
                            width =12,
                            status = "info",
                            solidHeader = T,
                            collapsible = T,
                            height = 1000,
                            # tableOutput("MSsnaps") #diagnostics for snapshots
                            uiOutput("plot5.ui")
                          )
                        ),
                        absolutePanel(
                          bottom = 200, right = 40, width = 200,
                          draggable = TRUE,
                          wellPanel(h3("Customisation"),
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
                                                width = '100%',
                                                onLabel = 'fixed',
                                                offLabel = 'free',
                                                onStatus = 'danger',
                                                offStatus = 'info',
                                                size = 'normal'),
                                    switchInput(inputId = "t.indic",
                                                label = 'scale',
                                                value = TRUE,
                                                width = '100%',
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
                                     checkboxInput("centroids_sel", "select all")
                                   ),
                                   tabPanel(
                                     title = 'Fit initialization',
                                     icon = icon("edit"),
                                     hotable('hotable3')
                                   )
                                 )
                          ),
                          boxPlus(
                            title = "Exchange plot (raw)",
                            width = 6,
                            status = "info",
                            solidHeader = T,
                            collapsible = T,
                            # tableOutput("MSsnaps") #diagnostics for snapshots
                            plotOutput("plot6")),
                          boxPlus(
                            title = "Exchange plot (NUS)",
                            width = 6,
                            status = "primary",
                            solidHeader = T,
                            collapsible = T,
                            # tableOutput("MSsnaps") #diagnostics for snapshots
                            plotOutput("plot7")
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
                          boxPlus(
                            title = "Kinetics data",
                            footer = "To be able to reimport the data, save as Excel. The number of scans to average and time range can be reprocessed. Caution: scans excluded will not be exported.",
                            width = 12,
                            status = "success",
                            solidHeader = T,
                            collapsible = T,
                            DTOutput("k.table")
                          ),
                          boxPlus(
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
                                    choices = c("Raw" = 'raw', "Corrected" = 'corrected'),
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
                          boxPlus(
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
                          )
                        )
               ),
               #panel titR------
               tabPanel("TitR",
                        icon = icon('chart-line'),
                        fluidRow(
                          boxPlus(
                            title = "Equilibrium data",
                            width = 12,
                            status = "info",
                            solidHeader = T,
                            collapsible = T,
                            DTOutput('eq.raw')
                          ),
                          boxPlus(
                            title = "Relative response factors",
                            width = 12,
                            status = "info",
                            solidHeader = T,
                            collapsible = T,
                            DTOutput('Rf')
                          ),
                          boxPlus(
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





#server---------
server <- function(input, output, session) {

  #splash screen===========

  Sys.sleep(1)

  hide_waiter()

  # Changing theme ----------------------------------------------------------
  callModule(module = serverChangeTheme, id = "moduleChangeTheme")

  #OligoR----------


  z <- reactive({
    as.numeric(input$z)
  })

  K <- reactive({
    as.numeric(input$K)
  })

  seq2 <- reactive({
    seq2<-data.frame(number=1:1, string=c(input$sequence), stringsAsFactors = F)
  })

  nbA <- reactive({
    nbA <- str_count(seq2()$string, "A")
  })

  nbT <- reactive({
    nbT <- str_count(seq2()$string, "T")
  })

  nbG <- reactive({
    nbG <- str_count(seq2()$string, "G")
  })

  nbC <- reactive({
    nbC <- str_count(seq2()$string, "C")
  })

  nb_PO <-reactive({
    #Number of nucleotides
    nb_nt <- nbA() + nbT() + nbG() + nbC()

    #Number of phosphates
    nb_PO <- nb_nt - 1
  })

  #Neutralized phosphate
  #(takes into account charge and potassium adducts that bring positive charges)
  nb_POH <- reactive({
    nb_POH <- nb_PO() - z() - K()
  })

  output$nb_PO <- renderText(nb_PO())
  output$nb_POH <- renderText(nb_POH())

  nC <- reactive({
    nC <- nbA()*10 + nbG()*10 + nbC()*9 + nbT()*10
  })

  nH <- reactive({ #This is the total amount of hydrogen across isotopes (among which nX are exchangeable)
    #Charge taken into account here, so H will not be taken out when calculating m/z
    nH <- nbA()*12 + nbG()*12 + nbC()*12 + nbT()*13 + 1 - z() - K()
  })

  nO <- reactive({
    nO <- nbA()*5 + nbG()*6 + nbC()*6 + nbT()*7 - 2
  })

  nN <- reactive({
    nN <- nbA()*5 + nbG()*5 + nbC()*3 + nbT()*2
  })

  nP <- reactive({
    nP <- nb_PO()
  })

  nK <- reactive({
    nK <- K()
  })

  chem.formula <- reactive({

    if (!is.na(nH())) {
      chem.formula <- paste(
        tags$b(style="color:tomato", 'Chemical formula: '), tags$span(style="color:tomato", "C"), tags$sub(style="color:tomato", nC()),
        tags$span(style="color:tomato", "H"), tags$sub(style="color:tomato",  nH()),
        tags$span(style="color:tomato", "O"), tags$sub(style="color:tomato", nO()),
        tags$span(style="color:tomato", "N"), tags$sub(style="color:tomato", nN()),
        tags$span(style="color:tomato", "P"), tags$sub(style="color:tomato", nP()),
        tags$span(style="color:tomato", "K"), tags$sub(style="color:tomato", nK()),
        sep = ''
      )
    }
  })

  output$chem.formula <- renderText(chem.formula())

  nX.user <- reactive({
    as.numeric(input$nX.user)
  })


  nX <- reactive({
    if(input$nX.user == ''){
      if(input$nX.select == 'A'){
        nb_PO() + 2 + nbA()*2 + nbT()*1 + nbG()*3 + nbC()*2 - z()
      } else {
        if(input$nX.select == 'B'){
          2 + nbA()*2 + nbT()*1 + nbG()*3 + nbC()*2
        } else {
          nbA()*2 + nbT()*1 + nbG()*3 + nbC()*2
        }
      }
    } else {
      nX.user()
    }
  })

  output$nX <- renderText(nX())

  #Mass calculations---------

  DC <- reactive({
    as.numeric(input$DC)
  })

  #Isotope abundances (averaged). Source: Isotopic compositions of the elements 2017. Available online at www.ciaaw.org.
  listIso <- reactive({
    list(
      H = c(0.999855, 0.000145),           #Natural 1H ``and 2H isotope abundances
      C = c(0.9894, 0.0106),
      N = c(0.996205, 0.003795),
      O = c(0.99757, 0.0003835, 0.002045),
      D = c(100-DC(), DC())/100,                #computed 1H and 2H isotope abundances for exchangeable sites dependeding on the deuterium content DC
      P = c(1),
      K = c(0.93258144, 0.0001171, 0.06730244)
    )
  })

  #Atomic masses. Current atomic masses available online at www.ciaaw.org based on Wang,M., Audi,G., Kondev,F.G., Huang,W.J., Naimi,S., et al. (2017) Chinese Phys. C, 41, 030003.
  listMass <- list(
    H = c(1.0078250322, 2.0141017781),
    C = c(12, 13.003354835),
    N = c(14.003074004, 15.000108899),
    O = c(15.994914619, 16.999131757, 17.999159613),
    D = c(1.0078250322, 2.0141017781),
    P = c(30.973761998),
    K = c(38.96370649, 39.9639982, 40.96182526)
  )

  #Monoisotopic mass
  MonoMW <- reactive({
    nC()*listMass$C[1] + nH()*listMass$H[1] + nN()*listMass$N[1] + nO()*listMass$O[1] +
      nP()*listMass$P[1] + nK()*listMass$K[1]
  })

  Monomz <- reactive({
    if (z()>0) {
      MonoMW()/z()
    } else {
      MonoMW()
    }
  })


  #Average mass calculation from number of atoms, isotopic masses and abundances.
  #nX is subtracted from nH because nH is the total number of H, including the exchangeable ones.
  AveMW <- reactive({
    AveMW <- nC()*(listIso()$C[1]*listMass$C[1] + listIso()$C[2]*listMass$C[2]) +
      (nH()-nX())*(listIso()$H[1]*listMass$H[1] + listIso()$H[2]*listMass$H[2]) +
      nN()*(listIso()$N[1]*listMass$N[1] + listIso()$N[2]*listMass$N[2]) +
      nO()*(listIso()$O[1]*listMass$O[1] + listIso()$O[2]*listMass$O[2] + listIso()$O[3]*listMass$O[3]) +
      nX()*(listIso()$D[1]*listMass$D[1] + listIso()$D[2]*listMass$D[2]) +
      nP()*(listIso()$P[1]*listMass$P[1]) +
      nK()*(listIso()$K[1]*listMass$K[1] + listIso()$K[2]*listMass$K[2] + listIso()$K[3]*listMass$K[3])
  })

  Avemz <- reactive({
    AveMW()/z() #this is called centroid in the initial, non shiny code.
  })


  oligo.data <- reactive({

    data.frame(
      "Parameters" = c('phosphates', 'neutralized phosphates', 'exchangeable sites', 'monoisotopic mass',
                       'average mass', 'monoisotopic m/z', 'average m/z'),
      "Values" = c(nb_PO(), nb_POH(), nX(), MonoMW(), AveMW(), Monomz(), Avemz())
    )
  })

  output$oligo.data <- renderDT(server = FALSE, {
    datatable(
      oligo.data(),
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
      formatRound(c('Values'), digits = 5, interval = 3, mark = '')

  })


  #FFT-----------
  #Below a different set of isotope abundances are used for the calculation of all isotopic peaks (full distribution), to make the use of FFT easier.

  nrPeaks <- reactive({
    as.numeric(input$nrPeaks.user)
  })

  Iso_Pattern <- reactive({
    #Isotopic peak abundance calculation. By default, 32 isotopic peaks are calculated but can be user-defined (more peaks may be necessary for larger and/or heavily deuterated species).
    c12 <- rep(0, nrPeaks()); h1 <- rep(0, nrPeaks()); n14 <- rep(0, nrPeaks());
    o16 <- rep(0, nrPeaks()); p31 <- rep(0, nrPeaks()); k39 <- rep(0,nrPeaks());
    h2 <- rep(0, nrPeaks());

    #Isotope abundances
    h1[1] = 0.999855;       h1[2] = 0.000145;                            #Natural abundances of H isotopes
    h2[1] = (1-DC()/100);     h2[2] = DC()/100;                              #Calculated abundances of H isotopes for exchangeable sites as a function of DC (user-supplied).
    c12[1] = listIso()$C[1]; c12[2] = listIso()$C[2];
    n14[1] = listIso()$N[1]; n14[2] = listIso()$N[2];
    o16[1] = listIso()$O[1]; o16[2] = listIso()$O[2]; o16[3] = listIso()$O[3];
    p31[1] = 1.0;
    k39[1] = listIso()$K[1]; k39[2] = listIso()$K[2]; k39[3] = listIso()$K[3];

    #Determination of the isotopic pattern by Fast Fourier Transform
    #(only yields abundance, not the corresponding m/z).
    #Based on J. Proteome Res. 2018 Jan 5; 17(1): 751–758.
    Iso_Pattern <- Re(fft(fft(c12)^nC()*fft(h1)^(nH()-nX())*fft(n14)^nN() * fft(o16)^nO() * fft(p31)^nP() * fft(k39)^nK() * fft(h2)^nX(),
                          inverse=TRUE))/length(c12)
  })

  #Isotopic peak m/z calculation based on the mono-isotopic mass calculated above.

  nbPeaks <- reactive({
    1:nrPeaks()
  })

  peak.position <- reactive({
    peak.position <- data.frame("nbPeaks1" = unlist(nbPeaks()), 'MonoMW1' = MonoMW()) %>%
      mutate('mass.th' = MonoMW1 + (nbPeaks1-1)*1.0078250321) %>%
      mutate('mz.th' = mass.th/z()) %>%
      dplyr::select(mz.th) %>%
      cbind('Iso.Pattern' = Iso_Pattern()) %>%
      mutate(Iso.Pattern = 1 - (max(Iso.Pattern)-Iso.Pattern)/(max(Iso.Pattern)-min(Iso.Pattern)))
  })

  output$peak.position <- renderDT(server = FALSE, {
    datatable(
      peak.position(),
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
      formatRound(c('Abundance'), digits = 3)

  })

  # output$peaks <- renderDT({
  #   MSsnaps.ref()
  # })

  output$plot99 <- renderPlot({
    ggplot(data = peak.position(), aes(x = mz.th, y = Iso.Pattern)) +
      geom_line(color = input$col.line.th, size = input$size.line.th) +
      geom_point(color = input$col.dot.th, size = input$size.dot.th) +
      geom_vline(xintercept = Avemz(), linetype = 'dashed', color = input$col.centroid.th, size = input$size.centroid.th) +
      xlab("m/z") +
      ylab('normalized abundance') +
      theme(strip.text.y = element_blank(),
            strip.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_line(colour = "grey50"),
            panel.grid.minor = element_line(colour = "grey"),
            panel.background = element_blank(),
            plot.background = element_blank(),
            axis.line.x = element_line(colour = "black", size = 0.75),
            axis.line.y = element_line(colour = "black", size = 0.75),
            axis.ticks.length=unit(0.1, "in"),
            axis.ticks.x = element_line(size = 0.75, colour = "black"),
            axis.ticks.y = element_line(size = 0.75, colour = "black"),
            axis.text.y = element_text(size = 16, color = "black", angle = 0),
            axis.text.x = element_text(size = 16, color = "black", angle = 0),
            axis.title.x = element_text(size=18,face="bold", color = 'black'),
            axis.title.y = element_text(size=18,face="bold", color = 'black'),
            plot.margin = margin(25, 0.5, 0.5, 0.5),
            legend.position="right",
            legend.box = "vertical",
            legend.title = element_text(size=18, face="bold", color = "black"),
            legend.key = element_rect(fill = "white"),
            legend.text = element_text(size=16, face="bold", color = "black"),
      )
  })

  plot98 <- reactive({
    ggplot(data = peak.position(), aes(x = mz.th, y = Iso.Pattern)) +
      geom_line(color = input$col.line.th, size = input$size.line.th) +
      geom_point(color = input$col.dot.th, size = input$size.dot.th) +
      geom_vline(xintercept = Avemz(), linetype = 'dashed', color = input$col.centroid.th, size = input$size.centroid.th) +
      geom_vline(xintercept = exp.centroid.ref(), linetype = 'dashed', color = input$col.centroid.exp, size = input$size.centroid.th) +
      geom_line(data = MSsnaps.ref(), aes(x = mz, y = 1 - (max(intensum)-intensum)/(max(intensum)-min(intensum))), inherit.aes = F, color = input$col.line.exp, size = input$size.line.exp) +
      # annotate(geom="text", x=Inf, y=Inf, label=OligoName, hjust = 1,
      #          color="black", size=6, fontface="bold") +
      # annotate(geom="text", x=Inf, y=0.85, label=deparse(annolab), hjust = 1,
      #          color = "black", size=5, parse=TRUE) +
      # annotate(geom="text", x=Inf, y=0.775, label=deparse(annolab2), hjust = 1,
      #          color = "black", size=5, parse=TRUE) +
      annotate(
        geom="text", x=Inf, y=0.90,
        label = paste('Reference centroid: ', round(exp.centroid.ref(),5),' m/z\nAccuracy: ', round(centroid.ac(), 0), ' ppm', sep = ""),
        hjust = 1,
        color="#6BC392", size=5, fontface = 2
      ) +
      xlab("m/z") +
      ylab('normalized abundance') +
      theme(strip.text.y = element_blank(),
            strip.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_line(colour = "grey50"),
            panel.grid.minor = element_line(colour = "grey"),
            panel.background = element_blank(),
            plot.background = element_blank(),
            axis.line.x = element_line(colour = "black", size = 0.75),
            axis.line.y = element_line(colour = "black", size = 0.75),
            axis.ticks.length=unit(0.1, "in"),
            axis.ticks.x = element_line(size = 0.75, colour = "black"),
            axis.ticks.y = element_line(size = 0.75, colour = "black"),
            axis.text.y = element_text(size = 16, color = "black", angle = 0),
            axis.text.x = element_text(size = 16, color = "black", angle = 0),
            axis.title.x = element_text(size=18,face="bold", color = 'black'),
            axis.title.y = element_text(size=18,face="bold", color = 'black'),
            plot.margin = margin(25, 0.5, 0.5, 0.5),
            legend.position="right",
            legend.box = "vertical",
            legend.title = element_text(size=18, face="bold", color = "black"),
            legend.key = element_rect(fill = "white"),
            legend.text = element_text(size=16, face="bold", color = "black"),
      )
  })

  output$plot98 <- renderPlot({
    plot98()
  })

  output$ref.accu.pdf <- downloadHandler(
    filename = function() { paste("stacked spectra", '.pdf', sep='') },
    content = function(file) {
      ggsave(file, plot = plot98(), device = "pdf",
             width = 200,
             height = 100,
             units = 'mm',
             dpi = 600)
    }
  )

  output$ref.accu.png <- downloadHandler(
    filename = function() { paste("stacked spectra", '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plot98(), device = "png",
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
    1000000 * abs(exp.centroid.ref()-Avemz())/Avemz()
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

  inputms <- reactive({
    #Progress bar. Appears upon file import, then again if m/z range is changed.
    withProgress(message = 'Calculation in progress',
                 detail = 'Please wait', value = 0, {

                   incProgress(amount=1/5)

                   #extraction of ms data, binding of scan number, retention time
                   df.temp <- lapply(id(),function(i) {
                     init <- data.frame(peaks(ms(), i))  %>%
                       add_column(scan = i) %>%
                       add_column(ret.time = ret.time()[i,]/60)
                   })

                   incProgress(amount=2/5)

                   #transformation to single dataframe
                   filling.df <- data.table::rbindlist(df.temp)

                   incProgress(amount=3/5)

                   #traceability
                   filling.df$file <- inFile()$name
                   # filling.df$sample <- input$sample.id

                   #binning
                   filling.df$X1 <- RoundTo(filling.df$X1, multiple = input$slider1, FUN = round)

                   incProgress(amount=4/5)

                   #naming
                   colnames(filling.df)[1:5] <- c("mz","intensity","scan", "time",'filename'#, 'sample'
                   )

                   #m/z range filtering
                   filling.df <- filling.df %>%
                     filter(mz > input$text11[1]) %>%  #m/z filtering
                     filter(mz < input$text11[2])

                   return(filling.df)

                   incProgress(amount=5/5)
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
      theme(strip.text.y = element_blank(),
            strip.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_line(colour = "grey50"),
            panel.grid.minor = element_line(colour = "grey"),
            panel.background = element_blank(),
            plot.background = element_blank(),
            axis.line.x = element_line(colour = "black", size = 0.75),
            axis.ticks.length=unit(0.1, "in"),
            axis.ticks.x = element_line(size = 0.75, colour = "black"),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 16, color = "black", angle = 0),
            axis.title.x = element_text(size=18,face="bold", color = 'black'),
            axis.title.y = element_blank(),
            plot.margin = margin(25, 0.5, 0.5, 0.5),
            legend.position="right",
            legend.box = "vertical",
            legend.title = element_text(size=18, face="bold", color = "black"),
            legend.key = element_rect(fill = "white"),
            legend.text = element_text(size=16, face="bold", color = "black"))
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
    inputms() %>%
      filter(scan %in% scansbrsh())
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
    selecscansbrsh() %>%
      # filter(mz > min(ranges$x)) %>%
      # filter(mz < max(ranges$x)) %>%
      group_by(mz, filename) %>%
      summarise("intensum" = sum(intensity)) %>%
      add_column("mean.time" = mean(selecscansbrsh()$time) + as.numeric(input$deadtxt)) %>%
      add_column("Species" = input$sample.id) %>%
      add_column('CFtime' = time.min()) %>%
      add_column('lgd.conc' = as.numeric(input$lgd.conc)) %>%
      add_column('Stoich' = as.numeric(input$Stoich))
  })

  #Definition of initial mz range-------
  ranges <- reactiveValues(x = mzlimits, y = NULL)   #place above to save on calculation time

  #Zoomed MS spectrum from brush----------------
  output$plot3 <- renderPlot({

    req(input$file1)

    ggplot(data = specsumbrsh(), aes(x = mz, y = intensum)) +
      geom_line(color = input$col.MS, size = input$size.line.MS) +
      xlab("m/z") +
      theme(strip.text.y = element_blank(),
            strip.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_line(colour = "grey"),
            panel.grid.minor = element_line(colour = "grey50"),
            panel.background = element_blank(),
            axis.line.x = element_line(colour = "black", size = 0.75),
            axis.ticks.length=unit(0.1, "in"),
            axis.ticks.x = element_line(size = 0.75),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(colour="black", size = 16, color = "black", angle = 0),
            axis.title.x = element_text(size=18,face="bold"),
            axis.title.y = element_blank(),
            plot.margin = margin(25, 0.5, 0.5, 0.5),
            legend.position="right",
            legend.box = "vertical",
            legend.title = element_text(size=18, face="bold"),
            legend.key = element_rect(fill = "white"),
            legend.text = element_text(size=16, face="bold")) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
  })


  #Zoomed MS spectrum from txt input----------
  output$plot4 <- renderPlot({

    req(input$file1)

    ggplot(data = specsumtxt(), aes(x = mz, y = intensum)) +
      geom_line(color = input$col.MS, size = input$size.line.MS) +
      xlab("m/z") +
      theme(strip.text.y = element_blank(),
            strip.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_line(colour = "grey"),
            panel.grid.minor = element_line(colour = "grey50"),
            panel.background = element_blank(),
            axis.line.x = element_line(colour = "black", size = 0.75),
            axis.ticks.length=unit(0.1, "in"),
            axis.ticks.x = element_line(size = 0.75),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(colour="black", size = 16, color = "black", angle = 0),
            axis.title.x = element_text(size=18,face="bold"),
            axis.title.y = element_blank(),
            plot.margin = margin(25, 0.5, 0.5, 0.5),
            legend.position="right",
            legend.box = "vertical",
            legend.title = element_text(size=18, face="bold"),
            legend.key = element_rect(fill = "white"),
            legend.text = element_text(size=16, face="bold")) +
      coord_cartesian(xlim = c(min(mztxt()), max(mztxt())), expand = FALSE)
  })

  #zoom event on MS plot---------
  observeEvent(input$plot3_brush, {
    brush <- input$plot3_brush
    ranges$x <- c(brush$xmin, brush$xmax)
    ranges$y <- c(brush$ymin, brush$ymax)
  })

  specsumbrsh.ms <- reactive({
    specsumbrsh.ms <- specsumbrsh() %>%
      filter(mz > min(ranges$x)) %>%
      filter(mz < max(ranges$x))
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
      inputsnap <- specsumbrsh.ms() %>%
        add_column(min.time = min(selectedData()$time), #traceability
                   max.time = max(selectedData()$time),
                   min.scan = min(selectedData()$scan),
                   max.scan = max(selectedData()$scan),
                   min.mz = min(specsumbrsh.ms()$mz),
                   max.mz = max(specsumbrsh.ms()$mz))
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
      filter(mean.time >= as.numeric(input$text33)) %>%
      filter(mean.time <= as.numeric(input$text34))

    return(processed.kin)

  })


  snaps42 <- data.frame()

  kin.brsh <- reactive({
    selecscansbrsh()
    # group_by(mz, filename) %>%
    # add_column("Species" = input$sample.id)
  })

  k.data <- eventReactive(input$bttn42, {

    k.init <- data.frame(kin.brsh()) %>%
      filter(mz > min(ranges$x)) %>%
      filter(mz < max(ranges$x)) %>%
      add_column("Species" = input$sample.id) %>%
      add_column(mz.range = paste0(round(min(ranges$x),2),"-",round(max(ranges$x), 2))) %>%
      group_by(filename, Species, scan, time, mz.range) %>%
      summarise(intensity = sum(intensity))

    newrow42 <-  data.frame(k.init)

    snaps42 <<- rbind(snaps42, newrow42)
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
      dplyr::select(-Species.y, -filename.y, -time.y, -mz.range.y) %>%
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
      k.norm.0()$mean.corr
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
                           'm/z range' = 'mz.range.x'),
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

  output$k.plot <- renderPlot({
    ggplot(data = k.norm.0(), aes(x = mean.time, y = kin.input(),
                                  color = name)) +
      geom_point(size = input$size.dot.kin, alpha = input$transp.kin) +
      scale_color_manual(name = "Species",
                         values = c(input$col.dot.kin1, input$col.dot.kin2, input$col.dot.kin3, input$col.dot.kin4, input$col.dot.kin5, input$col.dot.kin6, input$col.dot.kin7, input$col.dot.kin8)) +
      xlab("time (min)") +
      ylab("intensity") +
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
                                  'rel.intensity')

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
        mutate(rel.intensity = as.numeric(rel.intensity))
      # the relative intensity does not take into account the SI intensity


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
                  'End m/z'= 'max.mz'
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
        formatRound(c('Raw intensity', 'Relative intensity'), digits = 2)
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
    I.inv

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
    data.table(corr.C())
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


  #snaps plotting---------

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
  }
  )

  output$plot5.ui <- renderUI({
    plotOutput("plot5",
               width = as.numeric(input$plot5.w),
               height = as.numeric(input$plot5.h)
    )
  })


  #download spectra----------

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
      theme(strip.text.y = time.label(),
            strip.text.x = element_text(size = 16, color = "black", face = "bold"),
            strip.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line.x = element_line(colour = "black", size = 0.75),
            axis.ticks.length=unit(0.1, "in"),
            axis.ticks.x = element_line(size = 0.75),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(colour="black", size = 16, color = "black", angle = 0),
            axis.title.x = element_text(size=18,face="bold"),
            axis.title.y = element_blank(),
            plot.margin = margin(25, 0.5, 0.5, 0.5),
            legend.position = color.guide(),
            legend.box = "vertical",
            legend.title = element_text(size=18, face="bold"),
            legend.key = element_rect(fill = "white"),
            legend.text = element_text(size=16, face="bold"))  +
      coord_cartesian(expand = FALSE)
  })

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

  #centroids and NUS--------

  #centroid calculation
  centroids <- reactive({

    req(MSsnaps)

    MSsnaps() %>%
      # group_by(mean.time, Species) %>%
      group_by(mean.time, Species, CFtime, filename, min.time, max.time, min.scan, max.scan, min.mz, max.mz) %>%
      summarise(centroid = weighted.mean(mz, intensum)) #calculation of centroids
  })

  #hot table
  NUS.init0 <- reactive({

    req(centroids())

    NUS.init0 <- data.frame(Species = unique(centroids()$Species),
                            Name = unique(centroids()$Species),
                            Reference = rep(1500, length(unique(centroids()$Species))),
                            Charge = rep(4, length(unique(centroids()$Species))),
                            D.initial = rep(90, length(unique(centroids()$Species))),
                            D.final = rep(9, length(unique(centroids()$Species)))
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

  #NUS calculation
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

  #time scaling
  centroidscaled.init <- reactive({
    if (isTRUE(input$manu2)) {
      NUS() %>%
        add_column(timescale = NUS()$CFtime)
    } else {
      NUS() %>%
        add_column(timescale = NUS()$mean.time)
    }
  })

  #upload already processed data
  file.old <- reactive({
    if(is.null(input$file.old))
      return(NULL)

    input$file.old
  })

  processed.data <- reactive({

    if(is.null(input$file.old)) {
      return(NULL)
    } else {
      processed.data <- data.frame(read_excel(file.old()$datapath,
                                              skip = 1))

      colnames(processed.data) <- c('Species', 'Name', 'mean.time', 'mean.time.s', 'CFtime', 'CFtime.s', 'centroid', 'NUS',
                                    'Reference', 'Charge', 'filename', 'min.time', 'max.time', 'min.scan',
                                    'max.scan', 'min.mz', 'max.mz',	"timescale")

      return(processed.data)
    }
  })

  centroidscaled.import <- reactive({
    if (is.null(file.old)) {
      return(centroidscaled.init())
    } else {
      if (is.null(input$file1)) {
        return(processed.data())
      } else{
        processed.data() %>%
          rbind(centroidscaled.init())
      }
    }
  })

  centroidscaled <- reactive({
    if (isTRUE(input$manu2)) {
      centroidscaled.import() %>%
        mutate(timescale = centroidscaled.import()$CFtime)
    } else {
      centroidscaled.import() %>%
        mutate(timescale = centroidscaled.import()$mean.time)
    }
  })

  output$centroids <- DT::renderDT(server = FALSE, {
    datatable(data = centroidscaled(),  #data = NUS(),
              extensions = c('Buttons', 'Responsive', 'Scroller'),
              selection = 'multiple',
              colnames = c('TIC Time (min)' = 'mean.time',
                           'TIC Time (s)' = 'mean.time.s',
                           'Centroid' = 'centroid',
                           "Manual time (min)" = 'CFtime',
                           'Manual time (s)' = 'CFtime.s',
                           'File name' = 'filename',
                           'Start TIC time' = 'min.time',
                           'End TIC time' = 'max.time',
                           'Start scan' = 'min.scan',
                           'End scan' = 'max.scan',
                           'Start m/z' = 'min.mz',
                           'End m/z'= 'max.mz',
                           'Species number' = 'Species',
                           'Species name' = 'Name'),
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
                columnDefs = list(list(visible=FALSE, targets=c(11, 12, 13, 14, 15, 16, 17)))
              )
    ) %>%
      formatRound(c('TIC Time (min)', 'Centroid', 'NUS', "TIC Time (s)"), digits = 2)
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
      theme(strip.text.y = element_blank(),
            strip.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_line(colour = "black"),
            panel.grid.minor = element_line(colour = "grey"),
            panel.background = element_blank(),
            axis.line.x = element_line(colour = "black", size = 0.75),
            axis.line.y = element_line(colour = "black", size = 0.75),
            axis.ticks.length=unit(0.1, "in"),
            axis.ticks.x = element_line(size = 0.75),
            axis.ticks.y = element_line(size = 0.75),
            axis.text.y = element_text(colour="black", size = 16, color = "black", angle = 0),
            axis.text.x = element_text(colour="black", size = 16, color = "black", angle = 0),
            axis.title.x = element_text(size=18,face="bold"),
            axis.title.y = element_text(size=18,face="bold"),
            # plot.margin = margin(25, 0.5, 0.5, 0.5),
            legend.position="right",
            legend.box = "vertical",
            legend.title = element_blank(),
            legend.key = element_rect(fill = "white"),
            legend.text = element_text(size=16, face="bold")) +
      coord_cartesian(expand = T)
  })

  #HDX fit initialization----
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

  #HDX fit plot----
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
      theme(strip.text.y = element_blank(),
            strip.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line.x = element_line(colour = "black", size = 0.75),
            axis.line.y = element_line(colour = "black", size = 0.75),
            axis.ticks.length=unit(0.1, "in"),
            axis.ticks.x = element_line(size = 0.75),
            axis.ticks.y = element_line(size = 0.75),
            axis.text.y = element_text(colour="black", size = 16, color = "black", angle = 0),
            axis.text.x = element_text(colour="black", size = 16, color = "black", angle = 0),
            axis.title.x = element_text(size=18,face="bold"),
            axis.title.y = element_text(size=18,face="bold"),
            # plot.margin = margin(25, 0.5, 0.5, 0.5),
            legend.position="right",
            legend.box = "vertical",
            legend.title = element_blank(),
            legend.key = element_rect(fill = "white"),
            legend.text = element_text(size=16, face="bold")) +
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

  #Download kinetics plots-----------
  Plot6 <- reactive({
    ggplot(data = centroids(), aes(x = centroids()$mean.time, y = centroids()$centroid)) +
      geom_point(color = "steelblue", size = 3)
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


  #MELTR-------------------

  #melting data import-----

  #Selection of raw data path
  Data_file <- reactive({
    input$melting.input
  })

  #allows to toggle the blank subtraction on/off
  blk.subtract <- reactive({
    if (input$melt.blank == T) {
      blk.subtract = 1
    } else {
      blk.subtract = 0
    }
  })

  #import raw data
  melt <- reactive({

    melt.buffer <- read_excel(Data_file()$datapath,
                              col_types = c("numeric", "numeric",'numeric', "text", "text", "text",
                                            "text"),
                              col_names = c("T.unk", "abs.raw", 'abs.blk', "ramp", "comment",
                                            "rep", "oligo"),
                              skip = 1) %>%
      filter(!is.na(oligo)) %>% #removes empty lines
      group_by(oligo, comment, rep) %>%
      mutate(ramp = if_else(
        is.na(ramp), if_else( #if no ramp is provided, determine it by looking at next temperature value
          lead(T.unk) > T.unk, 'heating', 'cooling'), ramp)) %>%
      #no ramp found for last row of each spl (because there's no next T value)
      mutate(ramp = if_else(is.na(ramp), lag(ramp), ramp)) %>%
      ungroup() %>% #necessary to use data at derivative step (not sure why)
      mutate(id = paste(oligo, comment, ramp, rep, sep = '-'))%>% #create an experiment id
      # Detects whether the raw data is supplied in Celsius or Kelvin and converts to Kelvin if necessary
      mutate(T.K = if_else(abs.raw < 100, T.unk + 273.15, T.unk)) %>%
      add_column(blk.sub = blk.subtract()) %>%
      group_by(id) %>%
      #subtract the blank column is values are provided and toggle activated
      mutate(abs.melt = if_else(is.na(abs.blk), abs.raw,
                                if_else(blk.sub == 1, abs.raw - abs.blk, abs.raw))) %>%
      ungroup()

    return(melt.buffer)
  })


  #melting data selection-----

  output$select.melting.oligo <- renderUI({
    if(is.null(input$melting.input)) {
      pickerInput("select.melting.oligo",
                  label = "Choose oligos",
                  choices = "upload data first",
                  multiple = T
      )
    } else {
      pickerInput("select.melting.oligo",
                  label = "Choose oligos",
                  choices = unique(melt()$oligo),
                  selected = unique(melt()$oligo),
                  multiple = T,
                  options = pickerOptions(
                    actionsBox = T,
                    liveSearch = T
                  )
      )
    }
  })

  output$select.melting.ramp <- renderUI({
    if(is.null(input$melting.input)) {
      pickerInput("select.melting.oligo",
                  label = "Choose ramps",
                  choices = "upload data first",
                  multiple = T
      )
    } else {
      pickerInput("select.melting.ramp",
                  label = "Choose ramps",
                  choices = unique(melt()$ramp),
                  selected = unique(melt()$ramp),
                  multiple = T,
                  options = pickerOptions(
                    actionsBox = T,
                    liveSearch = T
                  )
      )
    }
  })

  output$select.melting.comment <- renderUI({
    if(is.null(input$melting.input)) {
      pickerInput("select.melting.oligo",
                  label = "Choose comments",
                  choices = "upload data first",
                  multiple = T
      )
    } else {
      pickerInput("select.melting.comment",
                  label = "Choose comments",
                  choices = unique(melt()$comment),
                  selected = unique(melt()$comment),
                  multiple = T,
                  options = pickerOptions(
                    actionsBox = T,
                    liveSearch = T
                  )
      )
    }
  })

  output$select.melting.rep <- renderUI({
    if(is.null(input$melting.input)) {
      pickerInput("select.melting.oligo",
                  label = "Choose replicates",
                  choices = "upload data first",
                  multiple = T
      )
    } else {
      pickerInput("select.melting.rep",
                  label = "Choose replicates",
                  choices = unique(melt()$rep),
                  selected = unique(melt()$rep),
                  multiple = T,
                  options = pickerOptions(
                    actionsBox = T,
                    liveSearch = T
                  )
      )
    }
  })

  output$select.melting.id <- renderUI({
    if(is.null(input$melting.input)) {
      pickerInput("select.melting.oligo",
                  label = "Choose id",
                  choices = "upload data first",
                  multiple = T
      )
    } else {
      pickerInput("select.melting.id",
                  label = "Choose id",
                  choices = unique(melt()$id),
                  selected = unique(melt()$id),
                  multiple = T,
                  options = pickerOptions(
                    actionsBox = T,
                    liveSearch = T
                  )
      )
    }
  })

  #melting data display----

  melt.filtered <- reactive({

    if(is.null(input$melting.input)) {
      return(NULL)
    } else {

      melt.filtered.buffer <-  melt() %>% #input data filtering
        filter(oligo %in% input$select.melting.oligo) %>%
        filter(ramp %in% input$select.melting.ramp) %>%
        filter(comment %in% input$select.melting.comment) %>%
        filter(rep %in% input$select.melting.rep) %>%
        filter(id %in% input$select.melting.id) %>%
        filter(T.K > min(input$slider.therm), T.K < max(input$slider.therm))

      if(input$melt.merge.replicates == T){
        melt.filtered.buffer <- melt.filtered.buffer %>%
          mutate(rounded.T.K = RoundTo(T.K, multiple = input$slider.melt.rounder, FUN = round)) %>%
          group_by(oligo, ramp, comment, rounded.T.K) %>%
          mutate(abs.melt = mean(abs.melt), T.K = mean(T.K)) %>%
          mutate(id = paste(oligo, comment, ramp, sep="-"))
      }
      return(melt.filtered.buffer)
    }

  })

  output$melt.filtered <- DT::renderDT({
    melt.filtered()
  })

  output$p.melt.filtered <- renderPlot({
    if(is.null(input$melting.input)) {return(NULL)}
    else {
      p45 <- ggplot(data = melt.filtered(),
                    aes(x = T.K, y = abs.melt, color = id, shape = ramp)) +
        geom_point(size = input$size.dot.melt, alpha = input$alpha.dot.melt) +
        scale_color_d3() +
        theme_pander() +
        xlab("Temperature (K)") +
        ylab("Absorbance")

      # p45 <- palette.modifier(plot = p45)

      return(p45)
    }
  })

  #Derivatives--------

  melt.derivative <- eventReactive(input$bttn.deriv.melt,{

    melt.derivative.calc <- data.frame() #initialize data frame for loop result collection

    for (i in unique(melt.filtered()$id)) {

      #extract data per id
      buffer.melt <- melt.filtered() %>%
        filter(i == melt.filtered()$id)

      #calculates differences
      diffy <- diff(buffer.melt$abs.melt)
      diffx <- diff(buffer.melt$T.K)

      melt.derivative.calc.buffer <- cbind(diffy, diffx) %>%
        as.data.frame() %>%
        mutate(diffyx = diffy/diffx) %>% #calculates derivative
        add_column(id = i) %>% #adds id
        #adds temperatures (and removes first row to match number of rows from differences)
        add_column(T.K = buffer.melt$T.K[2:length(buffer.melt$T.K)],
                   ramp = buffer.melt$ramp[2:length(buffer.melt$ramp)])

      #result collection
      melt.derivative.calc <- base::rbind(melt.derivative.calc.buffer, melt.derivative.calc)

    }

    #switches UI tab automatically to derivative when calculating it
    observeEvent(input$bttn.deriv.melt, {
      updateTabsetPanel(session = session,
                        inputId = "tabbox.1",
                        selected = 'Derivative plot'
      )
    })

    #Smoothing and removal of extrema
    melt.derivative <- melt.derivative.calc %>%
      group_by(id) %>%
      mutate(rM = abs(rollmean(diffyx, input$melt.deriv.smooth.width, fill = NA, align="right"))) %>% #rolling average
      slice((input$melt.deriv.smooth.width+1):(length(rM)-(input$melt.deriv.smooth.width+1))) #removes extrema

    return(melt.derivative)

  })

  #plot derivatives
  output$p.melt.derivative <- renderPlot({

    p46 <- ggplot(melt.derivative(), aes(T.K, rM, color = id, shape = ramp)) +
      geom_point(size = input$size.dot.melt, alpha = input$alpha.dot.melt) +
      theme_pander() +
      # scale_color_d3(palette = "category20") +
      xlab("Temperature (K)") +
      ylab("DA/DT")

    # p46 <- palette.modifier(plot = p46)

    return(p46)

  })

  Tm.init.deriv <- reactive({
    melt.derivative() %>%
      group_by(id) %>%
      filter(rM == max(rM)) %>%
      select(id, T.K)
  })

  output$melt.derivative <- DT::renderDT({
    Tm.init.deriv()
  })

  tm.init0 <- eventReactive(input$bttn.init.melt, {
    tm.init0 <- Tm.init.deriv() %>%
      rename("Tm.init" = "T.K") %>%
      add_column(P1.init = 1.3e+05,
                 P3.init = 1,
                 P4.init = 0.3,
                 P5.init = 0,
                 P6.init = -0.2)

    tm.init0$legend = tm.init0$id

    return(tm.init0)

  })


  tm.init.change <- reactive({
    as.data.frame(hot.to.df(input$hotable1))
  })

  output$hotable1 <- renderHotable({tm.init0() }, readOnly = F)

  #switches UI tab automatically to hottable when initializing it
  observeEvent(input$bttn.init.melt, {
    updateTabsetPanel(session = session,
                      inputId = "tabbox.2",
                      selected = 'Fit initialization'
    )
  })

  #switches UI tab automatically to hottable when initializing it
  observeEvent(input$bttn.fit.melt, {
    updateTabsetPanel(session = session,
                      inputId = "tabbox.2",
                      selected = 'Fit result'
    )
  })


  #Fitting----------------------------

  nlfit.melt <- eventReactive(input$bttn.fit.melt, {

    #initialize the data.frame to collect results
    fit.melt.results <- data.frame()

    #loops across all unique selected ids
    for (i in unique(melt.filtered()$id)) {

      #initialize Parameters
      fit.melt.init.par <- subset(tm.init.change(), id == i)

      P1s <- as.vector(fit.melt.init.par$P1.init)
      P2s <- as.vector(fit.melt.init.par$Tm.init)
      P3s <- as.vector(fit.melt.init.par$P3.init)
      P4s <- as.vector(fit.melt.init.par$P4.init)
      P5s <- as.vector(fit.melt.init.par$P5.init)
      P6s <- as.vector(fit.melt.init.par$P6.init)

      #buffers the data to fit
      fit.melt.input.buffer <- data.frame(melt.filtered()) %>%
        filter(id == i)

      #fit
      ms <- nls(
        data=fit.melt.input.buffer,
        fit.melt.input.buffer$abs.melt~(P3+P4*fit.melt.input.buffer$T.K)*1/(1+exp(-P1*(1-fit.melt.input.buffer$T.K/P2)/(8.31451*fit.melt.input.buffer$T.K)))+
          (P5+P6*fit.melt.input.buffer$T.K)*exp(-P1*(1-fit.melt.input.buffer$T.K/P2)/(8.31451*fit.melt.input.buffer$T.K))
        /(1+exp(-P1*(1-fit.melt.input.buffer$T.K/P2)/(8.31451*fit.melt.input.buffer$T.K))),
        start = list(P1 = P1s, P2 = P2s, P3=P3s, P4=P4s, P5=P5s, P6=P6s),
        nls.control(maxiter = input$nb.it.melt.fit,
                    warnOnly = T)
      )

      #buffers the fit results
      fit.melt.output.buffer <- data.frame(id = i,
                                           nb.data.pt = nobs(ms),
                                           init.Tm =  P2s,
                                           RSS = sum(residuals(ms)^2),
                                           SE.residual = sigma(ms),
                                           P1 = as.vector(coef(ms))[1],
                                           P1SD = summary(ms)$coefficient[1,2],
                                           P2 = as.vector(coef(ms))[2],
                                           P2SD = summary(ms)$coefficient[2,2],
                                           P3 = as.vector(coef(ms))[3],
                                           P3SD = summary(ms)$coefficient[3,2],
                                           P4 = as.vector(coef(ms))[4],
                                           P4SD = summary(ms)$coefficient[4,2],
                                           P5 = as.vector(coef(ms))[5],
                                           P5SD = summary(ms)$coefficient[5,2],
                                           P6 = as.vector(coef(ms))[6],
                                           P6SD = summary(ms)$coefficient[6,2],
                                           fit.Tm.K = round(as.vector(coef(ms))[2], 2),
                                           fit.Tm.C = round(as.vector(coef(ms))[2] - 273.15, 2),
                                           DeltaH = -as.vector(coef(ms))[1],
                                           DeltaS = -as.vector(coef(ms))[1]/as.vector(coef(ms))[2]
                                           # DeltaG = as.vector(coef(ms))[1] - input$slider.therm * as.vector(coef(ms))[1]/as.vector(coef(ms))[2]
                                           # DeltaG = -8.314 * input$slider.therm * log(as.vector(coef(ms))[1] * (1 - input$slider.therm/as.vector(coef(ms))[2])/8.314 * input$slider.therm)
      )


      #row bind the results acroos the loop
      fit.melt.results <- rbind(fit.melt.results, fit.melt.output.buffer)
    }

    return(fit.melt.results)
  })

  #fit results table output
  output$nlfit.melt.results <- DT::renderDT({
    datatable(
      nlfit.melt(),
      extensions = c('Buttons', 'Responsive', 'Scroller'),
      colnames = c("Data points" = "nb.data.pt",
                   'Initial Tm' = 'init.Tm',
                   "P1 sd" = 'P1SD',
                   "P2 sd" = 'P2SD',
                   "P3 sd" = 'P3SD',
                   "P4 sd" = 'P4SD',
                   "P5 sd" = 'P5SD',
                   "P6 sd" = 'P6SD',
                   "RMSE" = "SE.residual"),
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
        buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
        columnDefs = list(list(visible=FALSE, targets=c(3,6, 8, 10, 12, 14, 16, 17, 18, 19, 20)))
      )
    ) %>%
      formatRound(c("P4", 'P4 sd', "P6", 'P6 sd', 'RMSE'), digits = 5) %>%
      formatRound(c('P1', 'P1 sd'), digits = 0) %>%
      formatRound(c('P2', 'P2 sd'), digits = 2) %>%
      formatRound(c('P3', 'P3 sd', 'P5', 'P5 sd'), digits = 3) %>%
      formatRound(c('RSS'), digits = 6)
  })


  fit.melt.result.df <- reactive({
    if(is.null(input$melting.input)) {return(NULL)}
    else {
      left_join(melt.filtered(),nlfit.melt(),
                by = c("id")) %>% #join fit result with raw data (only selected ids)
        mutate(folded.fraction = (1/(1+exp(-P1*(1-T.K/P2)/(8.31451*T.K))))) %>%  #folded fraction
        mutate(folded.fraction.base = (P5+P6*T.K-abs.melt)/(P5+P6*T.K - P3-P4*T.K)) %>% #baseline corrected folded fraction
        #fitted line
        mutate(raw.fit.y = (P3+P4*T.K)*1/(1+exp(-P1*(1-T.K/P2)/(8.31451*T.K)))+(P5+P6*T.K)*exp(-P1*(1-T.K/P2)/(8.31451*T.K))/(1+exp(-P1*(1-T.K/P2)/(8.31451*T.K)))) %>%
        mutate(low.T.baseline = P3+P4*T.K) %>%
        mutate(high.T.baseline = P5+P6*T.K) %>%
        filter(T.K > min(input$slider.therm), T.K < max(input$slider.therm))
    }
  })

  fit.melt.result.summary <- reactive({
    if(is.null(input$melting.input)) {return(NULL)}
    else {
      fit.melt.result.df() %>%
        select(id, oligo, ramp, comment, rep, fit.Tm.K, fit.Tm.C, P2SD, DeltaH, DeltaS) %>%
        distinct() %>%
        group_by(id) %>%
        mutate(DeltaG = DeltaH - input$temp.therm * DeltaS) %>%
        group_by(oligo, ramp, comment) %>%
        mutate(mean.Tm.K = mean(fit.Tm.K), mean.Tm.C = mean(fit.Tm.C),
               sd.Tm.K = SD(fit.Tm.K), sd.Tm.C = SD(fit.Tm.C))
    }
  })

  #outputs the fitted raw data
  p.raw.melt.fit <- reactive({
    if(is.null(input$melting.input)) {return(NULL)}
    else {
      p0 <- ggplot(fit.melt.result.df()) +
        geom_point(aes(T.K, abs.melt, color = id), size = input$size.dot.melt, alpha = input$alpha.dot.melt, shape = 16) + #plots the experimental data
        geom_line(aes(x = T.K, y = raw.fit.y, color = id),
                  size = input$size.line.melt, alpha = input$alpha.line.melt) +
        ylab(bquote(bold("modeled folded fraction"))) + #modifies axes titles
        xlab("Temperature (K)") +
        # scale_y_continuous(limits=c(-0.1,1.1), breaks = c(0, 0.25, 0.5, 0.75, 1.0)) +
        labs(color="id") +
        # scale_color_d3(palette = "category20") +
        theme(axis.text=element_text(size=12), axis.title=element_text(size=16,face="bold")) + #axis style
        theme(axis.text.x = element_text(color = "black", size = 14, angle = 0),
              axis.text.y = element_text(color = "black", size = 14, angle = 0)) + #axis labels style
        theme(legend.position="right",
              legend.box = "vertical",
              legend.title = element_text(size=14,
                                          face="bold"),
              legend.key = element_rect(fill = "white"),
              legend.text = element_text(size=12,
                                         face="bold")) +
        theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) + #adds margins (top, right, bottom, left)
        theme(
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 0.75)) +
        theme(axis.ticks.length=unit(0.1, "in")) + #Set tick length
        theme(axis.ticks = element_line(size = 0.75))  + #Set tick thickness +
        theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) +
        coord_cartesian(clip = "off")  #no clipping

      #toggles baselines on and off
      if (input$toggle.baseline == T) {
        p0 <-  p0 + geom_line(aes(x = T.K, y = low.T.baseline, color = id),
                              size = input$size.baseline.melt, alpha = input$alpha.baseline.melt, linetype = "dashed") +
          geom_line(aes(x = T.K, y = high.T.baseline, color = id),
                    size = input$size.baseline.melt, alpha = input$alpha.baseline.melt, linetype = "dashed")
      } else { p0 }

      p0 <- palette.modifier(plot = p0)

      return(p0)
    }
  })

  output$p.raw.melt.fit <- renderPlot({
    if(is.null(input$melting.input)) {return(NULL)}
    else {
      p.raw.melt.fit()
    }
  })

  #outputs a plot of the modeled folded fraction
  p.folded.modeled <- reactive({
    if(is.null(input$melting.input)) {return(NULL)}
    else {
      p44 <- ggplot(fit.melt.result.df()) +
        geom_point(aes(T.K, folded.fraction, color = id),
                   size = input$size.dot.melt-2, alpha = input$alpha.dot.melt,
                   shape = 16) + #plots the experimental data
        ylab(bquote(bold("folded fraction"))) + #modifies axes titles
        xlab("Temperature (K)") +
        # scale_y_continuous(limits=c(-0.1,1.1), breaks = c(0, 0.25, 0.5, 0.75, 1.0)) +
        labs(color="id") +
        # scale_color_d3(palette = "category20") +
        theme(axis.text=element_text(size=12), axis.title=element_text(size=16,face="bold")) + #axis style
        theme(axis.text.x = element_text(color = "black", size = 14, angle = 0),
              axis.text.y = element_text(color = "black", size = 14, angle = 0)) + #axis labels style
        theme(legend.position="right",
              legend.box = "vertical",
              legend.title = element_text(size=14,
                                          face="bold"),
              legend.key = element_rect(fill = "white"),
              legend.text = element_text(size=12,
                                         face="bold")) +
        theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) + #adds margins (top, right, bottom, left)
        theme(
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 0.75)) +
        theme(axis.ticks.length=unit(0.1, "in")) + #Set tick length
        theme(axis.ticks = element_line(size = 0.75))  + #Set tick thickness +
        theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) +
        coord_cartesian(clip = "off")  #no clipping

      p44 <- palette.modifier(plot = p44)

      return(p44)
    }
  })

  output$p.folded.modeled <- renderPlotly({
    if(is.null(input$melting.input)) {return(NULL)}
    else {
      p.folded.modeled()
    }
  })

  #plots the baseline subtracted data
  p.folded.melt.fit <- reactive({
    if(is.null(input$melting.input)) {return(NULL)}
    else {
      p43 <- ggplot(fit.melt.result.df()) +
        geom_point(aes(T.K, folded.fraction.base, color = id),
                   size = input$size.dot.melt, alpha = input$alpha.dot.melt,
                   shape = 16) + #plots the experimental data
        ylab(bquote(bold("folded fraction"))) + #modifies axes titles
        xlab("Temperature (K)") +
        # scale_y_continuous(limits=c(-0.1,1.1), breaks = c(0, 0.25, 0.5, 0.75, 1.0)) +
        labs(color="id") +
        # scale_color_d3(palette = "category20") +
        theme(axis.text=element_text(size=12), axis.title=element_text(size=16,face="bold")) + #axis style
        theme(axis.text.x = element_text(color = "black", size = 14, angle = 0),
              axis.text.y = element_text(color = "black", size = 14, angle = 0)) + #axis labels style
        theme(legend.position="right",
              legend.box = "vertical",
              legend.title = element_text(size=14,
                                          face="bold"),
              legend.key = element_rect(fill = "white"),
              legend.text = element_text(size=12,
                                         face="bold")) +
        theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) + #adds margins (top, right, bottom, left)
        theme(
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 0.75)) +
        theme(axis.ticks.length=unit(0.1, "in")) + #Set tick length
        theme(axis.ticks = element_line(size = 0.75))  + #Set tick thickness +
        theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) +
        coord_cartesian(clip = "off")  #no clipping

      p43 <- palette.modifier(plot = p43)

      return(p43)
    }
  })

  output$p.folded.melt.fit <- renderPlot({
    if(is.null(input$melting.input)) {return(NULL)}
    else {
      p.folded.melt.fit()
    }
  })

  #summary table output
  output$fit.melt.result.summary <- DT::renderDT({
    if(is.null(input$melting.input)) {return(NULL)}
    else {
      datatable(
        fit.melt.result.summary(),
        extensions = c('Buttons', 'Responsive', 'Scroller'),
        colnames = c("Tm (K)" = "fit.Tm.K",
                     "Tm (°C)" = "fit.Tm.C",
                     "SD Tm (fit)" = "P2SD",
                     "SD Tm (K)" = "sd.Tm.K",
                     "SD Tm (°C)" = "sd.Tm.C",
                     "Mean Tm (K)" = "mean.Tm.K",
                     "Mean Tm (°C)" = "mean.Tm.C"),
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
          buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
          columnDefs = list(list(visible=FALSE, targets=c(1, 2, 3, 4)))
        )
      ) %>%
        formatRound(c("Tm (K)", "Tm (°C)", "DeltaH", "DeltaS", "DeltaG",
                      "Mean Tm (K)", "Mean Tm (°C)", "SD Tm (K)", "SD Tm (°C)",
                      "SD Tm (fit)"),
                    digits = 2)
    }
  })

  #output boxplot of summary
  fit.melt.result.plot <- reactive({
    if(is.null(input$melting.input)) {return(NULL)}
    else {
      p47 <- ggplot(data = fit.melt.result.summary()) +
        geom_boxplot(aes(x = paste(oligo, comment, sep = "-"), y = fit.Tm.C),
                     color = "grey75") +
        geom_point(aes(x = paste(oligo, comment, sep = "-"), y = fit.Tm.C, color = ramp, shape = factor(rep)),
                   size = input$size.dot.melt, alpha = input$alpha.dot.melt) +
        theme_pander() +
        xlab("") +
        ylab("Melting temperature (°C)")

      p47 <- palette.modifier(plot = p47)

      p47 <- p47 + scale_color_discrete(labels = c("cooling", "heating"))

      return(p47)
    }
  })

  output$fit.melt.result.plot <- renderPlot({
    if(is.null(input$melting.input)) {return(NULL)}
    else {
      fit.melt.result.plot()
    }
  })

  #Download melt plots------

  output$dwn.melt.fit <- downloadHandler(
    filename = function() { paste("fit", '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = p.raw.melt.fit(), device = "png")
    }
  )

  output$dwn.melt.model <- downloadHandler(
    filename = function() { paste("model", '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = p.folded.modeled(), device = "png")
    }
  )

  output$dwn.melt.folded <- downloadHandler(
    filename = function() { paste("folded", '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = p.folded.melt.fit(), device = "png")
    }
  )

  output$dwn.melt.Tm <- downloadHandler(
    filename = function() { paste("Tm", '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = fit.melt.result.plot(), device = "png")
    }
  )



  #output options-----------
  outputOptions(output, "plot5", suspendWhenHidden = FALSE)
  outputOptions(output, "k.plot", suspendWhenHidden = FALSE)
  outputOptions(output, "eq.raw", suspendWhenHidden = FALSE)


}

# Run the application
shinyApp(ui = ui, server = server)







}

