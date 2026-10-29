options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB upload limit

# --- MN2mrg package: exports all translation, VPC, forest, and helper
# --- functions this app relies on. Launch from an installed package via
# --- MN2mrg::run_app(); the previous root-level UTILS/*.R source()
# --- chain has been retired.
library(MN2mrg)

# --- Shiny UI + framework packages (Depends-propagation does not auto-attach
# --- in an installed-package Shiny run, so these must still be library()'d).
library(shiny)
library(shinydashboard)
library(shinyAce)
library(shinyjs)
library(shinyFiles)
library(stringr)
library(purrr)
library(readr)
library(tidyverse)
library(DT)
library(mrgsolve)
library(vpc)
library(plotly)
library(patchwork)
library(fs)
library(callr)

# --- Monolix connectors (optional) ----------------------------------------
# lixoftConnectors ships inside a MonolixSuite installation rather than any
# package repository, so it is a Suggests. A hard library() here aborted the
# whole app on any machine without MonolixSuite, taking the NONMEM half of
# the app down with it even though that half never touches Monolix.
MONOLIX_AVAILABLE <- requireNamespace("lixoftConnectors", quietly = TRUE)

if (MONOLIX_AVAILABLE) {
  library(lixoftConnectors)
} else {
  message("lixoftConnectors not found: the Monolix tabs will report that it ",
          "is missing when used. The NONMEM tabs are unaffected.")

  # This app calls the connector API unqualified in ~40 places. Rather than
  # guard each call site, bind the names it actually uses to one stub that
  # reports the only thing the user can act on. Three reasons this beats
  # simply dropping the library() call:
  #   1. The error says what to install instead of "could not find function
  #      getData". The Monolix observers already run inside tryCatch(), so
  #      this surfaces in a modal or the tab's own status panel rather than
  #      killing the session.
  #   2. It fails loudly. Several helpers below catch connector errors and
  #      fall back to a default, so a stub that returned an empty result
  #      would look like a successful parse of an empty project.
  #   3. "getData" is a generic name. Without the attach, an unqualified
  #      getData() could silently resolve to a same-named export from some
  #      other attached package; a stub in this environment shadows any such
  #      match, so a missing MonolixSuite can never become a wrong answer.
  # Keep this list in step with the connector calls in this file --
  # test-shiny-monolix-optional.R fails if a called function is not stubbed.
  monolix_not_installed <- function(...) {
    stop("This step requires the lixoftConnectors package, which ships with ",
         "MonolixSuite and is not installed. The NONMEM tabs work without ",
         "it. See the following guidance for installing the lixsoft connectors. ",
         "https://monolixsuite.slp-software.com/r-functions/2024R1/package-lixoftconnectors", call. = FALSE)
  }

  initializeLixoftConnectors       <- monolix_not_installed
  loadProject                      <- monolix_not_installed
  getData                          <- monolix_not_installed
  getCovariateInformation          <- monolix_not_installed
  getStructuralModel               <- monolix_not_installed
  getContinuousObservationModel    <- monolix_not_installed
  getObservationInformation        <- monolix_not_installed
  getIndividualParameterModel      <- monolix_not_installed
  getEstimatedPopulationParameters <- monolix_not_installed
}

# --- VPC execution mode ---------------------------------------------------
# HPC dispatch is switched off for this release. VPC simulations run
# in-process on whichever host is serving this app; nothing is submitted to a
# scheduler, and the app never assumes one exists. The SGE/clustermq
# infrastructure below and in run_vpc_sim() is deliberately retained for the
# follow-on work that will let users declare their own cluster connection.
#
# Re-enabling is this constant plus dropping the shinyjs::disable() call that
# greys the radio choice out in the server.
VPC_HPC_ENABLED <- FALSE

# Resolve the execution mode server-side rather than trusting the radio
# button. Shiny inputs are client-supplied, so a greyed-out choice is a UI
# affordance, not a guarantee: a crafted input message must not be able to
# make the app reach for a scheduler that is not there.
vpc_parallel_mode <- function(input) {
  if (!VPC_HPC_ENABLED) return("local")
  match.arg(input$vpc_parallel_mode, c("local", "hpc"))
}

# clustermq is a Suggests now, so it is deliberately not library()'d: the
# default local path never needs it and the app must start without it. These
# options cost nothing to set (options() does not load the package) and are
# what the HPC path needs when it is turned back on.
#
# The template path has to be absolute. Two reasons the previous
# "CONFIG/sge.tmpl" could never resolve: shiny::runApp() sets the working
# directory to inst/shiny/, and .Rbuildignore keeps CONFIG/ out of the
# installed package entirely. When clustermq cannot find the template it
# retries as system.file(paste0(template, ".tmpl"), package = "clustermq",
# mustWork = TRUE), which aborts with the bare message "no file found" during
# pool setup, before a single job is submitted. That is undiagnosable from the
# UI and reads like a cluster fault. The real template ships at
# inst/extdata/sge.tmpl.
sge_template <- system.file("extdata", "sge.tmpl", package = "MN2mrg")
if (!nzchar(sge_template))
  stop("SGE job template not found in the installed MN2mrg package ",
       "(expected inst/extdata/sge.tmpl).")
options(
  clustermq.scheduler = "SGE",
  clustermq.template  = sge_template
)


# Name of the raw csv header column that is headerType "id" (per
# getData()$headerTypes) -- this is what a Monolix .mlxtran [FILTER] block's
# "ID==..." condition actually refers to (see parse_mlx_dataset_filters()'s
# id_col argument), which need not be a literal "ID" column in this dataset.
mlx_identifier_col <- function() {
  tryCatch({
    data_info <- getData()
    col <- data_info$header[data_info$headerTypes == "id"]
    if (length(col) == 1) col else "ID"
  }, error = function(e) "ID")
}


# UI definition
ui <- dashboardPage(
  skin = "black",
  # Browser tab title. Must be supplied, and must be a plain string.
  # dashboardPage() does `title <- title %OR% extractTitle(header)`, and
  # extractTitle() returns the header title *object* rather than its text. The
  # header title below is deliberately a two-span tag so the sidebar logo can
  # style the name and the subtitle separately, so leaving this unset handed
  # that tag to bootstrapPage(title = ), which renders it inside <title>.
  # <title> is a plain-text element, so the browser showed the serialised
  # markup -- `<span class="logo-content">...` -- as the tab label.
  # Kept in step with DESCRIPTION's Package field by
  # test-metadata-consistency.R.
  title = "MN2mrg",
  dashboardHeader(
    title = tags$span(
      class = "logo-content",
      tags$span("MN2mrg", class = "logo-title"),
      tags$span("Global PK/PD & Pharmacometrics", class = "logo-subtitle")
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Instructions", tabName = "instructions", icon = icon("book")),
      
      # Model Translation section
      menuItem("Model Translation", icon = icon("exchange-alt"), startExpanded = TRUE,
        menuSubItem("NONMEM to mrgsolve", tabName = "nonmem_translate", icon = icon("file-code")),
        menuSubItem("Monolix to mrgsolve", tabName = "monolix_translate", icon = icon("file-code"))
      ),
      
      # mrgsolve Simulation section
      menuItem("mrgsolve Simulation", icon = icon("chart-line"), startExpanded = TRUE,
        menuSubItem("Dosing Simulation", tabName = "dosing_sim", icon = icon("syringe")),
        menuSubItem("Sensitivity Analysis", tabName = "sensitivity", icon = icon("sliders-h")),
        menuSubItem("VPC Generation", tabName = "vpc", icon = icon("chart-area")),
        menuSubItem("Forest Plot", tabName = "forest_plot", icon = icon("tree"))
      ),
      menuItem("Feedback", tabName = "feedback", icon = icon("comment"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(
        rel  = "stylesheet",
        href = paste0(
          "https://fonts.googleapis.com/css2?",
          "family=DM+Sans:ital,wght@0,300;0,400;0,500;0,600;1,400",
          "&family=DM+Mono:wght@400;500",
          "&display=swap"
        )
      ),
      # Absolute path via system.file(): shiny::runApp() sets the working
      # directory to the app's own directory (inst/shiny/), so a relative
      # "www/custom.css" resolves to inst/shiny/www/custom.css and can never
      # find the stylesheet, which ships one level up at inst/www/.
      includeCSS(system.file("www", "custom.css", package = "MN2mrg"))
    ),
    shinyjs::useShinyjs(),
    tabItems(
      # Instructions tab
      tabItem(tabName = "instructions",
              fluidRow(
                box(width = 12, title = "Welcome to the MN2mrg, a powerful tool for PK/PD Model Translation & Simulation", status = "primary", solidHeader = TRUE,
                    h4("Overview"),
                    p("This application provides tools for translating pharmacokinetic/pharmacodynamic models from 
                       NONMEM and Monolix formats into mrgsolve, and performing simulations including dosing exploration, 
                       sensitivity analysis, Visual Predictive Checks (VPC), and forest plots."),
                    hr(),
                    
                    h4("Application Structure"),
                    p("The application is organized into two main sections:"),
                    tags$ol(
                      tags$li(tags$strong("Model Translation"), " - Convert models from different platforms to mrgsolve format"),
                      tags$li(tags$strong("mrgsolve Simulation"), " - Run simulations and generate diagnostic plots")
                    ),
                    hr(),
                    
                    h4("Model Translation"),
                    p("Translate population PK/PD models to mrgsolve format for simulation in R."),
                    
                    tags$div(style = "margin-left: 20px;",
                      h5(icon("file-code"), " NONMEM to mrgsolve"),
                      p("Translate NONMEM control stream files to mrgsolve format."),
                      tags$ul(
                        tags$li("Upload NONMEM control stream (.ctl, .mod) files"),
                        tags$li("Parse $PRED, $PK, $DES, and $ERROR blocks"),
                        tags$li("Extract parameter estimates from .ext files"),
                        tags$li("Generate equivalent mrgsolve model code")
                      ),
                      br(),
                      
                      h5(icon("file-code"), " Monolix to mrgsolve"),
                      p("Translate Monolix project files to mrgsolve format."),
                      tags$ul(
                        tags$li(tags$strong("Project:"), " Load Monolix .mlxtran project files"),
                        tags$li(tags$strong("Parameters:"), " Review population parameters, omega, beta coefficients, and correlations"),
                        tags$li(tags$strong("Final Model:"), " Preview and save the translated mrgsolve code (.cpp)")
                      )
                    ),
                    hr(),
                    
                    h4("mrgsolve Simulation"),
                    p("Perform various simulations and analyses using translated or existing mrgsolve models."),
                    
                    tags$div(style = "margin-left: 20px;",
                      h5(icon("syringe"), " Dosing Simulation"),
                      p("Run deterministic simulations to explore dosing regimens."),
                      tags$ul(
                        tags$li("Load saved or uploaded mrgsolve model files"),
                        tags$li("Configure single or multiple dose regimens"),
                        tags$li("Select output variables to visualize"),
                        tags$li("Customize plot appearance with axis labels and log scales")
                      ),
                      br(),
                      
                      h5(icon("sliders-h"), " Sensitivity Analysis"),
                      p("Explore how varying model parameters affects predictions."),
                      tags$ul(
                        tags$li("Select parameters from the model's $PARAM block"),
                        tags$li("Define parameter variation range (min/max multipliers)"),
                        tags$li("Visualize output changes across parameter values"),
                        tags$li("Download sensitivity analysis results")
                      ),
                      br(),
                      
                      h5(icon("chart-area"), " VPC Generation"),
                      p("Create Visual Predictive Checks to evaluate model performance."),
                      tags$ul(
                        tags$li("Map dataset columns to standard format (ID, TIME, DV, AMT, etc.)"),
                        tags$li("Run stochastic simulations with customizable replicates"),
                        tags$li("Configure binning methods and confidence intervals"),
                        tags$li("Stratify by covariates and handle censored (BLQ) data"),
                        tags$li("Download publication-ready VPC plots"),
                        tags$li(tags$strong("Reproducibility:"), " download the YAML config and R script used to generate the VPC — re-run or modify outside the app at any time")
                      ),
                      br(),
                      
                      h5(icon("tree"), " Forest Plot"),
                      p("Generate forest plots for covariate effect visualization on a saved mrgsolve model."),
                      tags$ul(
                        tags$li("Visualize covariate effects on exposure/PK parameters (AUC, Cmax, Cmin, Cavg, or a custom parameter)"),
                        tags$li("Display median effect ratios with confidence intervals from covariance, bootstrap, or Bayes uncertainty"),
                        tags$li("Customize dosing regimen, shaded reference interval, and plot appearance"),
                        tags$li(tags$strong("Reproducibility:"), " download the plot, YAML config, data spec, and a full reproducibility bundle (forest.R + inputs) to re-run outside the app")
                      )
                    ),
                    hr(),
                    
                    h4("Getting Started"),
                    tags$ol(
                      tags$li("Expand ", tags$strong("Model Translation"), " in the sidebar"),
                      tags$li("Select ", tags$strong("Monolix to mrgsolve"), " or ", tags$strong("NONMEM to mrgsolve")),
                      tags$li("Follow the sub-tabs to load, configure, and save your translated model"),
                      tags$li("Expand ", tags$strong("mrgsolve Simulation"), " to run simulations and generate plots")
                    ),
                    br(),
                    p(tags$em("Tip: Click on the main menu items to expand sub-menus. The translated model is automatically available in the simulation tabs."))
                )
              )
      ),
      
      # ============ MODEL TRANSLATION SECTION ============

      # NONMEM to mrgsolve
      tabItem(tabName = "nonmem_translate",
        tabsetPanel(id = "nonmem_tabs", type = "tabs",

          # ---- Project tab ----
          tabPanel("Project", value = "nm_project",
            br(),
            fluidRow(
              box(width = 12, title = "NONMEM Project",
                fluidRow(
                  column(4,
                    shinyFiles::shinyFilesButton(
                      "nm_ctl_upload",
                      label    = "Upload Control Stream",
                      title    = "Select NONMEM control stream",
                      multiple = FALSE,
                      icon     = icon("upload"),
                      class    = "btn-primary"
                    ),
                    helpText("Accepted: .ctl, .mod, .inp")
                  ),
                  column(4,
                    shinyFiles::shinyFilesButton(
                      "nm_ext_upload",
                      label    = "Upload EXT File",
                      title    = "Select NONMEM EXT file",
                      multiple = FALSE,
                      icon     = icon("upload"),
                      class    = "btn-default"
                    ),
                    helpText("Required if not auto-detected")
                  ),
                  column(4,
                    actionButton(
                      "nm_load_project", "Load Project",
                      icon  = icon("play"),
                      class = "btn-success"
                    )
                  )
                ),
                br(),
                verbatimTextOutput("nm_project_info")
              )
            ),
            fluidRow(
              box(width = 12, title = "Conversion Notes",
                verbatimTextOutput("nm_project_notes")
              )
            )
          ),

          # ---- Parameters tab ----
          tabPanel("Parameters", value = "nm_parameters",
            br(),
            fluidRow(
              box(width = 6, title = "Fixed Effects (THETA)",
                DTOutput("nm_theta_table")
              ),
              box(width = 6, title = "Random Effects (OMEGA)",
                DTOutput("nm_omega_table")
              )
            ),
            fluidRow(
              box(width = 12, title = "Residual Error (SIGMA)",
                DTOutput("nm_sigma_table")
              )
            )
          ),

          # ---- Final Model tab ----
          tabPanel("Final Model", value = "nm_final_model",
            br(),
            fluidRow(
              box(width = 12, title = "Complete mrgsolve Model - Edit & Export",
                aceEditor("nm_model_editor", "", mode = "cpp",
                          theme = "chrome", height = "500px"),
                br(),
                fluidRow(
                  column(3,
                    actionButton("nm_compile_model", "Compile Model",
                                 icon = icon("cogs"), class = "btn-warning")
                  ),
                  column(3,
                    shinyFiles::shinySaveButton(
                      "nm_save_file_button", "Save File",
                      "Save mrgsolve model", class = "btn-success"
                    )
                  )
                ),
                br(),
                verbatimTextOutput("nm_save_status")
              )
            )
          )

        ,
        tabPanel("Verification", value = "nm_verification",
          br(),
          fluidRow(
            box(width = 12, title = "Model Verification",
              fluidRow(
                column(4,
                  shinyFilesButton("nm_tab_upload", "Table File (.tab/.txt)", "Select NONMEM Table File", multiple = FALSE),
                  br(), br(),
                  textInput("nm_censor_filter", "BQL exclusion condition (R syntax, e.g. to remove BQL records, type BQL==1 below)",
                            value = "", width = "100%"),
                  br(),
                  disabled(actionButton("nm_create_plot", "Run Verification", icon = icon("check-circle"), class = "btn-primary"))
                ),
                column(8,
                  plotOutput("nm_verify_plot", height = "500px")
                )
              ),
              br(),
              fluidRow(
                column(12, verbatimTextOutput("nm_verify_messages"))
              )
            )
          )
        )

        )  # end tabsetPanel
      ),  # end nonmem_translate tabItem


      tabItem(tabName = "monolix_translate",
              # Use tabsetPanel for sub-navigation within Monolix translation
              tabsetPanel(id = "monolix_tabs", type = "tabs",
                tabPanel("Project", value = "mlx_project",
                  br(),
                  fluidRow(
                    box(width = 12, title = "Monolix Project",
                        textInput("monolix_path", "MonolixSuite Path", 
                                  value = "/example/path/to/monolix/MonolixSuite2024R1"),
                        textInput("project_path", "Monolix Project Path (.mlxtran)", 
                                  value = ""),
                        actionButton("load_project", "Load Project", class = "btn-primary"),
                        br(), br(),
                        verbatimTextOutput("project_info")
                    )
                  )
                ),
                tabPanel("Parameters", value = "mlx_parameters",
                  br(),
                  fluidRow(
                    box(width = 6, title = "Population Parameters",
                        DTOutput("pop_params_table")
                    ),
                    box(width = 6, title = "Covariate Effect Parameters",
                        DTOutput("beta_params_table")
                    )
                  ),
                  fluidRow(
                    box(width = 6, title = "IIV Parameters",
                        DTOutput("omega_params_table")
                    ),
                    box(width = 6, title = "Residual Error Parameters",
                        DTOutput("sigma_params_table")
                    )
                  ),
                  fluidRow(
                    box(width = 6, title = "Individual Parameter Definitions",
                        verbatimTextOutput("param_def_lines")
                    ),
                    box(width = 6, title = "Residual Error Model Equations",
                        verbatimTextOutput("error_model_equations")
                    )
                  ),
                  fluidRow(
                    box(width = 6, title = "Continuous Covariates",
                        DTOutput("cont_covariates_table"),
                        helpText("Single-click on Value cells to edit. Values will be used in the Final Model.")
                    ),
                    box(width = 6, title = "Categorical Covariates",
                        DTOutput("cat_covariates_table"),
                        helpText("Single-click on Value cells to edit. Values will be used in the Final Model.")
                    )
                  ),
                  fluidRow(
                    box(width = 12, title = "Regressors",
                        DTOutput("regressors_table"),
                        helpText("Single-click on Value cells to edit. Values will be used in the Final Model.")
                    )
                  )
                ),
                tabPanel("Final Model", value = "mlx_final_model",
                  br(),
                  fluidRow(
                    box(width = 12, title = "Complete mrgsolve Model - Edit & Export",
                        aceEditor("complete_model_editor", "", mode = "cpp", theme = "chrome",
                                  height = "500px"),
                        br(),
                        fluidRow(
                          column(3,
                            actionButton("compile_mlx_model", "Compile Model",
                                         icon = icon("cogs"), class = "btn-warning")
                          ),
                          column(3,
                            shinyFiles::shinySaveButton("save_file_button", "Save File",
                                                       "Save model file", class = "btn-success")
                          )
                        ),
                        br(),
                        verbatimTextOutput("save_status")
                    )
                  )
                ),
        tabPanel("Verification", value = "mlx_verification",
          br(),
          fluidRow(
            box(width = 12, title = "Model Verification",
              fluidRow(
                column(4,
                  verbatimTextOutput("mlx_filter_status"),
                  br(),
                  disabled(actionButton("mlx_create_plot", "Run Verification", icon = icon("check-circle"), class = "btn-primary"))
                ),
                column(8,
                  plotOutput("mlx_verify_plot", height = "600px")
                )
              ),
              br(),
              fluidRow(
                column(12, verbatimTextOutput("mlx_verify_messages"))
              )
            )
          )
        )
              )  # End of tabsetPanel
      ),  # End of monolix_translate tabItem
      
      # ============ MRGSOLVE SIMULATION SECTION ============
      
      # Dosing Simulation tab
      tabItem(tabName = "dosing_sim",
              fluidRow(
                box(width = 12, title = "Model Setup",
                    fluidRow(
                      column(3,
                        fileInput("sim_model_file", "Upload Model File (.cpp)", 
                                 accept = c(".cpp", ".txt"))
                      ),
                      column(3,
                        actionButton("load_sim_model", "Load Model", class = "btn-primary"),
                        helpText("Or use saved model from translation")
                      ),
                      column(6,
                        verbatimTextOutput("sim_model_status")
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 6, title = "Dosing Parameters",
                    div(id = "dosing_events_container",
                      # Dosing Event 1 (always present)
                      div(id = "dose_event_1", class = "dosing-event-group",
                        tags$h5(tags$b("Dosing Event 1"), style = "margin-top: 0;"),
                        fluidRow(
                          column(4, numericInput("dose_time_1", "Dose Time:", value = 0, min = 0)),
                          column(4, numericInput("dose_amount_1", "Dose Amount:", value = 100, min = 0)),
                          column(4, selectInput("dose_cmt_1", "Compartment:", choices = c()))
                        ),
                        fluidRow(
                          column(4, numericInput("dose_addl_1", "Additional Doses:", value = 0, min = 0)),
                          column(4, numericInput("dose_ii_1", "Dosing Interval:", value = 24, min = 0)),
                          column(4)
                        ),
                        tags$hr()
                      )
                    ),
                    # Container for dynamically added dosing events
                    div(id = "extra_dosing_events"),
                    fluidRow(
                      column(6, actionButton("add_dose_event", "Add Dosing Event", 
                                             icon = icon("plus"), class = "btn-info btn-sm")),
                      column(6, actionButton("remove_dose_event", "Remove Last", 
                                             icon = icon("minus"), class = "btn-warning btn-sm"))
                    )
                ),
                box(width = 6, title = "Simulation Settings",
                    numericInput("sim_end_time", "Simulation End Time (hours):", value = 168, min = 0),
                    checkboxGroupInput("sim_request_vars", "Variables to Request:",
                                      choices = c(), selected = c()),
                    textInput("sim_plot_title", "Plot Title:", value = "", placeholder = "e.g., Drug Concentration Over Time"),
                    textInput("sim_xaxis_label", "X-axis Label:", value = "Time (hours)", placeholder = "e.g., Time (hours)"),
                    textInput("sim_yaxis_label", "Y-axis Label:", value = "", placeholder = "e.g., Concentration (ng/mL)"),
                    fluidRow(
                      column(3, numericInput("sim_xmin", "X min:", value = NA, step = 1)),
                      column(3, numericInput("sim_xmax", "X max:", value = NA, step = 1)),
                      column(3, numericInput("sim_ymin", "Y min:", value = NA, step = 1)),
                      column(3, numericInput("sim_ymax", "Y max:", value = NA, step = 1))
                    ),
                    checkboxInput("sim_log_x", "Log Scale X-axis", value = FALSE),
                    checkboxInput("sim_log_y", "Log Scale Y-axis", value = FALSE),
                    actionButton("run_simulation", "Run Simulation", class = "btn-success"),
                    br(), br(),
                    verbatimTextOutput("sim_execution_status")
                )
              ),
              fluidRow(
                box(width = 12, title = "Simulation Results",
                    plotlyOutput("sim_plot", height = "500px"),
                    br(),
                    downloadButton("download_sim_results", "Download Results (CSV)")
                )
              )
      ),
      
      # Sensitivity Analysis tab (separate from Dosing Simulation)
      tabItem(tabName = "sensitivity",
              fluidRow(
                box(width = 12, title = "Model Setup", status = "info",
                    p("This tab uses the same model and dosing regimen loaded in ", tags$strong("Dosing Simulation"), "."),
                    p("Please load a model in the Dosing Simulation tab first if you haven't already."),
                    verbatimTextOutput("sens_model_status")
                )
              ),
              fluidRow(
                box(width = 4, title = "Parameter Selection",
                    selectInput("sens_param_select", "Select Parameter to Vary:",
                               choices = c(), selected = NULL),
                    helpText("Select a parameter from the model's $PARAM block")
                ),
                box(width = 4, title = "Variation Settings",
                    numericInput("sens_param_min", "Minimum Multiplier:", value = 0.5),
                    helpText("Min value = base × multiplier"),
                    numericInput("sens_param_max", "Maximum Multiplier:", value = 2.0),
                    helpText("Max value = base × multiplier"),
                    numericInput("sens_param_steps", "Number of Steps:", value = 5, min = 2, max = 20)
                ),
                box(width = 4, title = "Output & Run",
                    selectInput("sens_output_var", "Output Variable to Plot:",
                               choices = c(), selected = NULL),
                    textInput("sens_xaxis_label", "X-axis Label:", value = "Time (hours)", placeholder = "e.g., Time (hours)"),
                    textInput("sens_yaxis_label", "Y-axis Label:", value = "", placeholder = "e.g., Concentration (ng/mL)"),
                    actionButton("run_sensitivity", "Run Sensitivity Analysis", class = "btn-warning"),
                    br(), br(),
                    verbatimTextOutput("sens_execution_status")
                )
              ),
              fluidRow(
                box(width = 12, title = "Sensitivity Analysis Results",
                    plotlyOutput("sens_plot", height = "500px"),
                    br(),
                    downloadButton("download_sens_results", "Download Sensitivity Results (CSV)")
                )
              )
      ),
      
      # VPC Generation tab
      tabItem(tabName = "vpc",
              fluidRow(
                box(width = 12, title = "VPC Model",
                    fluidRow(
                      column(3,
                        actionButton("load_vpc_model", "Load Saved Model", class = "btn-primary"),
                        helpText("Uses the model saved in Final Model tab")
                      ),
                      column(9,
                        verbatimTextOutput("vpc_model_status")
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 6, title = "Data Column Mapping & Simulation",
                    radioButtons("vpc_data_source", "Data Source:",
                                 choices = c("Monolix project" = "monolix",
                                             "NONMEM / CSV file" = "nonmem"),
                                 selected = "monolix", inline = TRUE),
                    conditionalPanel(
                      condition = "input.vpc_data_source == 'nonmem'",
                      verbatimTextOutput("vpc_nm_dataset_status"),
                      helpText("Dataset path is read automatically from the loaded NONMEM control file.")
                    ),
                    hr(),
                    # Collapsible data mapping section
                    tags$details(
                      tags$summary(tags$b("Column Mapping (click to expand)")),
                      br(),
                      fluidRow(
                        column(6, selectInput("vpc_id_column", "ID Column:", choices = c("ID"), selected = "ID")),
                        column(6, selectInput("vpc_time_column", "TIME Column:", choices = c("TIME"), selected = "TIME"))
                      ),
                      fluidRow(
                        column(6, selectInput("vpc_dv_column", "DV Column:", choices = c("DV"), selected = "DV")),
                        column(6, selectInput("vpc_amt_column", "AMT Column:", choices = c("", "AMT"), selected = ""))
                      ),
                      fluidRow(
                        column(6, selectInput("vpc_evid_column", "EVID Column:", choices = c("", "EVID"), selected = "")),
                        column(6, selectInput("vpc_dvid_column", "DVID/CMT Column:", choices = c("", "DVID"), selected = ""))
                      ),
                      fluidRow(
                        column(6, selectInput("vpc_rate_column", "RATE Column:", choices = c("", "RATE"), selected = "")),
                        column(6, selectInput("vpc_ii_column", "II Column:", choices = c("", "II"), selected = ""))
                      ),
                      fluidRow(
                        column(6, selectInput("vpc_addl_column", "ADDL Column:", choices = c("", "ADDL"), selected = "")),
                        column(6, selectInput("vpc_tad_column", "TAD Column (optional):", choices = c("", "TAD"), selected = ""))
                      ),
                      fluidRow(
                        column(6, selectInput("vpc_bql_column", "BQL/Censoring Column:", choices = c("", "BQL"), selected = ""))
                      ),
                      fluidRow(
                        column(12, selectizeInput("vpc_ignore_columns", "Columns to Ignore:",
                                                choices = c(), selected = NULL, multiple = TRUE,
                                                options = list(placeholder = "Select columns to exclude...")))
                      ),
                      helpText("Map dataset columns to standard names. Leave empty to auto-derive or if not applicable."),
                      hr()
                    ),
                    selectInput("vpc_potential_stratify_vars", "Potential Stratification Covariates:",
                               choices = c(), selected = NULL, multiple = TRUE),
                    helpText("Select covariates to include for stratification (can select multiple or none)"),
                    numericInput("vpc_n_sims", "Number of Simulations:", value = 500, min = 10, max = 1000, step = 10),
                    numericInput("vpc_seed", "Random Seed:", value = 1234, min = 1),
                    radioButtons("vpc_parallel_mode", "Parallel Mode:",
                                 choices = c("Local (sequential)" = "local",
                                             "HPC (SGE/clustermq)" = "hpc"),
                                 selected = "local", inline = TRUE),
                    helpText("Simulations run in this app's own R session, on the host serving",
                             "the app. No job is submitted to a scheduler. HPC dispatch is",
                             "disabled in this release; a later release will let you declare",
                             "your own cluster connection."),
                    conditionalPanel(
                      condition = "input.vpc_parallel_mode == 'hpc'",
                      numericInput("vpc_n_jobs", "Number of HPC Jobs:", value = 10, min = 1, max = 200)
                    ),
                    actionButton("run_vpc_sim", "Run VPC Simulation", class = "btn-success"),
                    conditionalPanel(
                      condition = "input.vpc_data_source == 'monolix'",
                      helpText("Dataset will be prepared automatically from the loaded project"),
                      verbatimTextOutput("vpc_mlx_filter_status")
                    ),
                    conditionalPanel(
                      condition = "input.vpc_data_source == 'nonmem'",
                      helpText("Dataset will be loaded from the uploaded CSV file")
                    ),
                    br(),
                    verbatimTextOutput("vpc_sim_status")
                ),
                box(width = 6, title = "VPC Plot Options",
                    selectInput("vpc_dvid_select", "Select DVID/CMT to Display:",
                               choices = c(), selected = NULL),
                    numericInput("vpc_lloq", "Lower Limit of Quantification (LLOQ):", 
                                value = NA, min = 0, step = 0.01),
                    helpText("Leave empty if not applicable. Probability of censored observations will be plotted if provided."),
                    hr(),
                    h5(tags$b("Stratification Settings")),
                    selectizeInput("vpc_stratify_vars", "Stratify by:",
                                  choices = c(), selected = NULL, multiple = TRUE,
                                  options = list(placeholder = "Select covariates...")),
                    uiOutput("vpc_stratify_config"),
                    uiOutput("vpc_strip_names_ui"),
                    hr(),
                    actionButton("generate_vpc_plot", "Generate VPC Plot", class = "btn-primary"),
                    br(), br(),
                    verbatimTextOutput("vpc_plot_status")
                )
              ),
              fluidRow(
                box(width = 12, title = "VPC Plot",
                    fluidRow(
                      column(3, checkboxInput("vpc_pred_corr", "Prediction-Corrected VPC", value = FALSE)),
                      column(2, checkboxInput("vpc_log_y", "Log Y-axis", value = FALSE)),
                      column(2, checkboxInput("vpc_smooth", "Smooth VPC", value = FALSE)),
                      column(3, checkboxInput("vpc_tad_axis", "Time after dose (TAD)", value = FALSE)),
                      column(2, checkboxInput("vpc_show_legend", "Show Legend", value = TRUE))
                    ),
                    fluidRow(
                      column(6, textInput("vpc_x_label", "X-axis Label:", value = "Time (hours)", placeholder = "e.g., Time (hours)")),
                      column(6, textInput("vpc_y_label", "Y-axis Label:", value = "Concentration (ng/mL)", placeholder = "e.g., Concentration (ng/mL)"))
                    ),
                    fluidRow(
                      column(3, numericInput("vpc_xmin", "X min:", value = NA, step = 1)),
                      column(3, numericInput("vpc_xmax", "X max:", value = NA, step = 1)),
                      column(3, numericInput("vpc_ymin", "Y min:", value = NA, step = 1)),
                      column(3, numericInput("vpc_ymax", "Y max:", value = NA, step = 1))
                    ),
                    fluidRow(
                      column(6, textInput("vpc_x_breaks", "X-axis Breaks:",
                                          value = "", placeholder = "e.g., 0, 24, 48, 96, 168")),
                      column(6, textInput("vpc_y_breaks", "Y-axis Breaks:",
                                          value = "", placeholder = "e.g., 0.1, 1, 10, 100"))
                    ),
                    fluidRow(
                      column(2,
                        selectInput("vpc_bin_method", "Binning Method:",
                                   choices = c("jenks", "pretty", "time", "data", "density", "none", "customized"),
                                   selected = "jenks")
                      ),
                      column(2,
                        selectInput("vpc_scales", "Facet Scales:",
                                   choices = c("free", "free_x", "free_y", "fixed"),
                                   selected = "free_x")
                      ),
                      column(2,
                        conditionalPanel(
                          condition = "input.vpc_bin_method == 'customized'",
                          textInput("vpc_custom_bins", "Custom Bins:",
                                   value = "",
                                   placeholder = "e.g., 0, 100, 500, 1000")
                        )
                      ),
                      column(3,
                        selectInput("vpc_pi_percent", "Prediction Interval (PI):",
                                   choices = c("80%" = 80, "85%" = 85, "90%" = 90, "95%" = 95, "99%" = 99),
                                   selected = 90)
                      ),
                      column(3,
                        selectInput("vpc_ci_percent", "Confidence Interval (CI):",
                                   choices = c("80%" = 80, "85%" = 85, "90%" = 90, "95%" = 95, "99%" = 99),
                                   selected = 90)
                      )
                    ),
                    fluidRow(
                      column(12,
                        actionButton("update_vpc_plot", "Update Plot", class = "btn-warning",
                                     style = "margin-bottom: 10px;")
                      )
                    ),
                    plotOutput("vpc_plot", height = "600px"),
                    br(),
                    fluidRow(
                      column(3,
                        numericInput("vpc_plot_width", "Plot Width (inches):", value = 7, min = 1, max = 20, step = 0.5)
                      ),
                      column(3,
                        numericInput("vpc_plot_height", "Plot Height (inches):", value = 7, min = 1, max = 20, step = 0.5)
                      ),
                      column(2,
                        downloadButton("download_vpc_plot", "Download Plot (PNG)")
                      ),
                      column(2,
                        downloadButton("download_vpc_yaml", "Download YAML Config")
                      ),
                      column(2,
                        downloadButton("download_vpc_script", "Download R Script")
                      )
                    ),
                    fluidRow(
                      column(3,
                        downloadButton("download_vpc_bundle", "Download Reproducibility Bundle")
                      )
                    )
                )
              )
      ),

      # Forest Plot tab
      tabItem(tabName = "forest_plot",
        fluidRow(
          box(width = 12, title = "1. Model", status = "primary", solidHeader = TRUE,
              helpText("Forest plots currently support NONMEM-translated mrgsolve models. Use a model just translated and saved in the NONMEM to mrgsolve tab, or select an already-translated mrgsolve .cpp file directly."),
              fluidRow(
                column(8, verbatimTextOutput("forest_model_path_display")),
                column(4, shinyFiles::shinyFilesButton("forest_model_upload", "Or Select Model File (.cpp)",
                                                        title = "Select an mrgsolve model file", multiple = FALSE,
                                                        icon = icon("upload")))
              ),
              actionButton("forest_load_model", "Load Model for Forest Plot", class = "btn-primary"),
              verbatimTextOutput("forest_model_status")
          )
        ),
        fluidRow(
          box(width = 6, title = "2. Dosing Regimen", status = "primary", solidHeader = TRUE,
              radioButtons("forest_regimen_type", "Regimen Type:",
                          choices = c("Single Dose" = "single", "Steady State" = "ss", "Multiple Doses" = "multiple"),
                          selected = "single"),
              fluidRow(
                column(6, numericInput("forest_amt", "Dose Amount:", value = 100, min = 0)),
                column(6, selectInput("forest_cmt", "Compartment:", choices = c()))
              ),
              conditionalPanel(
                condition = "input.forest_regimen_type == 'ss' || input.forest_regimen_type == 'multiple'",
                numericInput("forest_ii", "Dosing Interval (ii):", value = 24, min = 0)
              ),
              conditionalPanel(
                condition = "input.forest_regimen_type == 'multiple'",
                numericInput("forest_addl", "Additional Doses (addl):", value = 1, min = 0)
              ),
              fluidRow(
                column(6, numericInput("forest_tau", "AUC/Summary Window (tau):", value = 24, min = 0)),
                column(6, selectInput("forest_pred", "Output Variable (pred):", choices = c()))
              ),
              helpText("Window over which AUC/Cmax/Cmin/Cavg are summarized. For Steady State or Multiple Doses, set this equal to the Dosing Interval (ii) to summarize one true dosing interval.")
          ),
          box(width = 6, title = "3. Endpoints", status = "primary", solidHeader = TRUE,
              checkboxGroupInput("forest_endpoints", "Select Endpoints:",
                                choices = c("AUC" = "auc", "AUCinf" = "aucinf", "Cmax" = "cmax",
                                            "Cmin" = "cmin", "Cavg" = "cavg", "Model Parameter" = "parameter")),
              uiOutput("forest_endpoint_panels")
          )
        ),
        fluidRow(
          box(width = 12, title = "4. Covariate Data Spec", status = "primary", solidHeader = TRUE,
              radioButtons("forest_dataspec_source", NULL,
                          choices = c("Build automatically from the inputs below" = "build",
                                      "Upload an existing yspec YAML" = "upload"),
                          selected = "build"),
              conditionalPanel(
                condition = "input.forest_dataspec_source == 'upload'",
                fluidRow(
                  column(6,
                    shinyFiles::shinyFilesButton("forest_dataspec_upload", "Select yspec Data Spec YAML", title = "Select yspec data spec YAML",
                                                 multiple = FALSE, icon = icon("upload"))
                  ),
                  column(6, verbatimTextOutput("forest_dataspec_display"))
                ),
                helpText("Short label, unit, and categorical decoding for the plot are read automatically from this file -- ",
                         "you only need to provide the reference value and the values to simulate for each covariate below.")
              )
          )
        ),
        fluidRow(
          box(width = 12, title = "5. Covariates", status = "primary", solidHeader = TRUE,
              checkboxGroupInput("forest_covariates_selected", "Covariates to Vary:", choices = c(), inline = TRUE),
              helpText("Unselected model parameters stay fixed at their $PARAM default value.",
                       "For each selected covariate, enter the values to simulate as comma-separated numbers (e.g. \"50, 70, 100\"). ",
                       "If building the data spec automatically and the covariate is categorical, check \"Categorical\" and provide a display label for each value (for the plot only) -- e.g. values \"0, 1\" with labels \"Male, Female\"."),
              uiOutput("forest_covariate_panels")
          )
        ),
        fluidRow(
          box(width = 12, title = "6. Parameter Uncertainty Source", status = "primary", solidHeader = TRUE,
              radioButtons("forest_uncertainty_method", NULL,
                          choices = c("Covariance (.cov / .ext)" = "covariance",
                                      "Bootstrap (PsN raw_results_*.csv)" = "bootstrap",
                                      "Bayesian (.ext per chain)" = "bayes"),
                          selected = "covariance"),
              conditionalPanel(
                condition = "input.forest_uncertainty_method == 'covariance'",
                fluidRow(
                  column(6,
                    shinyFiles::shinyFilesButton("forest_cov_file", "Select .cov File", title = "Select NONMEM covariance file",
                                                 multiple = FALSE, icon = icon("upload"))
                  ),
                  column(6, verbatimTextOutput("forest_cov_ext_display"))
                )
              ),
              conditionalPanel(
                condition = "input.forest_uncertainty_method == 'bootstrap'",
                fluidRow(
                  column(6,
                    shinyFiles::shinyFilesButton("forest_bootstrap_file", "Select Bootstrap CSV", title = "Select PsN raw_results CSV",
                                                 multiple = FALSE, icon = icon("upload"))
                  ),
                  column(6, verbatimTextOutput("forest_bootstrap_check"))
                )
              ),
              conditionalPanel(
                condition = "input.forest_uncertainty_method == 'bayes'",
                numericInput("forest_bayes_n_chains", "Number of Chains:", value = 1, min = 1, max = 10),
                uiOutput("forest_bayes_chain_inputs")
              )
          )
        ),
        fluidRow(
          box(width = 12, title = "7. Output Settings", status = "primary", solidHeader = TRUE,
              fluidRow(
                column(3, textInput("forest_output_stem", "Output Stem:", value = "forest")),
                column(3, numericInput("forest_nrep", "nRep:", value = 1000, min = 1)),
                column(3, selectInput("forest_filetype", "File Type:", choices = c("png", "pdf"), selected = "png"))
              ),
              helpText("Width/Height/Text Size/Shape Size below control the downloaded plot file only -- ",
                       "the in-app preview is always scaled to fit its display area."),
              fluidRow(
                column(3, numericInput("forest_width", "Width (in):", value = 7, min = 1)),
                column(3, numericInput("forest_height", "Height (in):", value = 7, min = 1)),
                column(3, numericInput("forest_text_size", "Text Size:", value = 3.5, min = 0.1, step = 0.1)),
                column(3, numericInput("forest_shape_size", "Shape Size:", value = 2.5, min = 0.1, step = 0.1))
              ),
              fluidRow(
                column(3, numericInput("forest_shade_low", "Shaded Interval Low:", value = 0.8, step = 0.05)),
                column(3, numericInput("forest_shade_high", "Shaded Interval High:", value = 1.25, step = 0.05))
              )
          )
        ),
        fluidRow(
          box(width = 12, title = "Generate Forest Plot", status = "success", solidHeader = TRUE,
              actionButton("forest_generate", "Generate Forest Plot", icon = icon("tree"), class = "btn-success"),
              br(), br(),
              verbatimTextOutput("forest_job_status")
          )
        ),
        fluidRow(
          box(width = 12, title = "Results", status = "info", solidHeader = TRUE,
              uiOutput("forest_endpoint_selector_ui"),
              div(style = "width: 100%; text-align: center;",
                  imageOutput("forest_plot_view", height = "500px")),
              br(),
              fluidRow(
                column(2, downloadButton("download_forest_plot", "Download Plot")),
                column(3, downloadButton("download_forest_yaml", "Download YAML Config")),
                column(3, downloadButton("download_forest_dataspec", "Download Data Spec")),
                column(2, downloadButton("download_forest_bundle", "Download Reproducibility Bundle"))
              ),
              helpText(tags$em("To combine all plots into a single PDF, download the Reproducibility Bundle, ",
                       "set quarto_output: true in its forest.yaml, and re-run \"Rscript forest.R\"."))
          )
        )
      ),
      
      # Feedback tab
      tabItem(tabName = "feedback",
              fluidRow(
                box(width = 12, title = "Feedback", status = "info", solidHeader = TRUE,
                    h4("Author"),
                    p("Zengtao Wang"),
                    hr(),
                    h4("Contributors"),
                    tags$ul(
                      tags$li("Se Jin Kim"),
                      tags$li("Mike Heathman"),
                      tags$li("Samuel Miles")
                    ),
                    hr(),
                    h4("Feedback"),
                    # The feedback targets used to be hardcoded: a Microsoft
                    # Forms link carrying a tenant-specific form id, and a
                    # named author's corporate email address. Neither is
                    # reachable for anyone outside that tenant, and neither
                    # belongs in a published repository. Both are now
                    # optional deployment settings. Unset, the button points
                    # at the issue tracker declared in DESCRIPTION, which is
                    # correct for every deployment rather than one of them.
                    local({
                      form_url <- getOption(
                        "MN2mrg.feedback_url",
                        Sys.getenv("MN2MRG_FEEDBACK_URL", ""))
                      email <- getOption(
                        "MN2mrg.feedback_email",
                        Sys.getenv("MN2MRG_FEEDBACK_EMAIL", ""))
                      issues <- utils::packageDescription(
                        "MN2mrg")$BugReports %||% ""

                      target <- if (nzchar(form_url)) form_url else issues
                      label  <- if (nzchar(form_url)) " Open Feedback Form"
                                else " Open Issue Tracker"

                      tagList(
                        p("We value your feedback. Please report issues or",
                          "suggest improvements using the link below",
                          if (nzchar(email)) {
                            tagList(", or email ",
                                    tags$a(href = paste0("mailto:", email),
                                           email))
                          },
                          "."),
                        if (nzchar(target)) {
                          tags$a(href = target, target = "_blank",
                                 icon("external-link-alt"), label,
                                 class = "btn btn-primary btn-lg")
                        } else {
                          helpText("No feedback URL is configured. Set",
                                   "MN2MRG_FEEDBACK_URL to add one.")
                        }
                      )
                    })
                )
              )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Grey out the HPC choice while HPC dispatch is off, rather than removing
  # it, so the control still documents the capability that is coming. This is
  # presentation only: vpc_parallel_mode() does not read the input back while
  # VPC_HPC_ENABLED is FALSE.
  if (!VPC_HPC_ENABLED) {
    shinyjs::disable(selector = "#vpc_parallel_mode input[value='hpc']")
  }

  # Reactive values to store model components
  model_data <- reactiveValues(
    project_loaded = FALSE,
    project_dir = path.expand("~"),
    mlxtran_path = NULL,
    default_filename = "model_translated.cpp",
    structural_model_text = NULL,
    odes = NULL,
    alg_involving_vars = NULL,
    alg_not_involving_vars = NULL,
    pop_params = NULL,
    omega_params = NULL,
    beta_params = NULL,
    corr_params = NULL,
    param_def_lines = NULL,
    output_vars = NULL,
    regressors = NULL,
    cont_covariate_names = NULL,
    cat_covariate_names = NULL,
    cat_covariate_categories = NULL,  # List of category names for each categorical covariate (from beta params)
    cat_covariate_all_categories = NULL,  # List of ALL category names for each categorical covariate (from covariate info)
    cat_covariate_reference = NULL,   # List of reference category for each categorical covariate
    significant_cont_covariates = NULL,  # Logical vector: TRUE if covariate appears in beta parameters
    significant_cat_covariates = NULL,   # Logical vector: TRUE if covariate appears in beta parameters
    cont_covariate_params = NULL,  # List: parameters influenced by each continuous covariate
    cat_covariate_params = NULL,   # List: parameters influenced by each categorical covariate
    covariate_names = NULL,  # Keep for backward compatibility (all covariates)
    covariate_def_lines = NULL,
    cont_covariate_values = NULL,  # Store user-entered continuous covariate values
    cat_covariate_values = NULL,   # Store user-entered categorical covariate values
    covariate_values = NULL,  # Keep for backward compatibility
    regressor_values = NULL,  # Store user-entered regressor values
    table_block = NULL,  # Store $TABLE block for model regeneration
    combined_model = NULL,
    save_status = "",
    saved_model_path = NULL,  # Store path of saved model file for simulation
    sim_model = NULL,
    sim_model_loaded = FALSE,
    sim_results = NULL,
    sim_model_status = "",  # Status for model loading
    sim_execution_status = "",  # Status for simulation execution
    sim_model_d_cmts = character(0),  # Compartments with a generated D_<cmt> (Tk0) duration line
    n_dose_events = 1,  # Number of dosing events (starts at 1)
    # Sensitivity analysis reactive values
    sens_results = NULL,
    sens_param_name = NULL,
    sens_param_values = NULL,
    sens_output_var = NULL,
    sens_base_value = NULL,
    sens_base_idx = NULL,
    sens_execution_status = "",
    # VPC-related reactive values
    vpc_model = NULL,
    vpc_model_loaded = FALSE,
    vpc_model_path = NULL,
    vpc_dataset = NULL,
    vpc_dataset_prepared = FALSE,
    vpc_sim_results = NULL,
    vpc_current_plot = NULL,
    vpc_available_dvids = NULL,
    vpc_potential_stratify_vars = NULL,  # Covariates included in simulation for potential stratification
    vpc_model_status = "",
    vpc_sim_status = "",
    vpc_plot_status = "",

    # Forest Plot-related reactive values
    forest_model = NULL,
    forest_model_loaded = FALSE,
    forest_model_status = "",
    forest_model_path = NULL,  # Resolved path actually used to mread() the forest-plot model
    forest_model_upload_path = NULL,  # Path chosen via the "Or Select Model File" picker
    forest_outdir = NULL,
    forest_process = NULL,
    forest_job_running = FALSE,
    forest_job_status = "",
    forest_plot_files = NULL,
    forest_data_spec_built = FALSE,
    forest_yaml_path = NULL,
    forest_data_spec_path = NULL,
    forest_cov_path = NULL,
    forest_ext_path = NULL,
    forest_bootstrap_path = NULL,
    forest_dataspec_upload_path = NULL,
    forest_bootstrap_check = NULL,
    forest_bayes_paths = NULL
  )

  # Update shinyFileSave with full filesystem navigation
  observe({
    # Use project directory if available, otherwise use home directory
    default_path <- if (model_data$project_loaded && !is.null(model_data$project_dir)) {
      model_data$project_dir
    } else {
      path.expand("~")
    }
    
    shinyFiles::shinyFileSave(input, "save_file_button", 
                             roots = c(root = "/"), 
                             session = session, 
                             filetypes = c("cpp", "txt", ""),
                             defaultPath = default_path)
  })
  
  # ============================================================
  # SHARED DATASET PREPROCESSING HELPER
  # Collects all lixoftConnectors info, then delegates to prepare_monolix_dataset()
  # in UTILS/vpc_utils.R for column remapping, covariate encoding (including Monolix
  # transformed categoricals like tSPECIES), CMT assignment, DVID handling, and sort.
  # ============================================================

  prepare_mlx_dataset <- function(input_data, model_data) {

    structural_path <- getStructuralModel()
    # resolve_lib_model_path() maps a "lib:" reference into the Monolix
    # library-model directory and passes any other path straight through, so
    # the branch this replaced is no longer needed. The branch it replaced
    # substituted a literal "LIB_MODEL_STRUCTURAL/" prefix, which is not a
    # directory, an option or an environment variable anywhere in this
    raw_lines <- readLines(resolve_lib_model_path(structural_path), warn = FALSE)
    mlxtran_lines <- stringr::str_squish(stringr::str_replace(raw_lines, ";.*$", ""))

    opts <- list(
      data_info             = getData(),
      covariate_info        = getCovariateInformation(),
      mlxtran_lines         = mlxtran_lines,
      dvid_map              = model_data$dvid_map,
      has_dvid              = model_data$has_dvid,
      filter_mdv            = FALSE,
      mlx_filter_conditions = parse_mlx_dataset_filters(model_data$mlxtran_path, id_col = mlx_identifier_col())$conditions
    )

    result <- prepare_monolix_dataset(input_data, opts)
    result$data
  }

  # ============================================================
  # NONMEM TRANSLATION SERVER
  # ============================================================

  nm_ctlFile     <- reactiveVal("")
  nm_extFile     <- reactiveVal("")
  nm_mrg_code    <- reactiveVal("")
  nm_theta       <- reactiveVal(NULL)
  nm_omega       <- reactiveVal(NULL)
  nm_sigma       <- reactiveVal(NULL)
  nm_chooseExt   <- reactiveVal(FALSE)
  nm_modelReady  <- reactiveVal(FALSE)
  # Starting directory for the NONMEM file browser. Overridable so the
  # source release. UI convenience only: the browser's root is "/", so
  # every path stays reachable whatever this is set to.
  nm_currentDir  <- reactiveVal(
    getOption("MN2mrg.nonmem_root", path.expand("~"))
  )

  # File choosers
  observe({
    rel <- fs::path_rel(nm_currentDir(), "/")
    shinyFiles::shinyFileChoose(input, "nm_ctl_upload", roots = c(root = "/"),
                                defaultPath = rel,
                                filetypes = c("", "ctl", "mod", "inp"))
    shinyFiles::shinyFileChoose(input, "nm_ext_upload", roots = c(root = "/"),
                                defaultPath = rel,
                                filetypes = c("", "ext"))
    shinyFiles::shinyFileSave(input, "nm_save_file_button", roots = c(root = "/"),
                              defaultPath = rel,
                              filetypes = c("cpp", ""))
  })

  # Enable/disable EXT upload based on state
  observeEvent(nm_chooseExt(), {
    shinyjs::toggleState("nm_ext_upload", condition = nm_chooseExt())
  })

  # Enable/disable Load Project based on state
  observeEvent(nm_modelReady(), {
    shinyjs::toggleState("nm_load_project", condition = nm_modelReady())
    shinyjs::toggleState("nm_compile_model", condition = nm_modelReady())
    shinyjs::toggleState("nm_save_file_button", condition = nm_modelReady())
  })

  # CTL upload — auto-detect EXT in same directory
  observeEvent(input$nm_ctl_upload, {
    fp <- shinyFiles::parseFilePaths(roots = c(root = "/"), input$nm_ctl_upload)
    path <- as.character(fp$datapath)
    if (length(path) == 0 || nchar(path) == 0) return()
    nm_ctlFile(path)
    nm_currentDir(dirname(path))
    pot_ext <- sub("\\.[^.]+$", ".ext", path)
    if (file.exists(pot_ext)) {
      nm_extFile(pot_ext)
      nm_modelReady(TRUE)
      nm_chooseExt(FALSE)
      output$nm_project_info <- renderPrint(
        cat("Control stream: ", path, "\nEXT file (auto): ", pot_ext,
            "\n\nReady — click Load Project")
      )
    } else {
      nm_modelReady(FALSE)
      nm_chooseExt(TRUE)
      output$nm_project_info <- renderPrint(
        cat("Control stream: ", path,
            "\n\nEXT file not found — please upload manually")
      )
    }
  })

  # Manual EXT upload
  observeEvent(input$nm_ext_upload, {
    fp <- shinyFiles::parseFilePaths(roots = c(root = "/"), input$nm_ext_upload)
    path <- as.character(fp$datapath)
    if (length(path) == 0 || nchar(path) == 0) return()
    nm_extFile(path)
    nm_currentDir(dirname(path))
    nm_modelReady(TRUE)
    output$nm_project_info <- renderPrint(
      cat("Control stream: ", nm_ctlFile(), "\nEXT file: ", path,
          "\n\nReady — click Load Project")
    )
  })

  # Load Project — run conversion and populate all tabs
  observeEvent(input$nm_load_project, {
    shiny::req(nchar(nm_ctlFile()) > 0, nchar(nm_extFile()) > 0)
    output$nm_project_info <- renderPrint(cat("Translating model..."))

    tryCatch({
      result <- nonmem2mrgsolve(ctlFile = nm_ctlFile(), extFile = nm_extFile())

      if (is.null(result$code)) {
        output$nm_project_info <- renderPrint(
          cat("Translation failed:\n", paste(result$message, collapse = "\n"))
        )
        return()
      }

      # Store generated code
      code_str <- if (is.character(result$code) && length(result$code) > 1) {
        paste(result$code, collapse = "\n")
      } else {
        result$code
      }
      nm_mrg_code(code_str)
      updateAceEditor(session, "nm_model_editor", value = code_str)

      # Parse EXT file for parameter tables
      est <- read_ext(nm_extFile()) %>%
        filter(ITERATION == -1e9)

      theta_df <- est %>%
        select(contains("THETA")) %>%
        pivot_longer(everything(),
                     names_to = "Parameter", values_to = "Estimate")
      nm_theta(theta_df)

      omega_est <- est %>% select(contains("OMEGA"))
      if (ncol(omega_est) > 0) {
        nm_omega(pivot_longer(omega_est, everything(),
                              names_to = "Parameter", values_to = "Estimate"))
      }

      sigma_est <- est %>% select(contains("SIGMA"))
      if (ncol(sigma_est) > 0) {
        nm_sigma(pivot_longer(sigma_est, everything(),
                              names_to = "Parameter", values_to = "Estimate"))
      }

      notes <- paste(result$message, collapse = "\n")
      output$nm_project_info <- renderPrint(
        cat("Translation complete.\n\n",
            "See Parameters and Final Model tabs.")
      )
      output$nm_project_notes <- renderPrint(cat(notes))

    }, error = function(e) {
      output$nm_project_info <- renderPrint(
        cat("Error during translation:\n", e$message)
      )
    })
  })

  # Parameter DT tables
  output$nm_theta_table <- DT::renderDT({
    shiny::req(nm_theta())
    DT::datatable(nm_theta(), rownames = FALSE,
                  options = list(pageLength = 25, dom = "t"))
  })

  output$nm_omega_table <- DT::renderDT({
    shiny::req(nm_omega())
    DT::datatable(nm_omega(), rownames = FALSE,
                  options = list(pageLength = 25, dom = "t"))
  })

  output$nm_sigma_table <- DT::renderDT({
    shiny::req(nm_sigma())
    DT::datatable(nm_sigma(), rownames = FALSE,
                  options = list(pageLength = 25, dom = "t"))
  })

  # Compile model
  observeEvent(input$nm_compile_model, {
    code <- input$nm_model_editor
    if (is.null(code) || nchar(trimws(code)) == 0) {
      output$nm_save_status <- renderPrint(cat("No model to compile"))
      return()
    }
    build_messages <- character()
    tryCatch({
      stderr_file <- tempfile(fileext = ".txt")
      stderr_con  <- file(stderr_file, open = "wt")
      sink(stderr_con, type = "message")
      mcode_error <- NULL
      tryCatch(
        mrgsolve::mcode("nm_compile_check", code, quiet = FALSE),
        error   = function(e) { mcode_error <<- e },
        finally = { sink(type = "message"); close(stderr_con) }
      )
      build_messages <- readLines(stderr_file, warn = FALSE)
      unlink(stderr_file)
      if (!is.null(mcode_error)) stop(mcode_error$message)
      output$nm_save_status <- renderPrint(cat("Model compiled successfully"))
    }, error = function(e) {
      msg <- paste0("Compilation failed:\n", e$message)
      if (length(build_messages) > 0) {
        msg <- paste0(msg, "\n\n--- Build Log ---\n",
                      paste(build_messages, collapse = "\n"))
      }
      output$nm_save_status <- renderPrint(cat(msg))
    })
  })

  # Save model
  observeEvent(input$nm_save_file_button, {
    fp <- shinyFiles::parseSavePath(roots = c(root = "/"),
                                    input$nm_save_file_button)
    if (nrow(fp) == 0) return()
    file_path <- as.character(fp$datapath[1])
    code <- input$nm_model_editor
    if (is.null(code) || nchar(trimws(code)) == 0) {
      output$nm_save_status <- renderPrint(cat("No model to save"))
      return()
    }
    tryCatch({
      writeLines(code, file_path)
      nm_currentDir(dirname(file_path))
      model_data$saved_model_path <- file_path
      output$nm_save_status <- renderPrint(
        cat("File saved successfully to:\n", file_path)
      )
    }, error = function(e) {
      output$nm_save_status <- renderPrint(
        cat("Error saving file:", e$message)
      )
    })
  })

  # ============================================================
  # NONMEM VERIFICATION SERVER
  # ============================================================

  nm_tabFile     <- reactiveVal(NULL)
  nm_verify_msg  <- reactiveVal("")

  # Auto-detect dataset path from control stream $DATA block
  nm_datFile <- reactive({
    ctl_path <- nm_ctlFile()
    if (!nchar(ctl_path) || !file.exists(ctl_path)) return(NULL)
    parse_nonmem_datafile(readLines(ctl_path, warn = FALSE), ctl_dir = dirname(ctl_path))
  })

  # Auto-detect IGNORE/ACCEPT filters from control stream $DATA block
  nm_auto_filters <- reactive({
    ctl_path <- nm_ctlFile()
    if (!nchar(ctl_path) || !file.exists(ctl_path)) return(character(0))
    ig <- parse_nonmem_ignore(readLines(ctl_path, warn = FALSE))
    ig$conditions
  })

  shinyFileChoose(input, "nm_tab_upload", roots = c(root = "/"),
                  filetypes = c("tab", "txt", "csv"))

  observeEvent(input$nm_tab_upload, {
    p <- parseFilePaths(c(root = "/"), input$nm_tab_upload)
    if (nrow(p) > 0) {
      nm_tabFile(as.character(p$datapath[1]))
      dat_path <- nm_datFile()
      filters  <- nm_auto_filters()

      msg_parts <- character(0)

      # Report dataset status — apply same header-strip + ignore_chars + condition filters
      if (!is.null(dat_path) && file.exists(dat_path)) {
        ctl_path_msg <- nm_ctlFile()
        nm_ig_msg <- if (nchar(ctl_path_msg) > 0 && file.exists(ctl_path_msg))
          parse_nonmem_ignore(readLines(ctl_path_msg, warn = FALSE))
        else
          list(conditions = character(0), ignore_chars = character(0), ignore_nonnumeric = FALSE)

        raw_lines  <- readLines(dat_path, warn = FALSE)
        header_idx <- which(nchar(trimws(raw_lines)) > 0)[1]
        header     <- sub("^#", "", raw_lines[header_idx])
        body       <- raw_lines[-seq_len(header_idx)]
        body       <- apply_nonmem_ignore_rules(body, nm_ig_msg$ignore_chars, nm_ig_msg$ignore_nonnumeric)
        dat <- tryCatch({
          tmp <- tempfile(fileext = ".csv")
          writeLines(c(header, body), tmp)
          read.table(tmp, header = TRUE, sep = ",", na = ".")
        }, error = function(e) NULL)

        if (!is.null(dat)) {
          n_raw <- nrow(dat)
          # Apply condition filters to get final row count
          for (f in filters) {
            dat <- tryCatch(
              dplyr::filter(dat, eval(parse(text = f))),
              error = function(e) dat
            )
          }
          n_filtered <- nrow(dat)
          filter_note <- if (length(filters) > 0)
            paste0(" → ", n_filtered, " after filters") else ""
          msg_parts <- c(msg_parts,
            paste0("Dataset (auto): ", basename(dat_path),
                   "  (", n_raw, " rows", filter_note, ", ", ncol(dat), " columns)"))
        } else {
          msg_parts <- c(msg_parts, paste("Could not read dataset:", dat_path))
        }
      } else if (!is.null(dat_path)) {
        msg_parts <- c(msg_parts, paste("Dataset path from control stream:", dat_path, "(not found)"))
      } else {
        msg_parts <- c(msg_parts, "No dataset path found in control stream ($DATA block)")
      }

      # Report filters
      if (length(filters) > 0) {
        msg_parts <- c(msg_parts,
          paste0("Filters (auto): ", paste(filters, collapse = "; ")))
      } else {
        msg_parts <- c(msg_parts, "No IGNORE/ACCEPT conditions found")
      }

      # Report table file with MATCH/MISMATCH against filtered row count
      tab <- tryCatch(read.table(nm_tabFile(), header = TRUE, skip = 1),
                      error = function(e) NULL)
      if (!is.null(tab)) {
        match_str <- if (exists("n_filtered") && n_filtered == nrow(tab)) "MATCH" else "MISMATCH"
        msg_parts <- c(msg_parts,
          paste0("Table file: ", basename(nm_tabFile()),
                 "  (", nrow(tab), " rows) — ", match_str))
      } else {
        msg_parts <- c(msg_parts,
          paste("Failed to read table file:", basename(nm_tabFile())))
      }

      nm_verify_msg(paste(msg_parts, collapse = "\n"))
    }
  })

  observe({
    dat_path <- nm_datFile()
    dat_ok   <- !is.null(dat_path) && file.exists(dat_path)
    if (dat_ok && !is.null(nm_tabFile()) && isTRUE(nm_modelReady()))
      shinyjs::enable("nm_create_plot")
    else
      shinyjs::disable("nm_create_plot")
  })

  observeEvent(input$nm_create_plot, {
    nm_verify_msg("Running verification...")

    # Parse $DATA IGNORE rules once and let verifyModel() do the row-filtering
    # (ignore_chars / ignore_nonnumeric) internally -- mirrors the skill's
    # verifyModel() so filtering isn't duplicated/reimplemented here.
    ctl_path  <- nm_ctlFile()
    nm_ignore <- if (nchar(ctl_path) > 0 && file.exists(ctl_path))
      parse_nonmem_ignore(readLines(ctl_path, warn = FALSE))
    else
      list(conditions = character(0), ignore_chars = character(0), ignore_nonnumeric = FALSE)

    result <- tryCatch(
      verifyModel(
        model             = input$nm_model_editor,
        dataset           = nm_datFile(),
        table             = nm_tabFile(),
        filters           = nm_auto_filters(),
        ignore_chars      = nm_ignore$ignore_chars,
        ignore_nonnumeric = nm_ignore$ignore_nonnumeric,
        censor_filter     = if (nchar(trimws(input$nm_censor_filter)) > 0) trimws(input$nm_censor_filter) else NULL
      ),
      error = function(e) list(plot = NULL, message = paste("Error:", e$message))
    )

    output$nm_verify_plot <- renderPlot({ shiny::req(result$plot); result$plot })
    nm_verify_msg(if (!is.null(result$message)) result$message else "")
  })

  output$nm_verify_messages <- renderText({ nm_verify_msg() })

  # ============================================================
  # MONOLIX TRANSLATION SERVER
  # ============================================================

  # Load project button action
  observeEvent(input$load_project, {
    tryCatch({
      result <- monolix2mrgsolve(input$project_path, input$monolix_path)

      model_data$mlxtran_path <- input$project_path
      model_data$project_dir  <- dirname(input$project_path)
      model_data$default_filename <- paste0(
        tools::file_path_sans_ext(basename(input$project_path)), "_translated.cpp")

      comp <- result$components
      for (nm in names(comp)) model_data[[nm]] <- comp[[nm]]
      model_data$combined_model <- result$code
      model_data$project_loaded <- TRUE
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Error loading project:", e$message),
        easyClose = TRUE
      ))
    })
  })

  # Reassemble the model with the current covariate/regressor values
  generate_final_model <- function() {
    if (!model_data$project_loaded) return()

    comp <- shiny::reactiveValuesToList(model_data)
    assembled <- MN2mrg:::assemble_monolix_model(
      comp,
      regressor_values = model_data$regressor_values,
      cont_covariate_values = model_data$cont_covariate_values,
      cat_covariate_values = model_data$cat_covariate_values
    )
    model_data$combined_model <- assembled$code
    model_data$pop_param_lines <- assembled$pop_param_lines
  }
  
  # Update the filename input whenever the default filename changes
  observe({
    if (model_data$project_loaded) {
      updateTextInput(session, "save_filename", value = model_data$default_filename)
    }
  })
  
  # Project info output
  output$project_info <- renderPrint({
    if (!model_data$project_loaded) return("No project loaded")
    cat("Project loaded successfully!\n\n")
    cat("Model components extracted:\n")
    cat("- ODEs:", length(model_data$odes), "\n")
    cat("- Algebraic equations:", 
        length(model_data$alg_for_ode) + length(model_data$alg_for_main), "\n")
    cat("- Population parameters:", length(model_data$pop_params), "\n")
    cat("- Beta parameters:", length(model_data$beta_params), "\n")
    cat("- Omega parameters:", length(model_data$omega_params), "\n")
    cat("- Continuous covariates:", length(model_data$cont_covariate_names), "\n")
    cat("- Categorical covariates:", length(model_data$cat_covariate_names), "\n")
    cat("- Regressors:", length(model_data$regressors), "\n")
  })
  
  # Parameters tab outputs
  output$pop_params_table <- renderDT({
    if (is.null(model_data$pop_params) || length(model_data$pop_params) == 0) return()
    data.frame(
      Name = names(model_data$pop_params),
      Value = unname(model_data$pop_params),
      stringsAsFactors = FALSE
    )
  }, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  
  output$omega_params_table <- renderDT({
    # Combine omega and correlation parameters into one table
    omega_df <- NULL
    corr_df <- NULL
    
    if (!is.null(model_data$omega_params) && length(model_data$omega_params) > 0) {
      omega_df <- data.frame(
        Name = names(model_data$omega_params),
        Value = unname(model_data$omega_params),
        Variance = unname(model_data$omega_params)^2,
        Type = "Omega",
        stringsAsFactors = FALSE
      )
    }
    
    if (!is.null(model_data$corr_params) && length(model_data$corr_params) > 0) {
      corr_df <- data.frame(
        Name = names(model_data$corr_params),
        Value = unname(model_data$corr_params),
        Variance = NA,
        Type = "Correlation",
        stringsAsFactors = FALSE
      )
    }
    
    # Combine both data frames
    if (!is.null(omega_df) && !is.null(corr_df)) {
      rbind(omega_df, corr_df)
    } else if (!is.null(omega_df)) {
      omega_df
    } else if (!is.null(corr_df)) {
      corr_df
    } else {
      return()
    }
  }, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  
  output$sigma_params_table <- renderDT({
    if (is.null(model_data$sigma_params) || length(model_data$sigma_params) == 0) return()

    # Convert list of sigma structures to data.frame
    sigma_rows <- list()
    for (i in seq_len(length(model_data$sigma_params))) {
      sigma_info <- model_data$sigma_params[[i]]
      dvid_id <- sigma_info$dvid_identifier

      # Add additive error if present
      if (!is.na(sigma_info$a_param)) {
        sigma_rows[[length(sigma_rows) + 1]] <- data.frame(
          Name = sigma_info$a_name,
          Value = sigma_info$a_param,
          Variance = sigma_info$a_param^2,
          Type = "ADD",
          stringsAsFactors = FALSE
        )
      }

      # Add proportional error if present
      if (!is.na(sigma_info$b_param)) {
        sigma_rows[[length(sigma_rows) + 1]] <- data.frame(
          Name = sigma_info$b_name,
          Value = sigma_info$b_param,
          Variance = sigma_info$b_param^2,
          Type = "PROP",
          stringsAsFactors = FALSE
        )
      }
    }

    if (length(sigma_rows) > 0) {
      sigma_df <- do.call(rbind, sigma_rows)
      rownames(sigma_df) <- NULL
      sigma_df
    } else {
      data.frame()
    }
  }, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  
  output$error_model_equations <- renderPrint({
    if (is.null(model_data$error_model_lines) || length(model_data$error_model_lines) == 0) {
      cat("No error model equations available")
      return()
    }
    cat(paste(model_data$error_model_lines, collapse = "\n"))
  })
  
  output$beta_params_table <- renderDT({
    if (is.null(model_data$beta_params) || length(model_data$beta_params) == 0) return()
    data.frame(
      Name = names(model_data$beta_params),
      Value = unname(model_data$beta_params),
      stringsAsFactors = FALSE
    )
  }, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  
  output$cont_covariates_table <- renderDT({
    if (is.null(model_data$cont_covariate_names) || length(model_data$cont_covariate_names) == 0) return()
    
    # Initialize values if not already set
    if (is.null(model_data$cont_covariate_values)) {
      model_data$cont_covariate_values <- rep(1, length(model_data$cont_covariate_names))
      names(model_data$cont_covariate_values) <- model_data$cont_covariate_names
    }
    
    # Add significance indicator and influenced parameters
    significance <- if (!is.null(model_data$significant_cont_covariates)) {
      ifelse(model_data$significant_cont_covariates, "Yes", "No")
    } else {
      rep("No", length(model_data$cont_covariate_names))
    }
    
    # Get influenced parameters for each covariate
    influenced_params <- if (!is.null(model_data$cont_covariate_params)) {
      sapply(model_data$cont_covariate_names, function(cov_name) {
        params <- model_data$cont_covariate_params[[cov_name]]
        if (length(params) > 0) {
          paste(params, collapse = ", ")
        } else {
          "-"
        }
      })
    } else {
      rep("-", length(model_data$cont_covariate_names))
    }
    
    df <- data.frame(
      Name = model_data$cont_covariate_names,
      Value = model_data$cont_covariate_values,
      Significant = significance,
      Parameters = influenced_params,
      stringsAsFactors = FALSE
    )
    
    dt <- datatable(df, 
              editable = list(
                target = 'cell', 
                disable = list(columns = c(0, 2, 3)),  # Can't edit Name, Significant, or Parameters
                numeric = 1  # Column 1 (Value) is numeric
              ),
              options = list(
                pageLength = 10, 
                dom = 't',
                columnDefs = list(
                  list(className = 'dt-center', targets = c(1, 2)),  # Center align Value and Significant columns
                  list(className = 'editable-cell', targets = 1)  # Add class to editable column
                ),
                initComplete = JS(
                  "function(settings, json) {",
                  "  var table = this.api();",
                  "  table.on('click', 'td.editable-cell', function() {",
                  "    var cell = table.cell(this);",
                  "    if (!$(this).hasClass('editing')) {",
                  "      $(this).trigger('dblclick');",
                  "    }",
                  "  });",
                  "}"
                )
              ),
              rownames = FALSE)
    
    # Style the editable Value column to look like an input box
    dt <- dt %>%
      formatStyle('Value',
                  backgroundColor = '#ffffff',
                  border = '1px solid #ccc',
                  borderRadius = '3px',
                  cursor = 'text')
    
    # Highlight significant covariates (rows where Significant == "Yes")
    if (!is.null(model_data$significant_cont_covariates) && any(model_data$significant_cont_covariates)) {
      significant_rows <- which(model_data$significant_cont_covariates) - 1  # 0-indexed for DT
      dt <- dt %>% 
        formatStyle('Name',
                    target = 'row',
                    backgroundColor = styleEqual(
                      df$Name[model_data$significant_cont_covariates],
                      rep('#ffffcc', sum(model_data$significant_cont_covariates))
                    ))
    }
    
    dt
  })
  
  # Handle edits to continuous covariates table
  observeEvent(input$cont_covariates_table_cell_edit, {
    info <- input$cont_covariates_table_cell_edit
    row <- info$row
    col <- info$col
    value <- info$value
    
    # Column 1 is Value (0-indexed)
    if (col == 1) {
      # Try to convert to numeric
      num_value <- suppressWarnings(as.numeric(value))
      if (!is.na(num_value)) {
        model_data$cont_covariate_values[row] <- num_value
        # Update combined covariate_values for backward compatibility
        update_combined_covariate_values()
        # Trigger model regeneration
        generate_final_model()
      } else {
        showNotification("Please enter a numeric value", type = "error")
      }
    }
  })
  
  output$cat_covariates_table <- renderDT({
    if (is.null(model_data$cat_covariate_names) || length(model_data$cat_covariate_names) == 0) return()
    
    # Initialize values if not already set (default to 0 for categorical)
    if (is.null(model_data$cat_covariate_values)) {
      model_data$cat_covariate_values <- rep(0, length(model_data$cat_covariate_names))
      names(model_data$cat_covariate_values) <- model_data$cat_covariate_names
    }
    
    # Build dataframe with categories information
    cat_info <- sapply(seq_along(model_data$cat_covariate_names), function(i) {
      cat_name <- model_data$cat_covariate_names[i]
      is_significant <- if (!is.null(model_data$significant_cat_covariates)) {
        model_data$significant_cat_covariates[i]
      } else {
        FALSE
      }
      
      # Get data sources
      beta_categories <- model_data$cat_covariate_categories[[cat_name]]  # From beta params
      all_categories <- model_data$cat_covariate_all_categories[[cat_name]]  # From covariate_info
      reference <- model_data$cat_covariate_reference[[cat_name]]
      
      if (is_significant) {
        # Significant covariate: use "0=reference, 1=category1, 2=category2" format
        # Use categories from beta parameters, reference from covariate_info
        if (!is.null(beta_categories) && length(beta_categories) > 0) {
          ref_label <- if (!is.null(reference)) reference else "reference"
          cat_labels <- paste0(seq_along(beta_categories), "=", beta_categories)
          paste0("0=", ref_label, ", ", paste(cat_labels, collapse = ", "))
        } else {
          # Fallback if beta categories not available
          ref_label <- if (!is.null(reference)) reference else "reference"
          paste0("0=", ref_label)
        }
      } else {
        # Non-significant covariate: just display "category1, category2, category3" format
        # Use all categories from covariate_info
        if (!is.null(all_categories) && length(all_categories) > 0) {
          paste(all_categories, collapse = ", ")
        } else {
          "-"
        }
      }
    })
    
    # Add significance indicator
    significance <- if (!is.null(model_data$significant_cat_covariates)) {
      ifelse(model_data$significant_cat_covariates, "Yes", "No")
    } else {
      rep("No", length(model_data$cat_covariate_names))
    }
    
    # Get influenced parameters for each covariate
    influenced_params <- if (!is.null(model_data$cat_covariate_params)) {
      sapply(model_data$cat_covariate_names, function(cov_name) {
        params <- model_data$cat_covariate_params[[cov_name]]
        if (length(params) > 0) {
          paste(params, collapse = ", ")
        } else {
          "-"
        }
      })
    } else {
      rep("-", length(model_data$cat_covariate_names))
    }
    
    df <- data.frame(
      Name = model_data$cat_covariate_names,
      Value = model_data$cat_covariate_values,
      Categories = cat_info,
      Significant = significance,
      Parameters = influenced_params,
      stringsAsFactors = FALSE
    )
    
    dt <- datatable(df, 
              editable = list(
                target = 'cell', 
                disable = list(columns = c(0, 2, 3, 4)),  # Can't edit Name, Categories, Significant, or Parameters
                numeric = 1  # Column 1 (Value) is numeric
              ),
              options = list(
                pageLength = 10, 
                dom = 't', 
                columnDefs = list(
                  list(width = '300px', targets = 2),
                  list(className = 'dt-center', targets = c(1, 3)),  # Center align Value and Significant columns
                  list(className = 'editable-cell', targets = 1)  # Add class to editable column
                ),
                initComplete = JS(
                  "function(settings, json) {",
                  "  var table = this.api();",
                  "  table.on('click', 'td.editable-cell', function() {",
                  "    var cell = table.cell(this);",
                  "    if (!$(this).hasClass('editing')) {",
                  "      $(this).trigger('dblclick');",
                  "    }",
                  "  });",
                  "}"
                )
              ),
              rownames = FALSE)
    
    # Style the editable Value column to look like an input box
    dt <- dt %>%
      formatStyle('Value',
                  backgroundColor = '#ffffff',
                  border = '1px solid #ccc',
                  borderRadius = '3px',
                  cursor = 'text')
    
    # Highlight significant covariates (rows where Significant == "Yes")
    if (!is.null(model_data$significant_cat_covariates) && any(model_data$significant_cat_covariates)) {
      dt <- dt %>% 
        formatStyle('Name',
                    target = 'row',
                    backgroundColor = styleEqual(
                      df$Name[model_data$significant_cat_covariates],
                      rep('#ffffcc', sum(model_data$significant_cat_covariates))
                    ))
    }
    
    dt
  })
  
  # Handle edits to categorical covariates table
  observeEvent(input$cat_covariates_table_cell_edit, {
    info <- input$cat_covariates_table_cell_edit
    row <- info$row
    col <- info$col
    value <- info$value
    
    # Column 1 is Value (0-indexed)
    if (col == 1) {
      # Try to convert to numeric
      num_value <- suppressWarnings(as.numeric(value))
      if (!is.na(num_value)) {
        model_data$cat_covariate_values[row] <- num_value
        # Update combined covariate_values for backward compatibility
        update_combined_covariate_values()
        # Trigger model regeneration
        generate_final_model()
      } else {
        showNotification("Please enter a numeric value", type = "error")
      }
    }
  })
  
  # Helper function to update combined covariate values
  update_combined_covariate_values <- function() {
    cont_vals <- if (!is.null(model_data$cont_covariate_values)) model_data$cont_covariate_values else numeric(0)
    cat_vals <- if (!is.null(model_data$cat_covariate_values)) model_data$cat_covariate_values else numeric(0)
    model_data$covariate_values <- c(cont_vals, cat_vals)
  }
  
  output$regressors_table <- renderDT({
    if (is.null(model_data$regressors) || length(model_data$regressors) == 0) return()
    
    # Initialize values if not already set
    if (is.null(model_data$regressor_values)) {
      model_data$regressor_values <- rep(1, length(model_data$regressors))
      names(model_data$regressor_values) <- model_data$regressors
    }
    
    df <- data.frame(
      Name = model_data$regressors,
      Value = model_data$regressor_values,
      stringsAsFactors = FALSE
    )
    
    dt <- datatable(df, 
              editable = list(
                target = 'cell', 
                disable = list(columns = 0),
                numeric = 1  # Column 1 (Value) is numeric
              ),
              options = list(
                pageLength = 10, 
                dom = 't',
                columnDefs = list(
                  list(className = 'dt-center', targets = 1),  # Center align Value column
                  list(className = 'editable-cell', targets = 1)  # Add class to editable column
                ),
                initComplete = JS(
                  "function(settings, json) {",
                  "  var table = this.api();",
                  "  table.on('click', 'td.editable-cell', function() {",
                  "    var cell = table.cell(this);",
                  "    if (!$(this).hasClass('editing')) {",
                  "      $(this).trigger('dblclick');",
                  "    }",
                  "  });",
                  "}"
                )
              ),
              rownames = FALSE)
    
    # Style the editable Value column to look like an input box
    dt <- dt %>%
      formatStyle('Value',
                  backgroundColor = '#ffffff',
                  border = '1px solid #ccc',
                  borderRadius = '3px',
                  cursor = 'text')
    
    dt
  })
  
  # Handle edits to regressors table
  observeEvent(input$regressors_table_cell_edit, {
    info <- input$regressors_table_cell_edit
    row <- info$row
    col <- info$col
    value <- info$value
    
    # Column 1 is Value (0-indexed)
    if (col == 1) {
      # Try to convert to numeric
      num_value <- suppressWarnings(as.numeric(value))
      if (!is.na(num_value)) {
        model_data$regressor_values[row] <- num_value
        # Trigger model regeneration
        generate_final_model()
      } else {
        showNotification("Please enter a numeric value", type = "error")
      }
    }
  })
  
  # Parameters tab output
  output$param_def_lines <- renderPrint({
    if (is.null(model_data$param_def_lines) || length(model_data$param_def_lines) == 0) return()
    cat(model_data$param_def_lines, sep = "\n")
  })
  
  # Update model editor when data is available
  observe({
    if (is.null(model_data$combined_model) || length(model_data$combined_model) == 0) {
      return()
    }
    
    model_text <- paste(model_data$combined_model, collapse = "\n")
    # Use shinyjs to update the ace editor
    shinyjs::runjs(paste0("
      var editor = ace.edit('complete_model_editor');
      editor.setValue(`", gsub("`", "\\\\`", gsub("\n", "\\\\n", model_text)), "`);
    "))
  })
  
  # Display complete model with current edits
  output$complete_model_output <- renderPrint({
    shiny::req(input$complete_model_editor)
    cat(input$complete_model_editor)
  })
  
  # Compile Monolix translated model
  observeEvent(input$compile_mlx_model, {
    code <- input$complete_model_editor
    if (is.null(code) || nchar(trimws(code)) == 0) {
      model_data$save_status <- "✗ No model to compile"
      return()
    }
    build_messages <- character()
    tryCatch({
      stderr_file <- tempfile(fileext = ".txt")
      stderr_con <- file(stderr_file, open = "wt")
      sink(stderr_con, type = "message")
      mcode_error <- NULL
      tryCatch(
        mrgsolve::mcode("compile_check", code, quiet = FALSE),
        error = function(e) { mcode_error <<- e },
        finally = {
          sink(type = "message")
          close(stderr_con)
        }
      )
      build_messages <- readLines(stderr_file, warn = FALSE)
      unlink(stderr_file)
      if (!is.null(mcode_error)) stop(mcode_error$message)
      model_data$save_status <- "✓ Model compiled successfully"
    }, error = function(e) {
      error_msg <- paste0("✗ Compilation failed:\n", e$message)
      if (length(build_messages) > 0) {
        error_msg <- paste0(error_msg, "\n\n--- Build Log ---\n",
                            paste(build_messages, collapse = "\n"))
      }
      model_data$save_status <- error_msg
    })
  })

  # Handle file save dialog
  observeEvent(input$save_file_button, {
    if (is.null(input$complete_model_editor) || nchar(input$complete_model_editor) == 0) {
      model_data$save_status <- "✗ No model to save"
      return()
    }
    
    if (is.null(input$save_file_button)) {
      return()
    }
    
    tryCatch({
      # Get the selected file path from the file dialog
      file_path_list <- shinyFiles::parseSavePath(roots = c(root = "/"), 
                                                  selection = input$save_file_button)
      
      if (nrow(file_path_list) == 0) {
        model_data$save_status <- "✗ No file selected"
        return()
      }
      
      # The datapath from parseSavePath is the full file path selected by user
      file_path <- as.character(file_path_list$datapath[1])
      
      # Save the file
      writeLines(input$complete_model_editor, file_path)
      model_data$save_status <- paste0("✓ File saved successfully to:\n", file_path)
      
      # Store the saved file path for simulation tab
      model_data$saved_model_path <- file_path
      
      # Update simulation model status to show the saved file path (full path)
      model_data$sim_model_status <- paste0("Saved model ready:\n", file_path)
      
    }, error = function(e) {
      model_data$save_status <- paste("✗ Error saving file:", e$message)
    })
  })
  
  output$save_status <- renderPrint({
    cat(model_data$save_status)
  })

  # ============================================================
  # MONOLIX VERIFICATION SERVER
  # ============================================================

  # Auto-detected dataset row filter from the .mlxtran [FILTER] block —
  # shared by the Verification and VPC tabs (both read model_data$mlxtran_path).
  mlx_auto_filters <- reactive({
    parse_mlx_dataset_filters(model_data$mlxtran_path, id_col = mlx_identifier_col())
  })

  mlx_filter_status_text <- function() {
    f <- mlx_auto_filters()
    if (length(f$description) == 0)
      "Filters (auto): none detected"
    else
      paste0("Filters (auto): ", paste(f$description, collapse = "; "))
  }

  output$mlx_filter_status     <- renderText({ mlx_filter_status_text() })
  output$vpc_mlx_filter_status <- renderText({ mlx_filter_status_text() })

  mlx_verify_msg      <- reactiveVal("")
  mlx_verify_plot_obj <- reactiveVal(NULL)

  observe({
    if (!is.null(model_data$mlxtran_path))
      shinyjs::enable("mlx_create_plot")
    else
      shinyjs::disable("mlx_create_plot")
  })

  observeEvent(input$mlx_create_plot, {
    tryCatch({
      shiny::req(model_data$mlxtran_path)
      # Compile model from the translated code in the editor
      code <- input$complete_model_editor
      if (is.null(code) || nchar(trimws(code)) == 0)
        stop("No translated model found. Please load and translate a project first.")
      mod <- mrgsolve::mcode("mlx_verify_mod", code, quiet = TRUE)

      # Locate result folder: same dir as .mlxtran, same name (no extension)
      proj_name  <- tools::file_path_sans_ext(basename(model_data$mlxtran_path))
      result_dir <- file.path(dirname(model_data$mlxtran_path), proj_name)
      if (!dir.exists(result_dir))
        stop("Result folder not found: ", result_dir)

      # Read dataset directly from the loaded project and preprocess for mrgsolve
      input_data_file <- getData()$dataFile
      raw_dat <- data.table::fread(input_data_file, na.strings = ".") %>% as_tibble()

      # Tag each row with its position in the raw dataset file, before any
      # filtering/renaming/sorting. Monolix writes predictions.txt preserving
      # this same raw file order (dropping only excluded rows, e.g. censored
      # obs) -- verified empirically by comparing predictions.txt's (id,time)
      # sequence against the raw file's. prepare_mlx_dataset()/mrgsim() never
      # drop or reorder unrecognized columns, and none of the steps between
      # here and mrgsim() duplicate rows, so ROW stays unique and survives all
      # the way to the simulated output via carry_out() below. It doubles as
      # the rejoin key for EVID/DVID/DV_OBS/MDV AND the key for pairing each
      # mrgsolve row with its predictions.txt row by raw order -- not by value
      # (ID re-encoding differs from Monolix's own subject ordering, and
      # matching on TIME risks floating-point precision mismatches).
      raw_dat <- raw_dat %>% mutate(ROW = row_number())

      # Apply the project's [FILTER] block conditions BEFORE deriving
      # original_ids below. prepare_mlx_dataset() applies these same
      # conditions again internally (Step 0), then re-encodes its "id"
      # column via as.numeric(as.factor(ID)) over whatever rows remain --
      # that encoding depends on the SET of unique ID values present, so
      # original_ids must be computed from the identical (already-filtered)
      # row set, not the full unfiltered raw_dat, or the id_lookup built
      # below silently maps every row to the wrong subject (or none).
      raw_dat <- apply_mlx_filter_conditions(
        raw_dat,
        parse_mlx_dataset_filters(model_data$mlxtran_path, id_col = mlx_identifier_col())$conditions
      )

      # Capture original ID column (before prepare_mlx_dataset re-encodes it)
      # so we can apply the same encoding to eta_df$id when joining ETAs.
      # Monolix labels individual-result files (estimatedRandomEffects.txt,
      # etc.) using the [CONTENT] "identifier" column when the project
      # declares one (e.g. USUBJID), not the "id" grouping column -- even
      # when "id" is numeric (verified: a numeric ID like 21301 shows up in
      # that file as the USUBJID string). Not every project declares an
      # "identifier" column, though -- fall back to "id" itself when absent,
      # since Monolix then labels individual-result rows with "id" directly.
      # id_col_raw is needed separately regardless: dat$ID (what the mrgsolve
      # simulation actually joins on) is always derived from the "id" column.
      data_info_pre      <- getData()
      id_col_raw         <- data_info_pre$header[data_info_pre$headerTypes == "id"]
      if (length(id_col_raw) != 1) id_col_raw <- "ID"
      identifier_col_raw <- data_info_pre$header[data_info_pre$headerTypes == "identifier"]
      if (length(identifier_col_raw) != 1) identifier_col_raw <- id_col_raw
      original_ids       <- raw_dat[[id_col_raw]]
      id_key_vals        <- raw_dat[[identifier_col_raw]]

      # Occasion column (for IOV designs) -- estimatedRandomEffects.txt keeps the
      # dataset's raw occasion column name (e.g. "dose_cumsum"); dat keeps that
      # SAME name unchanged (prepare_mlx_dataset no longer renames it), because
      # it doubles as the mrgsolve $PARAM occasion-indicator regressor that the
      # translated model's $MAIN cascade reads directly.
      occ_col_raw <- data_info_pre$header[data_info_pre$headerTypes == "occ"]
      if (length(occ_col_raw) != 1) occ_col_raw <- NA_character_

      # prepare_mlx_dataset() (via prepare_monolix_dataset() in vpc_utils.R)
      # already drops Monolix "ignore"-marked columns/rows internally, before
      # renaming the id-headerType column to "ID" -- re-deriving ignore_col
      # here and filtering dat by it is not just redundant, it's wrong: if a
      # RAW column literally named "ID" happens to be the one marked
      # headerType "ignore" (distinct from whatever column is headerType
      # "id"), that raw column no longer exists in dat by this point, but the
      # NEW "ID" grouping column vpc_utils.R created collides on the same
      # name -- so the filter below would silently zero out every row
      # (encoded subject IDs are never 0/NA).
      dat <- raw_dat %>% prepare_mlx_dataset(model_data)

      # Load ETAs from IndividualParameters/estimatedRandomEffects.txt
      eta_file <- file.path(result_dir, "IndividualParameters",
                            "estimatedRandomEffects.txt")
      if (!file.exists(eta_file))
        stop("ETA file not found: ", eta_file)
      eta_df <- read.table(eta_file, header = TRUE, sep = ",",
                           check.names = FALSE)

      eta_cols  <- grep("^eta_.*_mode$", names(eta_df), value = TRUE)
      eta_param <- sub("^eta_(.*)_mode$", "\\1", eta_cols)
      is_iov    <- eta_param %in% model_data$iov_param_names

      # Align eta_df$id encoding with dat$ID. eta_df$id holds the identifier
      # column's raw values -- map those back to dat$ID's numeric encoding.
      # prepare_mlx_dataset applies as.numeric(as.factor(ID)) to the "id"
      # column when it isn't already numeric, so reproduce that same mapping
      # here to stay consistent.
      encoded_ids <- if (is.numeric(original_ids)) original_ids else as.numeric(as.factor(original_ids))
      id_lookup   <- stats::setNames(encoded_ids, as.character(id_key_vals))
      id_lookup   <- id_lookup[!duplicated(names(id_lookup))]
      eta_df      <- eta_df %>% mutate(id = unname(id_lookup[as.character(id)]))

      # Build one wide row per subject with every $OMEGA slot the translated
      # model declares:
      #  - pure-BSV parameters: a single omega_<param> value per id.
      #  - IOV parameters: estimatedRandomEffects.txt reports a single
      #    eta_<param>_mode value per (id, occasion) that is already the FULL
      #    combined BSV+IOV realization at that occasion (verified: it varies
      #    by occasion for the same subject) -- Monolix does not decompose it
      #    back into separate id-level and occasion-level components. For
      #    genuine Monte-Carlo simulation the translated model now has N
      #    independent omega_<param>_iov_<occ> slots (see $OMEGA/$MAIN
      #    generation), so we pivot wider and route each subject's per-occasion
      #    combined value into the matching slot; occasions that subject never
      #    had default to 0 (harmless -- the $MAIN cascade never selects an
      #    occasion the current record doesn't have). The id-level BSV slot
      #    omega_<param> is pinned to 0 so the combined per-occasion value
      #    alone reproduces Monolix's exact estimate.
      wide_df <- eta_df %>% select(id) %>% distinct()

      bsv_param <- unique(eta_param[!is_iov])
      if (length(bsv_param) > 0) {
        bsv_cols  <- paste0("eta_", bsv_param, "_mode")
        bsv_names <- paste0("omega_", bsv_param)
        bsv_df <- eta_df %>%
          select(id, all_of(bsv_cols)) %>%
          distinct(id, .keep_all = TRUE)
        names(bsv_df) <- c("id", bsv_names)
        wide_df <- wide_df %>% left_join(bsv_df, by = "id")
      }

      iov_param_names_here <- unique(eta_param[is_iov])
      if (length(iov_param_names_here) > 0 && !is.na(occ_col_raw) && occ_col_raw %in% names(eta_df)) {
        for (p in iov_param_names_here) {
          lvl      <- model_data$iov_param_to_level[[p]]
          occ_vals <- model_data$iov_occasion_values[[lvl]]
          val_col  <- paste0("eta_", p, "_mode")

          slot_df <- eta_df %>%
            select(id, occ = all_of(occ_col_raw), value = all_of(val_col)) %>%
            distinct(id, occ, .keep_all = TRUE) %>%
            mutate(slot = paste0("omega_", p, "_iov_", occ)) %>%
            select(id, slot, value) %>%
            pivot_wider(names_from = slot, values_from = value, values_fill = 0)

          # Ensure every occasion slot the $OMEGA block declares exists, even
          # if no subject in this dataset happened to have that occasion.
          missing_slots <- setdiff(paste0("omega_", p, "_iov_", occ_vals), names(slot_df))
          for (m in missing_slots) slot_df[[m]] <- 0

          wide_df <- wide_df %>% left_join(slot_df, by = "id")
          wide_df[[paste0("omega_", p)]] <- 0
        }
      }

      # etasrc = "data" requires ETA1, ETA2, ... matching $OMEGA diagonal order.
      # Use labels(omat()) — names(omat()) returns one entry per block, not per
      # parameter, and gives wrong positions for annotated/correlated OMEGA blocks.
      omega_order <- unlist(labels(omat(mod)))
      for (i in seq_along(omega_order)) {
        nm <- omega_order[i]
        if (nm %in% names(wide_df))
          wide_df[[paste0("ETA", i)]] <- wide_df[[nm]]
      }

      dat <- left_join(dat, wide_df, by = c("ID" = "id"))


      # Drop rows for subjects Monolix filtered out — they have no ETAs and
      # would inflate the mrgsolve output beyond the predictions file row count.
      if ("ETA1" %in% names(dat))
        dat <- dat %>% filter(!is.na(ETA1))

      pred_file_single <- file.path(result_dir, "predictions.txt")

      if (!is.null(model_data$dvid_map) && nrow(model_data$dvid_map) > 0) {
        dvid_map_i <- model_data$dvid_map

        # Monolix's file-naming depends on how many observation models are
        # actually FITTED (dvid_map, from getContinuousObservationModel()),
        # not on how many DVID values exist in the raw dataset. A dataset can
        # carry multiple DVIDs while only one is fitted (e.g. DVID=1 unused,
        # only DVID=2/y2 modeled) -- Monolix then writes the un-suffixed
        # "predictions.txt", never "predictions_y2.txt", so skip straight to
        # the single-file name rather than probing for one that can't exist.
        if (nrow(dvid_map_i) == 1) {
          expected_files <- pred_file_single
          found          <- file.exists(pred_file_single)
        } else {
          # Predictions files are always named "predictions_<obs_variable>.txt"
          # using Monolix's ORIGINAL observation variable name (verified against
          # Monolix output) -- construct the expected path directly per dvid_map
          # row instead of listing files and reverse-matching a "y"/"y_"-stripped
          # identifier. That approach silently dropped observations whose
          # obs_variable doesn't round-trip through the strip (e.g.
          # "y_BRAIN_PK_CD" -> "BRAIN_PK_CD" but the filename keeps the
          # underscore), and then shifted DVID indices for every remaining
          # observation since they were recomputed via seq_along() over the
          # shrunken file list rather than read from mrgsolve_dvid.
          # Use monolix_obs_variable, not obs_variable -- for purely-numeric
          # names (y3, y6) obs_variable gets renamed to y3_/y6_ elsewhere for
          # the mrgsolve translation, but Monolix's own output files keep the
          # original, un-renamed name.
          file_obs_var   <- if ("monolix_obs_variable" %in% names(dvid_map_i))
            dvid_map_i$monolix_obs_variable else dvid_map_i$obs_variable
          expected_files <- file.path(result_dir,
                                       paste0("predictions_", file_obs_var, ".txt"))
          found          <- file.exists(expected_files)
        }

        if (!any(found))
          stop("No predictions file found in: ", result_dir,
               "\nExpected one of: ", paste(basename(expected_files), collapse = ", "),
               "\nActual files present: ",
               paste(list.files(result_dir, pattern = "^predictions"), collapse = ", "))

        pred_files    <- expected_files[found]
        dvid_map_used <- dvid_map_i[found, ]
        dvid_labels   <- paste0("y", dvid_map_used$dvid_identifier)
        dvid_filter   <- dvid_map_used$mrgsolve_dvid
      } else {
        # Single-observation project (no dvid_map) -- fall back to listing.
        pred_files_all <- sort(list.files(result_dir,
                                          pattern = "^predictions_y.+\\.txt$",
                                          full.names = TRUE))
        if (length(pred_files_all) > 0) {
          file_dvid_ids <- sub("^predictions_y(.+)\\.txt$", "\\1", basename(pred_files_all))
          pred_files    <- pred_files_all
          dvid_labels   <- paste0("y", file_dvid_ids)
        } else if (file.exists(pred_file_single)) {
          pred_files  <- pred_file_single
          dvid_labels <- "y"
        } else {
          stop("No predictions file found in: ", result_dir,
               "\nActual files present: ",
               paste(list.files(result_dir, pattern = "^predictions"), collapse = ", "))
        }

        # Parse raw Monolix DVID values from filename labels (e.g. "y3" → 3).
        # Returns NA for alphanumeric names, falling back to sequential index.
        dvid_numeric <- suppressWarnings(as.integer(sub("^y", "", dvid_labels)))
        dvid_filter  <- ifelse(!is.na(dvid_numeric), dvid_numeric, seq_along(dvid_labels))
      }

      has_mdv <- "MDV" %in% names(dat)

      # Adjust RATE for F_CMT: mrgsolve duration = F_CMT * AMT / RATE,
      # but Monolix uses AMT / RATE. Multiplying RATE by F_CMT on dose rows
      # cancels the F_CMT and aligns the infusion duration.
      # PRED and IPRED need separate datasets: F depends on omega (individual
      # ETAs), so population vs individual simulations have different F values.
      {
        cmt_names   <- mod@cmtL
        f_var_names <- paste0("F_", cmt_names)
        defined_f   <- f_var_names[sapply(f_var_names, function(v)
          grepl(paste0("\\b", v, "\\s*="), code))]
        has_inf     <- any(
          dat$EVID == 1 & !is.na(dat$RATE) & dat$RATE > 0, na.rm = TRUE
        )

        if (length(defined_f) > 0 && has_inf) {
          new_f <- setdiff(defined_f, mod@capture)
          mod_f <- mcode(
            "_f_extract",
            paste0(code, "\n$CAPTURE ", paste(new_f, collapse = " ")),
            quiet = TRUE
          )

          extract_f_at_doses <- function(pre_fn, mrgsim_args = list()) {
            d_tmp <- dat %>% mutate(.ROW_F = row_number())
            sim   <- pre_fn(mod_f) %>% data_set(d_tmp) %>% carry_out(.ROW_F)
            out   <- do.call(mrgsim_df, c(list(sim), mrgsim_args))
            out %>%
              inner_join(
                d_tmp %>% dplyr::filter(EVID == 1) %>% select(.ROW_F),
                by = ".ROW_F"
              ) %>%
              select(.ROW_F, all_of(new_f))
          }

          apply_f_to_rate <- function(d, f_vals) {
            d_adj <- d %>%
              mutate(.ROW_F = row_number()) %>%
              left_join(f_vals, by = ".ROW_F")
            for (fv in new_f) {
              cmt_name <- sub("^F_", "", fv)
              mask <- d_adj$EVID == 1 & !is.na(d_adj$RATE) &
                d_adj$RATE > 0 & !is.na(d_adj[[fv]]) &
                d_adj$CMT == cmt_name
              d_adj$RATE[mask] <- d_adj$RATE[mask] * d_adj[[fv]][mask]
            }
            d_adj %>% select(-.ROW_F, -all_of(new_f))
          }

          f_pred    <- extract_f_at_doses(function(m) m %>% zero_re())
          dat_pred  <- apply_f_to_rate(dat, f_pred)

          f_ipred   <- extract_f_at_doses(
            function(m) m %>% zero_re(sigma),
            mrgsim_args = list(etasrc = "data")
          )
          dat_ipred <- apply_f_to_rate(dat, f_ipred)
        } else {
          dat_pred  <- dat
          dat_ipred <- dat
        }
      }

      # ROW was tagged on raw_dat before any filter/sort (see above) and
      # survives untouched through prepare_mlx_dataset()/apply_f_to_rate() --
      # neither drops unrecognized columns or duplicates rows -- so it's
      # already the raw-file-order key on dat_pred/dat_ipred here. Do NOT
      # reassign it via row_number() at this point: that would renumber by
      # the ID/TIME-sorted order mrgsolve simulates on, losing the original
      # file order predictions.txt follows.

      # Helper: filter sim output to the rows Monolix predictions cover.
      # After remapping, mrgsolve DVID i = i (sequential).
      filter_obs_rows <- function(sim_df, mrg_dvid) {
        rows <- sim_df %>% dplyr::filter(EVID == 0)
        if (!is.na(mrg_dvid)) rows <- rows %>% dplyr::filter(DVID == mrg_dvid)
        if (has_mdv)          rows <- rows %>% dplyr::filter(MDV  == 0)
        rows <- rows %>% dplyr::filter(!is.na(DV_OBS))
        rows
      }

      # PRED simulation (zero_re — all ETAs = 0)
      # Carry ROW (raw-file row order, tagged before any filter/sort) so rows
      # can be paired with predictions.txt by matching order below, and used
      # to rejoin EVID/DVID/DV_OBS/MDV after mrgsim.
      pred_sim_full <- mod %>% zero_re() %>%
        data_set(dat_pred) %>% carry_out(ROW) %>% mrgsim_df() %>%
        left_join(dat_pred %>% select(ROW, EVID, DVID, DV_OBS = DV, any_of("MDV")), by = "ROW")

      # IPRED simulation (zero sigma, ETAs from data columns)
      ipred_sim_full <- mod %>% zero_re(sigma) %>%
        data_set(dat_ipred) %>% carry_out(ROW) %>%
        mrgsim_df(etasrc = "data") %>%
        left_join(dat_ipred %>% select(ROW, EVID, DVID, DV_OBS = DV, any_of("MDV")), by = "ROW")

      # Build one row of plots per DVID
      plot_list <- lapply(seq_along(pred_files), function(i) {
        # read.table() preserves the file's row order, which is a Monolix
        # export -- Monolix writes predictions.txt in the same relative
        # order as the raw dataset it read (verified empirically), just
        # dropping excluded rows (e.g. censored obs). Sorting the mrgsolve
        # side by ROW recovers that same raw order, so the two line up
        # row-for-row without relying on ID re-encoding or TIME value
        # matching (dat$ID is re-encoded via as.numeric(as.factor(ID)),
        # which need not match Monolix's own subject ordering; TIME
        # equality risks floating-point precision mismatches).
        pred_tbl <- read.table(pred_files[[i]], header = TRUE,
                               sep = ",", check.names = FALSE) %>%
          mutate(across(everything(), as.numeric))

        # Always use "Y" — the model routes the correct endpoint per DVID
        # via if(DVID==N) in $TABLE. Filter by sequential mrgsolve DVID (i).
        pred_vals  <- filter_obs_rows(pred_sim_full,  dvid_filter[i]) %>%
          arrange(ROW) %>% pull(Y)
        ipred_vals <- filter_obs_rows(ipred_sim_full, dvid_filter[i]) %>%
          arrange(ROW) %>% pull(Y)

        if (nrow(pred_tbl) != length(pred_vals))
          stop(sprintf(
            "Row mismatch for %s: predictions file has %d rows, mrgsolve PRED has %d",
            dvid_labels[i], nrow(pred_tbl), length(pred_vals)))

        combined <- bind_cols(
          pred_tbl,
          MRGPRED  = pred_vals,
          MRGIPRED = ipred_vals
        )

        p1 <- ggplot(combined, aes(x = popPred,       y = MRGPRED)) +
          geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
          geom_point(alpha = 0.5) + theme_bw() + coord_equal() +
          labs(title = paste0("PRED [", dvid_labels[i], "]"),
               x = "Monolix popPred", y = "mrgsolve PRED")

        p2 <- ggplot(combined, aes(x = indivPred_mode, y = MRGIPRED)) +
          geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
          geom_point(alpha = 0.5) + theme_bw() + coord_equal() +
          labs(title = paste0("IPRED [", dvid_labels[i], "]"),
               x = "Monolix indivPred_mode", y = "mrgsolve IPRED")

        p1 + p2
      })

      final_plot <- Reduce(`/`, plot_list)
      mlx_verify_plot_obj(final_plot)
      mlx_verify_msg(paste0("Verification complete\n", mlx_filter_status_text()))

    }, error = function(e) {
      mlx_verify_msg(paste("Error:", e$message))
    })
  })

  output$mlx_verify_plot     <- renderPlot({
    shiny::req(mlx_verify_plot_obj())
    mlx_verify_plot_obj()
  })
  output$mlx_verify_messages <- renderText({ mlx_verify_msg() })

  # ============ SIMULATION TAB LOGIC ============
  
  # Load simulation model
  observeEvent(input$load_sim_model, {
    # Check if we should use the saved model path or the uploaded file
    model_path <- NULL
    
    if (!is.null(model_data$saved_model_path) && file.exists(model_data$saved_model_path)) {
      # Use the saved model path if available
      model_path <- model_data$saved_model_path
    } else if (!is.null(input$sim_model_file)) {
      # Use uploaded file if no saved model
      model_path <- input$sim_model_file$datapath
    } else {
      model_data$sim_model_status <- "✗ No model file available. Please save a translated model or upload a file."
      return()
    }
    
    build_messages <- character()
    tryCatch({
      # Read the model file
      model_text <- readLines(model_path)
      
      # Try to load with mrgsolve
      if (!require(mrgsolve)) {
        model_data$sim_model_status <- "✗ mrgsolve package not installed. Install with: install.packages('mrgsolve')"
        return()
      }
      
      # Load the model using mread, capturing stderr (compiler errors) for display
      # Use a temp file to capture stderr so build errors are preserved even if mread throws
      stderr_file <- tempfile(fileext = ".txt")
      stderr_con <- file(stderr_file, open = "wt")
      sink(stderr_con, type = "message")
      mread_error <- NULL
      tryCatch(
        sim_model <- mread(model_path, quiet = FALSE),
        error = function(e) { mread_error <<- e },
        finally = {
          sink(type = "message")
          close(stderr_con)
        }
      )
      build_messages <- readLines(stderr_file, warn = FALSE)
      unlink(stderr_file)
      
      # If mread failed, re-throw the error so the outer tryCatch handles it
      if (!is.null(mread_error)) stop(mread_error$message)
      model_data$sim_model <- sim_model
      model_data$sim_model_loaded <- TRUE

      # Detect compartments with a model-defined D_<cmt> (Tk0 zero-order
      # infusion duration) line in the generated code -- mrgsolve only honors
      # D_<cmt> when the dosing event's rate is exactly -2, so build_dose_event()
      # auto-applies rate=-2 for doses into these compartments.
      model_data$sim_model_d_cmts <- unique(na.omit(
        str_match(model_text, "\\bD_([A-Za-z0-9_]+)\\s*=")[, 2]
      ))
      
      # Compartment and capture names, read straight off the model object.
      # sim_model$cmt / $capture route through mrgsolve's `[[`, which builds
      # as.list(mod) and coerces param(), omat(), smat() and init() on the
      # way past: four S4 numericlists, none of them wanted here, and each a
      # chance to fail with "no method for coercing this S4 class to a
      # vector". outvars() reads x@cmtL and x@capL directly.
      sim_outvars  <- outvars(sim_model)
      compartments <- as.character(sim_outvars$cmt)
      
      # Update compartment selector for all dosing events
      for (i in seq_len(model_data$n_dose_events)) {
        updateSelectInput(session, paste0("dose_cmt_", i), choices = compartments, selected = compartments[1])
      }
      
      # Output variables from the model's $CAPTURE block
      output_vars <- as.character(sim_outvars$capture)
      
      # Update request variables checkboxes (include compartment names + capture variables)
      all_request_vars <- c(compartments, output_vars)
      updateCheckboxGroupInput(session, "sim_request_vars", 
                              choices = all_request_vars, 
                              selected = all_request_vars[1])
      
      # Display success message in the model status box (next to Load Model button)
      model_data$sim_model_status <- paste0("✓ Model loaded successfully\nCompartments: ", paste(compartments, collapse = ", "))
      
    }, error = function(e) {
      # Include captured build/compiler messages so user can see where to fix
      error_msg <- paste("✗ Error loading model:", e$message)
      if (length(build_messages) > 0) {
        # Filter to show relevant lines (compiler errors, warnings)
        error_msg <- paste0(error_msg, "\n\n--- Build Log ---\n", 
                           paste(build_messages, collapse = "\n"))
      }
      model_data$sim_model_status <- error_msg
    })
  })
  
  # --- Add Dosing Event ---
  observeEvent(input$add_dose_event, {
    n <- model_data$n_dose_events + 1
    model_data$n_dose_events <- n
    
    # Get compartment choices from loaded model
    cmt_choices <- if (!is.null(model_data$sim_model))
      as.character(outvars(model_data$sim_model)$cmt) else c()
    
    insertUI(
      selector = "#extra_dosing_events",
      where = "beforeEnd",
      ui = div(id = paste0("dose_event_", n), class = "dosing-event-group",
        tags$h5(tags$b(paste("Dosing Event", n)), style = "margin-top: 5px;"),
        fluidRow(
          column(4, numericInput(paste0("dose_time_", n), "Dose Time:", value = 0, min = 0)),
          column(4, numericInput(paste0("dose_amount_", n), "Dose Amount:", value = 100, min = 0)),
          column(4, selectInput(paste0("dose_cmt_", n), "Compartment:", choices = cmt_choices))
        ),
        fluidRow(
          column(4, numericInput(paste0("dose_addl_", n), "Additional Doses:", value = 0, min = 0)),
          column(4, numericInput(paste0("dose_ii_", n), "Dosing Interval:", value = 24, min = 0)),
          column(4)
        ),
        tags$hr()
      )
    )
  })
  
  # --- Remove Last Dosing Event ---
  observeEvent(input$remove_dose_event, {
    n <- model_data$n_dose_events
    if (n <= 1) return()  # Keep at least 1 dosing event
    
    removeUI(selector = paste0("#dose_event_", n))
    model_data$n_dose_events <- n - 1
  })
  
  # --- Helper: build combined dose event from all dosing event inputs ---
  build_dose_event <- function() {
    events <- list()
    for (i in seq_len(model_data$n_dose_events)) {
      d_time <- input[[paste0("dose_time_", i)]]
      d_amt  <- input[[paste0("dose_amount_", i)]]
      d_cmt  <- input[[paste0("dose_cmt_", i)]]
      d_addl <- as.numeric(input[[paste0("dose_addl_", i)]])
      d_ii   <- input[[paste0("dose_ii_", i)]]
      
      if (is.null(d_amt) || is.null(d_cmt)) next

      # Doses into a compartment with a model-defined D_<cmt> (Tk0) duration
      # only run as the intended zero-order infusion if rate == -2; otherwise
      # mrgsolve silently treats them as an instant bolus.
      ev_args <- list(time = d_time, amt = d_amt, cmt = d_cmt)
      if (d_cmt %in% model_data$sim_model_d_cmts) ev_args$rate <- -2
      if (!is.na(d_addl) && d_addl > 0) {
        ev_args$addl <- d_addl
        ev_args$ii   <- d_ii
      }
      events[[i]] <- do.call(ev, ev_args)
    }
    
    if (length(events) == 0) return(NULL)
    
    # Combine all events with c() — mrgsolve stacks them
    combined <- events[[1]]
    if (length(events) > 1) {
      for (j in 2:length(events)) {
        combined <- c(combined, events[[j]])
      }
    }
    return(combined)
  }
  
  # Run simulation
  observeEvent(input$run_simulation, {
    if (is.null(model_data$sim_model)) {
      model_data$sim_execution_status <- "✗ Model not loaded"
      return()
    }
    
    if (!isTRUE(model_data$sim_model_loaded)) {
      model_data$sim_execution_status <- "✗ Model not loaded"
      return()
    }
    
    tryCatch({
      model_data$sim_execution_status <- "Running simulation..."
      
      # Get simulation settings from UI
      end_time <- input$sim_end_time
      request_vars <- input$sim_request_vars
      
      if (length(request_vars) == 0) {
        model_data$sim_execution_status <- "✗ Please select at least one variable to request"
        return()
      }
      
      # Build combined dosing event from all dosing event inputs
      dose_event <- build_dose_event()
      if (is.null(dose_event)) {
        model_data$sim_execution_status <- "✗ No valid dosing events defined"
        return()
      }
      
      # Run simulation
      sim_results <- model_data$sim_model %>% 
        zero_re() %>% 
        mrgsim(event = dose_event, end = end_time, Request = request_vars, output = "df")
      
      model_data$sim_results <- sim_results
      model_data$sim_execution_status <- paste0("✓ Simulation completed successfully (", 
                                                 model_data$n_dose_events, " dosing event(s))")
    }, error = function(e) {
      model_data$sim_execution_status <- paste("✗ Simulation error:", e$message)
    })
  })
  
  # Display simulation status
  output$sim_model_status <- renderPrint({
    # Show the current model status (saved model path or load success/error)
    if (nchar(model_data$sim_model_status) > 0) {
      cat(model_data$sim_model_status)
    } else if (!is.null(model_data$saved_model_path) && file.exists(model_data$saved_model_path)) {
      cat(paste0("Saved model ready:\n", model_data$saved_model_path, "\n\nClick 'Load Model' to use it."))
    } else {
      cat("No saved model available.\nPlease upload a model file first.")
    }
  })
  
  output$sim_execution_status <- renderPrint({
    cat(model_data$sim_execution_status)
  })
  
  # Plot simulation results (interactive plotly)
  output$sim_plot <- renderPlotly({
    if (is.null(model_data$sim_results)) return(NULL)
    
    # Results are already a dataframe
    df <- model_data$sim_results
    
    # Get requested variables (exclude time and ID)
    vars_to_plot <- colnames(df)[!(colnames(df) %in% c("time", "ID", "id"))]
    
    if (length(vars_to_plot) == 0) {
      return(NULL)
    }
    
    # Get axis labels from input (with defaults)
    xaxis_label <- if (!is.null(input$sim_xaxis_label) && nchar(input$sim_xaxis_label) > 0) {
      input$sim_xaxis_label
    } else {
      "Time (hours)"
    }
    
    yaxis_label <- if (!is.null(input$sim_yaxis_label) && nchar(input$sim_yaxis_label) > 0) {
      input$sim_yaxis_label
    } else {
      if (length(vars_to_plot) == 1) vars_to_plot[1] else "Value"
    }
    
    # Get log scale options
    log_x <- !is.null(input$sim_log_x) && input$sim_log_x
    log_y <- !is.null(input$sim_log_y) && input$sim_log_y
    
    # Get custom plot title
    custom_title <- if (!is.null(input$sim_plot_title) && nchar(input$sim_plot_title) > 0) {
      input$sim_plot_title
    } else {
      "Simulation Results"
    }
    
    # Create color palette for multiple variables
    colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
    
    # Create plotly figure
    p <- plot_ly()
    
    for (i in seq_along(vars_to_plot)) {
      var <- vars_to_plot[i]
      color <- colors[(i - 1) %% length(colors) + 1]
      
      # Build hover text
      hover_text <- paste0(xaxis_label, ": ", round(df$time, 2),
                          "<br>", var, ": ", round(df[[var]], 4))
      
      p <- p %>% add_trace(
        x = df$time, 
        y = df[[var]], 
        type = 'scatter', 
        mode = 'lines',
        name = var,
        line = list(color = color, width = 2),
        hovertext = hover_text,
        hoverinfo = "text"
      )
    }
    
    # Configure layout with log scale options
    p <- p %>% layout(
      title = list(text = custom_title, x = 0.5),
      xaxis = list(
        title = xaxis_label,
        type = if (log_x) "log" else "linear",
        range = if (!is.na(input$sim_xmin) || !is.na(input$sim_xmax))
          list(input$sim_xmin, input$sim_xmax) else NULL
      ),
      yaxis = list(
        title = yaxis_label,
        type = if (log_y) "log" else "linear",
        range = if (!is.na(input$sim_ymin) || !is.na(input$sim_ymax))
          list(input$sim_ymin, input$sim_ymax) else NULL
      ),
      hovermode = "x unified",
      legend = list(orientation = "h", y = -0.15)
    )
    
    p
  })
  
  # Download simulation results
  output$download_sim_results <- downloadHandler(
    filename = function() {
      paste0("simulation_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (is.null(model_data$sim_results)) return()
      df <- model_data$sim_results
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # ============ PARAMETER SENSITIVITY ANALYSIS ============
  
  # Update sensitivity parameter dropdown when model is loaded
  observe({
    if (isTRUE(model_data$sim_model_loaded) && !is.null(model_data$sim_model)) {
      tryCatch({
        # Get parameters from the mrgsolve model
        params <- as.list(param(model_data$sim_model))
        param_names <- names(params)
        
        # Update parameter selection dropdown
        updateSelectInput(session, "sens_param_select", 
                         choices = param_names, 
                         selected = param_names[1])
        
        # Also update output variable dropdown with compartment + capture variables
        sens_outvars <- outvars(model_data$sim_model)
        output_vars  <- as.character(sens_outvars$capture)
        cmt_vars     <- as.character(sens_outvars$cmt)
        all_output_vars <- c(cmt_vars, output_vars)
        if (length(all_output_vars) > 0) {
          updateSelectInput(session, "sens_output_var",
                           choices = all_output_vars,
                           selected = all_output_vars[1])
        }
      }, error = function(e) {
        # Silently fail if parameter extraction fails
      })
    }
  })
  
  # Run sensitivity analysis
  observeEvent(input$run_sensitivity, {
    if (is.null(model_data$sim_model) || !isTRUE(model_data$sim_model_loaded)) {
      model_data$sens_execution_status <- "✗ Model not loaded"
      return()
    }
    
    param_name <- input$sens_param_select
    if (is.null(param_name) || param_name == "") {
      model_data$sens_execution_status <- "✗ Please select a parameter to vary"
      return()
    }
    
    output_var <- input$sens_output_var
    if (is.null(output_var) || output_var == "") {
      model_data$sens_execution_status <- "✗ Please select an output variable to plot"
      return()
    }
    
    tryCatch({
      model_data$sens_execution_status <- "Running sensitivity analysis..."
      
      # Get parameter variation settings
      param_min <- input$sens_param_min
      param_max <- input$sens_param_max
      n_steps <- input$sens_param_steps
      
      # Get current parameter value from model
      current_params <- as.list(param(model_data$sim_model))
      base_value <- current_params[[param_name]]
      
      # Calculate parameter values to test (n_steps values from min to max)
      # Then add the base value as an extra point
      param_values_range <- seq(base_value * param_min, base_value * param_max, length.out = n_steps)
      
      # Combine with base value and sort, keeping track of which is base
      param_values <- sort(unique(c(param_values_range, base_value)))
      base_idx_in_values <- which(param_values == base_value)
      
      # Get simulation parameters from UI (reuse from main simulation)
      end_time <- input$sim_end_time
      
      # Build combined dosing event from all dosing event inputs
      dose_event <- build_dose_event()
      if (is.null(dose_event)) {
        model_data$sens_execution_status <- "✗ No valid dosing events defined"
        return()
      }
      
      # Create idata with varying parameter values (length may be n_steps or n_steps+1 if base was added)
      n_scenarios <- length(param_values)
      idata <- data.frame(ID = 1:n_scenarios)
      idata[[param_name]] <- param_values
      
      # Run simulation with idata
      sim_results <- model_data$sim_model %>% 
        zero_re() %>% 
        mrgsim(event = dose_event, end = end_time, idata = idata, 
               Request = output_var, output = "df")
      
      # Store results with parameter info
      model_data$sens_results <- sim_results
      model_data$sens_param_name <- param_name
      model_data$sens_param_values <- param_values
      model_data$sens_output_var <- output_var
      model_data$sens_base_value <- base_value
      model_data$sens_base_idx <- base_idx_in_values
      
      model_data$sens_execution_status <- paste0("✓ Sensitivity analysis completed\n", 
                                                  "Parameter: ", param_name, 
                                                  "\nBase value: ", round(base_value, 4),
                                                  "\nRange: ", round(min(param_values), 4), " to ", round(max(param_values), 4),
                                                  "\nTotal scenarios: ", n_scenarios)
    }, error = function(e) {
      model_data$sens_execution_status <- paste("✗ Error:", e$message)
    })
  })
  
  # Display sensitivity model status
  output$sens_model_status <- renderPrint({
    if (isTRUE(model_data$sim_model_loaded)) {
      cat("✓ Model loaded and ready for sensitivity analysis")
    } else {
      cat("No model loaded. Please load a model in the Dosing Simulation tab.")
    }
  })
  
  # Display sensitivity execution status
  output$sens_execution_status <- renderPrint({
    cat(model_data$sens_execution_status)
  })
  
  # Plot sensitivity results (interactive plotly)
  output$sens_plot <- renderPlotly({
    if (is.null(model_data$sens_results)) return(NULL)
    
    df <- model_data$sens_results
    param_name <- model_data$sens_param_name
    param_values <- model_data$sens_param_values
    output_var <- model_data$sens_output_var
    base_value <- model_data$sens_base_value
    base_idx <- model_data$sens_base_idx
    
    # Get axis labels from sensitivity analysis inputs (with defaults)
    xaxis_label <- if (!is.null(input$sens_xaxis_label) && nchar(input$sens_xaxis_label) > 0) {
      input$sens_xaxis_label
    } else {
      "Time (hours)"
    }
    
    yaxis_label <- if (!is.null(input$sens_yaxis_label) && nchar(input$sens_yaxis_label) > 0) {
      input$sens_yaxis_label
    } else {
      output_var
    }
    
    # Create color palette (blue to red gradient)
    n_scenarios <- length(param_values)
    colors <- colorRampPalette(c("#0000FF", "#FF0000"))(n_scenarios)
    
    # Create legend labels
    legend_labels <- paste0(param_name, " = ", round(param_values, 4))
    if (!is.null(base_idx) && base_idx >= 1 && base_idx <= length(legend_labels)) {
      legend_labels[base_idx] <- paste0(legend_labels[base_idx], " (base)")
    }
    
    # Create plotly figure
    p <- plot_ly()
    
    for (i in 1:n_scenarios) {
      subset_df <- df[df$ID == i, ]
      param_val <- round(param_values[i], 4)
      is_base <- !is.null(base_idx) && i == base_idx
      
      # Build hover text
      hover_text <- paste0(xaxis_label, ": ", round(subset_df$time, 2),
                          "<br>", output_var, ": ", round(subset_df[[output_var]], 4),
                          "<br>", param_name, ": ", param_val,
                          if (is_base) " (base)" else "")
      
      # Make base line thicker and dashed
      line_width <- if (is_base) 3 else 2
      line_dash <- if (is_base) "dash" else "solid"
      
      p <- p %>% add_trace(
        x = subset_df$time, 
        y = subset_df[[output_var]], 
        type = 'scatter', 
        mode = 'lines',
        name = legend_labels[i],
        line = list(color = colors[i], width = line_width, dash = line_dash),
        hovertext = hover_text,
        hoverinfo = "text"
      )
    }
    
    # Configure layout
    p <- p %>% layout(
      title = list(text = paste0("Sensitivity Analysis: ", param_name), x = 0.5),
      xaxis = list(title = xaxis_label),
      yaxis = list(title = yaxis_label),
      hovermode = "x unified",
      legend = list(orientation = "v", x = 1.02, y = 1)
    )
    
    p
  })
  
  # Download sensitivity results
  output$download_sens_results <- downloadHandler(
    filename = function() {
      paste0("sensitivity_results_", model_data$sens_param_name, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (is.null(model_data$sens_results)) return()
      
      # Add parameter value column for clarity
      df <- model_data$sens_results
      param_values <- model_data$sens_param_values
      df$param_value <- param_values[df$ID]
      df$param_name <- model_data$sens_param_name
      
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # ============ VPC TAB LOGIC ============
  
  # Update column mapping dropdowns when project is loaded
  observe({
    if (model_data$project_loaded) {
      # Get all column headers from the dataset
      tryCatch({
        data_info_vpc <- getData()
        data_headers  <- data_info_vpc$header
        header_types  <- data_info_vpc$headerTypes

        # Helper function to find best match for a column
        find_column <- function(headers, standard_name) {
          # Check exact match (case-insensitive)
          match_idx <- which(toupper(headers) == standard_name)
          if (length(match_idx) > 0) return(headers[match_idx[1]])
          return(headers[1])  # Default to first column
        }

        # Exclude columns typed as "ignore" — they are dropped by prepare_monolix_dataset
        # and would cause vpc::check_stratification_columns_available to fail
        strat_candidates <- data_headers[header_types != "ignore"]

        # Update stratification dropdown
        updateSelectInput(session, "vpc_potential_stratify_vars",
                         choices = strat_candidates,
                         selected = NULL)
        
        # Update column mapping dropdowns
        # ID column
        id_selected <- find_column(data_headers, "ID")
        updateSelectInput(session, "vpc_id_column", choices = data_headers, selected = id_selected)
        
        # TIME column
        time_selected <- find_column(data_headers, "TIME")
        updateSelectInput(session, "vpc_time_column", choices = data_headers, selected = time_selected)
        
        # DV column
        dv_selected <- find_column(data_headers, "DV")
        updateSelectInput(session, "vpc_dv_column", choices = data_headers, selected = dv_selected)
        
        # AMT column (optional — observation-only datasets have no dosing)
        amt_match <- which(toupper(data_headers) == "AMT")
        amt_selected <- if (length(amt_match) > 0) data_headers[amt_match[1]] else ""
        updateSelectInput(session, "vpc_amt_column", choices = c("", data_headers), selected = amt_selected)
        
        # Optional columns (with empty option)
        headers_with_empty <- c("", data_headers)
        
        # EVID column
        evid_selected <- if ("EVID" %in% toupper(data_headers)) data_headers[which(toupper(data_headers) == "EVID")[1]] else ""
        updateSelectInput(session, "vpc_evid_column", choices = headers_with_empty, selected = evid_selected)
        
        # DVID column
        dvid_selected <- if ("DVID" %in% toupper(data_headers)) data_headers[which(toupper(data_headers) == "DVID")[1]] else ""
        updateSelectInput(session, "vpc_dvid_column", choices = headers_with_empty, selected = dvid_selected)
        
        # RATE column
        rate_selected <- if ("RATE" %in% toupper(data_headers)) data_headers[which(toupper(data_headers) == "RATE")[1]] else ""
        updateSelectInput(session, "vpc_rate_column", choices = headers_with_empty, selected = rate_selected)
        
        # II column
        ii_selected <- if ("II" %in% toupper(data_headers)) data_headers[which(toupper(data_headers) == "II")[1]] else ""
        updateSelectInput(session, "vpc_ii_column", choices = headers_with_empty, selected = ii_selected)
        
        # ADDL column
        addl_selected <- if ("ADDL" %in% toupper(data_headers)) data_headers[which(toupper(data_headers) == "ADDL")[1]] else ""
        updateSelectInput(session, "vpc_addl_column", choices = headers_with_empty, selected = addl_selected)

        # TAD column (optional)
        tad_selected <- if ("TAD" %in% toupper(data_headers)) data_headers[which(toupper(data_headers) == "TAD")[1]] else ""
        updateSelectInput(session, "vpc_tad_column", choices = headers_with_empty, selected = tad_selected)

        # BQL/Censoring column (optional — labels vary: BQL, BLQ, CENS, CENSOR)
        bql_candidates <- c("BQL", "BLQ", "CENS", "CENSOR")
        bql_idx        <- which(toupper(data_headers) %in% bql_candidates)
        bql_selected   <- if (length(bql_idx) > 0) data_headers[bql_idx[1]] else ""
        updateSelectInput(session, "vpc_bql_column", choices = headers_with_empty, selected = bql_selected)

        # Columns to ignore dropdown
        updateSelectizeInput(session, "vpc_ignore_columns", choices = data_headers, selected = NULL)
        
      }, error = function(e) {
        # Fallback to categorical covariates if getData() fails
        if (!is.null(model_data$cat_covariate_names)) {
          updateSelectInput(session, "vpc_potential_stratify_vars", 
                           choices = model_data$cat_covariate_names,
                           selected = NULL)
        }
      })
    }
  })

  # Auto-populate column mapping from NONMEM control file dataset (NONMEM mode)
  nm_vpc_csv_path <- reactive({
    ctl_path <- nm_ctlFile()
    if (!nchar(ctl_path) || !file.exists(ctl_path)) return(NULL)
    parse_nonmem_datafile(readLines(ctl_path, warn = FALSE),
                          ctl_dir = dirname(ctl_path))
  })

  output$vpc_nm_dataset_status <- renderText({
    csv_path <- nm_vpc_csv_path()
    if (is.null(csv_path))
      return("No NONMEM control file loaded. Please complete a NONMEM translation first.")
    if (!file.exists(csv_path))
      return(paste("Dataset not found:\n", csv_path))
    paste("Dataset:", csv_path)
  })

  observeEvent(list(nm_vpc_csv_path(), input$vpc_data_source), {
    csv_path <- nm_vpc_csv_path()
    if (is.null(csv_path) || !file.exists(csv_path)) return()
    if (input$vpc_data_source != "nonmem") return()
    tryCatch({
      # Strip character rows (IGNORE=@ / IGNORE=C) before reading headers
      ctl_path <- nm_ctlFile()
      nm_ig <- if (nchar(ctl_path) > 0 && file.exists(ctl_path))
        parse_nonmem_ignore(readLines(ctl_path, warn = FALSE))
      else
        list(ignore_chars = character(0), ignore_nonnumeric = FALSE)
      raw_lines  <- readLines(csv_path, warn = FALSE)
      header_idx <- which(nchar(trimws(raw_lines)) > 0)[1]
      body       <- raw_lines[-seq_len(header_idx)]
      body       <- apply_nonmem_ignore_rules(body, nm_ig$ignore_chars, nm_ig$ignore_nonnumeric)
      header_text <- paste(c(raw_lines[seq_len(header_idx)], body[1]), collapse = "\n")
      csv_headers <- names(data.table::fread(text = header_text, nrows = 0, na.strings = "."))
      headers_with_empty <- c("", csv_headers)
      find_col <- function(h, name) {
        idx <- which(toupper(h) == name)
        if (length(idx) > 0) h[idx[1]] else h[1]
      }
      col_sel <- function(h, name) {
        idx <- which(toupper(h) == name)
        if (length(idx) > 0) h[idx[1]] else ""
      }
      updateSelectInput(session, "vpc_id_column",
                        choices = csv_headers,
                        selected = find_col(csv_headers, "ID"))
      updateSelectInput(session, "vpc_time_column",
                        choices = csv_headers,
                        selected = find_col(csv_headers, "TIME"))
      updateSelectInput(session, "vpc_dv_column",
                        choices = csv_headers,
                        selected = find_col(csv_headers, "DV"))
      updateSelectInput(session, "vpc_amt_column",
                        choices = headers_with_empty,
                        selected = col_sel(csv_headers, "AMT"))
      updateSelectInput(session, "vpc_evid_column",
                        choices = headers_with_empty,
                        selected = col_sel(csv_headers, "EVID"))
      updateSelectInput(session, "vpc_dvid_column",
                        choices = headers_with_empty,
                        selected = col_sel(csv_headers, "CMT"))
      updateSelectInput(session, "vpc_rate_column",
                        choices = headers_with_empty,
                        selected = col_sel(csv_headers, "RATE"))
      updateSelectInput(session, "vpc_ii_column",
                        choices = headers_with_empty,
                        selected = col_sel(csv_headers, "II"))
      updateSelectInput(session, "vpc_addl_column",
                        choices = headers_with_empty,
                        selected = col_sel(csv_headers, "ADDL"))
      updateSelectInput(session, "vpc_tad_column",
                        choices = headers_with_empty,
                        selected = col_sel(csv_headers, "TAD"))
      col_sel_multi <- function(h, candidates) {
        idx <- which(toupper(h) %in% candidates)
        if (length(idx) > 0) h[idx[1]] else ""
      }
      updateSelectInput(session, "vpc_bql_column",
                        choices = headers_with_empty,
                        selected = col_sel_multi(csv_headers, c("BQL", "BLQ", "CENS", "CENSOR")))
      updateSelectizeInput(session, "vpc_ignore_columns",
                           choices = csv_headers, selected = NULL)
      updateSelectInput(session, "vpc_potential_stratify_vars",
                        choices = csv_headers, selected = NULL)
    }, error = function(e) {
      model_data$vpc_sim_status <- paste("✗ Error reading dataset headers:", e$message)
    })
  })

  # Load VPC model
  observeEvent(input$load_vpc_model, {
    if (is.null(model_data$saved_model_path) || !file.exists(model_data$saved_model_path)) {
      model_data$vpc_model_status <- "✗ No saved model available. Please save a translated model first."
      return()
    }
    
    tryCatch({
      if (!require(mrgsolve)) {
        model_data$vpc_model_status <- "✗ mrgsolve package not installed."
        return()
      }
      
      # Load the model
      vpc_model <- mread(model_data$saved_model_path, quiet = TRUE)
      model_data$vpc_model <- vpc_model
      model_data$vpc_model_loaded <- TRUE
      model_data$vpc_model_path <- model_data$saved_model_path
      
      model_data$vpc_model_status <- paste0("✓ Model loaded successfully from:\n", model_data$saved_model_path)
      
    }, error = function(e) {
      model_data$vpc_model_status <- paste("✗ Error loading model:", e$message)
    })
  })
  
  # Run VPC simulation (includes dataset preparation)
  observeEvent(input$run_vpc_sim, {
    if (!model_data$vpc_model_loaded) {
      model_data$vpc_sim_status <- "✗ Model not loaded. Please load model first."
      return()
    }

    if (input$vpc_data_source == "monolix" && !model_data$project_loaded) {
      model_data$vpc_sim_status <- "✗ No project loaded. Please load a Monolix project first."
      return()
    }
    
    # traceback() reports nothing from inside a tryCatch() error handler: it
    # only has a stack to print once an error has reached the top level
    # uncaught, which is why every failure this observer catches used to
    # report "No traceback available". Capture the stack at signal time with
    # withCallingHandlers(), which runs before the stack unwinds, and stash it
    # for the tryCatch() handler at the bottom of this block.
    sim_trace   <- NULL
    sge_log_dir <- NULL
    tryCatch(withCallingHandlers({
      model_data$vpc_sim_status <- "Preparing dataset..."

      # ── For NONMEM: parse control file IGNORE rules before loading data ───────
      nm_ignore <- list(conditions = character(0), ignore_chars = character(0))
      if (input$vpc_data_source == "nonmem") {
        ctl_path <- nm_ctlFile()
        if (nchar(ctl_path) > 0 && file.exists(ctl_path))
          nm_ignore <- parse_nonmem_ignore(readLines(ctl_path, warn = FALSE))
      }

      # ── For Monolix: parse .mlxtran [FILTER] block before loading data ────────
      mlx_filters <- list(conditions = character(0), description = character(0))
      if (input$vpc_data_source == "monolix")
        mlx_filters <- parse_mlx_dataset_filters(model_data$mlxtran_path, id_col = mlx_identifier_col())

      # ── Load dataset ──────────────────────────────────────────────────────────
      if (input$vpc_data_source == "nonmem") {
        csv_path <- nm_vpc_csv_path()
        if (is.null(csv_path) || !file.exists(csv_path)) {
          model_data$vpc_sim_status <- "✗ Dataset not found. Please complete a NONMEM translation first."
          return()
        }
        # Pre-filter rows matching IGNORE=char (e.g. IGNORE=@, IGNORE=C) before
        # fread so those rows never corrupt column types
        needs_prefilter <- isTRUE(nm_ignore$ignore_nonnumeric) ||
                           length(nm_ignore$ignore_chars) > 0
        if (needs_prefilter) {
          raw_lines  <- readLines(csv_path, warn = FALSE)
          # Header line (first non-empty line) must be preserved intact for fread
          header_idx <- which(nchar(trimws(raw_lines)) > 0)[1]
          body       <- raw_lines[-seq_len(header_idx)]
          body       <- apply_nonmem_ignore_rules(body, nm_ignore$ignore_chars, nm_ignore$ignore_nonnumeric)
          input_data <- data.table::fread(
            text = paste(c(raw_lines[seq_len(header_idx)], body), collapse = "\n"),
            na.strings = "."
          ) %>% as_tibble()
        } else {
          input_data <- data.table::fread(csv_path, na.strings = ".") %>% as_tibble()
        }
      } else {
        # Get dataset from Monolix (fread auto-detects delimiter for csv/txt files)
        input_data_file <- getData()$dataFile
        input_data <- data.table::fread(input_data_file, na.strings = ".") %>% as_tibble()
      }

      # ── Build column map + dataset prep opts ─────────────────────────────────
      mode <- if (input$vpc_data_source == "monolix") "monolix" else "nonmem"

      column_map <- list(
        ID   = input$vpc_id_column,
        TIME = input$vpc_time_column,
        DV   = input$vpc_dv_column,
        AMT  = input$vpc_amt_column,
        EVID = input$vpc_evid_column,
        RATE = input$vpc_rate_column,
        II   = input$vpc_ii_column,
        ADDL = input$vpc_addl_column,
        TAD  = input$vpc_tad_column,
        BQL  = input$vpc_bql_column
      )
      # NONMEM: user-selected column becomes CMT (keeps mrgsolve routing intact).
      # Monolix: column becomes DVID (obsid handling in prepare_monolix_dataset).
      if (mode == "nonmem") {
        column_map$CMT  <- input$vpc_dvid_column
      } else {
        column_map$DVID <- input$vpc_dvid_column
      }
      column_map <- column_map[sapply(column_map, function(v) !is.null(v) && nchar(v) > 0)]

      opts <- list(
        ignore_columns           = input$vpc_ignore_columns,
        nonmem_ignore_conditions = nm_ignore$conditions
      )

      if (mode == "monolix") {
        structural_model_path <- getStructuralModel()
        structural_model_text <- readLines(resolve_lib_model_path(structural_model_path))
        opts$mlxtran_lines  <- stringr::str_squish(
          stringr::str_replace(structural_model_text, ";.*$", ""))
        opts$covariate_info <- getCovariateInformation()
        opts$data_info      <- getData()
        opts$dvid_map       <- model_data$dvid_map
        opts$has_dvid       <- model_data$has_dvid
        opts$bql_col        <- input$vpc_bql_column
        opts$mlx_filter_conditions <- mlx_filters$conditions
      }

      # ── Prepare dataset via vpc_utils.R ───────────────────────────────────────
      model_data$vpc_sim_status <- "[step 1/3] Preparing dataset..."
      prep_result               <- prep_vpc_dataset(input_data, column_map, mode = mode, opts = opts)
      input_data_for_sim        <- prep_result$data
      cat_covariate_all_categories <- prep_result$cat_covariate_all_categories

      model_data$vpc_dataset_prepared             <- TRUE
      model_data$vpc_cat_covariate_all_categories <- cat_covariate_all_categories
      
      # Extract available DVIDs
      if ("DVID" %in% names(input_data_for_sim)) {
        dvids <- unique(input_data_for_sim$DVID[!is.na(input_data_for_sim$DVID)])
        dvids <- sort(dvids)
        model_data$vpc_available_dvids <- dvids

        # Build choice labels with meaningful names if DVID map exists
        if (!isTRUE(model_data$has_dvid) || is.null(model_data$dvid_map)) {
          # Single observation - no special labels needed
          dvid_choices <- dvids
        } else {
          # Multiple observations: only label observation DVIDs (exclude dosing DVID=0)
          obs_dvids <- dvids[dvids != 0]
          dvid_map <- model_data$dvid_map
          # Only apply setNames if lengths match (guard against mismatch)
          if (length(obs_dvids) == nrow(dvid_map)) {
            obs_choices <- setNames(
              obs_dvids,
              paste0(dvid_map$dvid_identifier, " (DVID=", dvid_map$mrgsolve_dvid, ")")
            )
          } else {
            obs_choices <- obs_dvids
          }
          dvid_choices <- c(obs_choices)
        }

        updateSelectInput(session, "vpc_dvid_select", choices = dvid_choices, selected = dvid_choices[1])
      }


      # ── Potential stratification variables ────────────────────────────────────
      potential_stratify_vars <- input$vpc_potential_stratify_vars
      if (is.null(potential_stratify_vars) || length(potential_stratify_vars) == 0) {
        potential_stratify_vars <- NULL
      }
      model_data$vpc_potential_stratify_vars <- potential_stratify_vars

      carry_list <- unique(c("DVID", "ROW", potential_stratify_vars))

      # Include TAD in carry if the user has mapped a TAD column
      tad_col <- input$vpc_tad_column %||% ""
      if (nchar(tad_col) > 0) carry_list <- unique(c(carry_list, "TAD"))

      # ── Run simulations via vpc_utils.R ───────────────────────────────────────
      n_sims        <- input$vpc_n_sims
      parallel_mode <- vpc_parallel_mode(input)
      n_jobs        <- if (parallel_mode == "hpc") input$vpc_n_jobs else NULL
      seed          <- input$vpc_seed
      # Resolved before the call so the error handler can point the user at
      # the worker logs even when run_vpc_sim() never returns.
      sge_log_dir <- if (parallel_mode == "hpc")
        vpc_sge_log_dir(model_data$vpc_model_path) else NULL
      # Name the computation path in the status line, not just the mode: in a
      # regulated context "it ran" is not enough, the reader has to be able to
      # tell from the log whether any job submission happened.
      where_msg <- if (parallel_mode == "hpc")
        paste0("SGE, ", n_jobs, " jobs") else "sequentially in this R session"
      model_data$vpc_sim_status <- paste0("[step 2/3] Running ", n_sims,
                                          " simulations (", where_msg, ")...")
      sim_result <- run_vpc_sim(
        mod_path      = model_data$vpc_model_path,
        data          = input_data_for_sim,
        n_rep         = n_sims,
        n_jobs        = n_jobs,
        carry_list    = carry_list,
        parallel_mode = parallel_mode,
        seed          = seed,
        log_dir       = sge_log_dir
      )

      # ── Store results ─────────────────────────────────────────────────────────
      model_data$vpc_sim_status  <- "[step 3/3] Merging results..."
      model_data$vpc_sim_results <- sim_result$sim_df
      model_data$vpc_dataset     <- sim_result$obs_df   # includes PRED

      # ── Update stratification UI ──────────────────────────────────────────────
      if (!is.null(potential_stratify_vars) && length(potential_stratify_vars) > 0) {
        updateSelectInput(session, "vpc_stratify_vars",
                         choices  = c("None" = "", potential_stratify_vars),
                         selected = "")
      } else {
        updateSelectInput(session, "vpc_stratify_vars",
                         choices = c("None" = ""), selected = "")
      }

      strat_msg <- if (!is.null(potential_stratify_vars) && length(potential_stratify_vars) > 0) {
        paste0("  Stratification covariates: ", paste(potential_stratify_vars, collapse = ", "))
      } else {
        "  Stratification covariates: None"
      }

      filter_msg <- if (length(mlx_filters$description) > 0) {
        paste0("  Filters (auto): ", paste(mlx_filters$description, collapse = "; "))
      } else if (mode == "monolix") {
        "  Filters (auto): none detected"
      } else {
        NULL
      }

      model_data$vpc_sim_status <- paste0(
        "✓ VPC simulations completed\n",
        "  Executed: ",              where_msg, "\n",
        "  Dataset rows: ",          nrow(model_data$vpc_dataset), "\n",
        "  Simulations: ",           n_sims, "\n",
        "  Total simulation rows: ", nrow(sim_result$sim_df), "\n",
        "  Available DVIDs: ",       paste(model_data$vpc_available_dvids, collapse = ", "), "\n",
        if (!is.null(filter_msg)) paste0(filter_msg, "\n") else "",
        strat_msg
      )
      
    }, error = function(e) {
      # Keep the deepest frames: those are the informative ones. The outer
      # Shiny observer/reactive frames are noise. Guarded so that a failure
      # to capture the trace cannot replace the original error.
      sim_trace <<- tryCatch(
        paste(tail(format(rlang::trace_back()), 40L), collapse = "\n"),
        error = function(e2) "Traceback capture failed."
      )
    }), error = function(e) {
      msg      <- conditionMessage(e)
      cl       <- conditionCall(e)
      tb       <- if (is.null(sim_trace)) "No traceback captured." else sim_trace
      call_txt <- if (is.null(cl)) "" else
        paste0("\nFailing call: ", paste(deparse(cl), collapse = " "))
      log_txt  <- if (is.null(sge_log_dir)) "" else
        paste0("\nSGE worker logs: ", sge_log_dir)
      full_msg <- paste0("✗ Error: ", msg, call_txt, log_txt,
                         "\n\nTraceback:\n", tb)
      message("[VPC sim error] ", msg, "\n", tb)
      model_data$vpc_sim_status <- full_msg
    })
  })

  # Generate VPC plot
  observeEvent(input$generate_vpc_plot, {
    if (is.null(model_data$vpc_sim_results)) {
      model_data$vpc_plot_status <- "✗ No simulation results. Please run VPC simulation first."
      return()
    }
    
    if (is.null(input$vpc_dvid_select)) {
      model_data$vpc_plot_status <- "✗ Please select a DVID to display."
      return()
    }
    
    tryCatch({
      if (!require(vpc)) {
        model_data$vpc_plot_status <- "✗ vpc package not installed. Install with: install.packages('vpc')"
        return()
      }
      
      model_data$vpc_plot_status <- "Generating VPC plot..."
      
      selected_dvid <- as.numeric(input$vpc_dvid_select)
      stratify_vars <- input$vpc_stratify_vars
      
      # Check if stratification variables are selected
      if (is.null(stratify_vars) || length(stratify_vars) == 0) {
        stratify_vars <- NULL
      }
      
      # Filter data for selected DVID
      sim_data <- model_data$vpc_sim_results %>% filter(DVID == selected_dvid, !is.na(Y))
      obs_data <- model_data$vpc_dataset %>% 
        filter(DVID == selected_dvid, !is.na(DV))
      
      # Get LLOQ value (use NULL if NA)
      lloq_value <- if (is.na(input$vpc_lloq)) NULL else input$vpc_lloq
      
      # Check if censored data handling is needed (based on LLOQ)
      has_censored_data <- !is.null(lloq_value) && !is.na(lloq_value)
      
      # Process stratification variables (handle both categorical and continuous)
      stratify_var_final <- c()
      
      if (!is.null(stratify_vars) && length(stratify_vars) > 0) {
        for (i in seq_along(stratify_vars)) {
          var_name <- stratify_vars[i]
          
          # Get covariate type and binning method from dynamic inputs
          cov_type <- input[[paste0("vpc_cov_type_", i)]]
          bin_method <- input[[paste0("vpc_cov_bin_", i)]]
          custom_breaks_str <- input[[paste0("vpc_cov_breaks_", i)]]
          
          # Default to categorical if type not yet set (UI not rendered yet)
          if (is.null(cov_type)) cov_type <- "categorical"
          
          if (cov_type == "continuous" && var_name %in% names(obs_data) && is.numeric(obs_data[[var_name]])) {
            # Continuous covariate - apply binning
            binned_col <- paste0(var_name, "_f")
            
            # Get distinct subject-level covariate values
            x_ref <- obs_data %>% 
              distinct(ID, .keep_all = TRUE) %>% 
              pull(!!sym(var_name))
            
            # Get min and max for breaks
            min_val <- floor(min(x_ref, na.rm = TRUE))
            max_val <- ceiling(max(x_ref, na.rm = TRUE))
            
            # Determine breaks based on method
            if (is.null(bin_method)) bin_method <- "median"
            
            if (bin_method == "median") {
              med_val <- median(x_ref, na.rm = TRUE)
              breaks <- c(min_val, med_val, max_val)
            } else if (bin_method == "quartile") {
              q_vals <- quantile(x_ref, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
              breaks <- c(min_val, q_vals, max_val)
            } else if (bin_method == "customized") {
              # Parse custom breaks from user input
              if (!is.null(custom_breaks_str) && custom_breaks_str != "") {
                custom_breaks <- as.numeric(unlist(strsplit(gsub(" ", "", custom_breaks_str), ",")))
                custom_breaks <- custom_breaks[!is.na(custom_breaks)]
                if (length(custom_breaks) > 0) {
                  breaks <- c(min_val, sort(custom_breaks), max_val)
                } else {
                  breaks <- c(min_val, median(x_ref, na.rm = TRUE), max_val)
                }
              } else {
                breaks <- c(min_val, median(x_ref, na.rm = TRUE), max_val)
              }
            } else {
              breaks <- c(min_val, median(x_ref, na.rm = TRUE), max_val)
            }
            
            # Apply cut to both datasets
            obs_data[[binned_col]] <- cut(obs_data[[var_name]], 
                                          breaks = breaks, 
                                          include.lowest = TRUE)
            sim_data[[binned_col]] <- cut(sim_data[[var_name]], 
                                          breaks = breaks, 
                                          include.lowest = TRUE)
            
            stratify_var_final <- c(stratify_var_final, binned_col)
          } else {
            # Categorical covariate - use as is
            stratify_var_final <- c(stratify_var_final, var_name)
          }
        }
      }
      
      # If no valid stratification variables, set to NULL
      if (length(stratify_var_final) == 0) {
        stratify_var_final <- NULL
      }

      # Decode numeric categorical stratification columns back to original labels
      # (they were encoded to numeric earlier for mrgsolve; labels needed for strip names)
      cat_all_cats <- model_data$vpc_cat_covariate_all_categories
      if (!is.null(stratify_var_final) && !is.null(cat_all_cats) && length(cat_all_cats) > 0) {
        for (var_name in stratify_var_final) {
          if (var_name %in% names(cat_all_cats)) {
            all_cats <- cat_all_cats[[var_name]]
            decode_cat <- function(x) {
              idx <- as.integer(x) + 1L
              ifelse(is.na(idx) | idx < 1L | idx > length(all_cats), as.character(x), all_cats[idx])
            }
            obs_data[[var_name]] <- decode_cat(obs_data[[var_name]])
            sim_data[[var_name]] <- decode_cat(sim_data[[var_name]])
          }
        }
      }

      # Store processed data for plot rendering
      model_data$vpc_sim_data <- sim_data
      model_data$vpc_obs_data <- obs_data
      model_data$vpc_has_censored_data <- has_censored_data
      model_data$vpc_stratify_vars <- stratify_var_final
      model_data$vpc_lloq_value <- lloq_value
      model_data$vpc_selected_dvid <- selected_dvid
      
      model_data$vpc_plot_status <- paste0("✓ VPC plot generated for DVID ", selected_dvid)
      
    }, error = function(e) {
      model_data$vpc_plot_status <- paste("✗ Error generating VPC plot:", e$message)
    })
  })
  
  # Dynamic UI for stratification covariate configuration
  output$vpc_stratify_config <- renderUI({
    selected_vars <- input$vpc_stratify_vars
    
    if (is.null(selected_vars) || length(selected_vars) == 0) {
      return(NULL)
    }
    
    # Create configuration inputs for each selected covariate
    config_list <- lapply(seq_along(selected_vars), function(i) {
      var_name <- selected_vars[i]
      var_id_type <- paste0("vpc_cov_type_", i)
      var_id_bin <- paste0("vpc_cov_bin_", i)
      var_id_breaks <- paste0("vpc_cov_breaks_", i)
      
      tagList(
        tags$div(
          style = "border: 1px solid #ddd; padding: 8px; margin-bottom: 8px; border-radius: 4px;",
          tags$strong(var_name),
          fluidRow(
            column(6, selectInput(var_id_type, "Type:", 
                                 choices = c("categorical", "continuous"), 
                                 selected = "categorical")),
            column(6, conditionalPanel(
              condition = paste0("input.", var_id_type, " == 'continuous'"),
              selectInput(var_id_bin, "Binning:", 
                         choices = c("median", "quartile", "customized"), 
                         selected = "median")
            ))
          ),
          conditionalPanel(
            condition = paste0("input.", var_id_type, " == 'continuous' && input.", var_id_bin, " == 'customized'"),
            textInput(var_id_breaks, "Breaks:", value = "", placeholder = "e.g., 30, 50, 70")
          )
        )
      )
    })
    
    do.call(tagList, config_list)
  })
  
  # Display VPC model status
  output$vpc_model_status <- renderPrint({
    cat(model_data$vpc_model_status)
  })
  
  # Display VPC simulation status
  output$vpc_sim_status <- renderPrint({
    cat(model_data$vpc_sim_status)
  })
  
  # Display VPC plot status
  output$vpc_plot_status <- renderPrint({
    cat(model_data$vpc_plot_status)
  })

  # Dynamic strip label customization inputs — one text field per stratification level
  output$vpc_strip_names_ui <- renderUI({
    strat_vars <- input$vpc_stratify_vars
    if (is.null(strat_vars) || length(strat_vars) == 0) return(NULL)
    if (is.null(model_data$vpc_obs_data)) return(NULL)

    obs_data <- model_data$vpc_obs_data

    panels <- lapply(seq_along(strat_vars), function(i) {
      var_name    <- strat_vars[i]
      binned_col  <- paste0(var_name, "_f")
      eff_col     <- if (binned_col %in% names(obs_data)) binned_col else var_name
      if (!eff_col %in% names(obs_data)) return(NULL)

      lvls <- sort(unique(as.character(obs_data[[eff_col]])))
      lvls <- lvls[!is.na(lvls) & lvls != "NA"]
      if (length(lvls) == 0) return(NULL)

      level_inputs <- lapply(seq_along(lvls), function(j) {
        column(6,
          textInput(
            paste0("vpc_strip_", make.names(var_name), "_", j),
            label = paste0(eff_col, " = ", lvls[j], ":"),
            value = lvls[j]
          )
        )
      })
      tagList(
        tags$b(paste0("Strip labels — ", eff_col)),
        fluidRow(do.call(tagList, level_inputs)),
        br()
      )
    })

    panels <- panels[!sapply(panels, is.null)]
    if (length(panels) == 0) return(NULL)

    tagList(
      hr(),
      h5(tags$b("Strip Label Customization")),
      helpText("Override facet strip labels. Changes appear immediately in the plot."),
      do.call(tagList, panels)
    )
  })

  # Render VPC plot — only fires on "Generate VPC Plot" or "Update Plot" button clicks
  output$vpc_plot <- renderPlot({
    if (is.null(model_data$vpc_sim_data) || is.null(model_data$vpc_obs_data)) {
      plot.new()
      text(0.5, 0.5, "No VPC plot available. Please generate a plot first.", cex = 1.2)
      return()
    }

    sim_data          <- model_data$vpc_sim_data
    obs_data          <- model_data$vpc_obs_data
    stratify_var_final <- model_data$vpc_stratify_vars
    if (is.null(stratify_var_final) || length(stratify_var_final) == 0 ||
        all(stratify_var_final == "")) {
      stratify_var_final <- NULL
    }

    # Apply custom strip label overrides (data already decoded by generate_vpc_plot step)
    strat_orig <- input$vpc_stratify_vars
    if (!is.null(stratify_var_final) && !is.null(strat_orig)) {
      for (i in seq_along(strat_orig)) {
        var_name   <- strat_orig[i]
        binned_col <- paste0(var_name, "_f")
        eff_col    <- if (binned_col %in% stratify_var_final) binned_col else var_name
        if (!eff_col %in% names(obs_data)) next

        lvls <- sort(unique(as.character(obs_data[[eff_col]])))
        lvls <- lvls[!is.na(lvls) & lvls != "NA"]

        for (j in seq_along(lvls)) {
          custom_label <- input[[paste0("vpc_strip_", make.names(var_name), "_", j)]]
          if (!is.null(custom_label) && nchar(trimws(custom_label)) > 0 &&
              custom_label != lvls[j]) {
            obs_data[[eff_col]] <- ifelse(
              as.character(obs_data[[eff_col]]) == lvls[j],
              custom_label, as.character(obs_data[[eff_col]]))
            sim_data[[eff_col]] <- ifelse(
              as.character(sim_data[[eff_col]]) == lvls[j],
              custom_label, as.character(sim_data[[eff_col]]))
          }
        }
      }
    }

    # PI/CI from percentage selectors
    pi_pct  <- as.numeric(input$vpc_pi_percent)
    ci_pct  <- as.numeric(input$vpc_ci_percent)
    pi_vals <- c((1 - pi_pct/100) / 2, 1 - (1 - pi_pct/100) / 2)
    ci_vals <- c((1 - ci_pct/100) / 2, 1 - (1 - ci_pct/100) / 2)

    # Axis limits
    xlim_vals <- c(input$vpc_xmin, input$vpc_xmax)
    ylim_vals <- c(input$vpc_ymin, input$vpc_ymax)

    # Resolve bins — early return for incomplete customized input
    bin_val <- input$vpc_bin_method
    if (bin_val == "customized") {
      custom_str <- input$vpc_custom_bins
      if (!is.null(custom_str) && nchar(trimws(custom_str)) > 0) {
        parsed <- suppressWarnings(
          as.numeric(unlist(strsplit(gsub(" ", "", custom_str), ","))))
        parsed <- parsed[!is.na(parsed)]
        if (length(parsed) < 2) {
          plot.new()
          text(0.5, 0.5, "Please enter at least 2 bin values (e.g., 0, 100, 500, 1000)", cex = 1.2)
          return()
        }
        bin_val <- parsed
      } else {
        plot.new()
        text(0.5, 0.5, "Please enter custom bin values (e.g., 0, 100, 500, 1000)", cex = 1.2)
        return()
      }
    }

    # Stratify spec: columns already binned + decoded → treat all as categorical
    stratify_cfg <- if (!is.null(stratify_var_final)) {
      setNames(lapply(stratify_var_final, function(v) list(type = "categorical")),
               stratify_var_final)
    } else {
      list()
    }

    # Build plot title
    plot_title <- paste0("VPC for DVID ", model_data$vpc_selected_dvid)
    if (!is.null(stratify_var_final) && length(stratify_var_final) > 0) {
      plot_title <- paste0(plot_title,
                           " (stratified by ", paste(stratify_var_final, collapse = ", "), ")")
    }

    vpc_spec <- build_vpc_plot_spec(list(
      title       = plot_title,
      time_column = if (isTRUE(input$vpc_tad_axis)) "TAD" else "TIME",
      logY     = isTRUE(input$vpc_log_y),
      predCorr = isTRUE(input$vpc_pred_corr),
      smooth   = isTRUE(input$vpc_smooth),
      lloq     = model_data$vpc_lloq_value,
      censor   = isTRUE(model_data$vpc_has_censored_data),
      show_legend = isTRUE(input$vpc_show_legend %||% TRUE),
      pi       = pi_vals,
      ci       = ci_vals,
      bins     = bin_val,
      scales   = input$vpc_scales %||% "free_x",
      stratify = stratify_cfg,
      x_label  = if (!is.null(input$vpc_x_label) && nchar(trimws(input$vpc_x_label)) > 0)
                   input$vpc_x_label else "Time",
      y_label  = if (!is.null(input$vpc_y_label) && nchar(trimws(input$vpc_y_label)) > 0)
                   input$vpc_y_label else "Concentration",
      xlim     = if (!all(is.na(xlim_vals))) xlim_vals else NULL,
      ylim     = if (!all(is.na(ylim_vals))) ylim_vals else NULL,
      x_breaks = {
        xb <- trimws(input$vpc_x_breaks %||% "")
        if (nchar(xb) > 0) {
          parsed <- suppressWarnings(as.numeric(unlist(strsplit(gsub(" ", "", xb), ","))))
          sort(parsed[!is.na(parsed)])
        } else NULL
      },
      y_breaks = {
        yb <- trimws(input$vpc_y_breaks %||% "")
        if (nchar(yb) > 0) {
          parsed <- suppressWarnings(as.numeric(unlist(strsplit(gsub(" ", "", yb), ","))))
          sort(parsed[!is.na(parsed)])
        } else NULL
      }
    ))

    # Data already decoded → pass NULL cat_all_cats to skip decode in render_vpc_plot
    final_plot <- render_vpc_plot(obs_data, sim_data, vpc_spec,
                                  dataspec                    = NULL,
                                  cat_covariate_all_categories = NULL)

    # Extract bin breaks for the customized binning UI (ggplot only, not cowplot grid)
    if (input$vpc_bin_method != "customized" && inherits(final_plot, "gg")) {
      bin_data <- tryCatch(final_plot$data, error = function(e) NULL)
      if (!is.null(bin_data) && !is.null(bin_data$bin_min)) {
        bin_breaks <- sort(unique(c(bin_data$bin_min, bin_data$bin_max)))
        updateTextInput(session, "vpc_custom_bins",
                        value = paste(round(bin_breaks, 2), collapse = ", "))
      }
    }

    model_data$vpc_current_plot <- final_plot
    print(final_plot)
  }) |> bindEvent(input$generate_vpc_plot, input$update_vpc_plot, ignoreNULL = FALSE, ignoreInit = TRUE)

  # Download VPC plot
  output$download_vpc_plot <- downloadHandler(
    filename = function() {
      paste0("vpc_plot_dvid", input$vpc_dvid_select, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      if (is.null(model_data$vpc_current_plot)) return()
      
      # Save the stored plot using ggsave with user-specified dimensions
      plot_width <- input$vpc_plot_width %||% 7
      plot_height <- input$vpc_plot_height %||% 7
      ggsave(file, plot = model_data$vpc_current_plot, width = plot_width, height = plot_height, dpi = 300)
    }
  )

  # Assemble the Monolix-only `opts` subset that prepare_monolix_dataset()
  # needs, exactly mirroring the interactive assembly at ~4675-4690 -- called
  # at bundle-download time and saved as vpc_opts.rds so the downloaded
  # script's prep_vpc_dataset() call is byte-for-byte what the interactive
  # VPC tab would have used (no re-derivation from the .mlxtran on script run).
  build_vpc_monolix_opts <- function() {
      mlx_filters <- parse_mlx_dataset_filters(model_data$mlxtran_path, id_col = mlx_identifier_col())

      structural_model_path <- getStructuralModel()
      structural_model_text <- readLines(resolve_lib_model_path(structural_model_path))

      list(
        mlxtran_lines  = stringr::str_squish(
          stringr::str_replace(structural_model_text, ";.*$", "")),
        covariate_info = getCovariateInformation(),
        data_info      = getData(),
        dvid_map       = model_data$dvid_map,
        has_dvid       = model_data$has_dvid,
        bql_col        = input$vpc_bql_column,
        mlx_filter_conditions = mlx_filters$conditions
      )
  }

  # Download VPC YAML config (Mike's vpc.yaml format for standalone reproducibility)
  # Shared by download_vpc_yaml and download_vpc_bundle so both stay in sync.
  build_vpc_yaml_spec <- function() {
      pi_pct  <- as.numeric(input$vpc_pi_percent)
      ci_pct  <- as.numeric(input$vpc_ci_percent)
      pi_vals <- c((1 - pi_pct/100) / 2, 1 - (1 - pi_pct/100) / 2)
      ci_vals <- c((1 - ci_pct/100) / 2, 1 - (1 - ci_pct/100) / 2)
      lloq_val <- if (is.na(input$vpc_lloq)) NULL else input$vpc_lloq

      # Build stratify section in Mike's yaml format:
      #   categorical -> NULL (yaml: "var: ~")
      #   continuous  -> numeric cutpoints or "median"/"quartiles" string
      strat_vars <- input$vpc_stratify_vars
      stratify_section <- NULL
      if (!is.null(strat_vars) && length(strat_vars) > 0) {
        stratify_section <- list()
        for (i in seq_along(strat_vars)) {
          var_name   <- strat_vars[i]
          cov_type   <- input[[paste0("vpc_cov_type_", i)]] %||% "categorical"
          if (cov_type == "continuous") {
            bin_method <- input[[paste0("vpc_cov_bin_", i)]] %||% "median"
            if (bin_method == "customized") {
              breaks_str <- input[[paste0("vpc_cov_breaks_", i)]] %||% ""
              breaks <- suppressWarnings(
                as.numeric(unlist(strsplit(gsub(" ", "", breaks_str), ",")))
              )
              breaks <- breaks[!is.na(breaks)]
              stratify_section[[var_name]] <- if (length(breaks) > 0) breaks else "median"
            } else if (bin_method == "quartile") {
              stratify_section[[var_name]] <- "quartiles"
            } else {
              stratify_section[[var_name]] <- "median"
            }
          } else {
            stratify_section <- c(stratify_section, setNames(list(NULL), var_name))
          }
        }
      }

      # Collect custom facet strip label overrides set via the Strip Label UI.
      # Keys are the original level strings; values are the display labels.
      strip_labels_section <- NULL
      if (!is.null(strat_vars) && length(strat_vars) > 0 && !is.null(model_data$vpc_obs_data)) {
        obs_snap <- model_data$vpc_obs_data
        sl <- list()
        for (i in seq_along(strat_vars)) {
          var_name   <- strat_vars[i]
          binned_col <- paste0(var_name, "_f")
          eff_col    <- if (binned_col %in% names(obs_snap)) binned_col else var_name
          if (!eff_col %in% names(obs_snap)) next
          lvls <- sort(unique(as.character(obs_snap[[eff_col]])))
          lvls <- lvls[!is.na(lvls) & lvls != "NA"]
          var_labels <- list()
          for (j in seq_along(lvls)) {
            custom <- input[[paste0("vpc_strip_", make.names(var_name), "_", j)]]
            if (!is.null(custom) && nchar(trimws(custom)) > 0 && custom != lvls[j])
              var_labels[[lvls[j]]] <- custom
          }
          if (length(var_labels) > 0) sl[[var_name]] <- var_labels
        }
        if (length(sl) > 0) strip_labels_section <- sl
      }

      is_nonmem <- (input$vpc_data_source == "nonmem")

      input_file <- if (is_nonmem) {
        nm_vpc_csv_path() %||% ""
      } else {
        tryCatch(getData()$dataFile %||% "", error = function(e) "")
      }

      dvid_val <- tryCatch(
        { v <- as.integer(input$vpc_dvid_select); if (is.na(v)) NULL else v },
        warning = function(w) NULL, error = function(e) NULL
      )

      # column_map: for NONMEM from UI selectors; for Monolix from getData() headerTypes
      vpc_column_map <- if (is_nonmem) {
        cm <- list(
          ID   = input$vpc_id_column,
          TIME = input$vpc_time_column,
          DV   = input$vpc_dv_column
        )
        if (nchar(input$vpc_amt_column  %||% "") > 0) cm$AMT  <- input$vpc_amt_column
        if (nchar(input$vpc_evid_column %||% "") > 0) cm$EVID <- input$vpc_evid_column
        if (nchar(input$vpc_dvid_column %||% "") > 0) cm$CMT  <- input$vpc_dvid_column
        if (nchar(input$vpc_rate_column %||% "") > 0) cm$RATE <- input$vpc_rate_column
        if (nchar(input$vpc_ii_column   %||% "") > 0) cm$II   <- input$vpc_ii_column
        if (nchar(input$vpc_addl_column %||% "") > 0) cm$ADDL <- input$vpc_addl_column
        if (nchar(input$vpc_tad_column  %||% "") > 0) cm$TAD  <- input$vpc_tad_column
        if (nchar(input$vpc_bql_column  %||% "") > 0) cm$BQL  <- input$vpc_bql_column
        cm
      } else {
        tryCatch({
          di          <- getData()
          type_to_std <- c(id = "ID", time = "TIME", observation = "DV",
                           amount = "AMT", evid = "EVID", obsid = "DVID",
                           rate = "RATE", interdoseinterval = "II", addl = "ADDL",
                           cens = "BQL")
          cm <- list()
          for (tp in names(type_to_std)) {
            col <- di$header[di$headerTypes == tp]
            if (length(col) == 1) cm[[type_to_std[[tp]]]] <- col
          }
          # TAD is not a recognized Monolix type — add from UI mapping if provided
          if (nchar(input$vpc_tad_column %||% "") > 0) cm$TAD <- input$vpc_tad_column
          # BQL fallback for datasets without a "cens" headerType (e.g. older
          # projects not using use=censored) — only applies if not auto-detected
          if (is.null(cm$BQL) && nchar(input$vpc_bql_column %||% "") > 0) cm$BQL <- input$vpc_bql_column
          cm
        }, error = function(e) list())
      }

      # dvid_map: Monolix only — maps string obsid labels to numeric mrgsolve DVIDs
      vpc_dvid_map <- if (!is_nonmem && isTRUE(model_data$has_dvid) &&
                          !is.null(model_data$dvid_map)) {
        as.list(setNames(model_data$dvid_map$mrgsolve_dvid,
                         model_data$dvid_map$dvid_identifier))
      } else NULL

      # ignore_columns + cat_covariate_info: Monolix only
      # ignore_columns: columns Monolix marks as headerType "ignore" — drop before simulation
      # cat_covariate_info: encoding info needed to convert string/factor categorical
      #   covariates to 0-based integers matching the translated mrgsolve model
      mlx_ignore_cols_yaml <- if (!is_nonmem) {
        tryCatch({
          di <- getData()
          cols <- di$header[di$headerTypes == "ignore"]
          if (length(cols) > 0) as.list(cols) else NULL
        }, error = function(e) NULL)
      } else NULL

      mlx_cat_cov_yaml <- if (!is_nonmem) {
        tryCatch({
          di  <- getData()
          cov <- getCovariateInformation()
          result <- list()

          # Transformed categoricals (e.g. tSPECIES derived from SPECIES)
          if (!is.null(cov$formula)) {
            for (trf_name in names(cov$formula)) {
              f <- cov$formula[[trf_name]]
              if (!is.list(f) || is.null(f$reference) || is.null(f$from) || is.null(f$transformed))
                next
              all_groups <- names(f$transformed)
              ref_group  <- f$reference
              non_ref    <- setdiff(all_groups, ref_group)
              ordered    <- c(ref_group, non_ref)
              src_to_grp <- list()
              for (grp in all_groups)
                for (sv in as.character(unlist(f$transformed[[grp]])))
                  src_to_grp[[sv]] <- grp
              result[[trf_name]] <- list(
                type       = "transformed",
                source_col = f$from,
                ordered    = as.list(ordered),
                src_to_grp = src_to_grp
              )
            }
          }

          # Raw categorical covariates (headerType "catcov")
          cat_cov_names <- di$header[di$headerTypes == "catcov"]
          for (cat_cov in cat_cov_names) {
            all_cats <- if (!is.null(cov$categories) && cat_cov %in% names(cov$categories))
              cov$categories[[cat_cov]]
            else {
              raw_vals <- tryCatch(
                sort(unique(as.character(
                  data.table::fread(di$dataFile, na.strings = ".", select = cat_cov)[[cat_cov]]
                ))),
                error = function(e) character(0)
              )
              raw_vals
            }
            if (length(all_cats) == 0) next
            result[[cat_cov]] <- list(type = "catcov", ordered = as.list(all_cats))
          }

          if (length(result) > 0) result else NULL
        }, error = function(e) NULL)
      } else NULL

      # Monolix active dataset filter (.mlxtran [FILTER] block) — mirrors the
      # interactive path's parse_mlx_dataset_filters() usage.
      mlx_filter_conditions_yaml <- if (!is_nonmem) {
        tryCatch({
          conds <- parse_mlx_dataset_filters(model_data$mlxtran_path, id_col = mlx_identifier_col())$conditions
          if (length(conds) > 0) as.list(conds) else NULL
        }, error = function(e) NULL)
      } else NULL

      # NONMEM IGNORE conditions parsed from control file
      nm_ig_yaml <- if (is_nonmem) {
        ctl_path <- nm_ctlFile()
        if (nchar(ctl_path) > 0 && file.exists(ctl_path)) {
          ig <- parse_nonmem_ignore(readLines(ctl_path, warn = FALSE))
          list(
            nonmem_ignore_nonnumeric = ig$ignore_nonnumeric,
            nonmem_ignore_chars      = ig$ignore_chars,
            nonmem_ignore_conditions = ig$conditions
          )
        } else list()
      } else list()

      vpc_yaml <- c(list(
        kind             = "vpc_plot",
        mode             = if (is_nonmem) "nonmem" else "monolix",
        input_filename   = input_file,
        data_spec        = "",
        input_model      = normalizePath(model_data$vpc_model_path %||% "", mustWork = FALSE),
        column_map         = vpc_column_map,
        dvid_map           = vpc_dvid_map,
        ignore_columns     = mlx_ignore_cols_yaml,
        cat_covariate_info = mlx_cat_cov_yaml,
        mlx_filter_conditions = mlx_filter_conditions_yaml,
        output_directory = "vpc_output",
        output_stem      = "vpc",
        nRep             = as.integer(input$vpc_n_sims),
        output_filetype  = "png",
        width            = input$vpc_plot_width  %||% 7,
        height           = input$vpc_plot_height %||% 7,
        `axis.text`      = 11,
        `axis.title`     = 11,
        `strip.text`     = 11,
        pi_fill          = "steelblue3",
        med_fill         = "grey60",
        quarto_output    = FALSE,
        plots = list(
          vpc1 = list(
            title       = "",
            time_column = if (isTRUE(input$vpc_tad_axis)) "TAD" else "TIME",
            sim_column  = "Y",
            dvid        = dvid_val,
            cmt         = 0,
            lloq        = lloq_val,
            stratify    = stratify_section,
            scales      = input$vpc_scales %||% "free_x",
            logY        = isTRUE(input$vpc_log_y),
            predCorr    = isTRUE(input$vpc_pred_corr),
            smooth      = isTRUE(input$vpc_smooth),
            pi          = pi_vals,
            ci          = ci_vals,
            bins        = {
              bm <- input$vpc_bin_method %||% "jenks"
              if (bm == "customized") {
                cs <- trimws(input$vpc_custom_bins %||% "")
                parsed <- suppressWarnings(as.numeric(unlist(strsplit(gsub(" ", "", cs), ","))))
                parsed <- parsed[!is.na(parsed)]
                if (length(parsed) >= 2) as.list(parsed) else "jenks"
              } else bm
            },
            obsdv       = TRUE,
            pici        = TRUE,
            censor      = !is.null(lloq_val),
            x_label     = if (!is.null(input$vpc_x_label) && nchar(trimws(input$vpc_x_label)) > 0)
                            input$vpc_x_label else NULL,
            y_label     = if (!is.null(input$vpc_y_label) && nchar(trimws(input$vpc_y_label)) > 0)
                            input$vpc_y_label else NULL,
            xlim        = {
              xmn <- input$vpc_xmin; xmx <- input$vpc_xmax
              if (!is.na(xmn) && !is.na(xmx)) c(xmn, xmx) else NULL
            },
            ylim        = {
              ymn <- input$vpc_ymin; ymx <- input$vpc_ymax
              if (!is.na(ymn) && !is.na(ymx)) c(ymn, ymx) else NULL
            },
            x_breaks    = {
              xb <- trimws(input$vpc_x_breaks %||% "")
              if (nchar(xb) > 0) {
                parsed <- suppressWarnings(as.numeric(unlist(strsplit(gsub(" ", "", xb), ","))))
                parsed <- sort(parsed[!is.na(parsed)])
                if (length(parsed) > 0) as.list(parsed) else NULL
              } else NULL
            },
            y_breaks    = {
              yb <- trimws(input$vpc_y_breaks %||% "")
              if (nchar(yb) > 0) {
                parsed <- suppressWarnings(as.numeric(unlist(strsplit(gsub(" ", "", yb), ","))))
                parsed <- sort(parsed[!is.na(parsed)])
                if (length(parsed) > 0) as.list(parsed) else NULL
              } else NULL
            },
            strip_labels = strip_labels_section,
            show_legend  = isTRUE(input$vpc_show_legend %||% TRUE)
          )
        )
      ), nm_ig_yaml)
      vpc_yaml
  }

  output$download_vpc_yaml <- downloadHandler(
    filename = function() paste0("vpc_config_", Sys.Date(), ".yaml"),
    content = function(file) {
      yaml::write_yaml(build_vpc_yaml_spec(), file)
    }
  )

  # Download standalone R script that reproduces VPC plots from the YAML config
  # Shared by download_vpc_script and download_vpc_bundle so both stay in sync.
  # Produces the standalone R script text that reproduces the interactive VPC.
  # The script sources the shared utility files (vpc_utils_common.R,
  # vpc_utils_monolix.R or vpc_utils_nonmem.R, mlx_parse_utils.R) that ship in
  # the bundle, so its data-prep/sim/plot logic is byte-for-byte the same as
  # the interactive VPC tab. When bundle_template=TRUE, the SGE header points
  # at the sge.tmpl included alongside in the bundle rather than a generic path.
  build_vpc_script_text <- function(yaml_filename = NULL, bundle_template = FALSE) {
      is_nonmem        <- isTRUE(input$vpc_data_source == "nonmem")
      dl_parallel_mode <- vpc_parallel_mode(input)
      dl_seed          <- input$vpc_seed
      dl_n_jobs        <- if (dl_parallel_mode == "hpc") input$vpc_n_jobs else NULL
      dl_seed_str      <- if (!is.null(dl_seed) && !is.na(dl_seed)) as.character(dl_seed) else "NULL"

      s_header_libs <- if (dl_parallel_mode == "hpc") {
        if (isTRUE(bundle_template)) {
'library(tidyverse)
library(mrgsolve)
library(yaml)
library(vpc)
library(ggplot2)
library(clustermq)
library(cowplot)
library(data.table)

# SGE parallel settings — sge.tmpl is included alongside this script in the bundle.
options(
  clustermq.scheduler = "SGE",
  clustermq.template = "sge.tmpl"
)

'
        } else {
'library(tidyverse)
library(mrgsolve)
library(yaml)
library(vpc)
library(ggplot2)
library(clustermq)
library(cowplot)
library(data.table)

# SGE parallel settings — update clustermq.template to your own template file if needed.
options(
  clustermq.scheduler = "SGE"
  # clustermq.template = "path/to/sge.tmpl"
)

'
        }
      } else {
'library(tidyverse)
library(mrgsolve)
library(yaml)
library(vpc)
library(ggplot2)
library(purrr)
library(cowplot)
library(data.table)

'
      }

      s_dep_note <- if (!isTRUE(bundle_template)) {
        if (is_nonmem) {
'# NOTE: this script requires the MN2mrg package to be installed.
# Download the "Reproducibility Bundle" to get the config and any other
# files it needs together.

'
        } else {
'# NOTE: this script requires the MN2mrg package to be installed and
# vpc_opts.rds living alongside it. Download the "Reproducibility Bundle"
# (not just this script) to get all of these files together.

'
        }
      } else ""

      s_source <- paste0(
        'library(MN2mrg)  # ', as.character(utils::packageVersion("MN2mrg")), '\n',
        '`%||%` <- function(a, b) if (!is.null(a)) a else b\n',
        "\n"
      )

      s_header <- paste0(s_header_libs, s_dep_note, s_source)

      s_prep_nonmem <- '# Dataset preparation: applies IGNORE conditions and column renaming,
# derives DVID from CMT, drops character columns, adds ROW index.
prep_vpc_data <- function(raw_dat, column_map,
                          nonmem_ignore_conditions = character(0)) {
  rename_col <- function(df, src, tgt) {
    if (is.null(src) || src == "") return(df)
    if (src %in% names(df)) {
      if (src != tgt) {
        if (tgt %in% names(df)) df <- dplyr::select(df, -dplyr::all_of(tgt))
        df <- dplyr::rename(df, !!tgt := !!rlang::sym(src))
      }
    } else {
      idx <- which(toupper(names(df)) == toupper(src))
      if (length(idx) == 1) {
        nm <- names(df)[idx]
        if (tgt %in% names(df) && nm != tgt)
          df <- dplyr::select(df, -dplyr::all_of(tgt))
        df <- dplyr::rename(df, !!tgt := !!rlang::sym(nm))
      }
    }
    df
  }

  for (expr_str in nonmem_ignore_conditions) {
    parsed <- tryCatch(parse(text = expr_str), error = function(e) NULL)
    if (is.null(parsed)) next
    used_cols <- tryCatch(all.vars(parsed), error = function(e) character(0))
    if (!all(used_cols %in% names(raw_dat))) next
    raw_dat <- tryCatch(dplyr::filter(raw_dat, !!parsed[[1]]), error = function(e) raw_dat)
  }

  for (std in c("ID","TIME","DV","AMT","EVID","CMT","RATE","II","ADDL","TAD","BQL")) {
    src <- column_map[[std]]
    if (!is.null(src) && nchar(src) > 0) raw_dat <- rename_col(raw_dat, src, std)
  }

  for (col in c("TIME","DV","AMT","EVID","RATE","II","ADDL"))
    if (col %in% names(raw_dat) && !is.numeric(raw_dat[[col]]))
      raw_dat <- dplyr::mutate(raw_dat, !!rlang::sym(col) := as.numeric(.data[[col]]))
  if ("ID" %in% names(raw_dat) && !is.numeric(raw_dat[["ID"]]))
    raw_dat <- dplyr::mutate(raw_dat, ID = as.numeric(as.factor(ID)))

  if (!"EVID" %in% names(raw_dat) && "AMT" %in% names(raw_dat))
    raw_dat <- dplyr::mutate(raw_dat, EVID = dplyr::if_else(is.na(AMT), 0, 1))
  if (!"CMT" %in% names(raw_dat))
    raw_dat <- dplyr::mutate(raw_dat, CMT = dplyr::if_else(EVID == 1, 1L, 0L))

  mdv_col <- names(raw_dat)[toupper(names(raw_dat)) == "MDV"]
  if (length(mdv_col) > 0) {
    if ("BQL" %in% names(raw_dat)) {
      raw_dat <- dplyr::filter(raw_dat, !(.data[[mdv_col]] == 1 & EVID == 0 & .data[["BQL"]] != 1))
    } else {
      raw_dat <- dplyr::filter(raw_dat, !(.data[[mdv_col]] == 1 & EVID == 0))
    }
  }

  raw_dat <- dplyr::mutate(raw_dat, DVID = as.integer(CMT))

  char_cols <- setdiff(names(raw_dat)[vapply(raw_dat, is.character, logical(1))], "CMT")
  if (length(char_cols) > 0)
    raw_dat <- dplyr::select(raw_dat, -dplyr::all_of(char_cols))

  raw_dat %>% dplyr::arrange(ID, TIME) %>% dplyr::ungroup() %>%
    dplyr::mutate(ROW = dplyr::row_number())
}

'

      s_vpc_top <- 'set_option <- function(lst, name, default = NULL) {
  val <- lst[[name]]
  if (is.null(val)) default else val
}

vpc.yaml <- function(yaml) {

  if (file.exists(yaml)) {
    vpcspec <- yaml::read_yaml(yaml)
  } else {
    cat("\nYAML file not found!\n\n")
    return(invisible(NULL))
  }

  stem       <- vpcspec$output_stem
  type       <- set_option(vpcspec, "output_filetype", "png")
  width      <- set_option(vpcspec, "width",  7)
  height     <- set_option(vpcspec, "height", 5)
  axis.text  <- set_option(vpcspec, "axis.text",  11)
  axis.title <- set_option(vpcspec, "axis.title", 11)
  strip.text <- set_option(vpcspec, "strip.text", 11)
  column_map <- set_option(vpcspec, "column_map", list())

  vpc_theme <- new_vpc_theme(list(
    sim_pi_fill     = set_option(vpcspec, "pi_fill", "steelblue3"),
    sim_median_fill = set_option(vpcspec, "med_fill", "grey60"),
    loq_color       = "#999999"
  ))

  out_dir <- set_option(vpcspec, "output_directory", ".")
  if (out_dir == ".") out_dir <- dirname(normalizePath(yaml))
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  datFile <- vpcspec$input_filename
  if (!file.exists(datFile)) {
    datFile <- file.path(dirname(normalizePath(yaml)), datFile)
  }
  if (!file.exists(datFile)) {
    cat(paste0("\nFile ", datFile, " does not exist!\n\n"))
    return(invisible(NULL))
  }

  # ── Load and prepare dataset ────────────────────────────────────────────────
'

      s_data_nonmem <- '  nm_ig_nonnumeric <- isTRUE(set_option(vpcspec, "nonmem_ignore_nonnumeric", FALSE))
  nm_ig_chars      <- set_option(vpcspec, "nonmem_ignore_chars",      character(0))
  nm_ig_conds      <- set_option(vpcspec, "nonmem_ignore_conditions", character(0))

  raw_lines  <- readLines(datFile)
  header_idx <- which(nchar(trimws(raw_lines)) > 0)[1]
  header     <- sub("^#", "", raw_lines[header_idx])
  body       <- raw_lines[-seq_len(header_idx)]
  if (nm_ig_nonnumeric)
    body <- body[grepl("^\\\\s*[-+.0-9]", body) | nchar(trimws(body)) == 0]
  if (length(nm_ig_chars) > 0) {
    fc   <- substr(trimws(body, "left"), 1, 1)
    body <- body[!fc %in% nm_ig_chars]
  }
  raw_dat <- data.table::fread(
    text = paste(c(header, body), collapse = "\n"),
    na.strings = "."
  ) %>% as_tibble()

  dat <- prep_vpc_data(
    raw_dat, column_map,
    nonmem_ignore_conditions = nm_ig_conds
  )
  cat("Dataset loaded:", nrow(dat), "rows (after IGNORE + column prep)\n")

'

      s_data_monolix <- '  raw_dat   <- data.table::fread(datFile, na.strings = ".") %>% as_tibble()
  opts_file <- file.path(dirname(normalizePath(yaml)), "vpc_opts.rds")
  if (!file.exists(opts_file)) {
    cat(paste0("\nFile ", opts_file, " does not exist!\n\n"))
    return(invisible(NULL))
  }
  dat <- prep_vpc_dataset(raw_dat, column_map, mode = "monolix",
                          opts = readRDS(opts_file))$data
  cat("Dataset loaded:", nrow(dat), "rows (after column prep)\n")

  filters <- map(vpcspec$filters, ~parse(text = .x))
  if (length(filters) > 0) {
    for (ii in seq_along(filters)) dat <- dat %>% filter(eval(filters[[ii]]))
    cat("After filters:", nrow(dat), "rows\n")
  }

'

      s_vpc_rest_a <- '  if (!file.exists(vpcspec$input_model)) {
    cat(paste0("\nFile ", vpcspec$input_model, " does not exist!\n\n"))
    return(invisible(NULL))
  }

  plots <- names(vpcspec$plots)
  if (length(plots) == 0) {
    cat("\nNo plots specified.\n\n")
    return(invisible(NULL))
  }

  carry <- c("EVID", "DVID", "ROW")
  if ("TAD" %in% names(dat)) carry <- c(carry, "TAD")
  for (ii in plots) {
    strat   <- names(set_option(vpcspec$plots[[ii]], "stratify", list()))
    carry   <- c(carry, strat)
  }
  carry <- unique(carry[carry %in% names(dat)])

  cat("Simulation input:", nrow(dat), "rows\n")

  mod  <- mread(vpcspec$input_model, quiet = TRUE)
  pred <- mod %>% zero_re() %>% data_set(dat) %>%
    carry_out(ROW) %>% mrgsim_df()

  cat("Running", vpcspec$nRep, "simulations...\n")
'

      s_vpc_sim <- if (dl_parallel_mode == "hpc") {
        paste0(
          '  n_jobs <- min(', dl_n_jobs, 'L, as.integer(vpcspec$nRep))\n',
          '  # SGE worker logs. sge.tmpl defaults {{ log_file }} to /dev/null,\n',
          '  # so without an explicit path a compute-node-side failure leaves\n',
          '  # no trace anywhere. Kept beside the model, which the workers must\n',
          '  # already be able to read, so it is cluster-visible by construction.\n',
          '  log_dir <- file.path(dirname(vpcspec$input_model), "vpc_sge_logs")\n',
          '  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)\n',
          '  sim_list <- Q(\n',
          '    fun      = vpc_worker_fn,\n',
          '    i        = seq_len(vpcspec$nRep),\n',
          '    n_jobs   = n_jobs,\n',
          '    const    = list(mod_path = vpcspec$input_model, dat = dat, seed = ', dl_seed_str, '),\n',
          '    template = list(log_file = file.path(log_dir, "vpc_worker_$TASK_ID.log"))\n',
          '  )\n'
        )
      } else {
        paste0(
          '  # Reuse the `mod` object already mread() above instead of\n',
          '  # re-reading/re-compiling it on every replicate.\n',
          '  sim_list <- purrr::map(\n',
          '    seq_len(vpcspec$nRep),\n',
          '    function(i) {\n',
          '      if (!is.null(', dl_seed_str, ')) set.seed(', dl_seed_str, ' + i)\n',
          '      mod %>% data_set(dat) %>% carry_out(ROW) %>%\n',
          '        mrgsim(atol = 1e-12, maxsteps = 50000) %>% dplyr::mutate(rep = i)\n',
          '    }\n',
          '  )\n'
        )
      }

      s_vpc_rest_b <- '  sim <- bind_rows(sim_list) %>%
    dplyr::rename_with(~dplyr::case_when(. == "time" ~ "TIME", . == "rep" ~ "IREP", TRUE ~ .)) %>%
    left_join(dat %>% select(all_of(carry)), by = "ROW")

  cat("Simulation complete:", nrow(sim), "rows across", vpcspec$nRep, "reps\n\n")

  map(plots, function(.x) {

    plotTitle <- set_option(vpcspec$plots[[.x]], "title",       "")
    timeCol   <- set_option(vpcspec$plots[[.x]], "time_column", "TIME")
    simCol    <- set_option(vpcspec$plots[[.x]], "sim_column",  "Y")
    dvid      <- set_option(vpcspec$plots[[.x]], "dvid",        NULL)
    scales    <- set_option(vpcspec$plots[[.x]], "scales",      "free")
    logY      <- set_option(vpcspec$plots[[.x]], "logY",        FALSE)
    predCorr  <- set_option(vpcspec$plots[[.x]], "predCorr",    FALSE)
    smooth    <- set_option(vpcspec$plots[[.x]], "smooth",      TRUE)
    bins      <- set_option(vpcspec$plots[[.x]], "bins",        "data")
    obsdv     <- set_option(vpcspec$plots[[.x]], "obsdv",       TRUE)
    pici      <- set_option(vpcspec$plots[[.x]], "pici",        TRUE)
    censor    <- set_option(vpcspec$plots[[.x]], "censor",      FALSE)
    lloq      <- set_option(vpcspec$plots[[.x]], "lloq",        NULL)
    stratify  <- set_option(vpcspec$plots[[.x]], "stratify",    NULL)
    xlim      <- set_option(vpcspec$plots[[.x]], "xlim",        NULL)
    ylim      <- set_option(vpcspec$plots[[.x]], "ylim",        NULL)
    x_breaks  <- { xb <- unlist(set_option(vpcspec$plots[[.x]], "x_breaks", NULL)); if (length(xb)) sort(as.numeric(xb)) else NULL }
    y_breaks  <- { yb <- unlist(set_option(vpcspec$plots[[.x]], "y_breaks", NULL)); if (length(yb)) sort(as.numeric(yb)) else NULL }
    xlab      <- set_option(vpcspec$plots[[.x]], "x_label",     "Time")
    ylab      <- set_option(vpcspec$plots[[.x]], "y_label",     "Observation")
    pi        <- unlist(set_option(vpcspec$plots[[.x]], "pi", c(0.05, 0.95)))
    ci        <- unlist(set_option(vpcspec$plots[[.x]], "ci", c(0.025, 0.975)))
    show_legend <- isTRUE(set_option(vpcspec$plots[[.x]], "show_legend", TRUE))

    if (predCorr) ylab <- paste0("Prediction-Corrected ", ylab)

    pred_col  <- pred %>% rename(PRED = !!sym(simCol)) %>% select(ROW, PRED)
    sim_plot  <- sim  %>% left_join(pred_col, by = "ROW")
    dat_plot  <- dat  %>% left_join(pred_col, by = "ROW")

    if (!is.null(dvid)) {
      datf <- dat_plot %>% filter(EVID == 0, DVID == dvid)
      simf <- sim_plot %>% filter(DVID == dvid)
    } else {
      datf <- dat_plot %>% filter(EVID == 0)
      simf <- sim_plot
    }

    # The vpc package own guess_software()/filter_dv() would otherwise re-drop
    # MDV!=0 rows (e.g. preserved BQL rows) once it classifies this as
    # NONMEM-format data. EVID is already filtered above, so dropping MDV here is safe.
    datf <- datf %>% select(-any_of("MDV"))
    simf <- simf %>% select(-any_of("MDV"))

    if (!is.null(stratify)) {
      for (ii in names(stratify)) {
        if (is.null(stratify[[ii]])) next
        breaks_val <- stratify[[ii]]
        # Use subject-level values (distinct per ID) for quantile calculation,
        # matching the app which also uses distinct(ID) to avoid repeated
        # rows inflating quantiles for time-varying covariates.
        x_ref <- datf %>% dplyr::distinct(ID, .keep_all = TRUE) %>% dplyr::pull(!!rlang::sym(ii))
        if (breaks_val[1] == "quartiles") {
          breaks_val <- c(floor(min(x_ref, na.rm = TRUE)),
                          quantile(x_ref, probs = c(0.25, 0.50, 0.75), na.rm = TRUE),
                          ceiling(max(x_ref, na.rm = TRUE)))
        } else if (breaks_val[1] == "median") {
          breaks_val <- c(floor(min(x_ref, na.rm = TRUE)),
                          quantile(x_ref, probs = 0.50, na.rm = TRUE),
                          ceiling(max(x_ref, na.rm = TRUE)))
        }
        factCol <- paste0(ii, "_f")
        datf <- datf %>% mutate(!!sym(factCol) := cut(.data[[ii]], breaks = breaks_val, include.lowest = TRUE))
        simf <- simf %>% mutate(!!sym(factCol) := cut(.data[[ii]], breaks = breaks_val, include.lowest = TRUE))
      }
      stratifyf <- map_chr(names(stratify), function(.s) {
        factCol <- paste0(.s, "_f")
        if (factCol %in% names(datf)) factCol else .s
      })
    } else {
      stratifyf <- NULL
    }

    # Apply custom facet strip label overrides from YAML strip_labels section.
    strip_labels <- set_option(vpcspec$plots[[.x]], "strip_labels", NULL)
    if (!is.null(strip_labels)) {
      for (sv in names(strip_labels)) {
        factCol <- paste0(sv, "_f")
        eff_col <- if (factCol %in% names(datf)) factCol else sv
        lbl_map <- strip_labels[[sv]]
        if (eff_col %in% names(datf)) {
          for (old_lbl in names(lbl_map)) {
            new_lbl <- lbl_map[[old_lbl]]
            datf[[eff_col]] <- ifelse(as.character(datf[[eff_col]]) == old_lbl, new_lbl, as.character(datf[[eff_col]]))
            simf[[eff_col]] <- ifelse(as.character(simf[[eff_col]]) == old_lbl, new_lbl, as.character(simf[[eff_col]]))
          }
        }
      }
    }

    cat("Plot:", .x, "| DVID:", dvid %||% "all",
        "| obs:", nrow(datf), "| sim:", nrow(simf), "\n")

    vpc_args <- list(
      obs      = datf,
      sim      = simf,
      obs_cols = list(idv = timeCol, dv = "DV",   pred = "PRED"),
      sim_cols = list(idv = timeCol, dv = simCol, sim = "IREP", pred = "PRED"),
      stratify = stratifyf,
      bins     = bins,
      ci       = ci,
      facet    = "wrap",
      vpc_theme = vpc_theme,
      labeller = label_value
    )

    p_vpc <- do.call(vpc, c(vpc_args, list(
      pi        = pi,
      lloq      = lloq,
      log_y     = logY,
      smooth    = smooth,
      scales    = scales,
      pred_corr = predCorr,
      show      = list(obs_dv = obsdv, obs_ci = TRUE,
                       pi = !pici, pi_as_area = !pici, pi_ci = pici),
      xlab = if (censor) "" else xlab,
      ylab = ylab
    ))) +
      theme_bw() +
      ggtitle(plotTitle) +
      theme(
        axis.text  = element_text(size = axis.text),
        axis.title = element_text(size = axis.title),
        strip.text = element_text(size = strip.text)
      )

    if (!is.null(xlim) || !is.null(ylim))
      p_vpc <- p_vpc + coord_cartesian(xlim = xlim, ylim = ylim)

    if (!is.null(x_breaks))
      p_vpc <- p_vpc + scale_x_continuous(breaks = x_breaks)
    if (!is.null(y_breaks))
      p_vpc <- p_vpc + if (logY) scale_y_log10(breaks = y_breaks) else scale_y_continuous(breaks = y_breaks)

    if (censor && !is.null(lloq)) {
      p_cens <- do.call(vpc_cens, c(vpc_args, list(
        lloq = lloq + 0.01,
        xlab = xlab,
        ylab = "Probability of <LLOQ"
      ))) +
        theme_bw() +
        theme(
          axis.text  = element_text(size = axis.text),
          axis.title = element_text(size = axis.title),
          strip.text = element_text(size = strip.text)
        )
      p_vpc <- strip_na_layers(p_vpc)
      if (show_legend) p_vpc <- add_vpc_legend(p_vpc, vpc_theme, pi = pi)
      p <- plot_grid(p_vpc, strip_na_layers(p_cens), ncol = 1, rel_heights = c(2, 1), align = "v")
    } else {
      p_vpc <- strip_na_layers(p_vpc)
      if (show_legend) p_vpc <- add_vpc_legend(p_vpc, vpc_theme, pi = pi)
      p <- p_vpc
    }

    stemx   <- paste0(stem, "-", .x)
    outfile <- file.path(out_dir, paste0(stemx, ".", type))
    ggsave(outfile, p, width = width, height = height, dpi = 150)
    cat("  Saved:", outfile, "\n")

  })

  invisible(NULL)
}

'
      yaml_filename <- if (!is.null(yaml_filename)) yaml_filename else paste0("vpc_config_", Sys.Date(), ".yaml")
      s_footer <- paste0(
        "# Update this path if you renamed or moved the YAML config file\n",
        "yaml_file <- \"", yaml_filename, "\"\n\n",
        "vpc.yaml(yaml_file)\n"
      )
      script <- paste0(
        s_header,
        if (is_nonmem) s_prep_nonmem else "",
        s_vpc_top,
        if (is_nonmem) s_data_nonmem else s_data_monolix,
        s_vpc_rest_a,
        s_vpc_sim,
        s_vpc_rest_b,
        s_footer
      )
      script
  }

  output$download_vpc_script <- downloadHandler(
    filename = function() "vpc_script.R",
    content = function(file) {
      writeLines(build_vpc_script_text(), file)
    }
  )

  # Bundles the YAML config, standalone R script and (for HPC mode) the SGE
  # template into a single zip for reproducibility. The script calls
  # library(MN2mrg) for its helpers. For Monolix, also saves the
  # vpc_opts.rds snapshot so the extracted script reproduces the interactive
  # prep_vpc_dataset() call without re-parsing the .mlxtran. sge.tmpl is
  # shipped from inst/extdata/.
  output$download_vpc_bundle <- downloadHandler(
    filename = function() "vpc_bundle.zip",
    content = function(file) {
      is_hpc    <- identical(vpc_parallel_mode(input), "hpc")
      is_nonmem <- isTRUE(input$vpc_data_source == "nonmem")

      bundle_dir <- tempfile("vpc_bundle_")
      dir.create(bundle_dir)
      on.exit(unlink(bundle_dir, recursive = TRUE), add = TRUE)

      yaml::write_yaml(build_vpc_yaml_spec(), file.path(bundle_dir, "vpc_config.yaml"))
      writeLines(
        build_vpc_script_text(yaml_filename = "vpc_config.yaml", bundle_template = is_hpc),
        file.path(bundle_dir, "vpc_script.R")
      )
      if (!is_nonmem) {
        saveRDS(build_vpc_monolix_opts(), file.path(bundle_dir, "vpc_opts.rds"))
      }
      if (is_hpc) {
        file.copy(
          system.file("extdata", "sge.tmpl", package = "MN2mrg"),
          file.path(bundle_dir, "sge.tmpl")
        )
      }

      zip_path <- normalizePath(file, mustWork = FALSE)
      old_wd <- getwd()
      setwd(bundle_dir)
      on.exit(setwd(old_wd), add = TRUE)
      utils::zip(zipfile = zip_path, files = list.files(bundle_dir))
    }
  )

  # ==========================================================================
  # Forest Plot tab
  # ==========================================================================

  output$forest_model_path_display <- renderText({
    if (!is.null(model_data$forest_model_upload_path)) {
      paste0("Selected model (will be used): ", model_data$forest_model_upload_path)
    } else if (!is.null(model_data$saved_model_path)) {
      paste0("Saved model: ", model_data$saved_model_path)
    } else {
      "No saved model yet, and no file selected -- translate and save a NONMEM model, or select an already-translated .cpp file."
    }
  })

  output$forest_model_status <- renderText({ model_data$forest_model_status })

  observeEvent(input$forest_load_model, {
    if (!is.null(model_data$forest_model_upload_path) && file.exists(model_data$forest_model_upload_path)) {
      # An explicitly selected file takes priority -- lets the user point at
      # a model translated in an earlier session without it clashing with a
      # saved_model_path left over from this session.
      model_path <- model_data$forest_model_upload_path
    } else if (!is.null(model_data$saved_model_path) && file.exists(model_data$saved_model_path)) {
      model_path <- model_data$saved_model_path
    } else {
      model_data$forest_model_status <- "✗ No saved model found and no file selected. Please translate and save a NONMEM model, or select an already-translated mrgsolve .cpp file."
      return()
    }
    # Tracked per step so a failure names the accessor that broke. The
    # reported "no method for coercing this S4 class to a vector" is base R's
    # as.vector() refusing an S4 object; it identifies neither the accessor
    # nor the class, which is why the original report could not be diagnosed.
    forest_step <- "mread(model_path)"
    tryCatch({
      mod <- mread(model_path, quiet = TRUE)
      model_data$forest_model <- mod
      model_data$forest_model_loaded <- TRUE
      model_data$forest_model_path <- model_path

      # outvars() reads the two slots straight off the model object
      # (list(cmt = x@cmtL, capture = x@capL)). mod$cmt and mod$capture go
      # through mrgsolve's `[[` method instead, which builds as.list(mod),
      # and that coerces param(), omat(), smat() and init() on the way past.
      # Every one of those is an S4 numericlist and every one is a chance to
      # hit exactly the coercion failure reported here, even though only the
      # compartment and capture names were ever wanted. The narrow accessor
      # returns the same values and removes that surface entirely.
      forest_step  <- "outvars(mod)"
      ov           <- outvars(mod)
      cmts         <- as.character(ov$cmt)
      capture_vars <- as.character(ov$capture)

      updateSelectInput(session, "forest_cmt", choices = cmts, selected = cmts[1])

      pred_choices <- c(cmts, capture_vars)
      default_pred <- if ("Y" %in% pred_choices) "Y" else pred_choices[1]
      updateSelectInput(session, "forest_pred", choices = pred_choices, selected = default_pred)

      forest_step <- "get_forest_covariate_candidates(mod)"
      cov_candidates <- get_forest_covariate_candidates(mod)
      updateCheckboxGroupInput(session, "forest_covariates_selected", choices = cov_candidates)

      model_data$forest_model_status <- paste0(
        "✓ Model loaded.\nCovariate candidates: ", paste(cov_candidates, collapse = ", "),
        "\nOutput variables: ", paste(pred_choices, collapse = ", ")
      )
    }, error = function(e) {
      model_data$forest_model_status <- paste0(
        "✗ Error loading model: ", conditionMessage(e),
        "\n  Step: ", forest_step,
        "\n  Model: ", model_path,
        "\n  mrgsolve: ", as.character(utils::packageVersion("mrgsolve")),
        if (grepl("coercing this S4 class", conditionMessage(e), fixed = TRUE))
          paste0("\n  That is base R refusing to coerce an S4 object. Please ",
                 "attach this .cpp file to the bug report together with the ",
                 "four lines above.")
        else ""
      )
    })
  })

  # --- Endpoint label / parameter panels ---
  output$forest_endpoint_panels <- renderUI({
    selected <- input$forest_endpoints
    if (is.null(selected) || length(selected) == 0) return(NULL)
    endpoint_display_names <- c(auc = "AUC", aucinf = "AUCinf", cmax = "Cmax",
                                cmin = "Cmin", cavg = "Cavg", parameter = "Model Parameter")
    mod <- model_data$forest_model
    # outvars() reads x@cmtL / x@capL directly; mod$cmt would build
    # as.list(mod) and coerce four S4 numericlists on the way past.
    param_choices <- if (!is.null(mod)) unlist(outvars(mod), use.names = FALSE) else c()
    panels <- lapply(selected, function(ep) {
      label_default <- endpoint_display_names[[ep]]
      extra <- NULL
      if (ep == "parameter") {
        extra <- selectInput("forest_endpoint_param_parameter", "Model Output Variable:", choices = param_choices)
      }
      wellPanel(
        tags$b(label_default),
        textInput(paste0("forest_endpoint_label_", ep), "Plot Label:", value = label_default),
        extra
      )
    })
    tagList(panels)
  })

  # --- Covariate reference/values/data-spec panels ---
  output$forest_covariate_panels <- renderUI({
    selected <- input$forest_covariates_selected
    if (is.null(selected) || length(selected) == 0) return(NULL)
    build_spec <- identical(input$forest_dataspec_source, "build")
    panels <- lapply(selected, function(cn) {
      wellPanel(
        tags$b(cn),
        fluidRow(
          column(4, numericInput(paste0("forest_cov_ref_", cn), "Reference Value:", value = NA)),
          column(if (build_spec) 5 else 8, textInput(paste0("forest_cov_values_", cn),
                              "Values (comma-separated numbers):", value = "")),
          if (build_spec) column(3, checkboxInput(paste0("forest_cov_categorical_", cn), "Categorical", value = FALSE))
        ),
        if (build_spec) {
          tagList(
            conditionalPanel(
              condition = sprintf("input['forest_cov_categorical_%s']", cn),
              textInput(paste0("forest_cov_labels_", cn),
                        "Labels for the plot (comma-separated, one per value, in the same order):", value = "")
            ),
            fluidRow(
              column(6, textInput(paste0("forest_cov_short_", cn), "Short Label:", value = cn)),
              conditionalPanel(
                condition = sprintf("!input['forest_cov_categorical_%s']", cn),
                column(6, textInput(paste0("forest_cov_unit_", cn), "Unit:", value = ""))
              )
            )
          )
        } else {
          helpText("Categorical labels/decoding for the plot come from the uploaded yspec YAML.")
        }
      )
    })
    tagList(panels)
  })

  # --- Uncertainty source: file choosers (shinyFiles, matches NONMEM tab pattern) ---
  observe({
    shinyFiles::shinyFileChoose(input, "forest_model_upload", roots = c(root = "/"), filetypes = c("", "cpp", "txt"))
    shinyFiles::shinyFileChoose(input, "forest_cov_file", roots = c(root = "/"), filetypes = c("", "cov"))
    shinyFiles::shinyFileChoose(input, "forest_bootstrap_file", roots = c(root = "/"), filetypes = c("", "csv"))
    shinyFiles::shinyFileChoose(input, "forest_dataspec_upload", roots = c(root = "/"), filetypes = c("", "yaml", "yml"))
    for (i in 1:10) {
      shinyFiles::shinyFileChoose(input, paste0("forest_bayes_ext_", i), roots = c(root = "/"), filetypes = c("", "ext"))
    }
  })

  observeEvent(input$forest_model_upload, {
    fp <- shinyFiles::parseFilePaths(roots = c(root = "/"), input$forest_model_upload)
    path <- as.character(fp$datapath)
    if (length(path) == 0 || nchar(path) == 0) return()
    model_data$forest_model_upload_path <- path
  })

  observeEvent(input$forest_cov_file, {
    fp <- shinyFiles::parseFilePaths(roots = c(root = "/"), input$forest_cov_file)
    path <- as.character(fp$datapath)
    if (length(path) == 0 || nchar(path) == 0) return()
    model_data$forest_cov_path <- path
    model_data$forest_ext_path <- derive_ext_from_cov(path)
  })

  output$forest_cov_ext_display <- renderText({
    if (is.null(model_data$forest_cov_path)) return("No .cov file selected yet.")
    paste0(".cov: ", model_data$forest_cov_path,
           "\nDerived .ext: ", model_data$forest_ext_path,
           "\n(Derived by substituting \"cov\" -> \"ext\" in the path, matching forest.R's own convention.",
           " Verify this if the path contains \"cov\" elsewhere.)")
  })

  observeEvent(input$forest_bootstrap_file, {
    fp <- shinyFiles::parseFilePaths(roots = c(root = "/"), input$forest_bootstrap_file)
    path <- as.character(fp$datapath)
    if (length(path) == 0 || nchar(path) == 0) return()
    model_data$forest_bootstrap_path <- path
    if (!is.null(model_data$forest_model)) {
      model_data$forest_bootstrap_check <- validate_bootstrap_thetas(path, model_data$forest_model)
    }
  })

  observeEvent(input$forest_dataspec_upload, {
    fp <- shinyFiles::parseFilePaths(roots = c(root = "/"), input$forest_dataspec_upload)
    path <- as.character(fp$datapath)
    if (length(path) == 0 || nchar(path) == 0) return()
    model_data$forest_dataspec_upload_path <- path
  })

  output$forest_dataspec_display <- renderText({
    if (is.null(model_data$forest_dataspec_upload_path)) return("No data spec YAML selected yet.")
    model_data$forest_dataspec_upload_path
  })

  output$forest_bootstrap_check <- renderText({
    if (is.null(model_data$forest_bootstrap_path)) return("No bootstrap file selected yet.")
    msg <- model_data$forest_bootstrap_check
    if (is.null(msg)) {
      paste0("✓ ", model_data$forest_bootstrap_path, "\nAll model THETAs found in bootstrap file.")
    } else {
      paste0(model_data$forest_bootstrap_path, "\n⚠ ", msg)
    }
  })

  output$forest_bayes_chain_inputs <- renderUI({
    n <- input$forest_bayes_n_chains
    if (is.null(n) || is.na(n) || n < 1) return(NULL)
    panels <- lapply(seq_len(n), function(i) {
      fluidRow(
        column(8, shinyFiles::shinyFilesButton(paste0("forest_bayes_ext_", i), paste0("Select Chain ", i, " .ext File"),
                                               title = paste0("Select .ext file for chain ", i),
                                               multiple = FALSE, icon = icon("upload"))),
        column(4, verbatimTextOutput(paste0("forest_bayes_ext_display_", i)))
      )
    })
    tagList(panels)
  })

  lapply(1:10, function(i) {
    local({
      ii <- i
      observeEvent(input[[paste0("forest_bayes_ext_", ii)]], {
        fp <- shinyFiles::parseFilePaths(roots = c(root = "/"), input[[paste0("forest_bayes_ext_", ii)]])
        path <- as.character(fp$datapath)
        if (length(path) == 0 || nchar(path) == 0) return()
        paths <- model_data$forest_bayes_paths
        if (is.null(paths)) paths <- list()
        paths[[ii]] <- path
        model_data$forest_bayes_paths <- paths
      })
      output[[paste0("forest_bayes_ext_display_", ii)]] <- renderText({
        paths <- model_data$forest_bayes_paths
        if (is.null(paths) || length(paths) < ii || is.null(paths[[ii]])) "Not selected" else paths[[ii]]
      })
    })
  })

  # --- Generate: validate inputs, build yaml/data_spec, launch background job ---
  observeEvent(input$forest_generate, {
    if (isTRUE(model_data$forest_job_running)) {
      model_data$forest_job_status <- "✗ A forest plot job is already running -- please wait for it to finish before starting another."
      return()
    }

    mod <- model_data$forest_model
    if (is.null(mod)) {
      model_data$forest_job_status <- "✗ Please load a model first."
      return()
    }

    endpoints <- input$forest_endpoints
    if (is.null(endpoints) || length(endpoints) == 0) {
      model_data$forest_job_status <- "✗ Please select at least one endpoint."
      return()
    }

    covs <- input$forest_covariates_selected
    if (is.null(covs) || length(covs) == 0) {
      model_data$forest_job_status <- "✗ Please select at least one covariate to vary."
      return()
    }

    if (is.null(input$forest_output_stem) || nchar(trimws(input$forest_output_stem)) == 0) {
      model_data$forest_job_status <- "✗ Please provide an output stem."
      return()
    }

    if (is.null(input$forest_amt) || is.na(input$forest_amt)) {
      model_data$forest_job_status <- "✗ Please provide a dose amount."
      return()
    }

    modout <- unlist(outvars(mod), use.names = FALSE)
    if (!(input$forest_pred %in% modout)) {
      model_data$forest_job_status <- paste0("✗ Output variable '", input$forest_pred, "' not found in model output.")
      return()
    }
    if ("aucinf" %in% endpoints && !("CL" %in% modout)) {
      model_data$forest_job_status <- "✗ AUCinf endpoint requires 'CL' in the model output but it was not found."
      return()
    }
    param_var <- if ("parameter" %in% endpoints) input$forest_endpoint_param_parameter else NULL
    if (!is.null(param_var) && !(param_var %in% modout)) {
      model_data$forest_job_status <- "✗ Selected model parameter output variable not found in model output."
      return()
    }

    labels <- setNames(lapply(endpoints, function(ep) input[[paste0("forest_endpoint_label_", ep)]]), endpoints)
    endpoint_spec <- build_forest_endpoints(endpoints, labels, param_var)

    references <- list()
    values_list <- list()
    cov_meta <- list()
    validation_error <- NULL
    for (cn in covs) {
      ref_val <- input[[paste0("forest_cov_ref_", cn)]]
      values_str <- input[[paste0("forest_cov_values_", cn)]]
      if (is.null(ref_val) || is.na(ref_val) || is.null(values_str) || nchar(trimws(values_str)) == 0) {
        validation_error <- paste0("✗ Please provide a reference value and a values grid for covariate '", cn, "'.")
        break
      }
      is_categorical <- isTRUE(input[[paste0("forest_cov_categorical_", cn)]])
      parsed_vals <- parse_numeric_values(values_str)
      if (length(parsed_vals) == 0 || any(is.na(parsed_vals))) {
        validation_error <- paste0(
          "✗ Could not parse values for covariate '", cn, "': \"", values_str,
          "\". Use comma-separated numbers, e.g. \"50, 70, 100\"."
        )
        break
      }
      values_list[[cn]] <- parsed_vals
      if (is_categorical) {
        labels_str <- input[[paste0("forest_cov_labels_", cn)]] %||% ""
        labels <- parse_label_list(labels_str)
        if (length(labels) != length(parsed_vals)) {
          validation_error <- paste0(
            "✗ Covariate '", cn, "' has ", length(parsed_vals), " value(s) (\"", values_str,
            "\") but ", length(labels), " label(s) (\"", labels_str,
            "\"). Provide exactly one label per value, in the same order."
          )
          break
        }
        cov_meta[[cn]] <- list(type = "categorical",
                               short = input[[paste0("forest_cov_short_", cn)]] %||% cn,
                               values = parsed_vals,
                               decode = labels)
      } else {
        cov_meta[[cn]] <- list(type = "continuous",
                               short = input[[paste0("forest_cov_short_", cn)]] %||% cn,
                               unit = input[[paste0("forest_cov_unit_", cn)]] %||% "")
      }
      references[[cn]] <- ref_val
    }
    if (!is.null(validation_error)) {
      model_data$forest_job_status <- validation_error
      return()
    }
    covariate_spec <- build_forest_covariates(covs, references, values_list)

    model_path <- model_data$forest_model_path
    if (is.null(model_path) || !file.exists(model_path)) {
      model_data$forest_job_status <- "✗ Please load a model first."
      return()
    }
    modelname <- tools::file_path_sans_ext(basename(model_path))
    ## Run entirely in a session temp dir -- the app never writes into the
    ## user's project folder. Users get outputs only via the download buttons.
    forest_dir <- tempfile(pattern = paste0("forest_", modelname, "_"))
    setup_forest_dir(forest_dir)

    data_spec_path <- file.path(forest_dir, "data_spec.yaml")
    data_spec_built <- identical(input$forest_dataspec_source, "build")
    if (data_spec_built) {
      data_spec_list <- build_forest_data_spec(cov_meta)
      yaml::write_yaml(data_spec_list, data_spec_path)
    } else {
      if (is.null(model_data$forest_dataspec_upload_path)) {
        model_data$forest_job_status <- "✗ Please upload a yspec data spec YAML, or switch to automatic."
        return()
      }
      file.copy(model_data$forest_dataspec_upload_path, data_spec_path, overwrite = TRUE)
    }
    model_data$forest_data_spec_built <- data_spec_built
    model_data$forest_data_spec_path <- data_spec_path

    unc_method <- input$forest_uncertainty_method
    uncertainty_spec <- NULL
    if (unc_method == "covariance") {
      if (is.null(model_data$forest_cov_path)) {
        model_data$forest_job_status <- "✗ Please select a .cov file for the covariance uncertainty source."
        return()
      }
      uncertainty_spec <- build_forest_uncertainty("covariance", covariance_path = model_data$forest_cov_path)
    } else if (unc_method == "bootstrap") {
      if (is.null(model_data$forest_bootstrap_path)) {
        model_data$forest_job_status <- "✗ Please select a bootstrap raw_results CSV file."
        return()
      }
      uncertainty_spec <- build_forest_uncertainty("bootstrap", bootstrap_path = model_data$forest_bootstrap_path)
    } else if (unc_method == "bayes") {
      paths <- model_data$forest_bayes_paths
      n_chains <- input$forest_bayes_n_chains
      chain_paths <- if (!is.null(paths)) unlist(paths[seq_len(n_chains)]) else character()
      if (length(chain_paths) < n_chains) {
        model_data$forest_job_status <- "✗ Please select an .ext file for every Bayesian chain."
        return()
      }
      uncertainty_spec <- build_forest_uncertainty("bayes", bayes_paths = chain_paths)
    }

    regimen_spec <- build_forest_regimen(
      type = input$forest_regimen_type,
      amt = input$forest_amt,
      cmt = input$forest_cmt,
      ii = input$forest_ii %||% 24,
      addl = input$forest_addl %||% 1,
      tau = input$forest_tau %||% 24,
      pred = input$forest_pred
    )

    yaml_spec <- build_forest_yaml_spec(
      input_model = normalizePath(model_path),
      data_spec = "data_spec.yaml",
      output_directory = ".",
      output_stem = input$forest_output_stem,
      nRep = input$forest_nrep,
      output_filetype = input$forest_filetype,
      width = input$forest_width,
      height = input$forest_height,
      text_size = input$forest_text_size,
      shape_size = input$forest_shape_size,
      shaded_interval = c(input$forest_shade_low, input$forest_shade_high),
      ## Quarto CLI (separate from the R quarto package) isn't guaranteed on the
      ## deployment server; the combined PDF report is only built when running the
      ## downloaded reproducibility bundle's forest.yaml/forest.R standalone.
      quarto_output = FALSE,
      regimen = regimen_spec,
      endpoint = endpoint_spec,
      covariates = covariate_spec,
      uncertainty = uncertainty_spec
    )
    yaml_path <- file.path(forest_dir, "forest.yaml")
    yaml::write_yaml(yaml_spec, yaml_path)
    model_data$forest_yaml_path <- yaml_path
    model_data$forest_outdir <- forest_dir

    model_data$forest_plot_files <- NULL
    proc <- run_forest_job(forest_dir)
    model_data$forest_process <- proc
    model_data$forest_job_running <- TRUE
    model_data$forest_job_status <- ""
    shinyjs::disable("forest_generate")
  })

  # --- Poll background job ---
  observe({
    if (!isTRUE(model_data$forest_job_running)) return()
    invalidateLater(1500, session)

    proc <- model_data$forest_process
    if (is.null(proc)) {
      model_data$forest_job_running <- FALSE
      shinyjs::enable("forest_generate")
      return()
    }

    if (proc$is_alive()) {
      return()
    } else {
      model_data$forest_job_running <- FALSE
      shinyjs::enable("forest_generate")
      exit_status <- proc$get_exit_status()
      if (identical(exit_status, 0L)) {
        stem <- input$forest_output_stem
        plot_files <- list.files(model_data$forest_outdir,
                                 pattern = paste0("^", stem, "([-_]?[0-9]+)?\\.(png|pdf)$"),
                                 full.names = TRUE)
        model_data$forest_plot_files <- plot_files
        if (length(plot_files) > 0) {
          model_data$forest_job_status <- paste0("✓ Forest plot generation complete. Generated ", length(plot_files), " plot(s).")
        } else {
          model_data$forest_job_status <- "⚠ Job finished but no output plots were found."
        }
      } else {
        log_path <- file.path(model_data$forest_outdir, "forest_run.log")
        log_tail <- if (file.exists(log_path)) {
          lines <- tail(readLines(log_path, warn = FALSE), 25)
          ## External tools invoked by forest.R (pandoc/lualatex) can emit
          ## non-UTF-8 bytes; sanitize before this reaches a Shiny text output,
          ## otherwise the invalid UTF-8 websocket frame silently kills the session.
          paste(iconv(lines, from = "UTF-8", to = "UTF-8", sub = "byte"), collapse = "\n")
        } else {
          "(no log available)"
        }
        model_data$forest_job_status <- paste0(
          "✗ forest.R failed (exit status ", exit_status, ").\n\n", log_tail
        )
      }
    }
  })

  output$forest_job_status <- renderText({ model_data$forest_job_status })

  output$forest_endpoint_selector_ui <- renderUI({
    files <- model_data$forest_plot_files
    if (is.null(files) || length(files) == 0) return(helpText("No plots generated yet."))
    choices <- setNames(files, basename(files))
    selectInput("forest_selected_plot", "View Plot:", choices = choices)
  })

  ## The PNG on disk is rendered by forest.R at the user's chosen
  ## width/height/text.size/shape.size -- those govern the downloaded file
  ## only. The in-app preview always scales that same file to fit its
  ## container, independent of the file's native pixel dimensions.
  output$forest_plot_view <- renderImage({
    sel <- input$forest_selected_plot
    if (is.null(sel) || !file.exists(sel) || !grepl("\\.png$", sel, ignore.case = TRUE)) {
      return(list(src = "", alt = "PDF selected -- in-app preview is PNG-only. Use Download Plot to get the file."))
    }
    list(src = sel, contentType = "image/png",
         style = "max-width: 100%; max-height: 480px; width: auto; height: auto; object-fit: contain;")
  }, deleteFile = FALSE)

  output$download_forest_plot <- downloadHandler(
    filename = function() {
      sel <- input$forest_selected_plot
      if (is.null(sel)) "forest_plot.png" else basename(sel)
    },
    content = function(file) {
      sel <- input$forest_selected_plot
      if (is.null(sel) || !file.exists(sel)) return()
      file.copy(sel, file, overwrite = TRUE)
    }
  )

  output$download_forest_yaml <- downloadHandler(
    filename = function() "forest.yaml",
    content = function(file) {
      if (is.null(model_data$forest_yaml_path) || !file.exists(model_data$forest_yaml_path)) return()
      file.copy(model_data$forest_yaml_path, file, overwrite = TRUE)
    }
  )

  output$download_forest_dataspec <- downloadHandler(
    filename = function() "data_spec.yaml",
    content = function(file) {
      if (is.null(model_data$forest_data_spec_path) || !file.exists(model_data$forest_data_spec_path)) return()
      file.copy(model_data$forest_data_spec_path, file, overwrite = TRUE)
    }
  )

  output$download_forest_bundle <- downloadHandler(
    filename = function() "forest_plot_bundle.zip",
    content = function(file) {
      if (is.null(model_data$forest_outdir)) return()
      zip_forest_bundle(model_data$forest_outdir, file)
    }
  )

}

# Run the application
shinyApp(ui = ui, server = server)
