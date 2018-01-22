library(shiny)
#library(shinythemes)

shinyUI(navbarPage("Visual Pruner", id= "mainNavbarPage",
    theme= "sandstone_LS.css",
    tabPanel("Upload",
        fluidRow(
            column(6,
                h4('Data:'),
                radioButtons('useExampleData', NULL,
                    c(
                        "Upload data (.csv or .rds)" = 0,
                        "Use example data" = 1
                    ),
                    0
                ),
                uiOutput("changeUploadedFile"),
                uiOutput("chooseDatafile"),
                tags$br(),
                h4('Treatment indicator:'),
                uiOutput("chooseGroup"),
                actionButton('groupChosenButton', "Click to confirm/update")
            ), # end column
            column(6,
                h4('Dataset information:'),
                uiOutput("noDataChosenText"),
                uiOutput("dataFnameText1"),
                uiOutput("dataFnameText2"),
                uiOutput("dataFnameText3"),
                uiOutput("dataDimText1"),
                uiOutput("dataDimText2"),
                tags$br(),
                uiOutput("groupLevelText1"),
                tableOutput("groupLevelTable")
            ) # end column
        ) # end fluidRow 
    ), # end data-import panel
    ##################################################
    ##################################################
    ##################################################
    tabPanel("Specify",
        fluidRow(
            column(5,
                h4('Handling missing values:'),
                h5('If units have missing values for variables in the propensity score model,'),
                radioButtons('forPSCompleteCasesOnly', NULL,
                    c(
                    "replace the missing values with the mean or mode" = 0,
                    "exclude the units from PS calculation" = 1
                    ),
                    0
                )
            ), # end column
            column(6, offset = 1,
                uiOutput("dataNonmissingDimText1"),
                textOutput("dataNonmissingDimText2")
            ) # end column
        ), # end fluidRow 
        fluidRow(
            column(5,
                h4('Propensity-score estimation method:'),
                radioButtons('psMethod', NULL,
                    c(
                        "Logistic regression" = 0,
                        "Probit regression" = 1
                    ),
                    0
                )
            ) # end column
        ), # end fluidRow 
        fluidRow(
            column(5,
                h4('Propensity score model:'),
                #helpText('Type RHS of R formula* for lrm(), e.g.'),
                uiOutput('psHelpGeneral1'),
                uiOutput('psHelpNA1'),
                tags$br(),
                uiOutput('getFormula')
            ), # end column
            column(6, offset = 1,
                h4('Variables in the dataset:'),
                uiOutput('noDataChosenText2'),
                tags$div(style = 'overflow-x: scroll', 
                    tableOutput('othervarsTable')
                )
            ) # end column
        ), # end fluidRow 
        fluidRow(
            column(5,
                actionButton('psTypedButton', "I have finished typing"),
                uiOutput('psNeedsCheckingText')
            ), # end column
            column(6, offset = 1,
                h4("Preliminary syntax check:"),
                uiOutput('psFormulaProblemText'),
                h4("Variable-name check:"),
                uiOutput('psVarsProblemText'),
                h4("Model-fitting check:"),
                uiOutput('psFitProblemTextPrePruning')
            ) # end column
        ), # end fluidRow 
        tags$hr(),
        fluidRow(
            column(12,
                h3("Estimated propensity score distributions"),
                uiOutput('psGraphsNotReady')
            ), # end column
            column(6,
                plotOutput("psPlot",
                    height= 300,
                    width= 'auto')
            ), # end column
            column(6,
                plotOutput("logitpsPlot",
                    height= 300,
                    width= 'auto')
            ) # end column
        ), # end fluidRow 
        tags$hr(),
        fluidRow(
            column(12,
                h3('Summary information from PS estimation procedure'),
                verbatimTextOutput("psSummary")
            ) # end column
        ), # end fluidRow 
        tags$hr(),
        fluidRow(
            column(6,
                h3("Notes"),
                tags$div(
                    tags$p("The plots on the next pages depend on the estimated propensity scores. If you want to view the plots without developing a propensity score model, just type a '1' (numeral one, no quotes) in the formula box above, and a model will be fit using just an intercept.")#, 
                    #tags$p("At this point the missing-value indicator variables are available only within the propensity-score estimation function") 
                )
            ) # end column
        ) # end fluidRow 
    ), # end Specify panel
    ##################################################
    ##################################################
    ##################################################
    tabPanel("Prune",
        fluidRow(
            column(5,
                h4('Variables to view and restrict:'),
                uiOutput("chooseVarsToRestrict"),
                h5('View numeric variables as discrete if they have fewer than __ distinct values in the original dataset:'),
                numericInput('numCont', 
                    NULL,
                    value= 10, 
                    min= 1,
                    step= 1,
                    width= '33%'),
                actionButton('generalGraphUpdateButton', 
                    HTML("(re-)Make graphs using updated variable list<br/>and/or discreteness preferences")),
                tags$br(),
                HTML(paste0(tags$span(class="text-info", 
                    "Note that this may take a few minutes for larger datasets."))),
                tags$br(),
                tags$br(),
                h4('Preferences for graphs:'),
                h5("Point/histogram opacity ('alpha')"),
                sliderInput('alphaSlider', NULL, 
                    min = 0.01, 
                    max = 1, 
                    value = 0.75, 
                    ticks= FALSE,
                    width= '33%'
                    ),
                h5("Symbol size for scatterplots"),
                sliderInput('pointsizeSlider', NULL, 
                    min = 0.01, 
                    max = 3, 
                    value = 1, 
                    ticks= FALSE,
                    width= '33%'
                    )
            ), # end column
            column(width= 6, offset = 1,
                h4("Current sample size"),
                tableOutput("pruneTable"),
                tags$br(),
                h4('Estimated propensity score distribution (brushable)'),
                checkboxInput('shadeBrushedArea',
                    label= 'Shade brushed area on covariate graphs',
                    value = TRUE),
                h5('Legend for this plot applies to all plots on page.'),
                uiOutput("logitpsPlotBrushable",
                    height= 300,
                    width= 'auto'),
                uiOutput('needPSText')
            ) # end column
        ), # end fluidRow 
        tags$hr(),
        fluidRow(
            #h4('After pruning:'),
            column(width= 4, offset= 2,
                actionButton('xgraphsUpdateButton', 
                    HTML("Update covariate graphs<br/>to reflect pruning choices"),
                    class="btn btn-primary"
                )
            ), # end column
            column(width= 4, offset= 2,
                actionButton('PSCalcUpdateButton', 
                    HTML("Recalculate PS for pruned sample<br/>(will also update all graphs)"),
                    class="btn btn-primary"
                ),
                uiOutput('psFitProblemTextPostPruning')
            ) # end column
        ), # end fluidRow 
        fluidRow(
            column(width= 12, offset= 3,
                HTML(paste0(tags$span(class="text-info", 
                    "(If making the plots was slow the first time, expect a delay after clicking either button.)")))
            ) # end column
        ), # end fluidRow 
        uiOutput("covariatePlotsAndInputs"),
        tags$hr(),
        fluidRow(
            column(6,
                h3("Notes"),
                tags$div(
                    tags$p("The thin black lines in the stripcharts indicate the mean; in the scatterplots, the thin black lines are loess curves."), 
                    tags$p("After pruning, the pruning limits you specified for continuous variables will be moved inward to the nearest sample value."),
                    tags$p(paste0("The upper subplots for each covariate include all points in the (pruned) dataset,",
                        " even if those points are missing from the subplots immediately below because the propensity score is missing.",
                        " This can happen if some variables have missing values and only complete cases are used to estimate the propensity score.")) 
                )
            ) # end column
        ) # end fluidRow 
    ), # end Prune panel
    ##################################################
    ##################################################
    ##################################################
    tabPanel("Compare",
        fluidRow(
            column(4, 
                h4('Variables to view:'),
                uiOutput("chooseVarsForSMD"),
                actionButton('smdGraphUpdateButton', 
                    HTML("(re-)Make graph using updated variable list")
                ),
                #h4('Show the following weightings in the SMD plot:'),
                uiOutput('introduceWeightingCheckboxes'),
                uiOutput('chooseToShowATE'),
                uiOutput('chooseToShowATT'),
                uiOutput('chooseToShowATM'),
                HTML(paste0(tags$span(class="text-info", 
                    "Note that each one may take several minutes."))),
                #verbatimTextOutput("tabonetest")
                tags$br(),
                checkboxInput('drawLinesSMD',
                    label= 'Connect points with line segments',
                    value = TRUE
                    )
            ), # end column
            column(8, 
                #uiOutput('noSMDText'),
                plotOutput('SMDPlot',
                    height= 800,
                    width= 'auto'
                ),
                tags$br(),
                HTML(paste0(tags$span(class="text-info", 
                    "Note that for larger datasets, the plot may take a few minutes to refresh.")))
            ) # end column
        ), # end fluidRow
        tags$hr(),
        fluidRow(
            column(6,
                h3("Notes"),
                uiOutput("explainWtsText"),
                tags$div(
                    tags$p(HTML(paste0(
                        "For information about how the absolute standardized mean differences shown in the plot above are calculated, see the documentation for ",
                            a("the tableone package", 
                                href="https://cran.r-project.org/web/packages/tableone/index.html", 
                                target="_blank"),
                        "."
                    ))), 
                    tags$p("The dotted vertical line at 0.1 marks a degree of imbalance that many researchers consider to be unacceptable."), 
                    tags$p("Visual Pruner currently displays in the SMD plot only those variables selected for viewing on the 'Prune' page. In general it is important to consider standardized mean differences for squared terms and interactions, as well as for missingness indicators. We hope to add automatic generation of these variables in the future, but in the meantime we recommend adding them to your dataset before importing so that you can select them for viewing.")
                )
            ) # end column
        ) # end fluidRow 
    ), # end SMD panel
    ##################################################
    ##################################################
    ##################################################
    tabPanel("Download",
        fluidRow(
            column(12,
                h4('The following R expression can be copied to select rows to KEEP:'),
                htmlOutput("keepAfterPruningCopyText"),
                tags$br(),
                downloadButton("downloadIncl", 
                    "Download inclusion criteria as .txt file"
                )#,
                #uiOutput("downloadHelp")
            ) # end column
        ), # end fluidRow 
        tags$hr(),
        fluidRow(
            column(6,
                h4('Current propensity score formula:'),
                uiOutput("psCopyText"),
                tags$br(),
                downloadButton("downloadPS", "Download PS formula as .txt file")
            ) # end column
        ), # end fluidRow 
        tags$hr(),
        fluidRow(
            column(6,
                h3("Notes"),
                tags$div(
                    tags$p(paste0(
                        "Before fitting the propensity score model, Visual Pruner ",
                        "first imputes missing covariate values with Hmisc::impute() if imputation is selected ",
                        "on the Specify tab. Missingness indicator variables are then created using ",
                        "Hmisc::is.imputed(). ")),
                    tags$p(paste0(
                        "The treatment indicator is converted to a factor ",
                        "before model fitting if it is not a factor already. ",
                        "The treatment variable name has been changed above ",
                        "as a reminder of this behavior, regardless of whether ",
                        "the treatment variable was already a factor."
                        )),
                    tags$p(paste0(
                        "See the R tab for more details. ")) 
                    #tags$p("At this point the missing-value indicator variables are available only within the propensity-score estimation function") 
                )
            ) # end column
        ) # end fluidRow 
    ), # end Copy panel
    ##################################################
    ##################################################
    ##################################################
    tabPanel("About",
        fluidRow(
            column(12,
                #h2("About Visual Pruner"),
                'Visual Pruner is a study-design tool for use with observational studies.', 
                tags$br(),
                tags$br(),
                HTML(paste0(
                    'Instructions for running locally and additional information can be found at ',
                    a("http://biostat.mc.vanderbilt.edu/VisualPruner", 
                        href="http://biostat.mc.vanderbilt.edu/VisualPruner", 
                        target="_blank"),
                    '.' 
                )),
                tags$hr(),
                h4('Version'),
                # see http://r-pkgs.had.co.nz/release.html
                # major.minor.patch.dev; I'm doing major.minor.patch
                '0.9',
                h4('License'),
                'GPL-3',
                h4('Authors'),
                'Lauren R. Samuels and Robert A. Greevy, Jr.',
                h4('Contact'),
                HTML(paste0(
                    a("http://biostat.mc.vanderbilt.edu/LaurieSamuels", 
                        href="http://biostat.mc.vanderbilt.edu/LaurieSamuels", 
                        target="_blank")
                )),
                tags$br(),
                'We welcome bug reports, suggestions, and requests.',
                tags$hr(),
                h4('Acknowledgements'),
                HTML(paste0(
                    'Visual Pruner is built using the ', 
                    a("R Shiny", 
                        href="http://shiny.rstudio.com", 
                        target="_blank"),
                        ' framework',
                    ', with CSS from ',
                    a("Bootswatch", 
                        href="http://bootswatch.com", 
                        target="_blank"),
                    ' (slightly modified).'
                )),
                tags$br(),
                'Many thanks to Meira Epplein, Qi Liu, Dale Plummer, Bryan Shepherd, and Matt Shotwell for their valuable suggestions.'
            ) # end column
        ) # end fluidRow
    ), # end About panel
    ##################################################
    ##################################################
    ##################################################
    tabPanel("R",
        fluidRow(
            column(12,
                #h4('You can ignore this tab if you are not interested in the R packages or source code used in making this app.'),
                HTML(paste0(tags$span(class="text-info", 
                    "You can ignore this tab if you are not interested in the R packages or source code used in making this app."))),
                tags$hr()
            ) # end column
        ), # end fluidRow 
        fluidRow(
            h3('R session information'),
            column(12,
                verbatimTextOutput("sessionInf"),
                tags$hr()
            ) # end column
        ), # end fluidRow 
        tags$hr(),
        fluidRow(
            h3('Auxiliary files (scroll down for main server.R and ui.R files)'),
            column(4,
                h4('plottingFunctions.R'),
                verbatimTextOutput("plottingFuncCode")
            ), # end column
            column(4,
                h4('psFunctions.R'),
                verbatimTextOutput("psFuncCode")
            ), # end column
            column(4,
                h4('smdFunctions.R'),
                verbatimTextOutput("smdFuncCode")
            ) # end column
        ), # end fluidRow 
        tags$hr(),
        fluidRow(
            h3('Main files'),
            column(6,
                h4('server.R'),
                verbatimTextOutput("serverCode")
            ), # end column
            column(6,
                h4('ui.R'),
                verbatimTextOutput("uiCode")
            ) # end column
        ) # end fluidRow 
    ) # end Code panel
))
