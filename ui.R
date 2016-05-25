library(shiny)
#library(shinythemes)

shinyUI(navbarPage("Visual Pruner",
    tabPanel("Upload",
        fluidRow(
            column(6,
                h4('Data:'),
                radioButtons('useExampleData', NULL,
                    c("Use example data" = 1,
                    "Upload data (.csv or .rds)" = 0),
                    1
                ),
                uiOutput("changeUploadedFile"),
                uiOutput("chooseDatafile"),
                tags$br(),
                h4('Treatment indicator:'),
                uiOutput("chooseGroup")
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
                tableOutput("groupLevelTable"),
                tags$br(),
                uiOutput("othervarsText1"),
                tableOutput("othervarsTable")
            ) # end column
        ) # end fluidRow 
    ), # end data-import panel
    tabPanel("Specify",
        fluidRow(
            column(6,
                h4('Handling missing values in PS model:'),
                radioButtons('completeCasesOnly', NULL,
                    c("Use complete cases only" = 1,
                    "Impute missing values using mean or mode" = 0),
                    1
                ),
                tags$br(),
                h4('Propensity score model:'),
                helpText('Type RHS of R formula* for lrm(), e.g.'),
                verbatimTextOutput("psHelpText"),
                uiOutput('getFormula'),
                actionButton('psTypedButton', "I have finished typing"),
                textOutput('psNeedsCheckingText'),
                tags$head(tags$style(
                    "#psNeedsCheckingText{color: magenta; font-size: 10px; }" )),
                tags$br(),
                tags$br(),
                helpText(a("*R formula help online", 
                    href="https://stat.ethz.ch/R-manual/R-devel/library/stats/html/formula.html", 
                    target="_blank"))
            ), # end column
            column(6,
                h4("Preliminary syntax check:"),
                uiOutput('psFormulaProblemText'),
                h4("Variable-name check:"),
                uiOutput('psVarsProblemText'),
                h4("Model-fitting check:"),
                uiOutput('psFitProblemTextPrePruning'),
                tags$br(),
                tags$br(),
                uiOutput("dataNonmissingDimText1"),
                uiOutput("dataNonmissingDimText2"),
                textOutput("dataNonmissingDimText3")
            ) # end column
        ), # end fluidRow 
        tags$hr(),
        fluidRow(
            h2("Estimated propensity score distributions"),
            uiOutput('psGraphsNotReady'),
            column(12, 
                h4("After brushing, points OUTSIDE the brushed area will be shown in rug plots on next tab.")
            ), # end column
            column(6,
                uiOutput("psPlotui")
            ), # end column
            column(6,
                uiOutput("logitpsPlotui")
            ), # end column
            checkboxInput('useProbScale', 
                'Use probability-scale plot for brushing', FALSE)
        ) # end fluidRow 
    ), # end Specify panel
    tabPanel("Prune",
        fluidRow(
            column(6,
                h4('Variables to view and restrict:'),
                uiOutput("chooseVarsToRestrict"),
                actionButton('varsToViewUpdateButton', 
                    HTML("Make graphs")),
                tags$br(),
                h4('Preferences for graphs:'),
                h5('Treat numeric variables as continuous if they have at least __ distinct values:'),
                numericInput('numCont', 
                    NULL,
                    value= 10, min= 1),
                h5('Number of decimal places to show for covariates:'),
                numericInput('xDigits', 
                    NULL,
                    value= 2, min= 1, max= 10)
            ), # end column
            column(width= 6,
                actionButton('xgraphsUpdateButton', 
                    HTML("Update covariate graphs")),
                tags$br(),
                tags$br(),
                actionButton('PSCalcUpdateButton', 
                    HTML("Recalculate PS for pruned sample<br/>(will update all graphs)")),
                textOutput('psFitProblemTextPostPruning'),
                tags$head(tags$style(
                    "#psFitProblemTextPostPruning{color: red; font-size: 12px; }" )),
                tags$br(),
                h4("Current sample size"),
                tableOutput("pruneTable")
            ) # end column
        ), # end fluidRow 
        uiOutput("univariatePlotsAndInputs")
    ), # end variable-selection panel
    tabPanel("Copy",
        fluidRow(
            column(6,
                h4('The following R expression can be copied to select rows to KEEP:'),
                htmlOutput("keepAfterPruningCopyText"),
                tags$hr(),
                h4('Current propensity score formula:'),
                textOutput("psCopyText")
            ) # end column
        ) # end fluidRow 
    ), # end Copy panel

    tabPanel("About",
        h1("About Visual Pruner"),
        'Visual Pruner is a study-design tool for use with observational studies. We hope to provide further documentation soon.',
        tags$hr(),
        h4('Version'),
        '0.0.0.9005',
        h4('License'),
        'GPL-3',
        h4('Authors'),
        'Lauren R. Samuels and Robert A. Greevy, Jr.',
        h4('Contact'),
        a("http://biostat.mc.vanderbilt.edu/LaurieSamuels", 
            href="http://biostat.mc.vanderbilt.edu/LaurieSamuels", 
            target="_blank"),
        tags$br(),
        'We welcome bug reports, suggestions, and requests.',
        tags$hr(),
        h4('Acknowledgements'),
        'Many thanks to Qi Liu for helpful suggestions.',
        tags$br(),
        'Visual Pruner is built using the', tags$code('R shiny'), 'framework.',
        '(See the R tab for more information)'
    ), # end About panel

    tabPanel("R",
        fluidRow(
            h4('You can ignore this tab if you are not interested in the R packages or source code used in making this app.'),
            tags$hr()
        ), # end fluidRow 
        fluidRow(
            h4('R session information:'),
            verbatimTextOutput("sessionInf"),
            tags$hr()
        ), # end fluidRow 
        fluidRow(
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
