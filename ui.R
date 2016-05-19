library(shiny)
#library(shinythemes)

shinyUI(navbarPage("Visual Pruner",
    tabPanel("Upload",
        fluidRow(
            column(6,
                h4('Data:'),
                #actionButton('useExample', "Use example data"),
                #actionButton('uploadData', "Upload data (.csv or .rds)"),
                radioButtons('useExampleData', NULL,
                    c("Use example data" = 1,
                    "Upload data (.csv or .rds)" = 0),
                    1
                ),
                #h4('Specify attributes:'),
                #checkboxInput('header', 'Header', TRUE),
                #radioButtons('sep', 'Separator',
                #    c(Comma = ',',
                #    Semicolon = ';',
                #    Tab = '\t'),
                #    ','
                #),
                #radioButtons('quote', 'Quote',
                #    c(None='',
                #    'Double Quote'='"',
                #    'Single Quote'="'"),
                #    '"'
                #),
                #tags$hr(),
                #h4('Upload file (.csv or .rds only):'),
                #fileInput('datafile', 
                #    label= NULL,
                #    accept= NULL
                #    # as far as I can tell, 'accept' does not actually limit anything
                #    #accept= c(
                #    #    'text/csv', 
                #    #    'text/comma-separated-values,text/plain', 
                #    #    '.csv'#,
                #    #    #'application/octet-stream'
                #    #    )
                #)
                uiOutput("changeUploadedFile"),
                uiOutput("chooseDatafile"),
                tags$hr(),
                #h4('Dataset dimensions:'),
                textOutput("dataDimText"),
                tags$hr(),
                h4('Treatment indicator:'),
                uiOutput("chooseGroup"),
                tags$hr(),
                textOutput("groupLevelText1"),
                textOutput("groupLevelText2")
            ), # end column
            column(6,
                h4('Preferences for graphs:'),
                numericInput('numCont', 
                    'Treat numeric variables as continuous if they have at least __ distinct values',
                    value= 10, min= 1),
                numericInput('xDigits', 
                    'Starting number of decimal places to show for covariates:',
                    value= 2, min= 1, max= 10)
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
                tags$hr(),
                h4('Propensity score model:'),
                helpText('Type RHS of R formula* for lrm(), e.g.'),
                verbatimTextOutput("psHelpText"),
                uiOutput('getFormula'),
                actionButton('psTypedButton', "I have finished typing"),
                textOutput('psNeedsCheckingText'),
                tags$head(tags$style(
                    "#psNeedsCheckingText{color: red; font-size: 10px; }" )),
                tags$hr(),
                h4("Syntax check:"),
                textOutput('psFormulaProblemText'),
                h4("Variable-name check:"),
                textOutput('psVarsProblemText'),
                h4("Model-fitting check:"),
                textOutput('psFitProblemTextPrePruning'),
                tags$hr(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$hr(),
                helpText(a("*R formula help online", 
                    href="https://stat.ethz.ch/R-manual/R-devel/library/stats/html/formula.html", 
                    target="_blank"))
            ), # end column
            column(6,
                h4('Variables to view and restrict:'),
                uiOutput("chooseVarsToRestrict"),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$hr(),
                textOutput("dataNonmissingDimText")
            ) # end column
        ) # end fluidRow 
    ), # end variable-selection panel
    tabPanel("Prune",
        fluidRow(
            h2("Estimated propensity score distributions"),
            textOutput('noPSText'),
            tags$head(tags$style(
                "#noPSText{color: red; font-size: 20px; }" )), 
            column(6,
                uiOutput("psPlotui")
            ), # end column
            column(6,
                uiOutput("logitpsPlotui")
            ), # end column
            checkboxInput('useProbScale', 
                'Use probability-scale plot for brushing', FALSE)
        ), # end fluidRow 
        fluidRow(
            column(width= 6, offset= 3,
                h4("After brushing, points OUTSIDE the brushed area will be shown in rug plots below.")
            ) # end column
        ), # end fluidRow 
        tags$hr(),
        h2("Covariate distributions"),
        textOutput('noSelectedVarsText'),
        tags$head(tags$style(
            "#noSelectedVarsText{color: red; font-size: 20px; }" )), 
        fluidRow(
            column(width= 4, offset= 4,
                actionButton('xgraphsUpdateButton', 
                    HTML("Update covariate graphs")),
                tags$br(),
                tags$br(),
                actionButton('PSCalcUpdateButton', 
                    HTML("Recalculate PS for pruned sample<br/>(will update all graphs)")),
                textOutput('psFitProblemTextPostPruning'),
                tags$head(tags$style(
                    "#psFitProblemTextPostPruning{color: red; font-size: 12px; }" )) 
            ), # end column
            column(4,
                h4("Current sample size"),
                tableOutput("pruneTable")
            ) # end column
        ), # end fluidRow 
        fluidRow(
            column(12,
                tags$hr()
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
