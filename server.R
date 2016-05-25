######################################################
#TODO:
# Remember: when updating on server, use library(Cairo)
#######################################################

library(shiny)
library(ggplot2)
library(rms)
library(data.table)
#library(Cairo) # for better graphics on Linux servers
#options(shiny.usecairo= TRUE)

#  colors, from http://www.sron.nl/~pault/
# these are slightly less cb-friendly, but optimized for screen
s.red    <- "#EE3333"
s.orange <- "#EE7722"
s.yellow <- "#FFEE33"
s.green  <- "#66AA55"
s.teal   <- "#11AA99"
s.dkblue <- "#3366AA"
s.magenta<- "#992288"
s.mustard<- "#CCCC55"
myColorScale <- c(
    s.orange,
    s.dkblue,
    s.magenta,
    s.teal  ,
    s.red    ,
    s.yellow,
    s.green ,
    s.mustard
)    

# number of decimal places to use w/ propensity scores
psdig <- 2

# alpha for graphs
alpha1 <- 0.7


# Allow upload of bigger files
# from http://stackoverflow.com/questions/18037737/how-to-change-maximum-upload-size-exceeded-restriction-in-shiny-and-save-user
# The first number is the number of MB
options(shiny.maxRequestSize=30*1024^2)


shinyServer(function(input, output, session) {
    ############################################################
    ############################################################
    ## Datasets 
    
    # I'm using reactiveValues() here so I can set the fileInfo
    #   to NULL when the user decides to upload a new file.
    datInfo <- reactiveValues()
    datInfo$inFileInfo <- NULL
    observe ({
        if (input$useExampleData == 0) {
            datInfo$inFileInfo <- input$dataInfo 
        }
    })
    
    output$changeUploadedFile <- renderUI({
        if (input$useExampleData == 1 | is.null(datInfo$inFileInfo)) return(NULL)
        actionButton("changeUpFile", "Upload different file")
    })
    
    observeEvent(input$changeUpFile, {
        datInfo$inFileInfo <- NULL
    })    
    
    
    output$chooseDatafile <- renderUI({
        #if (input$useExampleData == 1) return(NULL)
        if (input$useExampleData == 1 |
                (input$useExampleData == 0 & !is.null(datInfo$inFileInfo))) return(NULL)
        
        # File input from example on shiny website
        # input$dataInfo will be NULL initially. 
        # After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        fileInput('dataInfo', 
            label= 'Upload file (.csv or .rds only):',
            accept= NULL
            # as far as I can tell, 'accept' does not actually limit anything
            #accept= c(
            #    'text/csv', 
            #    'text/comma-separated-values,text/plain', 
            #    '.csv'#,
            #    #'application/octet-stream'
            #    )
        )
    })
    
    
    dset.orig <- reactive({
        if (input$useExampleData == 0 & is.null(datInfo$inFileInfo)) return(NULL)
    
        if (input$useExampleData == 1) {
            nt <- 300
            nc <- 700
            group <- rep(c("Exposed", "Unexposed"), times= c(nt, nc))
            height_ft <- c(rnorm(nt, 5.4, .3), rnorm(nc, 5.6, .2))
            gender <- c(rbinom(nt, 1, .66), rbinom(nc, 1, .5))
            gender[gender == 0] <- "Male"
            gender[gender == 1] <- "Female"
            age <- c(rnorm(nt, 45, 5), rnorm(nc, 50, 10))
            systolic_bp <- c(rnorm(nt, 115, 5), rnorm(nc, 110, 7)) 
            mydat <- data.table(group, height_ft, gender, age, systolic_bp)
        }  else if (!is.null(datInfo$inFileInfo)) {
            if (grepl("\\.csv\\>", datInfo$inFileInfo$name)) {
                mydat <- fread(datInfo$inFileInfo$datapath,
                    sep= ",",
                    header= TRUE,
                    data.table= TRUE
                )
            } else if (grepl("\\.rds\\>", datInfo$inFileInfo$name)){
                # todo: add error handling. 
                # Also: can it handle data.tables?
                mydat <- as.data.table(readRDS(datInfo$inFileInfo$datapath))
            }    
        }
        mydat
    })
    
    idvarName <- reactive({
        # The name produced by this function will be used as
        #   the name of the id var
        if(is.null(dset.orig())) return (NULL)
        if (input$useExampleData == 0 & is.null(datInfo$inFileInfo)) return(NULL)
        
        proposedName <- "MY__ID"
        while(proposedName %in% names(dset.orig())) {
            proposedName <- paste0(proposedName, "_NEW")    
        }    
        proposedName
    })    
    psvarName <- reactive({
        # The name produced by this function will be used as
        #   the name of the ps var in dset.psgraphs()
        if(is.null(dset.orig())) return (NULL)
        if (input$useExampleData == 0 & is.null(datInfo$inFileInfo)) return(NULL)
        
        proposedName <- "MY__PS"
        while(proposedName %in% names(dset.orig())) {
            proposedName <- paste0(proposedName, "_NEW")    
        }    
        proposedName
    })    
    logitpsvarName <- reactive({
        # The name produced by this function will be used as
        #   the name of the logit ps var in dset.psgraphs()
        if(is.null(dset.orig())) return (NULL)
        if (input$useExampleData == 0 & is.null(datInfo$inFileInfo)) return(NULL)
        
        proposedName <- "MY__LOGITPS"
        while(proposedName %in% names(dset.orig())) {
            proposedName <- paste0(proposedName, "_NEW")    
        }    
        proposedName
    })    

    nonMissingIDs <- reactive({
        if (is.null(varnamesFromModel())) return(NULL)
        
        if (input$completeCasesOnly == 1) {
            na.omit(dset.orig()[, c(varnamesFromModel(), idvarName()), with= FALSE])[[idvarName()]]
        } else {
            dset.orig()[[idvarName()]]
        }
    })
    
    observe({
        # Add a factor version of the treatment indicator, for plotting
        if (!is.null(groupvarFactorName())){
            if (groupvarname() %in% names(dset.orig())) {
                if (!is.factor(dset.orig()[[groupvarname()]])) {
                    dset.orig()[, groupvarFactorName() := factor(dset.orig()[[groupvarname()]])]
                }
            }
        }
    })

    observe({
        # Add an ID variable so we can match obsns w/ the PS dataset
        if (!is.null(idvarName())) { 
            if (!(idvarName() %in% names(dset.orig()))) {
                dset.orig()[, idvarName() := 1:nrow(dset.orig())]
                setkeyv(dset.orig(), idvarName())
            }
        }
    })
    
    dset.psgraphs <- reactive({
        # This dataset is used for making the PS plots.
        if (is.null(logitPS())) return(NULL)
        
        # todo: do this all using data.table
        dat1 <- data.frame(
            id       = unlist(dset.orig()[nonMissingIDs(), idvarName(), with= FALSE]),
            group    = unlist(dset.orig()[nonMissingIDs(), groupvarFactorName(), with= FALSE])
        )
        dat2 <- data.frame(
            id       = PSIDs(),
            logit.ps = logitPS(), 
            ps       = PS()
        )
        dat <- merge(dat1, dat2, by= "id")  # keep only subjects with values in both 
        names(dat)[names(dat) == "id"] <- idvarName()
        names(dat)[names(dat) == "ps"] <- psvarName()
        names(dat)[names(dat) == "logit.ps"] <- logitpsvarName()
        names(dat)[names(dat) == "group"] <- groupvarFactorName()
        
        dat <- as.data.table(dat)  
        setkeyv(dat, idvarName())
        dat
    })    

    observe({
        # for bar plots, need to turn discrete numeric vars into factors
        if (!is.null(varsToView())) {
            for (varname in varsToView()) {
                if (is.character(dset.orig()[[varname]]) | 
                        (is.numeric(dset.orig()[[varname]]) & 
                        !(varIsContinuous()[varname]))) {
                    # todo: make sure levels are transferring right
                    #if (!is.factor(dset.orig()[[varname]])) {
                        dset.orig()[, eval(varname) := factor(dset.orig()[[varname]])]
                    #}
                }
            }
        }    
    })    
    
    xgraphs.ids <- reactive({
        # IDs for making the covariate plots.
        if (is.null(nonMissingIDs())) return(NULL)

        intersect(nonMissingIDs(), idsToKeepAfterPruning())
    })    
    
    
    
    ############################################################
    ############################################################
    ## Variable selection, etc.
    
    varnames.orig <- reactive({
        names(dset.orig())  
    })
    output$chooseGroup <- renderUI({
        if (is.null(dset.orig())) return(NULL)
        selectizeInput('treatmentVarName', 
            #'Which variable is the treatment indicator?', 
            label= NULL, 
            choices= varnames.orig()[sapply(dset.orig(), 
                function(vec) length(unique(vec)) == 2)], 
            selected= NULL,
            multiple= FALSE
            )
    })
    groupvarname <- reactive({
        if (input$useExampleData == 0 & is.null(datInfo$inFileInfo)) return(NULL)
        
        input$treatmentVarName
    })
    groupvarFactorName <- reactive({
        # The name produced by this function will be used as
        # the name of the treatment group var 
        if (is.null(dset.orig())) return(NULL)
        if (is.null(groupvarname())) return(NULL)
        
        if (is.factor(dset.orig()[[groupvarname()]])) {
            return(groupvarname())    
        }    
        proposedName <- paste0(groupvarname(), '.factor')
        while(proposedName %in% names(dset.orig())) {
            proposedName <- paste0(proposedName, "NEW")    
        }    
        proposedName
    })    
    
        
    output$noDataChosenText <- renderUI({
        if (is.null(dset.orig())) {
            HTML(paste0(tags$span(style="color:orange", "No dataset selected.")))
        } else return(NULL)
    })
    output$dataFnameText1 <- renderUI({
        if (is.null(dset.orig()) | input$useExampleData == 1) return(NULL)

        HTML(paste0(tags$h5("Filename:")))
    })
    output$dataFnameText2 <- renderUI({
        if (is.null(dset.orig()) | input$useExampleData == 1) return(NULL)

        HTML(paste0(tags$span(paste0(datInfo$inFileInfo$name))))
    })
    output$dataFnameText3 <- renderUI({
        if (is.null(dset.orig()) | input$useExampleData == 1) return(NULL)

        HTML(paste0(tags$br()))
    })
    output$dataDimText1 <- renderUI({
        if (is.null(dset.orig())) return(NULL)

        HTML(paste0(tags$h5("Dimensions:")))
    })
    output$dataDimText2 <- renderUI({
        if (is.null(dset.orig())) return(NULL)

        HTML(paste0(tags$span(paste0("The dataset has ", ncol(dset.orig()), 
            " columns and ", nrow(dset.orig()), " rows."))))
    })

    output$groupLevelText1 <- renderUI({
        if (is.null(groupvarname())) return(NULL)

        HTML(paste0(tags$h5("The treatment indicator has the following levels:")))
    })
    output$groupLevelTable <- renderTable({
        if (is.null(groupvarname())) return(NULL)

        # the as.character lets this print right if var is already a factor
        dat <- data.frame(as.character(sort(unique(dset.orig()[[groupvarname()]])))) 
        names(dat) <- groupvarname()
        dat
    }, include.rownames = FALSE)

    output$othervarsText1 <- renderUI({
        if (is.null(groupvarFactorName())) return(NULL)

        HTML(paste0(tags$h5("Other variables in the dataset:")))
    })
    output$othervarsTable <- renderTable({
        if (is.null(groupvarFactorName())) return(NULL)

        namesToExclude <- c(idvarName(), groupvarname())
        if (groupvarname() != groupvarFactorName()) {
            namesToExclude <- c(namesToExclude, groupvarFactorName())
        }

        data.frame(Variables= setdiff(varnames.orig(), namesToExclude)) 
    }, include.rownames = FALSE, include.colnames= FALSE)


    possVarsToRestrict <- reactive({
        if (is.null(groupvarname())) return(NULL)
        # We don't want to allow restriction of the treatment var 
        setdiff(varnames.orig(), 
            c(groupvarname(), groupvarFactorName(), idvarName()))  
    })    
    
    output$chooseVarsToRestrict <- renderUI({
        selectizeInput('varsToRestrict', 
            NULL, 
            choices= possVarsToRestrict(), 
            selected= if (is.null(varnamesFromModel())) NULL else 
                #setdiff(varnamesFromModel(), groupvarname()),
                varnamesFromModel(),
            multiple= TRUE
        )
    })
    
    varsToView <- reactive({
        input$varsToRestrict
    })
    numvarsToView <- reactive({
        length(varsToView())    
    })    
    
    output$dataNonmissingDimText1  <- renderUI({
        if (is.null(nonMissingIDs()) | input$completeCasesOnly == 0) return(NULL)
        if (psNotChecked() | is.null(varnamesFromModelOK())) return(NULL)

        HTML(paste0(tags$hr()))
    })
    output$dataNonmissingDimText2  <- renderUI({
        if (is.null(nonMissingIDs()) | input$completeCasesOnly == 0) return(NULL)
        if (psNotChecked() | is.null(varnamesFromModelOK())) return(NULL)

        HTML(paste0(tags$h4("N after excluding rows:")))
    })
    output$dataNonmissingDimText3 <- renderText({
        if (is.null(nonMissingIDs()) | input$completeCasesOnly == 0) return(NULL)
        if (psNotChecked() | is.null(varnamesFromModelOK())) return(NULL)

        paste0("After removal of rows with missing values for the variables selected for the PS model, ",
            "the dataset has ", length(nonMissingIDs()), " rows.")
    })
    
    output$novarsToViewText <- renderText({
        if (is.null(varsToView())) {
            "To select variables, please return to the Specify tab."
        } else NULL
    })

    

    ############################################################
    ############################################################
    ## Propensity score calculation
    output$getFormula <- renderUI({
        if (is.null(dset.orig()) | is.null(groupvarname())) return(NULL)
        textInput('formulaRHS', 
            label= paste0(groupvarname(), ' ~ '), 
            value= ' '
        )
    })

    formRHS <- reactive({
        # Do nothing if button has never been clicked or if no dset
        # Dependencies
        if (input$psTypedButton == 0 | is.null(dset.orig())) return(NULL) 
        
        isolate(input$formulaRHS)
    })
    
    stringFormula <- reactive({
        paste0(groupvarname(), ' ~ ', formRHS())
    })    
    
    
    psForm <- reactive({
        tryCatch(as.formula(stringFormula()),
            error= function(e) {return(NULL)})
    })    
    psFormSyntaxOK <- reactive({
        if (psNotChecked()) return(NULL)
        
        if (is.null(psForm())) {
            FALSE
        } else {
            TRUE
        }
    })

    output$psHelpText <- renderText({
        "    age + gender"
    })
    
    psNotChecked <- reactive({
        if (input$psTypedButton == 0 |
            paste0(groupvarname(), ' ~ ', input$formulaRHS) != stringFormula()) TRUE else FALSE
    })
    
    output$psFormulaProblemText <- renderUI({
        # todo: this was in here before & I can't figure out why.
        #    I think it has something to do with re-fitting after pruning? but I don't know
        #if (is.null(pruneValTextList())) {
        if (psNotChecked()) {
            HTML(paste0(tags$span(style="color:orange", "Not checked yet.")))
        } else if (!psFormSyntaxOK()) {
            HTML(paste0(tags$span(style="color:red", "That is not a usable RHS. Please try again.")))
        } else {
            HTML(paste0(tags$span(style="color:green", "Formula syntax is OK.")))
        }
    })

    PSIDs <- reactive({
        # dependencies
        if (input$PSCalcUpdateButton == 0) return(nonMissingIDs()) 
        input$psTypedButton # in case model changed after pruning
        input$completeCasesOnly # in case model changed after pruning
            
        intersect(nonMissingIDs(), isolate(idsToKeepAfterPruning()))
    })

    varnamesFromModel <- reactive({
        if (is.null(psForm())) return(NULL)
        
        allvars <- setdiff(all.vars(psForm()), groupvarname())
        if (all(allvars %in% names(dset.orig()))) allvars else NULL
    })    
    varnamesFromModelOK <- reactive({
        if (psNotChecked()) return(NULL)
        if (is.null(psFormSyntaxOK())) return(NULL)
        
        if (is.null(isolate(varnamesFromModel()))) {
            FALSE
        } else {
            TRUE
        }
    })
    numvarsFromModel <- reactive({
        if (is.null(varnamesFromModel())) return(NULL)
        length(varnamesFromModel())
    })
    
    output$psVarsProblemText <- renderUI({
        if (psNotChecked() | is.null(psFormSyntaxOK())) {
            HTML(paste0(tags$span(style="color:orange", "Not checked yet.")))
        } else if (!psFormSyntaxOK()) {
            HTML(paste0(tags$span(style="color:orange", "Not checked yet.")))
        } else if (!varnamesFromModelOK()) {
            HTML(paste0(tags$span(style="color:red", "The formula uses variables that are not in the dataset. Please try again.")))
        } else {
            HTML(paste0(tags$span(style="color:green", "All variable names are OK.")))
        }
    })    

    output$psNeedsCheckingText <- renderText({
        if (psNotChecked()) {
            "Remember to click the button when you're done!"
        } else NULL   
    })

    dset.imputed <- reactive({
        if (is.null(varnamesFromModel())) return(NULL)
        if (input$completeCasesOnly == 1) return(NULL)
        
        # have to convert character vars to factors before imputing
        for (varname in varnamesFromModel()) {
            if (is.character(dset.orig()[[varname]])) {
                # todo: make sure levels are transferring right
                #if (!is.factor(dset.orig()[[varname]])) {
                    dset.orig()[, eval(varname) := factor(dset.orig()[[varname]])]
                #}
            }
        }
        
        myvars <- c(varnamesFromModel(), groupvarname())
        dat <- copy(dset.orig()[PSIDs(), myvars, with= FALSE])
        dat[, (myvars) := lapply(.SD, function(x) impute(x, fun= mean)),
            .SDcols = myvars]
        dat
    })
    
    lrmfit <- reactive({
        if (is.null(psForm()) | is.null(varnamesFromModel())) return(NULL)
        
        if (input$completeCasesOnly == 1) {
            tryCatch({lrm(psForm(), 
                data= dset.orig()[PSIDs()])},
                error= function(e) {return(NULL)})
        } else { # impute
            
            tryCatch({lrm(psForm(), 
                data= dset.imputed())},
                error= function(e) {return(NULL)})
        }    
    })

    output$psCopyText <- renderText({
        # dependencies
        if(is.null(lrmfit())) return(NULL)
    
        isolate(stringFormula())
    })

    output$psFitProblemTextPrePruning <- renderUI({
        # dependencies
        input$completeCasesOnly
        
        if (psNotChecked() | is.null(varnamesFromModelOK())) {
            HTML(paste0(tags$span(style="color:orange", "Not checked yet.")))
        } else if (!varnamesFromModelOK()) { # couldn't combine this with above
            HTML(paste0(tags$span(style="color:orange", "Not checked yet.")))
        } else if (is.null(isolate(lrmfit()))) {
            HTML(paste0(tags$span(style="color:red", "The propensity score formula can't be fit using the current dataset. Please modify the model and/or the variables selected for viewing.")))
        } else {
            HTML(paste0(tags$span(style="color:green", "PS model successfully fit.")))
        }
    })
    output$psGraphsNotReady <- renderUI({
        # dependencies
        input$completeCasesOnly
        
        if (is.null(dset.psgraphs())) {
            HTML(paste0(tags$span(style="color:orange", "Scores not yet estimated.")))
        } else {
            NULL
        }
    })
    output$psFitProblemTextPostPruning <- renderText({
        # dependencies
        if (psNotChecked() |  
            input$PSCalcUpdateButton  == 0) return (NULL)

        if (is.null(isolate(lrmfit()))) {
            "The propensity score formula can't be fit using the pruned dataset. Please modify the model and/or the pruning criteria."   
        } else {
            NULL
        }    
    })

    logitPS <- reactive({
        if (is.null(lrmfit())) return(NULL)
        
        lrmfit()$linear.predictors
    })

    PS <- reactive({
        if (is.null(logitPS())) return(NULL)
        
        exp(logitPS()) / (1 + exp(logitPS()))
    })
    
    
    ############################################################
    ############################################################
    ## Reactive text, etc. for PS graphs
    
    useLogit <- reactive({
        !(input$useProbScale)
    })
    
    psbrushmin <- reactive({
        if (is.null(dset.psgraphs()) |
            identical(buttonvalues$lastActionRug, 'choosePlot')) return(NULL)
        if (useLogit()) input$logitpsPlot_brush$xmin else input$psPlot_brush$xmin
    })
    psbrushmax <- reactive({
        if (is.null(dset.psgraphs()) |
            identical(buttonvalues$lastActionRug, 'choosePlot')) return(NULL)
        if (useLogit()) input$logitpsPlot_brush$xmax else input$psPlot_brush$xmax
    })
    scorename <- reactive({
        #if (is.null(psvarName()) | is.null(logitpsvarName())) return(NULL)

        ifelse(useLogit(), logitpsvarName(), psvarName())
    })
    hasScoreOutside <- reactive({
        if (is.null(dset.psgraphs())) return(NULL)

        if (is.null(psbrushmin())) return(rep(FALSE, dset.psgraphs()[, .N]))

        round(dset.psgraphs()[[scorename()]], psdig) < psbrushmin() |
            round(dset.psgraphs()[[scorename()]], psdig) > psbrushmax()
    })
    idsForRug <- reactive({
        if (is.null(hasScoreOutside())) return(NULL)
        
        ids <- dset.psgraphs()[hasScoreOutside(), ][[idvarName()]]
        
        intersect(ids, xgraphs.ids())
    })  

    # todo: is there a way to do this w/o making a third dataset?
    dset.psgraphs.plus <- reactive({
        if (is.null(hasScoreOutside())) return(NULL)

        dat <- copy(dset.psgraphs())

        dat[, outside := hasScoreOutside()]
        dat
    })
    

    ############################################################
    ############################################################
    ## Reactive text related to covariate graphs
        
    varIsContinuous <- reactive({
        if (is.null(varsToView())) return(NULL)
        
        vnames <- varsToView()
        myvec <- rep(FALSE, length(vnames))
        
        for(i in seq_along(vnames)) {
            varname <- vnames[i]
            
            if (is.numeric(dset.orig()[[varname]]) & 
                length(unique(dset.orig()[[varname]])) >= 
                input$numCont) myvec[i] <- TRUE
        }
        names(myvec) <- vnames
        myvec
    })    

    # number of decimal places to use w/ covariate graphs
    xdig <- reactive({
        input$xDigits
    })

    # from http://stackoverflow.com/questions/18816666/shiny-change-data-input-of-buttons
    # Create a reactiveValues object, to let us use settable reactive values
    # We'll use this later on
    buttonvalues <- reactiveValues()
    # To start out, lastActionX == NULL, meaning nothing clicked yet
    buttonvalues$lastActionX <- NULL
    # An observe block for each button, to record that the action happened
    observe({
        if (input$xgraphsUpdateButton != 0 | input$PSCalcUpdateButton != 0) {
            buttonvalues$lastActionX <- 'prune'
        }
    })
    observe({
        if (input$psTypedButton != 0) {
            buttonvalues$lastActionX <- 'specify'
        }
    })
    # and now one for the rug plots
    buttonvalues$lastActionRug <- NULL
    observe({
        if (useLogit() | !useLogit()) {
            buttonvalues$lastActionRug <- 'choosePlot'
        }
    })
    observe({
        if (!is.null(input$logitpsPlot_brush$xmin) | 
            !is.null(input$psPlot_brush$xmin) | 
            !is.null(input$logitpsPlot_brush$xmax) | 
            !is.null(input$psPlot_brush$xmax)) { 
            buttonvalues$lastActionRug <- 'brush'
        }
    })


    # Create the expression to use for pruning
    pruneValRawList <- reactive({
        if (is.null(varsToView())) return(NULL)

        mylist <- vector("list", numvarsToView())

        for (i in 1:numvarsToView()) {
            mylist[[i]]  <- input[[paste0("pruningChoices_", i)]]
        }
        mylist
    })
    keepNARawList <- reactive({
        if (is.null(varsToView())) return(NULL)

        mylist <- vector("list", numvarsToView())

        for (i in 1:numvarsToView()) {
            mylist[[i]]  <- input[[paste0("keepNAInput", i)]] == "1"
        }
        mylist
    })
    pruneValTextList <- reactive({
        # dependencies
        if (is.null(varsToView())) return(NULL)
        if ((input$xgraphsUpdateButton == 0 & input$PSCalcUpdateButton == 0) | 
            identical(buttonvalues$lastActionX, 'specify'))  {
            return(NULL)
        }
        
        mylist <- vector("list", numvarsToView())
        
        for(i in 1:numvarsToView()) {
            myvals  <- isolate(pruneValRawList())[[i]]
            keepna  <- isolate(keepNARawList())[[i]]
            varname <- varsToView()[i]
            
            # We are coding for the ones to KEEP
            mylist[[i]] <-  paste0(
                "((",
                if (keepna) "is.na(" else "!is.na(",
                varname, 
                if (keepna) ")) | " else  ")) & ",
                if (varIsContinuous()[varname]) {
                    myvals_numeric <- suppressWarnings(as.numeric(
                        unlist(strsplit(as.character(myvals), " "))))
                    if (length(na.omit(myvals_numeric)) == 2) {
                        paste0(
                            "(", varname, " >= ", myvals_numeric[1], 
                            " & ", varname, " <= ", myvals_numeric[2], ")"
                        )
                    } else { # user entered invalid text
                        TRUE
                    }
                } else { # var is not continuous
                    if (is.numeric(dset.orig()[[varname]])) {
                        paste0(
                            "(", varname, " %in% c(", paste(myvals, collapse= ","),"))"
                        ) 
                    } else { # character var
                        paste0(
                            "(", paste(varname, " == ", 
                            paste0("'", myvals, "'"), collapse= " | "),
                            ")"
                        )
                    }    
                }, 
                ")"
            )
        } # next i
        mylist
    })    
    exprToKeepAfterPruning <- reactive({
        if (is.null(pruneValTextList())) return(NULL)
        
        do.call("paste", list(pruneValTextList(), collapse= " & " ))
    })    
    output$keepAfterPruningCopyText <- renderUI({
        if (is.null(pruneValTextList())) return(NULL)

        HTML(do.call("paste", list(pruneValTextList(), collapse= " & <br/>" )))
    })    

    idsToKeepAfterPruning <- reactive({
        if (is.null(nonMissingIDs())) return(NULL)

        if (is.null(exprToKeepAfterPruning())) return(nonMissingIDs())
        
        # TODO: I don't remember why I have this error handling here.
        #    Doesn't seem like the best idea.
        tryCatch(dset.orig()[nonMissingIDs()][eval(parse(text= 
            exprToKeepAfterPruning()))][[idvarName()]],
            error= function(e) nonMissingIDs())
    })    
    
    output$pruneTable <- renderTable({
        if (is.null(nonMissingIDs())) return(NULL)
    
        dat <- dset.orig()[xgraphs.ids(), .N, by= eval(groupvarFactorName())]
        setnames(dat, old = groupvarFactorName(), new = groupvarname())
        rbind(dat, list("Total", dset.orig()[xgraphs.ids(), .N]))
    }, include.rownames = FALSE)

    ############################################################
    ############################################################
    ## Plotting 
    
    colorScale.mod <- reactive({
        if (is.null(nonMissingIDs())) return(NULL)
        myColorScale[1:length(unique(dset.orig()[[groupvarFactorName()]]))]
    })
    
    #############################################################
    output$psPlot <- renderPlot({
        if (is.null(dset.psgraphs())) return(NULL)
        
        p <- ggplot(data= dset.psgraphs(),
            aes_string(x= psvarName())) +
            geom_histogram(
                alpha    = alpha1, 
                position = 'identity', 
                bins     = 30,
                aes_string(fill= groupvarFactorName())) +
            theme_bw() +
            scale_fill_manual(groupvarname(), values= colorScale.mod()) +
            xlab(paste0("PS (n= ", nrow(dset.psgraphs()), ")")) +
            theme(legend.position= "right")

        # it is very important to have just p here, not print(p)!
        p 
    })    
    
    output$psPlotui <- renderUI({
        if (is.null(dset.psgraphs())) return(NULL)

        plotOutput("psPlot", 
            height = 300,
            brush  = if (useLogit() == FALSE) {
                brushOpts(
                    id = "psPlot_brush",
                    delay = 300,
                    delayType = "debounce",
                    direction = "x",
                    resetOnNew = TRUE
                )
            } else NULL
        )
    })
    
    output$logitpsPlot <- renderPlot({
        if (is.null(dset.psgraphs())) return(NULL)

        p <- ggplot(data= dset.psgraphs(),
            aes_string(x= logitpsvarName())) +
            geom_histogram(
                alpha    = alpha1, 
                position = 'identity', 
                bins     = 30,
                aes_string(fill= groupvarFactorName())) +
            theme_bw() +
            scale_fill_manual(groupvarname(), values= colorScale.mod()) +
            xlab(paste0("Logit PS (n= ", nrow(dset.psgraphs()), ")")) +
            theme(legend.position= "none")
            
        # it is very important to have just p here, not print(p)!
        p 
    })    
    
    output$logitpsPlotui <- renderUI({
        if (is.null(dset.psgraphs())) return(NULL)

        plotOutput("logitpsPlot", 
            height = 300,
            brush  = if (useLogit() == TRUE) {
                brushOpts(
                    id = "logitpsPlot_brush",
                    delay = 300,
                    delayType = "debounce",
                    direction = "x",
                    resetOnNew = TRUE
                )
            } else NULL
        )
    })
    #############################################################
    

    # modified from https://gist.github.com/wch/5436415/, with
    # help from a SO post I forgot to get the URL for
    # also from http://stackoverflow.com/questions/19130455/create-dynamic-number-of-input-elements-with-r-shiny
    observe({
        for (i in 1:numvarsToView()) {
            # My sources (above) say:
            # Need local so that each item gets its own number. 
            # Without it, the value # of i in the renderPlot() 
            # will be the same across all instances, 
            # because of when the expression is evaluated.
            local({
                my_i <- i

                varname         <- varsToView()[my_i]
                plotname        <- paste0("plot", my_i)
                plot2name       <- paste0("plot2", my_i)
                prunername      <- paste0("pruner", my_i)
                inputname       <- paste0("pruningChoices_", my_i)
                textCheckName   <- paste0("textcheck", my_i)
                keepNAName      <- paste0("keepNA", my_i)
                keepNAInputName <- paste0("keepNAInput", my_i)
                naTableName     <- paste0("naTable", my_i)
     
                # Call renderPlot for each selected variable. 
                output[[plotname]] <- renderPlot({
                    p <- ggplot(
                        data= dset.orig()[xgraphs.ids()][!is.na(eval(varname)), ],
                        mapping= aes_string(
                            x      = varname,
                            fill   = groupvarFactorName(),
                            colour = groupvarFactorName()
                        )) +
                        theme_bw() +
                        scale_fill_manual(groupvarname(), 
                            values= colorScale.mod()) +
                        scale_colour_manual(groupvarname(), 
                            values= colorScale.mod(), 
                            guide= FALSE)
                    
                    # Histogram or bar chart
                    if (varIsContinuous()[varname]) {    
                        p <- p + geom_histogram(
                            alpha    = alpha1, 
                            position = 'identity',
                            bins     = 30) 
                    } else {
                        p <- p + geom_bar(
                            alpha    = alpha1, 
                            position = position_dodge()) +
                            theme(axis.text.x = element_text(angle = 45,
                                hjust = 0.5, vjust = 0.5))
                    }    
                    
                    # legend
                    if (my_i == 1) {
                        p <- p + theme(legend.position= "top")
                    } else {
                        p <- p + theme(legend.position= "none")
                    }  
                    
                    # add the rug plots
                    if (!is.null(idsForRug())) { 
                        if (varIsContinuous()[varname]) {    
                            p <- p + geom_rug(
                                data= dset.orig()[idsForRug()][!is.na(eval(varname)), ],  
                                # keep aes() from above
                                position = 'identity',
                                sides= "b") 
                        } else {
                            # todo: keep working on this.
                            # see http://stackoverflow.com/questions/30287334/how-to-add-marginal-rugs-above-bars-of-a-bar-chart-with-ggplot2
                        }
                    }    
                    # just p here!  not print(p)!
                    p
                }) # end renderPlot

                # Call renderPlot again for each selected variable. 
                output[[plot2name]] <- renderPlot({
                    # TODO: to make these I will want the PS or logitPS merged in.
                    # Do the merge using data.table.
                    dat <- dset.orig()[xgraphs.ids()][!is.na(eval(varname)), ][, c(idvarName(), groupvarFactorName(), varname), with= FALSE][dset.psgraphs.plus()]

                    p <- ggplot(
                        data= na.omit(dat),
                        mapping= aes_string(
                            colour = groupvarFactorName(),
                            fill = groupvarFactorName()#,
                            #alpha  = 
                        )) +
                        theme_bw() +
                        scale_colour_manual(groupvarname(), 
                            values= colorScale.mod(), 
                            guide= FALSE) +
                        scale_fill_manual(groupvarname(), 
                            values= colorScale.mod(), 
                            guide= FALSE)
                    
                    # Scatterplot
                    if (varIsContinuous()[varname]) {    
                        p <- p + 
                            geom_point(
                                mapping= aes_string(
                                    x = varname,
                                    y = logitpsvarName()
                                ),
                                alpha= alpha1 
                            ) +
                            xlab(varname) +
                            ylab("Logit PS") 
                    } else {
                        p <- p + 
                            geom_jitter(
                                mapping= aes_string(
                                    y = varname,
                                    x = logitpsvarName()
                                ),
                                width= 0,
                                height= 0.3,
                                alpha= alpha1 
                            ) +
                            ylab(varname) +
                            xlab("Logit PS") 
                    }    
                    
                    # legend
                    if (my_i == 1) {
                        p <- p + theme(legend.position= "top")
                    } else {
                        p <- p + theme(legend.position= "none")
                    }  
                    
                    # just p here!  not print(p)!
                    p
                }) # end renderPlot

                # Create input function for each variable
                output[[prunername]] <- renderUI({
                    if (varIsContinuous()[varname]) {
                        textInput(
                            inputname, 
                            NULL,
                            # make min & max slightly more extreme than rounded min and max in data, so that we don't get accidental pruning using the default values
                            value= paste(
                                floor(10^xdig() *   min(unlist(dset.orig()[nonMissingIDs(), eval(varname), with= FALSE]), na.rm = TRUE)) / 10^xdig(),
                                ceiling(10^xdig() * max(unlist(dset.orig()[nonMissingIDs(), eval(varname), with= FALSE]), na.rm = TRUE)) / 10^xdig(),
                                collapse= ", ")
                        )
                    } else { # we have categorical var, either factor or char
                        checkboxGroupInput(
                            inputname, 
                            NULL,
                            # as.character corrects the printing of factor levels
                            choices=  as.character(sort(unique(unlist(dset.orig()[nonMissingIDs(), eval(varname), with= FALSE])))),
                            selected= as.character(sort(unique(unlist(dset.orig()[nonMissingIDs(), eval(varname), with= FALSE]))))
                        )
                    }
                }) # end renderUI

                # Check the textInput for each variable
                output[[textCheckName]] <- renderText({
                    if (is.null(pruneValTextList())) return(NULL)
                    if (varIsContinuous()[varname]) {
                        if (pruneValTextList()[[my_i]] == TRUE) {
                            "Please type min and max, separated by one space."
                        } else { # no problem
                            return(NULL)
                        }
                    } else { # categorical var, nothing to check
                        return(NULL)
                    }
                }) # end renderText
                
                # Create "keep NA?" input function for each variable
                output[[keepNAName]] <- renderUI({
                    radioButtons(keepNAInputName, NULL,
                        c("Keep units with missing values for this variable" = 1,
                        "Exclude units with missing values for this variable" = 0),
                        selected = 1,
                        width = '100%'
                    )
                }) # end renderUI

                # Create a missing-by-group table each variable
                output[[naTableName]] <- renderTable({
                    dat <- dset.orig()[xgraphs.ids(), .(prop.missing = mean(is.na(get(varname)))), by= eval(groupvarFactorName())]
                    setnames(dat, old = groupvarFactorName(), new = groupvarname())
                    dat
                }, include.rownames= FALSE) # end renderTable
                
                
            }) # end local
        } # end for
    }) # end observe    
    
    # Now put them all together
    output$univariatePlotsAndInputs <- renderUI({
        if (is.null(varsToView())) return(NULL)
        
        plot_and_input_list <- vector("list", numvarsToView())
        for(i in 1:numvarsToView()) {
            varname <- varsToView()[i]
            plotname <- paste0("plot", i)
            plot2name <- paste0("plot2", i)
            prunername <- paste0("pruner", i)
            textcheckname <- paste0("textcheck", i)
            keepNAName <- paste0("keepNA", i)
            naTableName <- paste0("naTable", i)
            
            plot_and_input_list[[i]] <-
                fluidRow(
                    tags$hr(),
                    column(4, 
                        plotOutput(plotname, 
                            # first plot taller to accomodate legend
                            height = ifelse(i == 1, 320, 280), 
                            width  = 400#,
                        ) # end plotOutput   
                    ), # end column
                    column(4, 
                        plotOutput(plot2name, 
                            # first plot taller to accomodate legend
                            height = ifelse(i == 1, 320, 280), 
                            width  = 400#,
                        ) # end plotOutput   
                    ), # end column
                    column(4, 
                        h4(varname),
                        if (varIsContinuous()[varname]) {
                            h5("Keep only units in this range (inclusive). Separate min and max by a space:") 
                        } else {
                            h5('Keep only units with the following value(s):')
                        },
                        uiOutput(prunername),
                        uiOutput(textcheckname),
                        uiOutput(naTableName),
                        uiOutput(keepNAName)
                    ) # end column
                )# end fluidRow
        } 
        plot_and_input_list
    })
 
    
    ############################################################
    ############################################################
    # Session Information
    output$sessionInf <- renderPrint({
        si <- sessionInfo() 
        si$loadedOnly <- NULL
        print(si, locale=FALSE)
    })
    
    # server.R code for display
    output$serverCode <- renderPrint({
        cat(readLines(con= "server.R"), sep= "\n")
    })

    # ui.R code for display
    output$uiCode <- renderPrint({
        cat(readLines(con= "ui.R"), sep= "\n")
    })
}) # end shiny server
    
