######################################################
#TODO:
# Remember: when updating on server, use library(Cairo)
#######################################################

library(shiny)
library(rms)
library(data.table)
library(survey)
library(tableone)

# On the department server, Cairo plots look worse
options(shiny.usecairo= FALSE)

# Allow upload of bigger files
# from http://stackoverflow.com/questions/18037737/how-to-change-maximum-upload-size-exceeded-restriction-in-shiny-and-save-user
# The first number is the number of MB
options(shiny.maxRequestSize=30*1024^2)

# text types for use w/ CSS
#<"text-muted">
#<"text-primary">
#<"text-warning">
#<"text-danger">
#<"text-success">
#<"text-info">

shinyServer(function(input, output, session) {
    ############################################################
    ############################################################
    ## Flags 
    
    # I'm using reactiveValues() here so I can set the fileInfo
    #   to NULL when the user decides to upload a new file.
    # Also I need a flag for when user switches datasets
    datInfo <- reactiveValues()
    datInfo$inFileInfo          <- NULL
    datInfo$newData             <- NULL
    datInfo$newDataNoVarsChosen <- NULL
    datInfo$newDataNotYetPruned <- NULL

    observe({
        if (input$useExampleData == 0) {
            datInfo$inFileInfo <- input$datafileInfo 
        } 
    })
    observeEvent(input$changeUpFileButton, {
        datInfo$inFileInfo <- NULL

        datInfo$newData             <- TRUE
        datInfo$newDataNoVarsChosen <- TRUE
        datInfo$newDataNotYetPruned <- TRUE
    })    
    observe({
        input$useExampleData

        datInfo$newData             <- TRUE
        datInfo$newDataNoVarsChosen <- TRUE
        datInfo$newDataNotYetPruned <- TRUE
    })
    observeEvent(input$psTypedButton, {
        if (!is.null(dsetOrig())) {
            datInfo$newData <- FALSE
        }
    })
    observeEvent(input$generalGraphUpdateButton, {
        if (!datInfo$newData) {
            datInfo$newDataNoVarsChosen <- FALSE
        }
    })
    observeEvent(
        input$xgraphsUpdateButton | input$PSCalcUpdateButton, {
        if (!(datInfo$newData | datInfo$newDataNoVarsChosen)) {
            datInfo$newDataNotYetPruned <- FALSE
        }
    })
    # TODO: keep in mind: input$mainNavbarPage gives name of current tab. could use this too
    
    ############################################################
    ############################################################
    ## Data import, etc.
    
    output$changeUploadedFile <- renderUI({
        if (input$useExampleData == 1 | is.null(datInfo$inFileInfo)) return(NULL)
        actionButton("changeUpFileButton", "Upload different file")
    })
    output$chooseDatafile <- renderUI({
        if (input$useExampleData == 1 |
                (input$useExampleData == 0 & !is.null(datInfo$inFileInfo))) return(NULL)
        
        # File input from example on shiny website.
        # input$datafileInfo will be NULL initially. 
        # After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        fileInput('datafileInfo', 
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
    
    dsetOrig <- reactive({
        if (input$useExampleData == 0 & is.null(datInfo$inFileInfo)) return(NULL)
    
        if (input$useExampleData == 1) {
            # Take care of the random seed.
            #    Code from Cole in nbpMatching pkg
            if(exists(".Random.seed", envir = .GlobalEnv)) {
                save.seed <- get(".Random.seed", envir= .GlobalEnv)
                on.exit(assign(".Random.seed", save.seed, envir = .GlobalEnv))
            } else {
                on.exit(rm(".Random.seed", envir = .GlobalEnv))
            }
            set.seed(3517)
            
            nt <- 300
            nc <- 700
            N <- nt + nc
            nmiss <- 0.05 * N
            
            exposed <- rep(c("Yes", "No"), times= c(nt, nc))
            
            # Tx group is older on average
            age <- c(rnorm(nt, 53, 8), rnorm(nc, 45, 8))
            
            # Tx group is less spread out in terms of height
            height_ft <- c(rnorm(nt, 5.6, .15), rnorm(nc, 5.6, .3))
            height_ft[sample(N, nmiss, replace= FALSE)] <- NA
            
            # Blood pressure is more likely to be missing in the treated group
            #systolic_bp <- c(rnorm(nt, 115, 5), rnorm(nc, 110, 7))
            systolicBP <- rnorm(N, 115, 5)
            systolicBP[sample(nt, nmiss * 3/4, replace= FALSE)] <- NA
            systolicBP[nt + sample(nc, nmiss * 1/4, replace= FALSE)] <- NA
            
            # Tx group has higher proportion of males
            gender <- c(
                sample(0:2, nt, replace= TRUE, prob= c(.746, .252, .002)),
                sample(0:2, nc, replace= TRUE, prob= c(.506, .492, .002))
            )
            gender[gender == 0] <- "Male"
            gender[gender == 1] <- "Female"
            gender[gender == 2] <- "Other"
            
            # Hardly any current smokers in tx group
            smoker <- c(
                sample(0:2, nt, replace= TRUE, prob= c(.895, .10, .005)),
                sample(0:2, nc, replace= TRUE, prob= c(.75, .05, .2))
            )
            smoker[smoker == 0] <- "Never"
            smoker[smoker == 1] <- "Former"
            smoker[smoker == 2] <- "Current"
            smoker[sample(N, nmiss, replace= FALSE)] <- NA
            
            # ABO is unrelated
            ABO <- c(
                rep(c('O', 'A', 'B', 'AB'), 
                    times= c(.44, .42, .10, .04) * nt),
                rep(c('O', 'A', 'B', 'AB'), 
                    times= c(.44, .42, .10, .04) * nc)
            )
            
            
            mydat <- data.table(exposed, age, height_ft, systolicBP, 
                gender, smoker, ABO)
            #tmpfit <- lrm(exposed ~ rcs(age) + rcs(height_ft) + rcs(systolicBP) + gender, data= mydat)
            #tmplogit <- tmpfit$linear.predictors
            #tmpprob <- exp(tmplogit) / (1 + exp(tmplogit))
            #tmpbinom <- rbinom(N, 1, tmpprob)
            #mydat[, exposed := ifelse(tmpbinom == 1, "Yes", "No")]
        }  else if (!is.null(datInfo$inFileInfo)) {
            if (grepl("\\.csv\\>", datInfo$inFileInfo$name)) {
                mydat <- fread(datInfo$inFileInfo$datapath,
                    sep= ",",
                    header= TRUE,
                    data.table= TRUE
                )
            } else if (grepl("\\.rds\\>", datInfo$inFileInfo$name)){
                # todo: add error handling. 
                mydat <- as.data.table(readRDS(datInfo$inFileInfo$datapath))
            }    
        }
        mydat
    })
    varnamesOrig <- reactive({
        if(is.null(dsetOrig())) return (NULL)
        names(dsetOrig())  
    })
    idVarName <- reactive({
        # The name produced by this function will be used as
        #   the name of the id var
        if(is.null(varnamesOrig())) return (NULL)
        
        # TODO: error handling here
        proposedName <- "MY__ID"
        while(proposedName %in% varnamesOrig()) {
            proposedName <- paste0(proposedName, "_NEW")    
        }    
        proposedName
    })    
    observe({
        # Add an ID variable so we can match obsns w/ the PS dataset
        if (!is.null(idVarName())) { 
            if (!(idVarName() %in% varnamesOrig())) {
                dsetOrig()[, idVarName() := 1:nrow(dsetOrig())]
                setkeyv(dsetOrig(), idVarName())
            }
        }
    })
    possGroupNamesOrig <- reactive({
        if(is.null(dsetOrig())) return (NULL)
        varnamesOrig()[sapply(dsetOrig(), 
            function(vec) length(unique(vec)) == 2)]
    })
    output$chooseGroup <- renderUI({
        if (is.null(dsetOrig())) return(NULL)
        if (input$useExampleData == 0 & is.null(datInfo$inFileInfo)) return(NULL)

        selectizeInput('treatmentVarName', 
            label    = NULL, 
            choices  = possGroupNamesOrig(), 
            selected = NULL,
            multiple = FALSE
            )
    })
    groupVarName <- reactive({
        if (is.null(dsetOrig())) return(NULL)
        if (input$useExampleData == 0 & is.null(datInfo$inFileInfo)) return(NULL)
        
        input$treatmentVarName
    })
    groupVarFactorName <- reactive({
        # The name produced by this function will be used as
        #     the name of the treatment group var 
        if (is.null(dsetOrig())) return(NULL)
        if (is.null(groupVarName())) return(NULL)
        
        if (is.factor(dsetOrig()[[groupVarName()]])) {
            return(groupVarName())    
        }    
        proposedName <- paste0(groupVarName(), '.factor')
        while(proposedName %in% varnamesOrig()) {
            proposedName <- paste0(proposedName, "NEW")    
        }    
        proposedName
    })    

    dsetGroupvar <- reactive({
        if (datInfo$newData == TRUE) return(NULL)
        if (is.null(dsetOrig())) return(NULL)
        if (is.null(groupVarFactorName())) return(NULL)
        if (is.null(idVarName())) return(NULL)

        dat <- data.table(dsetOrig()[[idVarName()]])
        setnames(dat, old = names(dat), new = idVarName())

        if (!is.factor(dsetOrig()[[groupVarName()]])) {
            dat[, groupVarFactorName() := 
                factor(dsetOrig()[[groupVarName()]])]
        } else {
            dat[, groupVarFactorName() := 
                dsetOrig()[[groupVarName()]]]
        }
        setkeyv(dat, idVarName())
        dat 
    })

    groupVarFactorLevelsSorted <- reactive({
        # for use in graphs
        if (is.null(dsetGroupvar())) return(NULL)

        tmp <- table(dsetGroupvar()[[groupVarFactorName()]])
        names(tmp)[order(tmp, decreasing= TRUE)]
    })
    smallestGroup <- reactive({
        if (is.null(groupVarFactorLevelsSorted())) return(NULL)

        groupVarFactorLevelsSorted()[length(
            groupVarFactorLevelsSorted())]
    })
    colorScale.mod <- reactive({
        if (is.null(dsetGroupvar())) return(NULL)

        #colors from  bootswatch sandstone
        primary <- "#325D88"
        success <- "#93C54B"
        info    <- "#29ABE0"
        warning <- "#F47C3C"
        danger  <- "#d9534f"
        myColorScale <- c(
            primary,
            warning,
            success,
            info,
            danger
        )
        
        sc <- myColorScale[1:length(groupVarFactorLevelsSorted())]
        names(sc) <- groupVarFactorLevelsSorted()
        sc
    })

    output$noDataChosenText <- renderUI({
        if (is.null(dsetOrig()) | 
            (input$useExampleData == 0 & is.null(datInfo$inFileInfo))) {
            HTML(paste0(tags$span(class="text-warning", "No dataset selected.")))
        } else return(NULL)
    })
    output$dataFnameText1 <- renderUI({
        if (is.null(dsetOrig()) | input$useExampleData == 1) return(NULL)

        HTML(paste0(tags$h5("Filename:")))
    })
    output$dataFnameText2 <- renderUI({
        if (is.null(dsetOrig()) | input$useExampleData == 1) return(NULL)

        HTML(paste0(tags$span(paste0(datInfo$inFileInfo$name))))
    })
    output$dataFnameText3 <- renderUI({
        if (is.null(dsetOrig()) | input$useExampleData == 1) return(NULL)

        HTML(paste0(tags$br()))
    })

    output$dataDimText1 <- renderUI({
        if (is.null(dsetOrig())) return(NULL)

        HTML(paste0(tags$h5("Dimensions:")))
    })
    output$dataDimText2 <- renderUI({
        if (is.null(dsetOrig())) return(NULL)

        HTML(paste0(tags$span(paste0("The dataset has ", ncol(dsetOrig()), 
            " columns and ", nrow(dsetOrig()), " rows."))))
    })

    output$groupLevelText1 <- renderUI({
        if (is.null(groupVarName())) return(NULL)

        HTML(paste0(tags$h5("The treatment indicator has the following levels:")))
    })
    output$groupLevelTable <- renderTable({
        if (is.null(groupVarName())) return(NULL)

        # the as.character lets this print right if var 
        #    is already a factor
        dat <- data.frame(as.character(sort(unique(
            dsetOrig()[[groupVarName()]])))) 
        names(dat) <- groupVarName()
        dat
    }, include.rownames = FALSE)

    output$othervarsText1 <- renderUI({
        if (is.null(groupVarFactorName())) return(NULL)

        HTML(paste0(tags$h5("Other variables in the dataset:")))
    })
    output$othervarsTable <- renderTable({
        if (is.null(groupVarFactorName())) return(NULL)

        namesToExclude <- c(idVarName(), groupVarName())
        if (groupVarName() != groupVarFactorName()) {
            namesToExclude <- c(namesToExclude, groupVarFactorName())
        }

        data.frame(Variables= setdiff(varnamesOrig(), namesToExclude)) 
    }, include.rownames = FALSE, include.colnames= FALSE)

    ############################################################
    ############################################################
    ## PS specification, etc.
    
    useCompleteCasesOnly <- reactive({
        input$completeCasesOnly == 1
    })
    
    nonMissingIDs <- reactive({
        # These are the IDs of people who can be used for PS calculation
        #   (will be used in intersection w/ keepAfterPruning IDs)
        if (datInfo$newData == TRUE) return(NULL)
        if (is.null(idVarName())) return (NULL)

        if (is.null(varnamesFromRHS())) return(dsetOrig()[[idVarName()]])
        
        if (useCompleteCasesOnly()) {
            na.omit(dsetOrig()[, c(varnamesFromRHS(), idVarName()), 
                with= FALSE])[[idVarName()]]
        } else {
            dsetOrig()[[idVarName()]]
        }
    })
    
    psVarName <- reactive({
        # The name produced by this function will be used as
        #   the name of the ps var in dsetPSGraphs()
        #   (we have to make sure it does not overlap w/ current var names)
        if(is.null(dsetOrig())) return (NULL)
        
        proposedName <- "MY__PS"
        while(proposedName %in% varnamesOrig()) {
            proposedName <- paste0(proposedName, "_NEW")    
        }    
        proposedName
    })    
    logitpsVarName <- reactive({
        # The name produced by this function will be used as
        #   the name of the logit ps var in dsetPSGraphs()
        #   (we have to make sure it does not overlap w/ current var names)
        if(is.null(dsetOrig())) return (NULL)
        
        proposedName <- "MY__LOGITPS"
        while(proposedName %in% varnamesOrig()) {
            proposedName <- paste0(proposedName, "_NEW")    
        }    
        proposedName
    })    
    naPrefix <- reactive({
        # The phrase produced by this function will be used as
        #   the prefix for the na.indicator vars
        if(is.null(dsetOrig())) return (NULL)
        
        proposedPrefix <- "is.na_"
        while(any(grepl(paste0("^", proposedPrefix), varnamesOrig()))) {
            proposedPrefix <- paste0(proposedPrefix, "_")    
        }    
        proposedPrefix
    })    

    dsetPSGraphs <- reactive({
        # This dataset is used for making the PS plots.
        if (is.null(dsetGroupvar())) return(NULL)
        if (is.null(logitPS())) return(NULL) 
        
        # todo: do this all using data.table
        dat1 <- data.frame(
            id    = unlist(dsetOrig()[nonMissingIDs(), 
                idVarName(), with= FALSE]),
            group = unlist(dsetGroupvar()[nonMissingIDs(), 
                groupVarFactorName(), with= FALSE])
        )
        dat2 <- data.frame(
            id       = PSIDs(),
            logit.ps = logitPS(), 
            ps       = PS()
        )
        # keep only subjects with values in both dsets
        dat <- merge(dat1, dat2, by= "id")  
        names(dat)[names(dat) == "id"] <- idVarName()
        names(dat)[names(dat) == "ps"] <- psVarName()
        names(dat)[names(dat) == "logit.ps"] <- logitpsVarName()
        names(dat)[names(dat) == "group"] <- groupVarFactorName()
        
        dat <- as.data.table(dat)  
        setkeyv(dat, idVarName())
        dat
    })    

    output$getFormula <- renderUI({
        if (is.null(groupVarName())) return(NULL)

        textInput('formulaRHS', 
            label= paste0(groupVarName(), ' ~ '), 
            value= ' ',
            width= '100%'
        )
    })

    formRHS <- reactive({
        # -- contains an isolate -- #
        # Dependencies
        if (datInfo$newData == TRUE) return(NULL)
        if (is.null(dsetOrig())) return(NULL)
        if (input$psTypedButton == 0) return(NULL) 
        groupVarName()
        useCompleteCasesOnly()
        
        isolate(input$formulaRHS)
    })
    stringFormula <- reactive({
        if (is.null(groupVarName())) return(NULL)

        paste0(groupVarName(), ' ~ ', formRHS())
    })    
    psForm <- reactive({
        if (is.null(stringFormula())) return(NULL)

        tryCatch(as.formula(stringFormula()),
            error= function(e) return(NULL))
    })    
    psNotChecked <- reactive({
        if (is.null(stringFormula())) return(TRUE)

        if (input$psTypedButton == 0 | 
            paste0(groupVarName(), ' ~ ', input$formulaRHS) != 
                stringFormula()) TRUE else FALSE
    })
    psFormSyntaxOK <- reactive({
        if (psNotChecked()) return(NULL)
        
        if (is.null(psForm())) {
            FALSE
        } else {
            TRUE
        }
    })

    output$psHelpGeneral1 <- renderUI({
        HTML(paste0(
            "Type RHS of ", 
            tags$a("R formula", 
                    href="https://stat.ethz.ch/R-manual/R-devel/library/stats/html/formula.html", 
                    target="_blank"),
            " for ", 
            tags$code("lrm()"), 
            ", e.g. ",
            tags$code("age + gender"), 
            "."
        ))
    })
    output$psHelpNA1 <- renderUI({
        if (useCompleteCasesOnly()) {
            return(NULL)
        } else {
            HTML(paste0(
                "To include missingness indicators, ", 
                "prefix the variable name with ",
                tags$code(naPrefix()),
                ", e.g. ", 
                tags$code(paste0(naPrefix(), "age")), 
                "."
            ))
        }
    })

    output$psFormulaProblemText <- renderUI({
        if (psNotChecked()) {
            HTML(paste0(tags$span(class="text-warning", "Not checked yet.")))
        } else if (!psFormSyntaxOK()) {
            HTML(paste0(tags$span(class="text-danger", "That is not a usable RHS. Please try again.")))
        } else {
            HTML(paste0(tags$span(class="text-success", "Formula syntax is OK.")))
        }
    })

    PSIDs <- reactive({
        # -- contains an isolate -- #
        # dependencies
        if (datInfo$newData == TRUE) return(NULL)
        if (input$PSCalcUpdateButton == 0) return(nonMissingIDs()) 
        
        intersect(nonMissingIDs(), isolate(idsToKeepAfterPruning()))
    })

    varnamesFromRHS <- reactive({
        if (is.null(psForm())) return(NULL)
        
        allvars <- setdiff(all.vars(psForm()), groupVarName())
        
        if (useCompleteCasesOnly()) {
            if (all(allvars %in% varnamesOrig())) allvars else NULL
        } else {
            allvars.noprefix <- 
                unique(gsub(paste0("^", naPrefix()), "", allvars))
            if (all(allvars.noprefix %in% varnamesOrig())) allvars else NULL
        }
    })    
    varnamesForIndicators <- reactive({
        if (datInfo$newData == TRUE) return(NULL)
        if (is.null(varnamesFromRHS())) return(NULL)
        
        setdiff(varnamesFromRHS(), varnamesOrig())
    })
    varnamesFromRHSOK <- reactive({
        # -- contains an isolate -- #
        if (datInfo$newData == TRUE) return(NULL)
        if (psNotChecked()) return(NULL)
        if (is.null(psFormSyntaxOK())) return(NULL)
        dsetOrig()
        
        if (is.null(isolate(varnamesFromRHS()))) {
            FALSE
        } else {
            TRUE
        }
    })
    output$psVarsProblemText <- renderUI({
        if (psNotChecked() | is.null(psFormSyntaxOK())) {
            HTML(paste0(tags$span(class="text-warning", "Not checked yet.")))
        } else if (!psFormSyntaxOK()) {
            HTML(paste0(tags$span(class="text-warning", "Not checked yet.")))
        } else if (!varnamesFromRHSOK()) {
            if (useCompleteCasesOnly() & 
                any(grepl(paste0("^", naPrefix()), all.vars(psForm())))) {
                HTML(paste0(tags$span(class="text-danger", paste0(
                    "The formula uses missingness indicators, but you have ",
                    "chosen to use complete cases only. ",
                    "Please change your selection or remove the indicators."))))
            } else {
                HTML(paste0(tags$span(class="text-danger", "The formula uses variables that are not in the dataset. Please try again.")))
            }
        } else {
            HTML(paste0(tags$span(class="text-success", "All variable names are OK.")))
        }
    })    

    output$psNeedsCheckingText <- renderUI({
        if (psNotChecked()) {
            HTML(paste0(tags$span(class="text-info", "Remember to click the button when you're done!")))
        } else NULL   
    })

    dsetImputed <- reactive({
        if (datInfo$newData == TRUE) return(NULL)
        if (is.null(varnamesFromRHS())) return(NULL)
        if (is.null(PSIDs())) return(NULL)
        if (useCompleteCasesOnly()) return(NULL)
        
        origvars <- unique(gsub(paste0("^", naPrefix()), "", varnamesFromRHS()))
        myvars <- c(origvars, groupVarName())
        dat <- copy(dsetOrig()[PSIDs(), myvars, with= FALSE])
        # have to convert character vars to factors before imputing
        for (varname in origvars) {
            if (is.character(dat[[varname]])) {
                dat[, eval(varname) := factor(dat[[varname]])]
            }
        }
        
        # now do the imputation
        dat[, (myvars) := lapply(.SD, function(x) impute(x, fun= mean)),
            .SDcols = myvars]
        
        # now add the missingness indicators
        if (length(varnamesForIndicators()) > 0) {
            for (varname in varnamesForIndicators()) {
                varnameOrig <- gsub(paste0("^", naPrefix()), "", varname)
                dat[, eval(varname) := is.imputed(dat[[varnameOrig]])]
            }
        }
        
        dat
    })
    
    lrmFit <- reactive({
        if (datInfo$newData == TRUE) return(NULL)

        if (is.null(psForm()) | 
            is.null(varnamesFromRHS())) return(NULL)
        
        if (useCompleteCasesOnly()) {
            tryCatch({lrm(psForm(), 
                data  = dsetOrig()[PSIDs()])},
                error = function(e) return(NULL))
        } else { # use imputed data
            tryCatch({lrm(psForm(), 
                data  = dsetImputed())},
                error = function(e) return(NULL))
        }    
    })

    output$psCopyText <- renderUI({
        # -- contains an isolate -- #
        # dependencies
        if(is.null(lrmFit())) return(NULL)
    
        HTML(paste0(
            tags$code(isolate(stringFormula())) 
        ))
    })

    output$psFitProblemTextPrePruning <- renderUI({
        # -- contains an isolate -- (waiting for the "done typing" button)
        # dependencies
        if (datInfo$newData == TRUE) return(NULL)
        useCompleteCasesOnly()
        
        if (psNotChecked() | is.null(varnamesFromRHSOK())) {
            HTML(paste0(tags$span(class="text-warning", "Not checked yet.")))
        } else if (!varnamesFromRHSOK()) { # couldn't combine this with above
            HTML(paste0(tags$span(class="text-warning", "Not checked yet.")))
        } else if (is.null(isolate(lrmFit()))) {
            HTML(paste0(tags$span(class="text-danger", "The propensity score formula can't be fit using the current dataset. Please modify the model and/or the variables selected for viewing.")))
        } else {
            HTML(paste0(tags$span(class="text-success", "PS model successfully fit.")))
        }
    })
    output$psGraphsNotReady <- renderUI({
        # dependencies
        useCompleteCasesOnly()
        
        if (is.null(dsetPSGraphs())) {
            HTML(paste0(tags$span(class="text-warning", "Scores not yet estimated.")))
        } else {
            NULL
        }
    })
    psFitProblemPostPruning <- reactive({
        # -- contains an isolate -- #
        # dependencies
        if (datInfo$newData == TRUE) return(FALSE)
        if (psNotChecked() |  
            input$PSCalcUpdateButton  == 0) return (FALSE)

        if (is.null(isolate(lrmFit()))) {
            TRUE
        } else {
            FALSE
        }    
    })
    output$psFitProblemTextPostPruning <- renderUI({
        if (psFitProblemPostPruning()) {
            HTML(paste0(tags$span(class="text-danger", 
                "The propensity score formula can't be fit using the pruned dataset. Please modify the model and/or the pruning criteria.")))
        } else {
            NULL
        }    
    })

    logitPS <- reactive({
        if (is.null(lrmFit())) return(NULL)
        
        lrmFit()$linear.predictors
    })
    PS <- reactive({
        if (is.null(logitPS())) return(NULL)
        
        exp(logitPS()) / (1 + exp(logitPS()))
    })
    
    output$dataNonmissingDimText1  <- renderUI({
        if (is.null(nonMissingIDs()) | 
            !useCompleteCasesOnly()) return(NULL)
        if (psNotChecked() | 
            is.null(varnamesFromRHSOK())) return(NULL)

        HTML(paste0(tags$hr()))
    })
    output$dataNonmissingDimText2  <- renderUI({
        if (is.null(nonMissingIDs()) | 
            !useCompleteCasesOnly()) return(NULL)
        if (psNotChecked() | 
            is.null(varnamesFromRHSOK())) return(NULL)

        HTML(paste0(tags$h4("N after excluding rows:")))
    })
    output$dataNonmissingDimText3 <- renderText({
        if (is.null(nonMissingIDs()) | 
            !useCompleteCasesOnly()) return(NULL)
        if (psNotChecked() | 
            is.null(varnamesFromRHSOK())) return(NULL)

        paste0("After removal of rows with missing values for the variables selected for the PS model, ",
            "the dataset has ", length(nonMissingIDs()), " rows.")
    })
    output$psPlot <- renderPlot({
        if (is.null(dsetPSGraphs())) return(NULL)
        
        histlist <- vector("list", length(groupVarFactorLevelsSorted()))
        names(histlist) <- groupVarFactorLevelsSorted() 
        for (lev in groupVarFactorLevelsSorted()) {
            x <- dsetPSGraphs()[get(groupVarFactorName()) == lev, get(psVarName())]
            if (length(x) > 0) {
                histlist[[lev]] <- hist(x, plot= FALSE, breaks= 30)
            } else {
                histlist[[lev]] <- NULL
            }
        }
        histcounts <- do.call(c, lapply(histlist, function(hl) hl$counts))
        plot(dsetPSGraphs()[[psVarName()]], runif(dsetPSGraphs()[, .N]), 
            ylim= c(0, max(histcounts)), 
            xlab= "PS",
            ylab= "Count",
            bty= "n",
            type= "n",
            cex.lab= 1,
            cex.axis= 0.8
        )
        # modified from http://www.r-bloggers.com/overlapping-histogram-in-r/
        for (lev in groupVarFactorLevelsSorted()) {
            myColor <- colorScale.mod()[lev] 
            if (!is.null(histlist[[lev]])) {
                plot(histlist[[lev]], 
                    freq   = TRUE, 
                    col    = adjustcolor(myColor, alpha.f= alphaVal()),
                    border = NA,
                    lty    = 0, # this is to help with rendering on server. Not sure it does though.
                    add    = TRUE
                )
            }
        }
    }, res= 100)    
    
    
    output$logitpsPlot <- renderPlot({
        if (is.null(dsetPSGraphs())) return(NULL)

        histlist <- vector("list", length(groupVarFactorLevelsSorted()))
        names(histlist) <- groupVarFactorLevelsSorted() 
        for (lev in groupVarFactorLevelsSorted()) {
            x <- dsetPSGraphs()[get(groupVarFactorName()) == lev, get(logitpsVarName())]
            if (length(x) > 0) {
                histlist[[lev]] <- hist(x, plot= FALSE, breaks= 30)
            } else {
                histlist[[lev]] <- NULL
            }
        }
        histcounts <- do.call(c, lapply(histlist, function(hl) hl$counts))
        plot(dsetPSGraphs()[[logitpsVarName()]], runif(dsetPSGraphs()[, .N]), 
            ylim= c(0, max(histcounts)), 
            xlab= "Logit PS",
            ylab= "Count",
            bty= "n",
            type= "n",
            cex.lab= 1,
            cex.axis= 0.8
        )
        # modified from http://www.r-bloggers.com/overlapping-histogram-in-r/
        for (lev in groupVarFactorLevelsSorted()) {
            myColor <- colorScale.mod()[lev] 
            if (!is.null(histlist[[lev]])) {
                plot(histlist[[lev]], 
                    freq   = TRUE, 
                    col    = adjustcolor(myColor, alpha.f= alphaVal()),
                    border = NA,
                    lty    = 0, # this is to help with rendering on server. Not sure it does though.
                    add    = TRUE
                )
            }
        }
        legend(
            "topleft",
            inset= .05,
            cex = .8,
            title= NULL,
            groupVarFactorLevelsSorted(),
            horiz = FALSE,
            bty= "n",
            border= NA,
            fill = do.call(c, lapply(groupVarFactorLevelsSorted(), function(x)
                adjustcolor(colorScale.mod()[x], alpha.f= alphaVal())))
        )
    }, res= 100)    
    
    ############################################################
    ############################################################
    
    #hasScoreOutside <- reactive({
    #    if (is.null(dsetPSGraphs())) return(NULL)

    #    if (is.null(psbrushmin())) return(rep(FALSE, dsetPSGraphs()[, .N]))

    #    psdig <- 2
    #    round(dsetPSGraphs()[[logitpsVarName()]], psdig) < psbrushmin() |
    #        round(dsetPSGraphs()[[logitpsVarName()]], psdig) > psbrushmax()
    #})
    #idsForRug <- reactive({
    #    if (is.null(hasScoreOutside())) return(NULL)
    #    
    #    ids <- dsetPSGraphs()[hasScoreOutside(), ][[idVarName()]]
    #    
    #    intersect(ids, idsToKeepAfterPruning())
    #})  

    ## todo: is there a way to do this w/o making a third dataset?
    ## TODO: might not need this anyway
    #dsetPSGraphs.plus <- reactive({
    #    if (is.null(hasScoreOutside())) return(NULL)

    #    dat <- copy(dsetPSGraphs())

    #    dat[, outside := hasScoreOutside()]
    #    dat
    #})
    ############################################################
    ############################################################
    ## Covariate selection & graphs
        
    possVarsToRestrict <- reactive({
        if (datInfo$newData == TRUE) return(NULL)
        if (is.null(varnamesOrig())) return(NULL)
        if (is.null(groupVarName())) return(NULL)
        if (is.null(idVarName())) return(NULL)

        # We don't want to allow restriction of the treatment var 
        setdiff(varnamesOrig(), 
            c(groupVarName(), idVarName()))  
    })    
    output$chooseVarsToRestrict <- renderUI({
        if (is.null(possVarsToRestrict())) return(NULL)

        selectizeInput('varsToRestrict', 
            NULL, 
            choices= possVarsToRestrict(), 
            # Do not autopopulate w/ a reactive var! Will reset & cause problems.
            selected = NULL,
            multiple= TRUE,
            width= '100%'
        )
    })

    varsToView <- reactive({
        # -- contains an isolate -- #
        #dependencies
        if (datInfo$newDataNoVarsChosen == TRUE) return(NULL)
        if (is.null(dsetOrig())) return(NULL)
        if (is.null(possVarsToRestrict())) return(NULL)

        input$generalGraphUpdateButton
        
        # trying to buy time when switching between datasets
        #vec <- intersect(isolate(input$varsToRestrict), varnamesOrig())
        vec <- isolate(input$varsToRestrict)
        vec
    })
    numvarsToView <- reactive({
        length(varsToView())    
    })    
    output$needPSText <- renderUI({
        if (is.null(dsetPSGraphs()) & !psFitProblemPostPruning()) {
            HTML(paste0(
                tags$span(class="text-warning", "To see graphs, "),
                tags$span(class="text-warning", "specify a propensity score model"),
                tags$br(),
                tags$span(class="text-warning", "on the 'Specify' tab page.") ))
        } else return(NULL)
    })

    varIsContinuous <- reactive({
        # -- contains an isolate -- #
        if (datInfo$newData == TRUE) return(NULL)
        if (is.null(varsToView())) return(NULL)

        vnames <- varsToView()
        myvec  <- rep(FALSE, length(vnames))
        
        for(i in seq_along(vnames)) {
            varname <- vnames[i]
            
            if (is.numeric(dsetOrig()[[varname]]) & 
                length(unique(dsetOrig()[[varname]])) >= 
                isolate(input$numCont)) myvec[i] <- TRUE
        }
        names(myvec) <- vnames
        myvec
    })    
    catVars <- reactive({
        if (is.null(varIsContinuous())) return(NULL)
        
        varsToView()[!varIsContinuous()[varsToView()]]
    })

    alphaVal <- reactive({
        input$alphaSlider
    })
    pointSizeVal <- reactive({
        input$pointsizeSlider
    })
    # number of decimal places to use w/ covariate graph inputs
    xdig <- reactive({
        2
    })

    psbrushmin <- reactive({
        if (is.null(dsetPSGraphs()) ) return(NULL)
        input$logitpsPlot_brush$xmin 
    })
    psbrushmax <- reactive({
        if (is.null(dsetPSGraphs()) ) return(NULL)
        input$logitpsPlot_brush$xmax
    })
    # Create the expression to use for pruning
    pruneValRawList <- reactive({
        if (datInfo$newDataNotYetPruned == TRUE) return(NULL)
        if (is.null(varsToView())) return(NULL)

        mylist <- vector("list", numvarsToView())
        names(mylist) <- varsToView()

        for (vname in varsToView()) {
            # doing it this way to fit w/ old code---
            #   could redo later

            # this is to help with the timing of the dependencies
            if (is.null(input[[paste0("pruningChoices1_", vname)]])) {
                mylist[[vname]] <-  NULL
            } else {
                mylist[[vname]] <- 
                    input[[paste0("pruningChoices1_", vname)]]
                if (varIsContinuous()[vname]) {
                    mylist[[vname]]  <- paste(mylist[[vname]],
                        input[[paste0("pruningChoices2_", vname)]],
                        collapse= " ")
                } 
            }
        }
        mylist
    })
    keepNARawList <- reactive({
        if (datInfo$newDataNotYetPruned == TRUE) return(NULL)
        if (is.null(varsToView())) return(NULL)

        mylist <- vector("list", numvarsToView())
        names(mylist) <- varsToView()

        for (vname in varsToView()) {
            # this is to help with the timing of the dependencies
            if (is.null(input[[paste0("keepNAInput_", vname)]])) {
                mylist[[vname]] <-  NULL
            } else {
                mylist[[vname]]  <- input[[paste0("keepNAInput_", vname)]] == "1"
            }
        }
        mylist
    })
    pruneValTextList <- reactive({
        # -- contains an isolate -- #
        # TODO: do I want the isolate in the RawLists instead?
        # dependencies
        if (datInfo$newDataNotYetPruned == TRUE) return(NULL)
        if (is.null(varsToView())) return(NULL)
        if (input$xgraphsUpdateButton == 0 & 
            input$PSCalcUpdateButton == 0) {
            return(NULL)
        }
        
        mylist <- vector("list", numvarsToView())
        names(mylist) <- varsToView()
        
        for (varname in varsToView()) {
            myvals  <- isolate(pruneValRawList())[[varname]]
            keepna  <- isolate(keepNARawList())[[varname]]
            
            # We are coding for the ones to KEEP
            # give the dependencies time to catch up
            if (is.null(myvals) | is.null(keepna)) {
                mylist[[varname]] <-  NULL
            } else {
                mylist[[varname]] <-  paste0(
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
                        if (is.numeric(dsetOrig()[[varname]])) {
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
                ) # end of paste0
            }
        } # next varname
        mylist
    })    

    exprToKeepAfterPruning <- reactive({
        if (is.null(pruneValTextList())) return(NULL)
        
        do.call("paste", list(pruneValTextList(), collapse= " & " ))
    })    
    output$keepAfterPruningCopyText <- renderUI({
        if (is.null(pruneValTextList())) return(NULL)

        mytext <- do.call("paste", list(pruneValTextList(), collapse= " & <br/>" ))
        HTML(paste0(
            tags$code(HTML(mytext)) 
        ))
    })    

    idsToKeepAfterPruning <- reactive({
        if (datInfo$newData == TRUE) return(NULL)
        if (is.null(dsetOrig())) return(NULL)
        if (is.null(idVarName())) return(NULL)

        if (is.null(exprToKeepAfterPruning())) return(dsetOrig()[[idVarName()]])
        
        dsetOrig()[eval(parse(text= 
            exprToKeepAfterPruning()))][[idVarName()]]
    })    
    
    output$pruneTable <- renderTable({
        if (is.null(idsToKeepAfterPruning())) return(NULL)
        if (is.null(dsetGroupvar())) return(NULL)
    
        dat <- dsetGroupvar()[idsToKeepAfterPruning(), .N, by= eval(groupVarFactorName())]
        setnames(dat, old = groupVarFactorName(), new = groupVarName())
        rbind(dat, list("Total", dsetOrig()[idsToKeepAfterPruning(), .N]))
    }, include.rownames = FALSE)
    
    output$logitpsPlotForBrushing <- renderPlot({
        # exact duplicate of the other one; can't call the same plot twice in the UI 
        if (is.null(dsetPSGraphs())) return(NULL)

        histlist <- vector("list", length(groupVarFactorLevelsSorted()))
        names(histlist) <- groupVarFactorLevelsSorted() 
        for (lev in groupVarFactorLevelsSorted()) {
            x <- dsetPSGraphs()[get(groupVarFactorName()) == lev, get(logitpsVarName())]
            if (length(x) > 0) {
                histlist[[lev]] <- hist(x, plot= FALSE, breaks= 30)
            } else {
                histlist[[lev]] <- NULL
            }
        }
        histcounts <- do.call(c, lapply(histlist, function(hl) hl$counts))
        plot(dsetPSGraphs()[[logitpsVarName()]], runif(dsetPSGraphs()[, .N]), 
            ylim= c(0, max(histcounts)), 
            xlab= "Logit PS",
            ylab= "Count",
            bty= "n",
            type= "n",
            cex.lab= 1,
            cex.axis= 0.8
        )
        # modified from http://www.r-bloggers.com/overlapping-histogram-in-r/
        for (lev in groupVarFactorLevelsSorted()) {
            myColor <- colorScale.mod()[lev] 
            if (!is.null(histlist[[lev]])) {
                plot(histlist[[lev]], 
                    freq   = TRUE, 
                    col    = adjustcolor(myColor, alpha.f= alphaVal()),
                    border = NA,
                    lty    = 0, # this is to help with rendering on server. Not sure it does though.
                    add    = TRUE
                )
            }
        }
        legend(
            "topleft",
            inset= .05,
            cex = .8,
            title= NULL,
            groupVarFactorLevelsSorted(),
            horiz = FALSE,
            bty= "n",
            border= NA,
            fill = do.call(c, lapply(groupVarFactorLevelsSorted(), function(x)
                adjustcolor(colorScale.mod()[x], alpha.f= alphaVal())))
        )
    }, res= 100)    

    output$logitpsPlotBrushable <- renderUI({
        if (is.null(dsetPSGraphs())) return(NULL)

        # from sandstone.css variables:
        #gray:                   #98978B;
        #gray-light:             #DFD7CA;
        #gray-lighter:           #F8F5F0;
        plotOutput("logitpsPlotForBrushing", 
            height = 300,
            width  = 'auto',
            brush  = brushOpts(
                id = "logitpsPlot_brush",
                fill = "#DFD7CA",
                stroke = "#98978B",
                delay = 300,
                delayType = "debounce",
                direction = "x",
                clip = TRUE,
                resetOnNew = TRUE
            )
        )
    })
    
    
    #############################################################

    # modified from https://gist.github.com/wch/5436415/, with
    # help from a SO post I forgot to get the URL for
    # also from http://stackoverflow.com/questions/19130455/create-dynamic-number-of-input-elements-with-r-shiny
    observe({if (datInfo$newDataNoVarsChosen == FALSE) {
        
        # all vars: ylim for row2 plots
        myYlimPS <- if (is.null(dsetPSGraphs())) {
            NA
        } else {
            range(dsetPSGraphs()[[logitpsVarName()]])
        }
        
        for (i in seq_along(varsToView())) {
            # My sources (above) say:
            # Need local so that each item gets its own number. 
            # Without it, the value # of i in the renderPlot() 
            # will be the same across all instances, 
            # because of when the expression is evaluated.
            local({
                my_i <- i
                varname  <- varsToView()[my_i]

                plotname <- paste0("plot_", varname)
     
                output[[plotname]] <- renderPlot({
                    if (is.null(dsetPSGraphs())) return(NULL)
                    if (is.null(dsetGroupvar())) return(NULL)

                    # For plot alignment
                    testing <- FALSE

                    # core dataset for plots and naTable: ID, group, x
                    #    Making within each because of dependency problems
                    datx <- dsetOrig()[idsToKeepAfterPruning()][, 
                        c(idVarName(), varname), 
                        with= FALSE][dsetGroupvar()[idsToKeepAfterPruning()]]
                    # convert character & discrete numeric to factor
                    if (is.character(datx[[varname]]) | 
                            (is.numeric(datx[[varname]]) & 
                            !(varIsContinuous()[varname]))) {
                        datx[, eval(varname) := 
                            factor(datx[[varname]])]
                    }
                    
                    # use datx.nona for marginal histogram/barchart
                    datx.nona <- na.omit(datx)
                    
                    # for the right-hand top plot
                    datx.xna <- datx[is.na(get(varname)), ]
                    
                    # subset of datx that has a PS
                    datxps <- datx[dsetPSGraphs()]
                    
                    # use datxps.nona for central scatterplot/stripchart
                    datxps.nona <- na.omit(datxps)
                    # preserve any levels that might have been lost
                    if(is.factor(datx[[varname]])) {
                        myLevels <- levels(datx[[varname]])
                        datxps.nona[, eval(varname) := factor(get(varname),
                            levels= myLevels)]
                    }
                    
                    # for the right-hand central (row2) plot
                    datxps.xna <- datxps[is.na(get(varname)), ]
                    
                    # x-axis limits, etc. for col2 plots
                    if(varIsContinuous()[varname]) {
                        myXlim <- range(datx.nona[[varname]])
                    } else {
                        myAtOrig <- seq_along(levels(datx.nona[[varname]]))
                        names(myAtOrig) <- levels(datx.nona[[varname]])
                    }
                    # for RH plots for continuous, and all plots for cat.
                    num.levs <- length(groupVarFactorLevelsSorted())
                    # jitter: we want the points to take up 7/8 of the
                    #    the available space, regardless of # groups;
                    #    and the jitter extends in each direction
                    myWidth <- 7/8
                    myJitter <- (1 / num.levs) * myWidth / 2
                    # alignment for each group
                    myAtAdds <- 2 * myJitter * (1:num.levs)
                    #shift so centered at 0
                    myAtAdds <- myAtAdds - mean(myAtAdds)
                    names(myAtAdds) <- groupVarFactorLevelsSorted()
                    

                    # ylim for top plots: 
                    datx.xna.counts <- table(datx.xna[[groupVarFactorName()]])
                    if(varIsContinuous()[varname]) {
                        # first make all histograms but do not plot,
                        #    in order to get ylim
                        histlist <- vector("list", length(groupVarFactorLevelsSorted()))
                        names(histlist) <- groupVarFactorLevelsSorted() 
                        for (lev in groupVarFactorLevelsSorted()) {
                            x <- datx.nona[get(groupVarFactorName()) == lev, get(varname)]
                            if (length(x) > 0) {
                                histlist[[lev]] <- hist(x, plot= FALSE)
                            } else {
                                histlist[[lev]] <- NULL
                            }
                        }
                        histcounts <- do.call(c, lapply(histlist, function(hl) hl$counts))
                        myYlimCounts <- max(c(histcounts, datx.xna.counts)) 
                    } else {
                        xtbl <- table(
                            datx.nona[[groupVarFactorName()]],
                            datx.nona[[varname]] 
                        )
                        myYlimCounts <- max(c(xtbl, datx.xna.counts))
                    }
                    
                    # multi-panel plot adapted from 
                    #   http://www.r-bloggers.com/example-10-3-enhanced-scatterplot-with-marginal-histograms/
                    # save the old graphics settings
                    def.par <- par(no.readonly = TRUE)

                    # the matrix shows the layout of the plots:
                    zones <- matrix(c(
                        0, 4, 6, 
                        1, 5, 3, 
                        0, 2, 0), 
                        ncol = 3, byrow = TRUE)
                    layout(zones, 
                        respect = TRUE,
                        widths  = c(0.4, 4, 0.6),
                        heights = c(3/5, 10.5/5, 1.5/5)
                    )
                    # certain plots need to have the same margins
                    topPlots.mar.b <- 0
                    topPlots.mar.t <- 0.35
                    
                    # "row2Plots" appear as the bottom (nothing but labels in row 3)
                    row2Plots.mar.b <- 
                        if (varIsContinuous()[varname]) 2 else {
                            max(min(max(nchar(levels(
                                datx[[varname]])))[1] / 2, 8), 2)
                    }
                    row2Plots.mar.t <- 0.65 
                    
                    col2Plots.mar.l <- 2
                    col2Plots.mar.r <- 0.35
                    
                    col3Plots.mar.l <- 0.35
                    col3Plots.mar.r <- 0.65

                    # for all plots: 
                    #   drop the axis titles and omit boxes, set up margins
                    par(xaxt     = "n", 
                        yaxt     = "n", 
                        ann      = FALSE,
                        bty      = if (testing) "o" else "n",
                        oma      = c(0,0,0,0),
                        cex.axis = 1.1
                    ) 

                    # fig 1: Y axis label for central plot. 
                    par(mar = c(row2Plots.mar.b - 1.7, 0.7, .3, 0) +.05) # b, l, t, r
                    plot(x= 1, y= 1, type= "n", ylim= c(-1, 1), xlim= c(-1, 1))
                    text(0, 0, paste("Logit PS"), cex= 1.4, srt= 90)
                    if (testing) box("outer", col= "blue")
                    if (testing) box("figure", col= "green")
                    
                    # fig 2:  X axis label for central plot. 
                    par(mar = c(0, 2, .3, 0) +.05) # b, l, t, r
                    plot(x= 1, y= 1, type="n", ylim= c(-1, 1), xlim= c(-1, 1))
                    text(0, 0, paste(varname), cex=1.4)
                    if (testing) box("figure", col= "green")

                    #################################################
                    # fig 3, right-side central (row2) plot
                    par(mar  = c(row2Plots.mar.b, col3Plots.mar.l, 
                            row2Plots.mar.t, col3Plots.mar.r), # bltr
                        xaxt ="s"
                    )
                    
                    plot(1, 0,
                        xlim = c(0, 2), 
                        ylim = myYlimPS, 
                        axes = FALSE,
                        type = "n"
                    )
                    if (input$shadeBrushedArea == TRUE & !is.null(psbrushmin())) {
                        par.usr <- par("usr")
                        rect(
                            xleft   = par.usr[1], 
                            ybottom = psbrushmin(), 
                            xright  = par.usr[2], 
                            ytop    = psbrushmax(),
                            density = NA,
                            border  = NA,
                            col     = "#DFD7CA"
                        )
                    }
                    if (datxps.xna[, .N] >= 1) {
                        for (lev in groupVarFactorLevelsSorted()) {
                            y <- datxps.xna[get(groupVarFactorName()) == lev, 
                                get(logitpsVarName())]
                            stripchart(y,
                                vertical = TRUE,
                                add      = TRUE,
                                method   = "jitter",
                                jitter   = myJitter,
                                pch      = 20,
                                at       = 1 + myAtAdds[lev],
                                cex      = pointSizeVal(),
                                col      = adjustcolor(colorScale.mod()[lev], 
                                            alpha.f= alphaVal())
                            )
                        }
                        myMean <- mean(datxps.xna[[logitpsVarName()]])   
                        segments(
                            1 - myWidth / 2, myMean,
                            1 + myWidth / 2, myMean
                        )
                    }
                    axis(1, at = 1, labels= "Missing")
                    if (testing) box("figure", col= "green")
                    if (testing) box("plot", col= "black")
                    ################################################


                    #################################################
                    # fig 4, top central plot. 
                    par(mar  = c(topPlots.mar.b, col2Plots.mar.l, 
                                topPlots.mar.t, col2Plots.mar.r), #bltr
                        xaxt ="n"
                    )
                    if (varIsContinuous()[varname]) {    
                        plot(datx.nona[[varname]], runif(datx.nona[, .N]), 
                            ylim = c(0, myYlimCounts), 
                            bty  = if (testing) "o" else "n",
                            type = "n"
                        )
                        # modified from http://www.r-bloggers.com/overlapping-histogram-in-r/
                        for (lev in groupVarFactorLevelsSorted()) {
                            myColor <- colorScale.mod()[lev] 
                            if (!is.null(histlist[[lev]])) {
                                plot(histlist[[lev]], 
                                    freq   = TRUE, 
                                    col    = adjustcolor(myColor, alpha.f= alphaVal()),
                                    border = NA,
                                    lty    = 0, # this is to help with rendering on server. Not sure it does though.
                                    add    = TRUE
                                )
                            }
                        }
                    } else { # discrete
                        # https://flowingdata.com/2016/03/22/comparing-ggplot2-and-r-base-graphics/
                        plot(1, 0,
                            xlim = c(min(myAtOrig) - 1, max(myAtOrig) + 1), 
                            ylim = c(0, myYlimCounts), 
                            bty  = if (testing) "o" else "n",
                            type = "n")

                        for (grouplev in groupVarFactorLevelsSorted()) {
                            for (varlev in levels(datx.nona[[varname]])) {
                                rect(
                                    xleft   = myAtOrig[varlev] + myAtAdds[grouplev] - myJitter,
                                    ybottom = 0,
                                    xright  = myAtOrig[varlev] + myAtAdds[grouplev] + myJitter,
                                    ytop    = xtbl[grouplev, varlev],
                                    density = NA,
                                    border  = NA,
                                    col     = colorScale.mod()[grouplev] 
                                )
                            }
                        }
                    }
                    if (testing) box("figure", col= "green")
                    ################################################


                    ################################################
                    # fig 5, the main plot
                    par(
                        mar  = c(row2Plots.mar.b, col2Plots.mar.l, 
                            row2Plots.mar.t, col2Plots.mar.r), #bltr
                        xaxt = "s", 
                        yaxt = "s", 
                        bty= if (testing) "o" else "n"
                    )
                    if (varIsContinuous()[varname]) {    
                        plot(datxps.nona[[varname]], datxps.nona[[logitpsVarName()]], 
                            xlim = myXlim, 
                            ylim = myYlimPS, 
                            bty  = if (testing) "o" else "n",
                            type = "n"
                        )
                    } else {
                        par(las= 2) #TODO: try to angle instead

                        plot(1, 0,
                            xlim = c(min(myAtOrig) - 1, max(myAtOrig) + 1), 
                            ylim = myYlimPS, 
                            axes = FALSE,
                            type = "n")
                    }
                    if (input$shadeBrushedArea == TRUE & !is.null(psbrushmin())) {
                        par.usr <- par("usr")
                        rect(
                            xleft   = par.usr[1], 
                            ybottom = psbrushmin(), 
                            xright  = par.usr[2], 
                            ytop    = psbrushmax(),
                            density = NA,
                            border  = NA,
                            col     = "#DFD7CA"
                        )
                    }
                    if (varIsContinuous()[varname]) {    
                        for (lev in groupVarFactorLevelsSorted()) {
                            x <- datxps.nona[get(groupVarFactorName()) == lev, get(varname)]
                            y <- datxps.nona[get(groupVarFactorName()) == lev, 
                                get(logitpsVarName())]
                            points(x, y,
                                pch = 20,
                                cex = pointSizeVal(),
                                col = adjustcolor(colorScale.mod()[lev], 
                                    alpha.f= alphaVal()))
                        }
                        lines(loess.smooth(datxps.nona[[varname]], datxps.nona[[logitpsVarName()]]))
                    } else {
                        for (lev in groupVarFactorLevelsSorted()) {
                            x <- datxps.nona[get(groupVarFactorName()) == lev, get(varname)]
                            y <- datxps.nona[get(groupVarFactorName()) == lev, 
                                get(logitpsVarName())]
                            stripchart(y ~ x,
                                vertical = TRUE,
                                add      = TRUE,
                                method   = "jitter",
                                jitter   = myJitter,
                                pch      = 20,
                                at       = myAtOrig + myAtAdds[lev],
                                cex      = pointSizeVal(),
                                col      = adjustcolor(colorScale.mod()[lev], 
                                            alpha.f= alphaVal())
                            )
                        }
                        means <- unlist(lapply(levels(datxps.nona[[varname]]), function(levl)
                            mean(datxps.nona[get(varname) == levl, get(logitpsVarName())])))
                        for (i in seq_along(levels(datxps.nona[[varname]]))) {
                            levl <- levels(datxps.nona[[varname]])[i]
                            myMean <- mean(datxps.nona[get(varname) == levl, 
                                get(logitpsVarName())])   
                            segments(
                                myAtOrig[i] - myWidth / 2, myMean,
                                myAtOrig[i] + myWidth / 2, myMean
                            )
                        }
                        axis(1, at = myAtOrig, labels = names(myAtOrig))
                        axis(2)
                    }
                    if (testing) box("figure", col= "green")
                    if (testing) box("plot", col= "black")
                    #################################################

                    #################################################
                    # fig 6, top right plot. 
                    par(
                        mar  = c(topPlots.mar.b, col3Plots.mar.l, 
                                topPlots.mar.t, col3Plots.mar.r), #bltr
                        xaxt = "n", 
                        yaxt = "n"
                    )
                    plot(1, 0,
                        xlim = c(0, 2), 
                        ylim = c(0, myYlimCounts), 
                        bty  = if (testing) "o" else "n",
                        type = "n")

                    for (grouplev in groupVarFactorLevelsSorted()) {
                        rect(
                            xleft   = 1 + myAtAdds[grouplev] - myJitter,
                            ybottom = 0,
                            xright  = 1 + myAtAdds[grouplev] + myJitter,
                            ytop    = datx.xna.counts[grouplev],
                            density = NA,
                            border  = NA,
                            col     = colorScale.mod()[grouplev] 
                        )
                    }
                    if (testing) box("figure", col= "green")
                    #################################################

                    # reset the graphics
                    par(def.par)
                }, res= 100
                ) # end renderPlot
            }) # end local
        } # end for
    }}) # end observe    


    observe({
        if (datInfo$newDataNoVarsChosen == FALSE) for (i in seq_along(varsToView())) {
            local({
                my_i <- i
                varname     <- varsToView()[my_i]
                naTableName <- paste0("naTable_", varname)

                # Create a missing-by-group table for each variable
                output[[naTableName]] <- renderTable({
                    if (is.null(dsetOrig())) return(NULL)
                    if (is.null(dsetGroupvar())) return(NULL)
                    if (is.null(idsToKeepAfterPruning())) return(NULL)

                    # core dataset for plots and naTable: ID, group, x
                    #    Making within each because of dependency problems
                    datx <- dsetOrig()[idsToKeepAfterPruning()][, 
                        c(idVarName(), varname), 
                        with= FALSE][dsetGroupvar()[idsToKeepAfterPruning()]]
                    dat <- datx[, .(pct.missing = 100 * mean(is.na(get(varname)))), 
                        by= eval(groupVarFactorName())]
                    setnames(dat, old = groupVarFactorName(), new = groupVarName())
                    dat
                }, digits= 1, include.rownames= FALSE
                ) # end renderTable

            }) # end local
        } # end for
    }) # end observe    

    observe({
        if (datInfo$newDataNoVarsChosen == FALSE) for (i in seq_along(varsToView())) {
            local({
                my_i <- i
                varname         <- varsToView()[my_i]

                pruner1name      <- paste0("pruner1_", varname)
                pruner2name      <- paste0("pruner2_", varname)
                input1name       <- paste0("pruningChoices1_", varname)
                input2name       <- paste0("pruningChoices2_", varname)

                # Create input functions for each variable
                output[[pruner1name]] <- renderUI({
                    if (is.null(varIsContinuous())) return(NULL)
                    if (is.null(varIsContinuous()[varname])) return(NULL)
                    if (is.null(idsToKeepAfterPruning())) return(NULL)

                    if (varIsContinuous()[varname]) {
                        #textInputRow(
                        textInput(
                            inputId= input1name, 
                            label="Min:", 
                            width= '75%',
                            value = floor(10^xdig() *   
                                min(unlist(dsetOrig()[idsToKeepAfterPruning(), 
                                    eval(varname), with= FALSE]), 
                                    na.rm = TRUE)) / 10^xdig()
                        )
                        
                    } else { # we have categorical var, either factor or char
                        checkboxGroupInput(
                            input1name, 
                            NULL,
                            # as.character corrects the printing of factor levels
                            choices=  as.character(sort(unique(na.omit(unlist(
                                dsetOrig()[, eval(varname), with= FALSE]))))),
                            selected= as.character(sort(unique(unlist(
                                dsetOrig()[idsToKeepAfterPruning(), eval(varname), with= FALSE]))))
                        )
                    }
                }) # end renderUI

                output[[pruner2name]] <- renderUI({
                    if (is.null(varIsContinuous())) return(NULL)
                    if (is.null(varIsContinuous()[varname])) return(NULL)
                    if (is.null(idsToKeepAfterPruning())) return(NULL)

                    if (varIsContinuous()[varname]) {
                        textInput(
                            inputId= input2name, 
                            label="Max:", 
                            width= '75%',
                            value = ceiling(10^xdig() * 
                                max(unlist(dsetOrig()[idsToKeepAfterPruning(), eval(varname), with= FALSE]), na.rm = TRUE)) / 10^xdig()

                        )
                    } else { # we have categorical var, either factor or char
                        NULL
                    }
                }) # end renderUI
            }) # end local
        } # end for
    }) # end observe    

    observe({
        if (datInfo$newDataNoVarsChosen == FALSE) for (i in seq_along(varsToView())) {
            local({
                my_i <- i
                varname         <- varsToView()[my_i]

                keepNAName      <- paste0("keepNA_", varname)
                keepNAInputName <- paste0("keepNAInput_", varname)
     
                # Create "keep NA?" input function for each variable
                output[[keepNAName]] <- renderUI({
                    radioButtons(keepNAInputName, NULL,
                        c("Keep units with missing values for this variable" = 1,
                        "Exclude units with missing values for this variable" = 0),
                        selected = 1,
                        width = '100%'
                    )
                }) # end renderUI
            }) # end local
        } # end for
    }) # end observe    

    
    # Keeping the observe for the textcheck separate
    observe({
        if (datInfo$newDataNoVarsChosen == FALSE) for (i in seq_along(varsToView())) {
            local({
                my_i <- i
                varname         <- varsToView()[my_i]

                textCheck1Name   <- paste0("textcheck1_", varname)
                #textCheck2Name   <- paste0("textcheck2_", varname)
                input1name       <- paste0("pruningChoices1_", varname)
                input2name       <- paste0("pruningChoices2_", varname)
                
                # Check the textInput for each variable
                # TODO: break this up for the two input boxes
                output[[textCheck1Name]] <- renderUI({
                    # give the dependencies time to catch up
                    if (is.null(varIsContinuous())) return(NULL)
                    if (is.null(varIsContinuous()[varname])) return(NULL)
                    if (is.null(input[[input1name]])) return(NULL)
                    if (is.null(input[[input2name]])) return(NULL)
                    
                    if (varIsContinuous()[varname]) {
                        if (is.na(as.numeric(input[[input1name]])) | 
                            is.na(as.numeric(input[[input2name]]))) {    
                            HTML(paste0(tags$span(class="text-danger", 
                                "Please make sure both boxes contain numbers.")))
                        } else { # no problem
                            return(NULL)
                        }
                    } else { # categorical var, nothing to check
                        return(NULL)
                    }
                }) # end renderUI
            }) # end local
        } # end for
    }) # end observe    
    
    
    # Now put them all together
    output$covariatePlotsAndInputs <- renderUI({
        if (datInfo$newDataNoVarsChosen == TRUE) return(NULL)
        if (input$generalGraphUpdateButton == 0) return(NULL)
        if (is.null(varsToView())) return(NULL)
        
        plot_and_input_list <- vector("list", numvarsToView())
        for (i in seq_along(varsToView())) {
            varname        <- varsToView()[i]
            plotname       <- paste0("plot_", varname)
            pruner1name    <- paste0("pruner1_", varname)
            pruner2name    <- paste0("pruner2_", varname)
            textcheck1name <- paste0("textcheck1_", varname)
            keepNAName     <- paste0("keepNA_", varname)
            naTableName    <- paste0("naTable_", varname)
            
            plot_and_input_list[[i]] <- fluidRow(
                tags$hr(),
                h4(paste0("Variable: ", varname)),
                column(width= 5, offset= 1, 
                    plotOutput(plotname, 
                        #inline= TRUE
                        height= 300,
                        width  = "auto"
                    ) # end plotOutput   
                ), # end column
                column(6,
                    if (varIsContinuous()[varname]) {
                        h5("Keep only units in this range (inclusive):") 
                    } else {
                        h5('Keep only units with the following value(s):')
                    },
                    uiOutput(textcheck1name),
                    fluidRow(
                        column(3, 
                            uiOutput(pruner1name)
                        ),
                        column(3, 
                            uiOutput(pruner2name)
                        )
                    ),
                    tags$br(),
                    uiOutput(naTableName),
                    uiOutput(keepNAName)
                ) # end column
            )# end fluidRow
        } 
        plot_and_input_list
    })
 
    ############################################################
    ############################################################
    ## SMDs
    wantATEWts <- reactive({
        input$showATE
    })
    wantATTWts <- reactive({
        input$showATT
    })
    wantATMWts <- reactive({
        input$showATM
    })
    
    # adapted from https://github.com/kaz-yos/tableone/blob/master/vignettes/smd.Rmd
    tabOrig <- reactive({
        if (is.null(varsToView())) return(NULL)

        ## Create a TableOne object
        tab2 <- CreateTableOne(
            vars       = varsToView(),
            strata     = groupVarName(),
            data       = dsetOrig(),
            factorVars = catVars(),
            includeNA  = TRUE,
            test       = FALSE,
            smd        = TRUE
        )
    })    
    tabPruned <- reactive({
        if (is.null(varsToView())) return(NULL)
        if (is.null(exprToKeepAfterPruning())) return(NULL)
        if (is.null(idsToKeepAfterPruning())) return(NULL)

        ## Create a TableOne object
        tab2 <- CreateTableOne(
            vars       = varsToView(),
            strata     = groupVarName(),
            data       = dsetOrig()[idsToKeepAfterPruning()],
            factorVars = catVars(),
            includeNA  = TRUE,
            test       = FALSE,
            smd        = TRUE
        )
    })    

    #output$tabonetest <- renderPrint({
    #    if (is.null(tabOrig())) return(NULL)

    #    print(tabPruned())
    #})

    ## Construct a data frame containing variable name and SMD from all methods
    dsetSMDs <- reactive({
        if (is.null(tabOrig())) return(NULL)

        dat <- data.table(
            variable  = names(ExtractSmd(tabOrig())),
            Original  = ExtractSmd(tabOrig())
        )
        if (!is.null(tabPruned())) {
            dat[, Pruned := ExtractSmd(tabPruned())]
        }
        # todo: add for 3 other options here
        setkey(dat, variable)
        
        varNames <- as.character(dat[, variable])[order(
                dat[, Original])]

        datSorted <- dat[varNames]

        datSorted[, varnum := 1:.N]
        setkey(datSorted, varnum)
    })
    output$SMDPlot <- renderPlot({
        if (is.null(dsetSMDs())) return(NULL)

        nvars <- nrow(dsetSMDs())
        tabTypes <- intersect(
            c("Original", "Pruned", "ATE", "ATT", "ATM"),
            names(dsetSMDs())
        )  
        nTabTypes <- length(tabTypes)      
        myColors <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')[1:nTabTypes]
        myShapes <- (21:25)[1:nTabTypes]
    
        maxX <- max(dsetSMDs()[, tabTypes, with= FALSE], na.rm= TRUE)
        
        def.par <- par(no.readonly = TRUE)
        par(
            ann      = FALSE,
            oma      = c(0,0,0,0),
            cex.axis = 1.1
        ) 
        plot(0, 1,
            type= "n",
            bty = "n",
            xlim = c(0, maxX),
            ylim = c(0.5, nvars),
            axes= FALSE
        )
        axis(1)
        axis(2, at= 1:nvars, labels= dsetSMDs()[, variable])
        # TODO next:
        # increase L margin
        # rotate L margin text
        # add linetypes
        # add legend
        # change shape (put into a column in ui, w/ width & height)
        # then move on to weighted tableone's!
        for (j in 1:nTabTypes) {
            lines(
                x= as.numeric(dsetSMDs()[[tabTypes[j]]]),
                y= 1:nvars,
                #lty= TODO,
                col= myColors[j]
            )
        }
        for (i in 1:nvars) {
            abline(h= i, lty= 'dotted', col= 'gray')
            points(
                x= as.numeric(dsetSMDs()[i][, tabTypes, with= FALSE]), 
                y= rep(i, nTabTypes), 
                pch= myShapes,
                col= myColors,
                bg= myColors
            )
        }

        # reset the graphics
        par(def.par)
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
    
