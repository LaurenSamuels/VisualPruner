# Visual Pruner

library(shiny)
library(rms)
library(data.table)
library(survey)
library(tableone)
library(dplyr)

# On the department server, Cairo plots look worse
options(shiny.usecairo= FALSE)

# Allow upload of bigger files
# from http://stackoverflow.com/questions/18037737/how-to-change-maximum-upload-size-exceeded-restriction-in-shiny-and-save-user
# The first number is the number of MB
options(shiny.maxRequestSize=30*1024^2)

# text types for use w/ CSS, just here as a reference
#<"text-muted">
#<"text-primary">
#<"text-warning">
#<"text-danger">
#<"text-success">
#<"text-info">

shinyServer(function(input, output, session) {
    source("plottingFunctions.R")
    source("smdFunctions.R")
    source("psFunctions.R")
    
    ############################################################
    ## reactiveValues() and related observers
    ############################################################
    
    datInfo <- reactiveValues()
    # This workaround lets me set the fileInfo
    #   to NULL when the user decides to upload a new file.
    #   I think an easier way is coming w/ new versions of Shiny.
    datInfo$inFileInfo          <- NULL
    # Flags 
    datInfo$newData             <- NULL
    datInfo$newDataNoVarsChosen <- NULL
    datInfo$newDataNotYetPruned <- NULL

    observe({
        if (input$useExampleData == 0) {
            datInfo$inFileInfo <- input$datafileInfo 
        } 
    })
    observe({
        input$useExampleData

        datInfo$newData             <- TRUE
        datInfo$newDataNoVarsChosen <- TRUE
        datInfo$newDataNotYetPruned <- TRUE
    })
    observeEvent(input$changeUpFileButton, {
        datInfo$inFileInfo <- NULL

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
    observeEvent(input$xgraphsUpdateButton | input$PSCalcUpdateButton, {
        if (!(datInfo$newData | datInfo$newDataNoVarsChosen)) {
            datInfo$newDataNotYetPruned <- FALSE
        }
    })
    
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
        
        # File input, from example on shiny website.
        # input$datafileInfo will be NULL initially. 
        # After the user selects and uploads a file, 
        # it will be a data frame with 'name',
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
        if (input$useExampleData == 0 & is.null(datInfo$inFileInfo)) {
            return(NULL)
        }
    
        if (input$useExampleData == 1) {
            # Take care of the random seed.
            # Code from Cole in nbpMatching pkg
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
        }  else if (!is.null(datInfo$inFileInfo)) {
            if (grepl("\\.csv\\>", datInfo$inFileInfo$name)) {
                mydat <- fread(datInfo$inFileInfo$datapath,
                    sep= ",",
                    header= TRUE,
                    data.table= TRUE
                )
            } else if (grepl("\\.rds\\>", datInfo$inFileInfo$name)){
                mydat <- as.data.table(readRDS(datInfo$inFileInfo$datapath))
            }    
        }
        mydat
    })
    varnamesOrig <- reactive({
        req(dsetOrig())
        names(dsetOrig())  
    })
    nOrig <- reactive({
        req(dsetOrig())
        nrow(dsetOrig())  
    })
    idVarName <- reactive({
        # The name produced by this function will be used as
        #   the name of the id var
        req(varnamesOrig())
        
        proposedName <- "MY__ID"
        while(proposedName %in% varnamesOrig()) {
            proposedName <- paste0(proposedName, "_NEW")    
        }    
        proposedName
    })    
    observe({
        # Add an ID variable so we can match obsns w/ the PS dataset
        req(idVarName())
        if (!(idVarName() %in% varnamesOrig())) {
            dsetOrig()[, idVarName() := paste0("id", 1:nrow(dsetOrig()))]
            setkeyv(dsetOrig(), idVarName())
        }
    })
    possGroupNamesOrig <- reactive({
        # Returns a vector of names of the vars that have exactly 2 unique vals
        req(dsetOrig())
        varnamesOrig()[sapply(dsetOrig(), 
            function(vec) length(unique(vec)) == 2)]
    })
    output$chooseGroup <- renderUI({
        req(dsetOrig())
        
        # TODO: May be able to take out this next line at some point
        if (input$useExampleData == 0 & is.null(datInfo$inFileInfo)) return(NULL)

        selectizeInput('treatmentVarName', 
            label    = NULL, 
            choices  = possGroupNamesOrig(), 
            selected = NULL,
            multiple = FALSE
            )
    })
    groupVarName <- reactive({
        req(dsetOrig())

        # TODO: May be able to take out this next line at some point
        if (input$useExampleData == 0 & is.null(datInfo$inFileInfo)) return(NULL)
        
        input$treatmentVarName
    })
    groupVarFactorName <- reactive({
        # The name produced by this function will be used as
        #     the name of the treatment group var 
        req(dsetOrig())
        req(groupVarName())
        
        if (is.factor(dsetOrig()[[groupVarName()]])) {
            return(groupVarName())    
        }    
        proposedName <- paste0(groupVarName(), '.factor')
        while(proposedName %in% varnamesOrig()) {
            proposedName <- paste0(proposedName, "NEW")    
        }    
        proposedName
    })    


    # Text related to data import, etc.
    output$noDataChosenText <- renderUI({
        req(dsetOrig())
        if ((input$useExampleData == 0 & is.null(datInfo$inFileInfo))) {
            HTML(paste0(tags$span(class="text-warning", 
                "No dataset selected.")))
        } else return(NULL)
    })
    output$dataFnameText1 <- renderUI({
        req(dsetOrig())
        if (input$useExampleData == 1) return(NULL)

        HTML(paste0(tags$h5("Filename:")))
    })
    output$dataFnameText2 <- renderUI({
        req(dsetOrig())
        if (input$useExampleData == 1) return(NULL)

        HTML(paste0(tags$span(paste0(datInfo$inFileInfo$name))))
    })
    output$dataFnameText3 <- renderUI({
        req(dsetOrig())
        if (input$useExampleData == 1) return(NULL)

        HTML(paste0(tags$br()))
    })

    output$dataDimText1 <- renderUI({
        req(dsetOrig())

        HTML(paste0(tags$h5("Dimensions:")))
    })
    output$dataDimText2 <- renderUI({
        req(dsetOrig())

        HTML(paste0(tags$span(paste0("The dataset has ", ncol(dsetOrig()), 
            " columns and ", nrow(dsetOrig()), " rows."))))
    })

    output$groupLevelText1 <- renderUI({
        req(groupVarName())

        HTML(paste0(tags$h5("The treatment indicator has the following levels:")))
    })
    output$groupLevelTable <- renderTable({
        req(groupVarName())

        # the as.character lets this print right if var 
        #    is already a factor
        dat <- data.frame(as.character(sort(unique(
            dsetOrig()[[groupVarName()]])))) 
        names(dat) <- groupVarName()
        dat
    }, include.rownames = FALSE)


    ############################################################
    ############################################################
    ## PS specification, etc.
    
    psUseCompleteCasesOnly <- reactive({
        input$forPSCompleteCasesOnly == 1
    })
    
    idsWithVarsOKForPS <- reactive({
        # These are the IDs of people who can be used for PS calculation
        #    in terms of the non-missingness of the relevant variables
        if (datInfo$newData == TRUE) return(NULL)
        req(idVarName())

        if (is.null(varnamesFromRHS())) return(dsetOrig()[[idVarName()]])
        
        if (psUseCompleteCasesOnly()) {
            na.omit(dsetOrig()[, c(varnamesFromRHS(), idVarName()), 
                with= FALSE])[[idVarName()]]
        } else {
            dsetOrig()[[idVarName()]]
        }
    })
    
    psVarName <- reactive({
        # The name produced by this function will be used as
        #   the name of the ps var in dsetPSGraphs()
        #   (we make sure it does not overlap w/ varnames from original dset)
        req(dsetOrig())
        
        proposedName <- "MY__PS"
        while(proposedName %in% varnamesOrig()) {
            proposedName <- paste0(proposedName, "_NEW")    
        }    
        proposedName
    })    
    logitpsVarName <- reactive({
        # The name produced by this function will be used as
        #   the name of the logit ps var in dsetPSGraphs()
        #   (we make sure it does not overlap w/ varnames from original dset)
        req(dsetOrig())
        
        proposedName <- "MY__LOGITPS"
        while(proposedName %in% varnamesOrig()) {
            proposedName <- paste0(proposedName, "_NEW")    
        }    
        proposedName
    })    
    naPrefix <- reactive({
        # The phrase produced by this function will be used as
        #   the prefix for the na.indicator vars
        req(dsetOrig())
        
        proposedPrefix <- "is.na_"
        while(any(grepl(paste0("^", proposedPrefix), varnamesOrig()))) {
            proposedPrefix <- paste0(proposedPrefix, "_")    
        }    
        proposedPrefix
    })    
    treatedVarName <- reactive({
        # The name produced by this function will be used as
        #   the name of the 0/1 tx indicator var in dsetPSGraphs()
        #   (we make sure it does not overlap w/ varnames from original dset)
        req(dsetOrig())
        
        proposedName <- "MY__TX01"
        while(proposedName %in% varnamesOrig()) {
            proposedName <- paste0(proposedName, "_NEW")    
        }    
        proposedName
    })    
    ateWtVarName <- reactive({
        # The name produced by this function will be used as
        #   the name of the ATE wt var in dsetPSGraphs()
        #   (we make sure it does not overlap w/ varnames from original dset)
        req(dsetOrig())
        
        proposedName <- "MY__ATE"
        while(proposedName %in% varnamesOrig()) {
            proposedName <- paste0(proposedName, "_NEW")    
        }    
        proposedName
    })    
    attWtVarName <- reactive({
        # The name produced by this function will be used as
        #   the name of the ATT wt var in dsetPSGraphs()
        #   (we make sure it does not overlap w/ varnames from original dset)
        req(dsetOrig())
        
        proposedName <- "MY__ATT"
        while(proposedName %in% varnamesOrig()) {
            proposedName <- paste0(proposedName, "_NEW")    
        }    
        proposedName
    })    
    atmWtVarName <- reactive({
        # The name produced by this function will be used as
        #   the name of the ATM wt var in dsetPSGraphs()
        #   (we have to make sure it does not overlap w/ current var names)
        req(dsetOrig())
        
        proposedName <- "MY__ATM"
        while(proposedName %in% varnamesOrig()) {
            proposedName <- paste0(proposedName, "_NEW")    
        }    
        proposedName
    })    

    dsetGroupvar <- reactive({
        # A dataset with two columns, id and group.
        # Contains all rows from original dataset.
        # Not created until after the PS formula is typed.
        if (datInfo$newData == TRUE) return(NULL)
        req(dsetOrig())
        req(groupVarFactorName())
        req(idVarName())

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
        req(dsetGroupvar())

        tmp <- table(dsetGroupvar()[[groupVarFactorName()]])
        names(tmp)[order(tmp, decreasing= TRUE)]
    })
    smallestGroup <- reactive({
        req(groupVarFactorLevelsSorted())

        groupVarFactorLevelsSorted()[length(
            groupVarFactorLevelsSorted())]
    })
    colorScale <- reactive({
        req(dsetGroupvar())

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
    dsetPSGraphs <- reactive({
        # This dataset is used for making the PS plots.
        req(dsetGroupvar())
        req(logitPS())
        req(smallestGroup())
        req(idsToKeepAfterPruning())
        
        dat1 <- data.frame(
            id    = unlist(dsetOrig()[idsWithVarsOKForPS(), 
                idVarName(), with= FALSE]),
            group = unlist(dsetGroupvar()[idsWithVarsOKForPS(), 
                groupVarFactorName(), with= FALSE])
        )
        dat1$id <- as.character(dat1$id)
        dat2 <- data.frame(
            id       = PSIDs(),
            logit.ps = logitPS(), 
            ps       = PS(),
            stringsAsFactors= FALSE
        )
        # keep only units with values in both dsets
        dat <- dplyr::inner_join(dat1, dat2, by= "id")  
        names(dat)[names(dat) == "id"] <- idVarName()
        names(dat)[names(dat) == "ps"] <- psVarName()
        names(dat)[names(dat) == "logit.ps"] <- logitpsVarName()
        names(dat)[names(dat) == "group"] <- groupVarFactorName()
        
        dat <- as.data.table(dat)  
        setkeyv(dat, idVarName())

        # now add the weights
        dat[, eval(treatedVarName()):= 
            get(groupVarFactorName()) %in% smallestGroup()] 
        dat[, eval(ateWtVarName()):= 
            get(treatedVarName()) / get(psVarName()) +
            (1 - get(treatedVarName())) / (1 - get(psVarName()))
        ] 
        dat[, eval(attWtVarName()):= 
            get(treatedVarName())  +
            (1 - get(treatedVarName())) * get(psVarName()) / (1 - get(psVarName()))
        ] 
        dat[, eval(atmWtVarName()):= 
            pmin(get(psVarName()), (1 - get(psVarName()))) /
            (get(treatedVarName()) * get(psVarName()) +
            (1 - get(treatedVarName())) * (1 - get(psVarName())))
        ] 
        
        # PSIDs() might be different from idsToKeepAfterPruning,
        #     because sometimes we prune but keep old PS.
        #     Then have to do na.omit because some people might
        #     be in idsToKeepAfterPruning but not in PSIDs...
        dat2 <- na.omit(dat[idsToKeepAfterPruning()])
        setkeyv(dat2, idVarName())
        dat2
    })    

    output$getFormula <- renderUI({
        req(groupVarName())

        textInput('formulaRHS', 
            label= paste0(groupVarName(), ' ~ '), 
            value= ' ',
            width= '100%'
        )
    })

    formRHS <- reactive({
        # -- contains an isolate -- #
        req(dsetOrig())
        if (input$psTypedButton == 0) return(NULL) 
        if (datInfo$newData == TRUE) return(NULL)
        # Other dependencies
        groupVarFactorName()
        psUseCompleteCasesOnly()
        
        isolate(input$formulaRHS)
    })
    stringFormula <- reactive({
        req(groupVarFactorName())

        paste0(groupVarFactorName(), ' ~ ', formRHS())
    })    
    psForm <- reactive({
        req(stringFormula())

        tryCatch(as.formula(stringFormula()),
            error= function(e) return(NULL))
    })    
    psNotChecked <- reactive({
        if (is.null(stringFormula())) return(TRUE)

        if (input$psTypedButton == 0 | 
            paste0(groupVarFactorName(), ' ~ ', input$formulaRHS) != 
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
        if (psUseCompleteCasesOnly()) {
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

    output$noDataChosenText2 <- renderUI({
        req(dsetOrig())
        if (input$useExampleData == 0 & is.null(datInfo$inFileInfo)) {
            HTML(paste0(tags$span(class="text-warning", 
                "No dataset selected.")))
        } else return(NULL)
    })
    output$othervarsTable <- renderTable({
        req(groupVarFactorName())

        namesToExclude <- c(idVarName(), groupVarName())
        if (groupVarName() != groupVarFactorName()) {
            namesToExclude <- c(namesToExclude, groupVarFactorName())
        }
        
        myvars <- setdiff(varnamesOrig(), namesToExclude)
        nvars <- length(myvars)
        nrows <- min(ceil(nvars / 2), 5)
        numExtras <- (nrows - (nvars %% nrows)) %% nrows
        data.frame(matrix(c(myvars, rep(NA, numExtras)), 
            nrow = nrows, byrow = TRUE)) 
    }, include.rownames = FALSE, include.colnames= FALSE)
    
    PSIDs <- reactive({
        # The IDs that will be used in PS calculation
        
        # -- contains an isolate -- #
        # dependencies
        if (datInfo$newData == TRUE) return(NULL)
        if (input$PSCalcUpdateButton == 0) return(idsWithVarsOKForPS()) 
        
        intersect(idsWithVarsOKForPS(), isolate(idsToKeepAfterPruning()))
    })

    varnamesFromRHS <- reactive({
        req(psForm())
        
        allvars <- setdiff(all.vars(psForm()), groupVarFactorName())
        
        if (psUseCompleteCasesOnly()) {
            if (all(allvars %in% varnamesOrig())) allvars else NULL
        } else {
            allvars.noprefix <- 
                unique(gsub(paste0("^", naPrefix()), "", allvars))
            if (all(allvars.noprefix %in% varnamesOrig())) allvars else NULL
        }
    })    
    varnamesForIndicators <- reactive({
        # "Indicators" means "missingness indicator variables" here
        if (datInfo$newData == TRUE) return(NULL)
        req(varnamesFromRHS())
        
        setdiff(varnamesFromRHS(), varnamesOrig())
    })
    varnamesFromRHSOK <- reactive({
        # -- contains an isolate -- #
        if (datInfo$newData == TRUE) return(NULL)
        if (psNotChecked()) return(NULL)
        req(psFormSyntaxOK())
        # other dependencies
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
            if (psUseCompleteCasesOnly() & 
                any(grepl(paste0("^", naPrefix()), all.vars(psForm())))) {
                HTML(paste0(tags$span(class="text-danger", paste0(
                    "The formula uses missingness indicators, but you have ",
                    "chosen to use complete cases only. ",
                    "Please change your selection or remove the indicators."))))
            } else {
                HTML(paste0(tags$span(class="text-danger", 
                    "The formula uses variables that are not in the dataset. Please try again.")))
            }
        } else {
            HTML(paste0(tags$span(class="text-success", 
                "All variable names are OK.")))
        }
    })    

    output$psNeedsCheckingText <- renderUI({
        if (psNotChecked()) {
            HTML(paste0(tags$span(class="text-info", 
                "Remember to click the button when you're done!")))
        } else NULL   
    })

    dsetImputed <- reactive({
        if (datInfo$newData == TRUE) return(NULL)
        req(varnamesFromRHS())
        req(PSIDs())
        if (psUseCompleteCasesOnly()) return(NULL)
        
        origvars <- unique(gsub(paste0("^", naPrefix()), "", varnamesFromRHS()))
        myvars <- c(idVarName(), origvars, groupVarName())
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
    
    psFitMethod <- reactive({
        if (input$psMethod == 0) {
            "logistic"
        } else if (input$psMethod == 1) {
            "probit"    
        } else {
            NA    
        }
    })
    psFit <- reactive({
        # fit the PS model using the chosen method
        
        if (datInfo$newData == TRUE) return(NULL)
    
        req(psForm())
        req(varnamesFromRHS())
        
        if (psUseCompleteCasesOnly()) {
            dat <- dsetOrig()[PSIDs()]
        } else { # use imputed data
            dat <- dsetImputed()
        }    
        # now merge in the factor groupvar variable
        dat <- dplyr::left_join(dat, dsetGroupvar(), by= idVarName())

        tryCatch(getPSFit(dat, psFitMethod(), psForm()),
            error = function(e) return(NULL)
        )
    })
    output$psCopyText <- renderUI({
        # -- contains an isolate -- #
        # dependency. 
        # TODO: I'm not totally sure I want this behavior; see downloadPS.
        if (is.null(psFit())) return(NULL)
    
        HTML(paste0(
            tags$code(isolate(stringFormula())) 
        ))
    })
    output$downloadPS <- downloadHandler(
        # Download PS formula
        
        filename = function() {
            paste('VisualPruner_PSFormula_', Sys.Date(), '.txt', sep='')
        },
        content = function(myfile) {
            # -- contains an isolate -- #
            # dependency. 
            # TODO: I'm not totally sure I want this behavior; see psCopyText.
            if (is.null(psFit())) { 
                cat("NULL", file= myfile)
            } else {
                cat(isolate(stringFormula()), file= myfile)
            }
        }
    )
    output$downloadIncl <- downloadHandler(
        # Download inclusion criteria
        
        filename = function() {
            paste('VisualPruner_Inclusion_', Sys.Date(), '.txt', sep='')
        },
        content = function(myfile) {
        if (is.null(pruneValTextList())) return(NULL)
            if (is.null(pruneValTextList())) { 
                cat("NULL", file= myfile)
            } else {
                cat(do.call("paste", list(pruneValTextList(), collapse= " &\n")), 
                    file= myfile
                )
            }
        }
    )

    output$psFitProblemTextPrePruning <- renderUI({
        # -- contains an isolate -- (waiting for the "done typing" button)
        # dependencies
        psUseCompleteCasesOnly()
        psFitMethod()
        
        if (datInfo$newData == TRUE | psNotChecked() | is.null(varnamesFromRHSOK())) {
            HTML(paste0(tags$span(class="text-warning", "Not checked yet.")))
        } else if (!varnamesFromRHSOK()) { # couldn't combine this with above
            HTML(paste0(tags$span(class="text-warning", "Not checked yet.")))
        } else if (is.null(isolate(psFit()))) {
            HTML(paste0(tags$span(class="text-danger", 
                "The propensity score formula can't be fit using the current dataset. Please modify the model and/or the variables selected for viewing.")))
        } else {
            HTML(paste0(tags$span(class="text-success", 
                "PS model successfully fit.")))
        }
    })
    output$psGraphsNotReady <- renderUI({
        # dependencies
        psUseCompleteCasesOnly()
        
        if (is.null(dsetPSGraphs())) {
            HTML(paste0(tags$span(class="text-warning", 
                "Scores not yet estimated.")))
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

        if (is.null(isolate(psFit()))) {
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

    PS <- reactive({
        req(psFit())
        
        getPS(psFit(), psFitMethod())
    })
    logitPS <- reactive({
        req(PS())
        
        qlogis(PS())
    })
    
    output$dataNonmissingDimText1  <- renderUI({
        req(idsWithVarsOKForPS())
        req(varnamesFromRHSOK())
        
        if (!psUseCompleteCasesOnly()) return(NULL)
        if (psNotChecked()) return(NULL)

        HTML(paste0(tags$h4("Impact of not imputing missing values:")))
    })
    output$dataNonmissingDimText2 <- renderText({
        req(idsWithVarsOKForPS())
        req(PSIDs())
        if (!psUseCompleteCasesOnly()) return(NULL)
        req(varnamesFromRHSOK())
        if (psNotChecked()) return(NULL)

        # TODO: could add "out of" but would need to decide whether
        #     it should be out of original or pruned dataset
        paste0("Propensity scores can be estimated for ",
            length(PSIDs()), " units.")
    })
    output$psPlot <- renderPlot({
        req(dsetPSGraphs())
        
        makePSPlot(
            dat= dsetPSGraphs(),
            sortedFactorLevels= groupVarFactorLevelsSorted(),
            grpVarFactorName= groupVarFactorName(),
            xvarName= psVarName(),
            colorscale= colorScale(),
            alphaval = alphaVal(),
            logit= FALSE
        )
    }, res= 100)    
    
    output$logitpsPlot <- renderPlot({
        req(dsetPSGraphs())

        makePSPlot(
            dat= dsetPSGraphs(),
            sortedFactorLevels= groupVarFactorLevelsSorted(),
            grpVarFactorName= groupVarFactorName(),
            xvarName= logitpsVarName(),
            colorscale= colorScale(),
            alphaval = alphaVal()
        )
    }, res= 100)   
    
    output$psSummary <- renderPrint({
        req(psFit())   
        
        # TODO: continue here
        # Also remember to update the ps model download to include fitter
        
    })
    
    ############################################################
    ############################################################
    
    ############################################################
    ############################################################
    ## Covariate selection & graphs
        
    possVarsToRestrict <- reactive({
        if (datInfo$newData == TRUE) return(NULL)
        req(varnamesOrig())
        req(groupVarName())
        req(idVarName())

        # We don't want to allow restriction of the treatment var 
        setdiff(varnamesOrig(), 
            c(groupVarName(), idVarName()))  
    })    
    output$chooseVarsToRestrict <- renderUI({
        req(possVarsToRestrict())

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
        req(dsetOrig())
        req(possVarsToRestrict())

        input$generalGraphUpdateButton
        
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
        # vector of booleans, same length as varsToView()
        
        # -- contains an isolate -- #
        if (datInfo$newData == TRUE) return(NULL)
        req(varsToView())

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
    catVarsToView <- reactive({
        req(varsToView())
        req(varIsContinuous())
        
        varsToView()[!varIsContinuous()[varsToView()]]
    })

    alphaVal <- reactive({
        input$alphaSlider
    })
    pointSizeVal <- reactive({
        input$pointsizeSlider
    })
    # number of decimal places to use w/ covariate graph inputs
    # May have this changeable at some point, so I'm setting it up
    #    as a reactive to begin with
    xdig <- reactive({
        2
    })

    psbrushmin <- reactive({
        req(dsetPSGraphs()) 
        input$logitpsPlot_brush$xmin 
    })
    psbrushmax <- reactive({
        req(dsetPSGraphs()) 
        input$logitpsPlot_brush$xmax
    })
    # Create the expression to use for pruning
    pruneValRawList <- reactive({
        if (datInfo$newDataNotYetPruned == TRUE) return(NULL)
        req(varsToView())

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
        req(varsToView())

        mylist <- vector("list", numvarsToView())
        names(mylist) <- varsToView()

        for (vname in varsToView()) {
            # this is to help with the timing of the dependencies
            if (is.null(input[[paste0("keepNAInput_", vname)]])) {
                mylist[[vname]] <-  NULL
            } else {
                mylist[[vname]]  <- 
                    input[[paste0("keepNAInput_", vname)]] == "1"
            }
        }
        mylist
    })
    pruneValTextList <- reactive({
        # -- contains an isolate -- #
        # TODO: do I want the isolate in the RawLists instead?
        # dependencies
        if (datInfo$newDataNotYetPruned == TRUE) return(NULL)
        req(varsToView())
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
                                "(", varname, " %in% c(", 
                                paste(myvals, collapse= ","),"))"
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
        # Note that we don't want req here, 
        #   because of the way idsToKeepAfterPruning uses the value
        if(is.null(pruneValTextList())) return(NULL)
        
        do.call("paste", list(pruneValTextList(), collapse= " & " ))
    })    
    output$keepAfterPruningCopyText <- renderUI({
        req(pruneValTextList())

        mytext <- do.call("paste", list(pruneValTextList(), collapse= " & <br/>" ))
        HTML(paste0(
            tags$code(HTML(mytext)) 
        ))
    })    

    idsToKeepAfterPruning <- reactive({
        if (datInfo$newData == TRUE) return(NULL)
        req(dsetOrig())
        req(idVarName())

        if (is.null(exprToKeepAfterPruning())) {
            return(dsetOrig()[[idVarName()]])
        }
        
        dsetOrig()[eval(parse(text= 
            exprToKeepAfterPruning()))][[idVarName()]]
    })    
    
    output$pruneTable <- renderTable({
        req(idsToKeepAfterPruning())
        req(groupVarName())
    
        dat <- dsetOrig()[idsToKeepAfterPruning(), .N, 
            by= eval(groupVarName())]
        rbind(dat, list("Total", 
            dsetOrig()[idsToKeepAfterPruning(), .N]))
    }, include.rownames = FALSE)
    
    output$logitpsPlotForBrushing <- renderPlot(
        {
            req(dsetPSGraphs())
    
            makePSPlot(
                dat= dsetPSGraphs(),
                sortedFactorLevels= groupVarFactorLevelsSorted(),
                grpVarFactorName= groupVarFactorName(),
                xvarName= logitpsVarName(),
                colorscale= colorScale(),
                alphaval = alphaVal()
            )
        }, 
        res= 100
    )    

    output$logitpsPlotBrushable <- renderUI({
        req(dsetPSGraphs())

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
    
    
    dsetXGraphs <- reactive({
        if (datInfo$newDataNoVarsChosen == TRUE) return(NULL)
        req(varsToView())
        req(dsetPSGraphs())
        req(dsetGroupvar())

        # core dataset for plots: ID, group, x
        dat <- merge(
            dsetOrig()[idsToKeepAfterPruning()][, 
                c(idVarName(), varsToView(), groupVarName()), with= FALSE],
            dsetGroupvar()[idsToKeepAfterPruning()],
            # just in case
            suffixes = c("", ".y")
        )
        setkeyv(dat, idVarName())
        dat
    })
    dsetXPS <- reactive({
        if (datInfo$newDataNoVarsChosen == TRUE) return(NULL)
        req(dsetXGraphs())
        req(dsetPSGraphs())
    
        # dset w/ x and PS. Either could be missing.
        dat <- merge(
            dsetXGraphs(), 
            dsetPSGraphs(),
            all= TRUE, 
            # just in case
            suffixes= c("", ".y"))
        setkeyv(dat, idVarName())
        dat
    })


    #############################################################

    # modified from https://gist.github.com/wch/5436415/, 
    # with help from a SO post I forgot to get the URL for
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
                    if (is.null(dsetXGraphs())) return(NULL)
                    if (is.null(dsetXPS())) return(NULL)

                    # For adjusting plot alignment during development
                    testing <- FALSE

                    # core dataset for plots: ID, group, x
                    datxnames <- c(idVarName(), varname, groupVarFactorName())
                    datx <- dsetXGraphs()[, datxnames, with= FALSE]
                    
                    varIsCont <- varIsContinuous()[varname]
                    wantShading <- input$shadeBrushedArea == TRUE & !is.null(psbrushmin())
                    
                    # convert character & discrete numeric to factor
                    if (is.character(datx[[varname]]) | 
                            (is.numeric(datx[[varname]]) & 
                            !(varIsCont))) {
                        datx[, eval(varname) := 
                            factor(datx[[varname]])]
                    }
                    
                    # use datx.nona for marginal histogram/barchart
                    datx.nona <- na.omit(datx)
                    
                    # for the right-hand top plot
                    datx.xna <- datx[is.na(get(varname)), ]
                    
                    # dset w/ x and PS. Either could be missing.
                    datxps <- dsetXPS()[, c(datxnames, logitpsVarName()), with= FALSE]
                    
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
                    if(varIsCont) {
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
                    if(varIsCont) {
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
                        if (varIsCont) 2 else {
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
                    makeFig1(testing)
                    
                    # fig 2:  X axis label for central plot. 
                    par(mar = c(0, 2, .3, 0) +.05) # b, l, t, r
                    makeFig2(varname, testing)
                    
                    # fig 3, right-side central (row2) plot
                    par(mar  = c(row2Plots.mar.b, col3Plots.mar.l, 
                            row2Plots.mar.t, col3Plots.mar.r), # bltr
                        xaxt ="s"
                    )
                    makeFig3(dat= datxps.xna,
                        yLim= myYlimPS, 
                        WantShading= wantShading,
                        brushmin= psbrushmin(),
                        brushmax= psbrushmax(),
                        gVarFactorLevelsSorted = groupVarFactorLevelsSorted(),
                        gVarFactorName = groupVarFactorName(),
                        lpsVarName= logitpsVarName(),
                        pSizeVal= pointSizeVal(),
                        colScale= colorScale(),
                        alphVal= alphaVal(),
                        MyJitter= myJitter, MyAtAdds= myAtAdds, MyWidth= myWidth,
                        Testing= testing
                    )
                    
                    #################################################
                    # fig 4, top central plot. 
                    par(mar  = c(topPlots.mar.b, col2Plots.mar.l, 
                                topPlots.mar.t, col2Plots.mar.r), #bltr
                        xaxt ="n"
                    )
                    makeFig4(
                        dat= datx.nona,
                        VarIsCont= varIsCont,
                        Varname= varname,
                        myYLimCounts = myYlimCounts,
                        gVarFactorLevelsSorted = groupVarFactorLevelsSorted(),
                        colScale= colorScale(),
                        alphVal= alphaVal(),
                        Histlist= histlist,
                        MyAtOrig= myAtOrig, MyAtAdds= myAtAdds, MyJitter= myJitter,  
                        Xtbl= xtbl,
                        Testing= testing
                    )

                    ################################################
                    # fig 5, the main plot
                    par(
                        mar  = c(row2Plots.mar.b, col2Plots.mar.l, 
                            row2Plots.mar.t, col2Plots.mar.r), #bltr
                        xaxt = "s", 
                        yaxt = "s", 
                        bty= if (testing) "o" else "n"
                    )
                    makeFig5(
                        dat= datxps.nona,
                        VarIsCont= varIsCont,
                        Varname= varname,
                        lpsVarName= logitpsVarName(),
                        MyXlim= myXlim, 
                        MyYlimPS= myYlimPS, 
                        MyAtOrig= myAtOrig, MyAtAdds= myAtAdds, MyJitter= myJitter,
                        WantShading= wantShading,
                        brushmin= psbrushmin(),
                        brushmax= psbrushmax(),
                        gVarFactorLevelsSorted = groupVarFactorLevelsSorted(),
                        gVarFactorName = groupVarFactorName(),
                        MyWidth= myWidth,
                        pSizeVal= pointSizeVal(),
                        colScale= colorScale(),
                        alphVal= alphaVal(),
                        Testing= testing
                    )
                    #################################################

                    #################################################
                    # fig 6, top right plot. 
                    par(
                        mar  = c(topPlots.mar.b, col3Plots.mar.l, 
                                topPlots.mar.t, col3Plots.mar.r), #bltr
                        xaxt = "n", 
                        yaxt = "n"
                    )
                    makeFig6(
                        dat= datx.xna.counts,
                        MyYlimCounts= myYlimCounts,
                        gVarFactorLevelsSorted = groupVarFactorLevelsSorted(),
                        MyAtAdds= myAtAdds, MyJitter= myJitter,
                        colScale= colorScale(),
                        Testing= testing
                    )
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
                underPlotTextName <- paste0("underPlot_", varname)

                # Create the under-plot text for each variable, if applicable
                output[[underPlotTextName]] <- renderUI({
                    if (!psUseCompleteCasesOnly()) return(NULL)
                    req(dsetXPS())

                    nWithXButNoPS <- length(
                        dsetXPS()[is.na(get(logitpsVarName())) & !is.na(get(varname)), 
                            ][[idVarName()]]
                    )
                    if (nWithXButNoPS == 0) return(NULL)
                    
                    HTML(paste0("There are ", nWithXButNoPS, 
                        " units with values for this variable but with no propensity score.")
                    )

                }) # end renderUI

            }) # end local
        } # end for
    }) # end observe    

    observe({
        if (datInfo$newDataNoVarsChosen == FALSE) for (i in seq_along(varsToView())) {
            local({
                my_i <- i
                varname     <- varsToView()[my_i]
                naTableName <- paste0("naTable_", varname)

                # Create a missing-by-group table for each variable
                output[[naTableName]] <- renderTable({
                    if (is.null(dsetOrig())) return(NULL)
                    if (is.null(groupVarName())) return(NULL)
                    if (is.null(idsToKeepAfterPruning())) return(NULL)

                    datx <- dsetOrig()[idsToKeepAfterPruning()][, 
                        c(idVarName(), varname, groupVarName()), 
                        with= FALSE]
                    dat <- datx[, .(pct.missing = 100 * mean(is.na(get(varname)))), 
                        by= eval(groupVarName())]
                    names(dat)[names(dat) == "pct.missing"] <- "% Missing"
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
                    req(varIsContinuous())
                    req(varIsContinuous()[varname])
                    req(idsToKeepAfterPruning())

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
                                dsetOrig()[idsToKeepAfterPruning(), 
                                    eval(varname), with= FALSE]))))
                        )
                    }
                }) # end renderUI

                output[[pruner2name]] <- renderUI({
                    req(varIsContinuous())
                    req(varIsContinuous()[varname])
                    req(idsToKeepAfterPruning())

                    if (varIsContinuous()[varname]) {
                        textInput(
                            inputId= input2name, 
                            label="Max:", 
                            width= '75%',
                            value = ceiling(10^xdig() * 
                                max(unlist(dsetOrig()[idsToKeepAfterPruning(), 
                                    eval(varname), with= FALSE]), na.rm = TRUE)
                                ) / 10^xdig()

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

    
    observe({
        if (datInfo$newDataNoVarsChosen == FALSE) for (i in seq_along(varsToView())) {
            local({
                my_i <- i
                varname         <- varsToView()[my_i]

                textCheck1Name   <- paste0("textcheck1_", varname)
                input1name       <- paste0("pruningChoices1_", varname)
                input2name       <- paste0("pruningChoices2_", varname)
                
                # Check the textInput for each variable
                # TODO: break this up for the two input boxes
                output[[textCheck1Name]] <- renderUI({
                    # give the dependencies time to catch up
                    req(varIsContinuous())
                    req(varIsContinuous()[varname])
                    req(input[[input1name]])
                    req(input[[input2name]])
                    
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
        req(varsToView())
        
        plot_and_input_list <- vector("list", numvarsToView())
        for (i in seq_along(varsToView())) {
            varname        <- varsToView()[i]
            plotname       <- paste0("plot_", varname)
            underPlotname  <- paste0("underPlot_", varname)
            pruner1name    <- paste0("pruner1_", varname)
            pruner2name    <- paste0("pruner2_", varname)
            textcheck1name <- paste0("textcheck1_", varname)
            keepNAName     <- paste0("keepNA_", varname)
            naTableName    <- paste0("naTable_", varname)
            
            plot_and_input_list[[i]] <- fluidRow(
                tags$hr(),
                h4(paste0("Variable: ", varname)),
                column(width= 5, offset= 1, 
                    uiOutput(underPlotname),
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
                    uiOutput(keepNAName),
                    uiOutput(naTableName)
                ) # end column
            )# end fluidRow
        } 
        plot_and_input_list
    })
 
    ############################################################
    ############################################################
    ## SMD plots
    wantATEWts <- reactive({
        input$showATE
    })
    wantATTWts <- reactive({
        input$showATT
    })
    wantATMWts <- reactive({
        input$showATM
    })
    
    wantLinesSMD <- reactive({
        input$drawLinesSMD
    })
    
    # adapted from https://github.com/kaz-yos/tableone/blob/master/vignettes/smd.Rmd
    tabOrig <- reactive({
        req(varsToView())

        makeTableOne(dsetOrig(), 
            vars       = varsToView(),
            strata     = groupVarName(),
            factorVars = catVarsToView()
        )
    })    
    tabPruned <- reactive({
        req(varsToView())
        req(idsToKeepAfterPruning())

        makeTableOne(dsetOrig()[idsToKeepAfterPruning()], 
            vars       = varsToView(),
            strata     = groupVarName(),
            factorVars = catVarsToView()
        )
    })    
    dsetForSMDs <- reactive({
        req(varsToView())
        req(idsToKeepAfterPruning())
        req(dsetPSGraphs())
        if (input$showATE == FALSE & 
            input$showATT == FALSE &
            input$showATM == FALSE) return(NULL)

        # merge covariate data w/ weight data
        # TODO: consider making this a free-standing dataset 
        #    (outside of these functions)
        dat <- merge(
            dsetOrig()[idsToKeepAfterPruning()][, c(idVarName(), 
                varsToView(), groupVarName()), with= FALSE],
            dsetPSGraphs(),
            suffixes= c("", ".y")
        )
        setkeyv(dat, idVarName())

        dat
    })
    tabATE <- reactive({
        # we don't want req in the line below
        if(is.null(dsetForSMDs())) return(NULL)
        if (input$showATE == FALSE) return(NULL)

        makeWeightedTableOne(dsetForSMDs(), 
            wtvarname  = ateWtVarName(),
            vars       = varsToView(),
            strata     = groupVarName(),
            factorVars = catVarsToView()
        )
    })    
    tabATT <- reactive({
        # we don't want req in the line below
        if(is.null(dsetForSMDs())) return(NULL)
        if (input$showATT == FALSE) return(NULL)

        makeWeightedTableOne(dsetForSMDs(), 
            wtvarname  = attWtVarName(),
            vars       = varsToView(),
            strata     = groupVarName(),
            factorVars = catVarsToView()
        )
    })    
    tabATM <- reactive({
        # we don't want req in the line below
        if(is.null(dsetForSMDs())) return(NULL)
        if (input$showATM == FALSE) return(NULL)

        makeWeightedTableOne(dsetForSMDs(), 
            wtvarname  = atmWtVarName(),
            vars       = varsToView(),
            strata     = groupVarName(),
            factorVars = catVarsToView()
        )
    })    

    output$explainWtsText <- renderUI({
        HTML(paste0(tags$div(
            tags$p("ATE weighting is used to estimate the Average Treatment Effect on the whole (pruned) sample."),
            tags$p(HTML(paste0(
                "ATT weighting is used to estimate the Average Treatment effect on the Treated units", 
            if (!(is.null(smallestGroup()))) {
                paste0(
                    ", in this case those people with ",
                    tags$code(groupVarName()),
                    " equal to ",
                    tags$code(smallestGroup())
                )
            } else NULL,
            "."
            ))),
            tags$p(HTML(paste0(
                "ATM weighting is used to estimate the Average Treatment effect on the evenly Matchable units, using 'matching weights' as introduced in ",
                    a("Li, L., & Greene, T. (2013)", 
                        href="http://doi.org/10.1515/ijb-2012-0030", 
                        target="_blank"),
                "."
            ))) 
        )))
    })
    output$noSMDText <- renderUI({
        req(dsetOrig())
        if (datInfo$newDataNoVarsChosen) {
            HTML(paste0(tags$span(class="text-warning", 
                "To view SMD plot, first generate graphs on the 'Prune' page.")))
        } else return(NULL)
    })
    #output$tabonetest <- renderPrint({
    #    if (is.null(tabOrig())) return(NULL)

    #    print(tabATE(), smd= TRUE)
    #})

    ## Construct a data frame containing variable name and SMD from all methods
    dsetOfSMDs <- reactive({
        req(tabOrig())

        dat <- data.table(
            variable  = names(ExtractSmd(tabOrig())),
            Original  = ExtractSmd(tabOrig())
        )
        if (!is.null(tabPruned())) {
            dat[, Pruned := ExtractSmd(tabPruned())]
        }
        if (!is.null(tabATE())) {
            dat[, WeightedATE := ExtractSmd(tabATE())]
        }
        if (!is.null(tabATT())) {
            dat[, WeightedATT := ExtractSmd(tabATT())]
        }
        if (!is.null(tabATM())) {
            dat[, WeightedATM := ExtractSmd(tabATM())]
        }
        setkey(dat, variable)
        
        varNames <- as.character(dat[, variable])[order(
                dat[, Original])]

        datSorted <- dat[varNames]

        datSorted[, varnum := 1:.N]
        setkey(datSorted, varnum)
    })
    output$SMDPlot <- renderPlot({
        req(dsetOfSMDs())
        
        makeSMDPlot(dat= dsetOfSMDs(), wantLines= wantLinesSMD())
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
    
