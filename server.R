######################################################
#TODO:
# Remember: when updating on server, use library(Cairo)
#######################################################

library(shiny)
library(ggplot2)
library(gridExtra)
library(rms)
library(data.table)
#library(Cairo) # for better graphics on Linux servers
#options(shiny.usecairo= TRUE)


# number of decimal places to use w/ propensity scores


# Allow upload of bigger files
# from http://stackoverflow.com/questions/18037737/how-to-change-maximum-upload-size-exceeded-restriction-in-shiny-and-save-user
# The first number is the number of MB
options(shiny.maxRequestSize=30*1024^2)

# from http://stackoverflow.com/questions/17370460/scatterplot-with-alpha-transparent-histograms-in-r?lq=1
#theme0 <- function(...) {
#    theme( 
#        legend.position = "none",
#        panel.background = element_blank(),
#        panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(),
#        panel.margin = unit(0,"null"),
#        axis.ticks        = element_blank(),
#        axis.text.x       = element_blank(),
#        axis.text.y       = element_blank(),
#        axis.title.x      = element_blank(),
#        axis.title.y      = element_blank(),
#        axis.ticks.length = unit(0,"null"),
#        #axis.ticks.margin = unit(0,"null"),
#        axis.text.x = element_text(margin=margin(0,0,0,0,"null")),
#        axis.text.y = element_text(margin=margin(0,0,0,0,"null")),
#        panel.border=element_rect(color=NA),
#    ...)
#}


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
        if (input$useExampleData == 1 |
                (input$useExampleData == 0 & !is.null(datInfo$inFileInfo))) return(NULL)
        
        # File input from example on shiny website.
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
            N <- nt + nc
            nmiss <- 0.05 * N

            group <- rep(c("Exposed", "Unexposed"), times= c(nt, nc))
            height_ft <- c(rnorm(nt, 5.4, .3), rnorm(nc, 5.6, .2))
            height_ft[sample(N, nmiss, replace= FALSE)] <- NA
            gender <- c(rbinom(nt, 1, .66), rbinom(nc, 1, .5))
            gender[gender == 0] <- "Male"
            gender[gender == 1] <- "Female"
            gender[sample(N, nmiss, replace= FALSE)] <- NA
            age <- c(rnorm(nt, 45, 5), rnorm(nc, 50, 10))
            age[sample(N, nmiss, replace= FALSE)] <- NA
            systolic_bp <- c(rnorm(nt, 115, 5), rnorm(nc, 110, 7)) 
            systolic_bp [sample(N, nmiss, replace= FALSE)] <- NA

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
        #   (we have to make sure it does not overlap w/ current var names)
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
        #   (we have to make sure it does not overlap w/ current var names)
        if(is.null(dset.orig())) return (NULL)
        if (input$useExampleData == 0 & is.null(datInfo$inFileInfo)) return(NULL)
        
        proposedName <- "MY__LOGITPS"
        while(proposedName %in% names(dset.orig())) {
            proposedName <- paste0(proposedName, "_NEW")    
        }    
        proposedName
    })    

    nonMissingIDs <- reactive({
        # These are the IDs of people who can be used for PS calculation

        if (is.null(varnamesFromRHS())) return(dset.orig()[[idvarName()]])
        
        if (input$completeCasesOnly == 1) {
            na.omit(dset.orig()[, c(varnamesFromRHS(), idvarName()), 
                with= FALSE])[[idvarName()]]
        } else {
            dset.orig()[[idvarName()]]
        }
    })
    
    observe({
        # Add a factor version of the treatment indicator, 
        #    for plotting
        if (!is.null(groupvarFactorName())){
            # I think this next line is necessary in case of dset switch. But it's possible it could be taken out.
            if (groupvarname() %in% names(dset.orig())) {
                if (!is.factor(dset.orig()[[groupvarname()]])) {
                    dset.orig()[, groupvarFactorName() := 
                        factor(dset.orig()[[groupvarname()]])]
                }
            }
        }
    })
    groupvarFactorLevelsSorted <- reactive({
        # for use in graphs
        if (is.null(groupvarFactorName())) return(NULL)
        tmp <- table(dset.orig()[[groupvarFactorName()]])
        names(tmp)[order(tmp, decreasing= TRUE)]
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
            id    = unlist(dset.orig()[nonMissingIDs(), 
                idvarName(), with= FALSE]),
            group = unlist(dset.orig()[nonMissingIDs(), 
                groupvarFactorName(), with= FALSE])
        )
        dat2 <- data.frame(
            id       = PSIDs(),
            logit.ps = logitPS(), 
            ps       = PS()
        )
        # keep only subjects with values in both dsets
        dat <- merge(dat1, dat2, by= "id")  
        names(dat)[names(dat) == "id"] <- idvarName()
        names(dat)[names(dat) == "ps"] <- psvarName()
        names(dat)[names(dat) == "logit.ps"] <- logitpsvarName()
        names(dat)[names(dat) == "group"] <- groupvarFactorName()
        
        dat <- as.data.table(dat)  
        setkeyv(dat, idvarName())
        dat
    })    
    
    # TODO: can I get rid of this and just use idsToKeepAfterPruning()?
    xgraphs.ids <- reactive({
        # IDs for making the covariate plots.
        #if (is.null(nonMissingIDs())) return(NULL)
        if (is.null(idsToKeepAfterPruning())) return(dset.orig()[[idvarName()]])

        #intersect(nonMissingIDs(), idsToKeepAfterPruning())
        idsToKeepAfterPruning()
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
            #'Which variable is the treatment indicator?', #'
            label= NULL, 
            choices= c(
                #"Choose one" = "", 
                varnames.orig()[sapply(dset.orig(), 
                    function(vec) length(unique(vec)) == 2)]
                ), 
            selected= NULL,
            multiple= FALSE
            )
    })
    groupvarname <- reactive({
        if (input$useExampleData == 0 & 
            is.null(datInfo$inFileInfo)) return(NULL)
        
        input$treatmentVarName
    })
    groupvarFactorName <- reactive({
        # The name produced by this function will be used as
        #     the name of the treatment group var 
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

        # the as.character lets this print right if var 
        #    is already a factor
        dat <- data.frame(as.character(sort(unique(
            dset.orig()[[groupvarname()]])))) 
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


    ############################################################
    ############################################################
    ## Propensity score calculation
    output$getFormula <- renderUI({
        if (is.null(dset.orig()) | 
            is.null(groupvarname())) return(NULL)

        textInput('formulaRHS', 
            label= paste0(groupvarname(), ' ~ '), 
            value= ' ',
            width= '100%'
        )
    })

    formRHS <- reactive({
        # Dependencies
        if (input$psTypedButton == 0 | 
            is.null(dset.orig())) return(NULL) 
        
        isolate(input$formulaRHS)
    })
    stringFormula <- reactive({
        paste0(groupvarname(), ' ~ ', formRHS())
    })    
    psForm <- reactive({
        tryCatch(as.formula(stringFormula()),
            error= function(e) {return(NULL)})
    })    
    psNotChecked <- reactive({
        if (input$psTypedButton == 0 | paste0(groupvarname(), ' ~ ', 
            input$formulaRHS) != stringFormula()) TRUE else FALSE
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
    
    output$psFormulaProblemText <- renderUI({
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
        # this next one should be covered by nonMissingIDs,
        #    but just in case:
        input$completeCasesOnly 
            
        intersect(nonMissingIDs(), isolate(idsToKeepAfterPruning()))
    })

    varnamesFromRHS <- reactive({
        if (is.null(psForm())) return(NULL)
        
        allvars <- setdiff(all.vars(psForm()), groupvarname())
        if (all(allvars %in% names(dset.orig()))) allvars else NULL
    })    
    varnamesFromRHSOK <- reactive({
        if (psNotChecked()) return(NULL)
        if (is.null(psFormSyntaxOK())) return(NULL)
        
        if (is.null(isolate(varnamesFromRHS()))) {
            FALSE
        } else {
            TRUE
        }
    })
    output$psVarsProblemText <- renderUI({
        if (psNotChecked() | is.null(psFormSyntaxOK())) {
            HTML(paste0(tags$span(style="color:orange", "Not checked yet.")))
        } else if (!psFormSyntaxOK()) {
            HTML(paste0(tags$span(style="color:orange", "Not checked yet.")))
        } else if (!varnamesFromRHSOK()) {
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
        if (is.null(varnamesFromRHS())) return(NULL)
        if (input$completeCasesOnly == 1) return(NULL)
        
        myvars <- c(varnamesFromRHS(), groupvarname())
        dat <- copy(dset.orig()[PSIDs(), myvars, with= FALSE])
        # have to convert character vars to factors before imputing
        for (varname in varnamesFromRHS()) {
            if (is.character(dat[[varname]])) {
                dat[, eval(varname) := factor(dat[[varname]])]
            }
        }
        
        dat[, (myvars) := lapply(.SD, function(x) impute(x, fun= mean)),
            .SDcols = myvars]
        dat
    })
    
    lrmfit <- reactive({
        if (is.null(psForm()) | 
            is.null(varnamesFromRHS())) return(NULL)
        
        if (input$completeCasesOnly == 1) {
            tryCatch({lrm(psForm(), 
                data  = dset.orig()[PSIDs()])},
                error = function(e) return(NULL))
        } else { # use imputed data
            tryCatch({lrm(psForm(), 
                data  = dset.imputed())},
                error = function(e) return(NULL))
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
        
        if (psNotChecked() | is.null(varnamesFromRHSOK())) {
            HTML(paste0(tags$span(style="color:orange", "Not checked yet.")))
        } else if (!varnamesFromRHSOK()) { # couldn't combine this with above
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
    psFitProblemPostPruning <- reactive({
        # dependencies
        if (psNotChecked() |  
            input$PSCalcUpdateButton  == 0) return (FALSE)

        if (is.null(isolate(lrmfit()))) {
            TRUE
        } else {
            FALSE
        }    
    })
    output$psFitProblemTextPostPruning <- renderText({
        if (psFitProblemPostPruning()) {
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
    
    output$dataNonmissingDimText1  <- renderUI({
        if (is.null(nonMissingIDs()) | 
            input$completeCasesOnly == 0) return(NULL)
        if (psNotChecked() | 
            is.null(varnamesFromRHSOK())) return(NULL)

        HTML(paste0(tags$hr()))
    })
    output$dataNonmissingDimText2  <- renderUI({
        if (is.null(nonMissingIDs()) | 
            input$completeCasesOnly == 0) return(NULL)
        if (psNotChecked() | 
            is.null(varnamesFromRHSOK())) return(NULL)

        HTML(paste0(tags$h4("N after excluding rows:")))
    })
    output$dataNonmissingDimText3 <- renderText({
        if (is.null(nonMissingIDs()) | 
            input$completeCasesOnly == 0) return(NULL)
        if (psNotChecked() | 
            is.null(varnamesFromRHSOK())) return(NULL)

        paste0("After removal of rows with missing values for the variables selected for the PS model, ",
            "the dataset has ", length(nonMissingIDs()), " rows.")
    })
    
    ############################################################
    ############################################################
    ## Reactive text, etc. for PS graphs
    
    # TODO: not currently using these
    psbrushmin <- reactive({
        if (is.null(dset.psgraphs()) ) return(NULL)
        #if (useLogit()) input$logitpsPlot_brush$xmin else input$psPlot_brush$xmin
        NULL
    })
    psbrushmax <- reactive({
        if (is.null(dset.psgraphs()) ) return(NULL)
        #if (useLogit()) input$logitpsPlot_brush$xmax else input$psPlot_brush$xmax
        NULL
    })
    hasScoreOutside <- reactive({
        if (is.null(dset.psgraphs())) return(NULL)

        if (is.null(psbrushmin())) return(rep(FALSE, dset.psgraphs()[, .N]))

        psdig <- 2
        round(dset.psgraphs()[[logitpsvarName()]], psdig) < psbrushmin() |
            round(dset.psgraphs()[[logitpsvarName()]], psdig) > psbrushmax()
    })
    idsForRug <- reactive({
        if (is.null(hasScoreOutside())) return(NULL)
        
        ids <- dset.psgraphs()[hasScoreOutside(), ][[idvarName()]]
        
        intersect(ids, xgraphs.ids())
    })  

    # todo: is there a way to do this w/o making a third dataset?
    # TODO: might not need this anyway
    dset.psgraphs.plus <- reactive({
        if (is.null(hasScoreOutside())) return(NULL)

        dat <- copy(dset.psgraphs())

        dat[, outside := hasScoreOutside()]
        dat
    })
    

    ############################################################
    ############################################################
    ## Reactive text related to covariate graphs
        
    possVarsToRestrict <- reactive({
        if (is.null(groupvarname())) return(NULL)

        # We don't want to allow restriction of the treatment var 
        setdiff(varnames.orig(), 
            c(groupvarname(), groupvarFactorName(), idvarName()))  
    })    
    output$chooseVarsToRestrict <- renderUI({
        if (is.null(possVarsToRestrict())) return(NULL)

        selectizeInput('varsToRestrict', 
            NULL, 
            choices= possVarsToRestrict(), 
            selected= if (is.null(varnamesFromRHS())) NULL else 
                varnamesFromRHS(),
            multiple= TRUE,
            width= '100%'
        )
    })

    varsToView <- reactive({
        #dependency
        input$generalGraphUpdateButton

        # to cover switching between datasets
        intersect(isolate(input$varsToRestrict), names(dset.orig()))
    })
    numvarsToView <- reactive({
        length(varsToView())    
    })    
    output$needPSText <- renderUI({
        if (is.null(dset.psgraphs()) & !psFitProblemPostPruning()) {
            HTML(paste0(
                tags$span(style="color:red", "To see graphs, "),
                tags$span(style="color:red", "specify a propensity score model"),
                tags$br(),
                tags$span(style="color:red", "on the 'Specify' tab page.") ))
        } else return(NULL)
    })

    varIsContinuous <- reactive({
        #dependencies
        input$generalGraphUpdateButton
        if (is.null(varsToView())) return(NULL)
        
        vnames <- varsToView()
        myvec <- rep(FALSE, length(vnames))
        
        for(i in seq_along(vnames)) {
            varname <- vnames[i]
            
            if (is.numeric(dset.orig()[[varname]]) & 
                length(unique(dset.orig()[[varname]])) >= 
                isolate(input$numCont)) myvec[i] <- TRUE
        }
        names(myvec) <- vnames
        myvec
    })    

    alphaval <- reactive({
        #dependency
        input$generalGraphUpdateButton

        isolate(input$alphaSlider)
    })
    pointsizeval <- reactive({
        #dependency
        input$generalGraphUpdateButton

        isolate(input$pointsizeSlider)
    })
    # number of decimal places to use w/ covariate graphs
    # TODO: get rid of this if not using brushing on PS
    xdig <- reactive({
        #input$xDigits
        2
    })

    # from http://stackoverflow.com/questions/18816666/shiny-change-data-input-of-buttons
    # Create a reactiveValues object, to let us use settable reactive values
    # We'll use this later on
    buttonvalues <- reactiveValues()
    # To start out, lastActionX == NULL, meaning nothing clicked yet
    buttonvalues$lastActionX <- NULL
    # An observe block for each button, to record that the action happened
    observe({
        if (input$xgraphsUpdateButton != 0 | 
            input$PSCalcUpdateButton != 0 | 
            input$xgraphsUpdateButton != 0) {
            buttonvalues$lastActionX <- 'pruneOrPlot'
        }
    })
    observe({
        if (input$psTypedButton != 0 | input$completeCasesOnly %in% c(0,1)) {
            buttonvalues$lastActionX <- 'specify'
        }
    })


    # Create the expression to use for pruning
    pruneValRawList <- reactive({
        if (is.null(varsToView())) return(NULL)

        mylist <- vector("list", numvarsToView())

        for (i in 1:numvarsToView()) {
            # doing it this way to fit w/ old code---
            #   could redo later
            mylist[[i]]  <- 
                input[[paste0("pruningChoices1_", i)]]
            if (varIsContinuous()[varsToView()[i]]) {
                mylist[[i]]  <- paste(mylist[[i]],
                    input[[paste0("pruningChoices2_", i)]],
                    collapse= " ")
            } 
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

        if (input$xgraphsUpdateButton == 0 & 
            input$PSCalcUpdateButton == 0) {
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
            ) # end of paste0
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
        if (is.null(exprToKeepAfterPruning())) return(dset.orig()[[idvarName()]])
        
        dset.orig()[eval(parse(text= 
            exprToKeepAfterPruning()))][[idvarName()]]
    })    
    
    output$pruneTable <- renderTable({
        if (is.null(nonMissingIDs())) return(NULL)
    
        dat <- dset.orig()[idsToKeepAfterPruning(), .N, by= eval(groupvarFactorName())]
        setnames(dat, old = groupvarFactorName(), new = groupvarname())
        rbind(dat, list("Total", dset.orig()[idsToKeepAfterPruning(), .N]))
    }, include.rownames = FALSE)

    ############################################################
    ############################################################
    ## Plotting 
    
    colorScale.mod <- reactive({
        if (is.null(nonMissingIDs())) return(NULL)

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
        sc <- myColorScale[1:length(unique(dset.orig()[[groupvarFactorName()]]))]
        names(sc) <- levels(dset.orig()[[groupvarFactorName()]])
        sc
    })
    
    #############################################################
    output$psPlot <- renderPlot({
        if (is.null(dset.psgraphs())) return(NULL)
        
        histlist <- vector("list", length(groupvarFactorLevelsSorted()))
        names(histlist) <- groupvarFactorLevelsSorted() 
        for (lev in groupvarFactorLevelsSorted()) {
            x <- dset.psgraphs()[get(groupvarFactorName()) == lev, get(psvarName())]
            if (length(x) > 0) {
                histlist[[lev]] <- hist(x, plot= FALSE, breaks= 30)
            } else {
                histlist[[lev]] <- NULL
            }
        }
        histcounts <- do.call(c, lapply(histlist, function(hl) hl$counts))
        plot(dset.psgraphs()[[psvarName()]], runif(dset.psgraphs()[, .N]), 
            ylim= c(0, max(histcounts)), 
            xlab= "PS",
            ylab= "Count",
            bty= "n",
            type= "n",
            cex.lab= 1,
            cex.axis= 0.8
        )
        # modified from http://www.r-bloggers.com/overlapping-histogram-in-r/
        for (lev in groupvarFactorLevelsSorted()) {
            if (!is.null(histlist[[lev]])) {
                plot(histlist[[lev]], 
                    col= adjustcolor(colorScale.mod()[lev], alpha.f= alphaval()), 
                    freq   = TRUE, 
                    border = NA,
                    add    = TRUE
                )
            }
        }
    }, res= 100)    
    
    
    output$logitpsPlot <- renderPlot({
        if (is.null(dset.psgraphs())) return(NULL)

        histlist <- vector("list", length(groupvarFactorLevelsSorted()))
        names(histlist) <- groupvarFactorLevelsSorted() 
        for (lev in groupvarFactorLevelsSorted()) {
            x <- dset.psgraphs()[get(groupvarFactorName()) == lev, get(logitpsvarName())]
            if (length(x) > 0) {
                histlist[[lev]] <- hist(x, plot= FALSE, breaks= 30)
            } else {
                histlist[[lev]] <- NULL
            }
        }
        histcounts <- do.call(c, lapply(histlist, function(hl) hl$counts))
        plot(dset.psgraphs()[[logitpsvarName()]], runif(dset.psgraphs()[, .N]), 
            ylim= c(0, max(histcounts)), 
            xlab= "Logit PS",
            ylab= "Count",
            bty= "n",
            type= "n",
            cex.lab= 1,
            cex.axis= 0.8
        )
        # modified from http://www.r-bloggers.com/overlapping-histogram-in-r/
        for (lev in groupvarFactorLevelsSorted()) {
            if (!is.null(histlist[[lev]])) {
                plot(histlist[[lev]], 
                    col= adjustcolor(colorScale.mod()[lev], alpha.f= alphaval()), 
                    freq   = TRUE, 
                    border = NA,
                    add    = TRUE
                )
            }
        }
        legend(
            "topleft",
            inset= .05,
            cex = .8,
            title= NULL,
            groupvarFactorLevelsSorted(),
            horiz = FALSE,
            bty= "n",
            border= NA,
            fill = do.call(c, lapply(groupvarFactorLevelsSorted(), function(x)
                adjustcolor(colorScale.mod()[x], alpha.f= alphaval())))
        )
    }, res= 100)    
    output$logitpsPlot2 <- renderPlot({
        # exact duplicate of the other one; can't call the same plot twice in the UI 
        if (is.null(dset.psgraphs())) return(NULL)

        histlist <- vector("list", length(groupvarFactorLevelsSorted()))
        names(histlist) <- groupvarFactorLevelsSorted() 
        for (lev in groupvarFactorLevelsSorted()) {
            x <- dset.psgraphs()[get(groupvarFactorName()) == lev, get(logitpsvarName())]
            if (length(x) > 0) {
                histlist[[lev]] <- hist(x, plot= FALSE, breaks= 30)
            } else {
                histlist[[lev]] <- NULL
            }
        }
        histcounts <- do.call(c, lapply(histlist, function(hl) hl$counts))
        plot(dset.psgraphs()[[logitpsvarName()]], runif(dset.psgraphs()[, .N]), 
            ylim= c(0, max(histcounts)), 
            xlab= "Logit PS",
            ylab= "Count",
            bty= "n",
            type= "n",
            cex.lab= 1,
            cex.axis= 0.8
        )
        # modified from http://www.r-bloggers.com/overlapping-histogram-in-r/
        for (lev in groupvarFactorLevelsSorted()) {
            if (!is.null(histlist[[lev]])) {
                plot(histlist[[lev]], 
                    col= adjustcolor(colorScale.mod()[lev], alpha.f= alphaval()), 
                    freq   = TRUE, 
                    border = NA,
                    add    = TRUE
                )
            }
        }
        legend(
            "topleft",
            inset= .05,
            cex = .8,
            title= NULL,
            groupvarFactorLevelsSorted(),
            horiz = FALSE,
            bty= "n",
            border= NA,
            fill = do.call(c, lapply(groupvarFactorLevelsSorted(), function(x)
                adjustcolor(colorScale.mod()[x], alpha.f= alphaval())))
        )
    }, res= 100)    
    
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
                pruner1name      <- paste0("pruner1_", my_i)
                pruner2name      <- paste0("pruner2_", my_i)
                input1name       <- paste0("pruningChoices1_", my_i)
                input2name       <- paste0("pruningChoices2_", my_i)
                textCheck1Name   <- paste0("textcheck1_", my_i)
                textCheck2Name   <- paste0("textcheck2_", my_i)
                keepNAName      <- paste0("keepNA", my_i)
                keepNAInputName <- paste0("keepNAInput", my_i)
                naTableName     <- paste0("naTable", my_i)
     
                testing <- FALSE
                output[[plotname]] <- renderPlot({
                    if (is.null(dset.psgraphs())) return(NULL)

                    # core dataset
                    datx <- dset.orig()[idsToKeepAfterPruning()][, 
                        c(idvarName(), groupvarFactorName(), varname), 
                        with= FALSE]

                    # convert character & discrete numeric to factor
                    if (is.character(datx[[varname]]) | 
                            (is.numeric(datx[[varname]]) & 
                            !(varIsContinuous()[varname]))) {
                        datx[, eval(varname) := 
                            factor(datx[[varname]])]
                    }

                    # use datx.nona for marginal histogram/barchart
                    datx.nona <- na.omit(datx)
                    my.xlim <- if(varIsContinuous()[varname]) {
                        range(datx.nona[[varname]])
                    } else NA

                    datxps <- datx[dset.psgraphs.plus()]
                    my.ylim <- range(datxps[[logitpsvarName()]])

                    # use datxps.nona for central scatterplot/stripchart
                    datxps.nona <- na.omit(datxps)
                    # preserve any levels that might have been lost
                    if(is.factor(datx[[varname]])) {
                        my.levels <- levels(datx[[varname]])
                        datxps.nona[, eval(varname) := factor(get(varname),
                            levels= my.levels)]
                    }

                    # adapted from http://www.r-bloggers.com/example-10-3-enhanced-scatterplot-with-marginal-histograms/

                    # save the old graphics settings-- they may be needed
                    def.par <- par(no.readonly = TRUE)

                    # the matrix shows the layout of the plots:
                    zones <- matrix(c(
                        0, 4, 0, 
                        1, 5, 3, 
                        0, 2, 0), 
                        ncol = 3, byrow = TRUE)
                    layout(zones, 
                        respect= TRUE,
                        #widths  = c(0.45, 4, 0.6),
                        #heights = c(3, 10, 1)
                        widths  = c(0.4, 4, 0.6),
                        heights = c(3/5, 10.5/5, 1.5/5)
                    )
                    bottomMargin <- if (varIsContinuous()[varname]) 2 else {
                        max(min(max(nchar(levels(datx[[varname]])))[1] / 2, 8), 2)
                    }

                    # for all plots: 
                    #   drop the axis titles and omit boxes, set up margins
                    par(xaxt="n", 
                        yaxt="n", 
                        ann= FALSE,
                        bty= if (testing) "o" else "n",
                        oma= c(0,0,0,0),
                        cex.axis= 1.1
                        ) 

                    # fig 1 = Y axis label. 
                    #par(mar = c(bottomMargin - 1.7, 2, .3, 0) +.05) # b, l, t, r
                    par(mar = c(bottomMargin - 1.7, 0.7, .3, 0) +.05) # b, l, t, r
                    plot(x= 1, y= 1, type= "n", ylim= c(-1, 1), xlim= c(-1, 1))
                    text(0, 0, paste("Logit PS"), cex= 1.4, srt= 90)
                    if (testing) box("outer", col= "blue")
                    if (testing) box("figure", col= "green")
                    
                    # fig 2 = X axis label for main plot. 
                    #par(mar = c(.3, 2, .3, 0) +.05) # b, l, t, r
                    par(mar = c(0, 2, .3, 0) +.05) # b, l, t, r
                    plot(x= 1, y= 1, type="n", ylim= c(-1, 1), xlim= c(-1, 1))
                    text(0, 0, paste(varname), cex=1.4)
                    if (testing) box("figure", col= "green")

                    ###############################################################
                    # fig 3, right-side plot, needs different margins. 
                    # no margin on the left
                    par(mar = c(bottomMargin, 0.3, 0.65, 0.7))
                    par(xaxt="s")

                    datxps.xna <- datxps[is.na(get(varname)), ]
                    datxps.xna[, randx := runif(datxps.xna[, .N])]

                    #plot(xna.logitps[, randx], xna.logitps[[logitpsvarName()]], 
                    plot(datxps.xna[, randx], datxps.xna[[logitpsvarName()]], 
                        xlim= 0:1, 
                        ylim= my.ylim, 
                        type= "n", axes= FALSE)

                    #for (lev in levels(xna.logitps[[groupvarFactorName()]])) {}
                    # TODO: keep the above line for ref. May need intersect()
                    for (lev in groupvarFactorLevelsSorted()) {
                        x <- datxps.xna[get(groupvarFactorName()) == lev, randx]
                        y <- datxps.xna[get(groupvarFactorName()) == lev, get(logitpsvarName())]
                        points(x, y, 
                            pch= 0,
                            cex= pointsizeval(),
                            col= adjustcolor(colorScale.mod()[lev], alpha.f= alphaval()))
                    }
                    axis(1, at= 0.5, labels= "Missing")
                    if (testing) box("figure", col= "green")
                    ###############################################################


                    ###############################################################
                    # fig 4, top plot. needs no margin on the bottom. 
                    par(mar = c(0, 2, 1, .65))
                    par(xaxt="n")
                    if (varIsContinuous()[varname]) {    
                        # first make all histograms but do not plot,
                        #    in order to get ylim
                        histlist <- vector("list", length(groupvarFactorLevelsSorted()))
                        names(histlist) <- groupvarFactorLevelsSorted() 
                        for (lev in groupvarFactorLevelsSorted()) {
                            x <- datx.nona[get(groupvarFactorName()) == lev, get(varname)]
                            if (length(x) > 0) {
                                histlist[[lev]] <- hist(x, plot= FALSE)
                            } else {
                                histlist[[lev]] <- NULL
                            }
                        }
                        histcounts <- do.call(c, lapply(histlist, function(hl) hl$counts))
                        plot(datx.nona[[varname]], runif(datx.nona[, .N]), 
                            ylim= c(0, max(histcounts)), 
                            type= "n"
                        )
                        # modified from http://www.r-bloggers.com/overlapping-histogram-in-r/
                        for (lev in groupvarFactorLevelsSorted()) {
                            if (!is.null(histlist[[lev]])) {
                                plot(histlist[[lev]], 
                                    col= adjustcolor(colorScale.mod()[lev], alpha.f= alphaval()), 
                                    freq   = TRUE, 
                                    border = NA,
                                    add    = TRUE
                                )
                            }
                        }
                    } else { # discrete
                        # first: we will use this x-axis below also
                        my.at.orig <- seq_along(levels(datx.nona[[varname]]))
                        names(my.at.orig) <- levels(datx.nona[[varname]])
                        num.levs <- length(groupvarFactorLevelsSorted())
                        my.jitter <- min(0.1, 1 / length(my.at.orig))
                        my.at.adds <- 2 * my.jitter * (1:num.levs)
                        #shift so centered at 0
                        my.at.adds <- my.at.adds - mean(my.at.adds)
                        names(my.at.adds) <- groupvarFactorLevelsSorted()
                        
                        # https://flowingdata.com/2016/03/22/comparing-ggplot2-and-r-base-graphics/
                        xtbl <- table(
                            datx.nona[[groupvarFactorName()]],
                            datx.nona[[varname]] 
                        )
                        
                        plot(1, 0,
                            xlim= c(min(my.at.orig) - 1, max(my.at.orig) + 1), 
                            ylim= c(0, max(xtbl)), 
                            type= "n")

                        for (grouplev in groupvarFactorLevelsSorted()) {
                            for (varlev in levels(datx.nona[[varname]])) {
                                rect(
                                    xleft= my.at.orig[varlev] + my.at.adds[grouplev] - my.jitter,
                                    ybottom= 0,
                                    xright= my.at.orig[varlev] + my.at.adds[grouplev] + my.jitter,
                                    ytop= xtbl[grouplev, varlev],
                                    density= NA,
                                    border= NA,
                                    col= adjustcolor(colorScale.mod()[grouplev], 
                                        alpha.f= alphaval())
                                )
                            }
                        }
                    }
                    if (testing) box("figure", col= "green")
                    ###############################################################


                    ###############################################################
                    # fig 5, finally, the main plot-- needs regular axes,
                    #  different margins

                    par(mar = c(bottomMargin, 2, .5, .5), 
                        xaxt="s", yaxt="s", bty="n")
                    if (varIsContinuous()[varname]) {    
                        plot(datxps.nona[[varname]], datxps.nona[[logitpsvarName()]], 
                            xlim= my.xlim, 
                            type= "n"
                        )
                        for (lev in groupvarFactorLevelsSorted()) {
                            x <- datxps.nona[get(groupvarFactorName()) == lev, get(varname)]
                            y <- datxps.nona[get(groupvarFactorName()) == lev, 
                                get(logitpsvarName())]
                            points(x, y,
                                pch= 20,
                                cex= pointsizeval(),
                                col= adjustcolor(colorScale.mod()[lev], 
                                    alpha.f= alphaval()))
                        }
                    } else {
                        par(las= 2) #TODO: try to angle instead

                        plot(1,0,
                            xlim= c(min(my.at.orig) - 1, max(my.at.orig) + 1), 
                            ylim= my.ylim, 
                            axes= FALSE,
                            type= "n")
                        for (lev in groupvarFactorLevelsSorted()) {
                            x <- datxps.nona[get(groupvarFactorName()) == lev, get(varname)]
                            y <- datxps.nona[get(groupvarFactorName()) == lev, 
                                get(logitpsvarName())]
                            stripchart(y ~ x,
                                vertical= TRUE,
                                add= TRUE,
                                method= "jitter",
                                jitter= my.jitter,
                                pch= 20,
                                at = my.at.orig + my.at.adds[lev],
                                cex= pointsizeval(),
                                col= adjustcolor(colorScale.mod()[lev], 
                                    alpha.f= alphaval())
                            )
                        }
                        axis(1, at= my.at.orig, labels= names(my.at.orig))
                        axis(2)
                    }
                    if (testing) box("figure", col= "green")
                    ###############################################################

                    # reset the graphics
                    par(def.par)
                }, res= 100#, height= 300, width = 500 
                ) # end renderPlot

                # Create input functions for each variable
                output[[pruner1name]] <- renderUI({
                    if (varIsContinuous()[varname]) {
                        #textInputRow(
                        textInput(
                            inputId= input1name, 
                            label="Min:", 
                            width= '75%',
                            value = floor(10^xdig() *   
                                min(unlist(dset.orig()[nonMissingIDs(), eval(varname), with= FALSE]), na.rm = TRUE)) / 10^xdig()

                        )
                        
                    } else { # we have categorical var, either factor or char
                        checkboxGroupInput(
                            input1name, 
                            NULL,
                            # as.character corrects the printing of factor levels
                            choices=  as.character(sort(unique(unlist(dset.orig()[nonMissingIDs(), eval(varname), with= FALSE])))),
                            selected= as.character(sort(unique(unlist(dset.orig()[nonMissingIDs(), eval(varname), with= FALSE]))))
                        )
                    }
                }) # end renderUI

                output[[pruner2name]] <- renderUI({
                    if (varIsContinuous()[varname]) {
                        textInput(
                            inputId= input2name, 
                            label="Max:", 
                            width= '75%',
                            value = ceiling(10^xdig() * 
                                max(unlist(dset.orig()[nonMissingIDs(), eval(varname), with= FALSE]), na.rm = TRUE)) / 10^xdig()

                        )
                    } else { # we have categorical var, either factor or char
                        NULL
                    }
                }) # end renderUI

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
                    dat <- dset.orig()[idsToKeepAfterPruning(), 
                        .(pct.missing = 100 * mean(is.na(get(varname)))), 
                        by= eval(groupvarFactorName())]
                    setnames(dat, old = groupvarFactorName(), new = groupvarname())
                    dat
                }, digits= 1, include.rownames= FALSE) # end renderTable
                
            }) # end local
        } # end for
    }) # end observe    
    
    # Keeping the observe for the textcheck separate
    observe({
        for (i in 1:numvarsToView()) {
            local({
                my_i <- i
                varname         <- varsToView()[my_i]

                textCheck1Name   <- paste0("textcheck1_", my_i)
                #textCheck2Name   <- paste0("textcheck2_", my_i)
                
                # Check the textInput for each variable
                # TODO: break this up for the two input boxes
                output[[textCheck1Name]] <- renderUI({
                    if (is.null(pruneValTextList())) return(NULL)
                    
                    if (varIsContinuous()[varname]) {
                        #if (grepl("TRUE", pruneValTextList()[[my_i]], fixed= TRUE)) {
                        if (is.na(as.numeric(input[[paste0("pruningChoices1_", my_i)]])) | 
                            is.na(as.numeric(input[[paste0("pruningChoices2_", my_i)]]))) {    
                            HTML(paste0(tags$span(style="color:red", "Please make sure both boxes contain numbers.")))
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
    output$univariatePlotsAndInputs <- renderUI({
        if (input$generalGraphUpdateButton == 0) return(NULL)
        if (is.null(varsToView())) return(NULL)
        
        plot_and_input_list <- vector("list", numvarsToView())
        for(i in 1:numvarsToView()) {
            varname       <- varsToView()[i]
            plotname      <- paste0("plot", i)
            pruner1name    <- paste0("pruner1_", i)
            pruner2name    <- paste0("pruner2_", i)
            textcheck1name <- paste0("textcheck1_", i)
            keepNAName    <- paste0("keepNA", i)
            naTableName   <- paste0("naTable", i)
            
            plot_and_input_list[[i]] <-
                fluidRow(
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
                        tags$hr(),
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
    
