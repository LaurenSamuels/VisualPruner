# functions for calculating SMDs
makeTableOne <- function(dat, vars, strata, factorVars) {
     CreateTableOne(
        vars       = vars,
        strata     = strata,
        data       = dat,
        factorVars = factorVars,
        includeNA  = FALSE,
        test       = FALSE,
        smd        = TRUE
    )   
}

makeWeightedTableOne <- function(dat, wtvarname, vars, strata, factorVars) {
    # Create a survey object
    svydat <- svydesign(ids= ~ 0, data= dat, 
        weights= ~ get(wtvarname))

    # Create a TableOne object
    svyCreateTableOne(
        vars       = vars,
        strata     = strata,
        data       = svydat,
        factorVars = factorVars,
        includeNA  = FALSE,
        test       = FALSE,
        smd        = TRUE
    )
}

