# functions related to PS estimation


getPSFit <- function(dat, Method, form) {
    # todo: keep in mind:
    #   advantage of using glm instead: don't have to use
    #       is.na_ prefix (can use is.na()).
    #   disadvantage: need binary exposure indicator
    #glm(psForm(), data = dsetImputed(), family= 'binomial')
    if (Method == "logistic") {
        fit <- lrm(form, data= dat)
    } else if (Method == "probit") {
        fit <- Glm(form, family= binomial(link= "probit"), data= dat)
    } else {
        fit <- NULL    
    }
    return(fit)
}

getPS <- function(fit, Method) {
    if (Method == "logistic") {
        plogis(fit$linear.predictors)
    } else if (Method == "probit") {
        pnorm(fit$linear.predictors)
        #fit$fitted.values
    } else {
        NULL    
    }
}