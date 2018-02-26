# functions related to PS estimation


getPSCall <- function(Method, form) {
    # todo: keep in mind:
    #   advantage of using glm instead: don't have to use
    #       is.na_ prefix (can use is.na()).
    #   disadvantage: need binary exposure indicator
    #glm(psForm(), data = dsetImputed(), family= 'binomial')
    if (Method == "logistic") {
        mycall <- call("lrm", form, data= quote(dat))
    } else if (Method == "probit") {
        mycall <- call("Glm", form, data= quote(dat), 
            family= quote(binomial(link= "probit")))
    } else {
        mycall <- NULL    
    }
    return(mycall)
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

showPSSummary <- function(fit, Method) {
    if (Method == "logistic") {
        print(fit) 
    } else if (Method == "probit") {
        print(fit)
        #print(summary.glm(fit))
    } else {
        NULL    
    }
}
