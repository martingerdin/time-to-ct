#' create plot data
#'
#' creates plot data
#' @param model the model object, no default
#' @param bs_models list of bootstrapped model objects, defaults to NULL
#' @param or logical, if true exp(lp) is returned
#' @param evar explanatory variable, i.e. the independent variable in which association with the outcome you're interested in, no default
#' @param ci logical, if TRUE the bs_models are used to estimate a confidence interval, defaults to TRUE
#' @param ci_probs the probabilities what will be used to estimate to lower and upper bound of the confidence interval, ignored if ci is FALSE, defaults to c(0.025, 0.975)
#' @export
create.plot.data <- function(model,
                             bs_models = NULL,
                             evar,
                             or = FALSE,
                             ci = TRUE,
                             ci_probs = c(0.025, 0.975)
                             )
{
    md <- model # assign model to md
    bm <- bs_models # assign bs_models to bm
    d <- md$model # get model data
    ex <- d[, grep(evar, colnames(d))] # get data frame with only explanatory variable data
    ms <- c(list(md), bs_models) # merge original model with bootstrapped models
    ## get coefficients from all models
    cs <- lapply(ms, function(m) {
        cm <- m$coefficients # extract model coefficients
        cs <- cm[names(cm) %in% colnames(ex)] # keep only those for the explanatory variables
        return(cs)
    })
    o <- cs[[1]] # get original model coefficients
    cs[[1]] <- NULL # set that list element to NULL
    csd <- do.call(rbind, cs) # make coefficient list a data frame
    ## do this only if a confidence interval should be estimated
    if (ci) { 
        diff <- data.frame(o - csd) # calculate difference between original coefficients and all bootstrapped coefficients
        qs <- lapply(diff, quantile, probs = ci_probs) # estimate quantiles based on the ci probabilities
        b <- list() # empty list to hold bounts
        for (i in seq_along(o)) b[[names(o[i])]] <- o[i] - qs[[i]] # calculate bounds
        csc <- csd # make copy of csd
        ## keep only those models which coefficients are within the ci of the original models coefficients
        for (i in seq_along(b)) {
            v <- names(b[i]) # get variable name
            lb <- min(b[[i]]) # extract lower bound
            ub <- max(b[[i]]) # upper bound
            csc <- csc[csc[, v] > lb & csc[, v] < ub, ] # remove models not within this interval
        }
        csd <- csc # replace original data with this modified copy
    }
    csd <- rbind(o, csd) # attach original coefficent to coefficient data
    ## get linear predictors for all models
    pd <- data.frame(apply(csd, 1, function(x) {
        f <- paste0(x, " * ex$", colnames(ex), collapse = " + ") # formula as string, using coefficients from csd and data from ex
        lp <- eval(parse(text = f)) # evaluate that formula to get the linear predictor
        if (or) lp <- exp(lp)
        return(lp)
    }))
    colnames(pd) <- paste0("y", 1:ncol(pd)) # rename columns
    x <- ex[, 1] # get plot data x
    y <- pd$y1 # y
    lb <- apply(pd[, -(1:2)], 1, min) # lower bound 
    ub <- apply(pd[, -(1:2)], 1, max) # upper
    ggd <- data.frame(cbind(x, y, lb, ub, pd[, -1])) # merge into final plot data
    return(ggd)
}
