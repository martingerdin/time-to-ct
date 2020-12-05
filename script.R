## install and load required packages
## install.packages("tableone")
## install.packages("rockchalk")
## install.packages("xtable")
## install.packages("rms")
## install.packages("ggplot2")
## install.packages("reshape2")
## install.packages("devtools")
## library(devtools); install_github("martingerdin/bengaltiger")
library(tableone)
library(rms)
library(rockchalk)
library(xtable)
library(ggplot2)
library(reshape2)
library(bengaltiger)
## set working directory
## setwd("~/Documents/Läkarprogrammet/T8 Examensarbete/titco")
## import data
titco.data <- read.csv("../data/titco-I-full-dataset-v1.csv")
str(titco.data)
dim(titco.data)
## modify data to make it easier to work with
titco.data$age <- as.numeric(as.character(titco.data$age)) # Converting age (character variable) to a numeric variable
# creating a time to ct variable from doi, toi, doct, toct
timeofinjury <- as.POSIXct(paste(titco.data$doi, titco.data$toi), format="%Y-%m-%d %H:%M")
timeofct <- as.POSIXct(paste(titco.data$doct, titco.data$toct), format="%Y-%m-%d %H:%M")
timetoct <- difftime(timeofct, timeofinjury, units = "hours")
timetoct[timetoct > 24] <- NA # setting timetoct to missing if more than 24 hours
titco.data$timetoct <- timetoct # adding timetoct to titco.data
titco.data$timetoct <- as.numeric(as.character(titco.data$timetoct)) # converting timetoct to a numeric variable
#creating a variable called timeinhospital which measures time from arrival to death/discharge in days
timeofarrival <- as.POSIXct(paste(titco.data$doar, titco.data$toar), format="%Y-%m-%d %H:%M")
timeofdd <- as.POSIXct(paste(titco.data$dodd, titco.data$todd), format="%Y-%m-%d %H:%M")
timeinhospital <- difftime(timeofdd, timeofarrival, units = "days")
titco.data$timeinhospital <- timeinhospital # adding timeinhospital to titco.data
titco.data$timeinhospital <- as.numeric(as.character(titco.data$timeinhospital)) # converting timeinhospital to numeric variable
# create outcome variable status at 30 days in the hospital
titco.data$status <- ifelse(titco.data$died == "Yes" & titco.data$timeinhospital <= 30, "Died", "Alive")
table(titco.data$status)
# create variables for tableone
titco.vars <- c("hos", "age", "sex", "moi", "sbp_1", "hr_1", "gcs_t_1", "iss", "timetoct", "status", "seqn", "timeinhospital")
# excluding patients from study sample
titco.data.included <- subset(titco.data, age>=15 & tran=="No" & ct=="Yes")
# creating a smaller dataset called "d" and keeping only variables in titco.vars
d <- titco.data.included[, titco.vars]
# Merging the road traffic injury subcategories into a single RTI category
d$moi <- combineLevels(d$moi, levs = c("Road traffic injury (bicyclist)", "Road traffic injury (driver)", "Road traffic injury (motorcyclist)", "Road traffic injury (passenger)", "Road traffic injury (pedestrian)", "Road traffic injury (unspecified)"), newLabel = c("Road traffic injury"))
## Include burns in others because of very few burns
d$moi <- combineLevels(d$moi, levs = c("Burn", "Other"), newLabel = c("Other"))
## estimating the amount of missing in the study sample
summary(complete.cases(d$sbp_1, d$timetoct, d$hr_1, d$gcs_t_1, d$iss))
dcc <- na.omit(d) # keeping only complete observations
nrow(dcc)/nrow(d) # proportion of complete cases
# function to get number and proportion of mising per variable
miss <- lapply(setNames(nm = colnames(d)),
               function(x) c(n = sum(is.na(d[, x])), p = mean(is.na(d[, x]))))
miss
nrow(dcc)/nrow(titco.data) # proportion of cases out of all cases that will be analysed 
round(nrow(dcc)/nrow(titco.data) * 16000) # final sample size out of complete dataset
# I require a sample of 1600, meaning I can safely loose 416 observations and still reach required sample size

## changing colnames by creating a named vector with the current column names as names and the new names as items
new_colnames <- c(age = "age",
                  sbp_1 = "sbp",
                  hr_1 = "hr", 
                  gcs_t_1 = "gcs",
                  timetoct = "ttct")
# applying the new names to the dataframe
colnames(dcc) <- unlist(lapply(colnames(dcc), function(x) if (x %in% names(new_colnames)) new_colnames[x] else x))

## splitting the dataset into test and validation sample according to seqn
dcc.order <- dcc[order(dcc$seqn), ] # order dataset using seqn

## Recode centre
dcc.order$hos <- as.factor(as.numeric(as.factor(dcc.order$hos)))

## use bengaltiger to create table of sample characteristics
codebook <- list(age = list(full.label = "Age in years",
                            abbreviated.label = ""),
                 sex = list(full.label = "Sex",
                            abbreviated.label = ""),
                 status = list(full.label = "30 day mortality outcome",
                            abbreviated.label = ""),
                 moi = list(full.label = "Mechanism of injury",
                            abbreviated.label = ""),
                 hos = list(full.label = "Centre",
                            abbreviated.label = ""),
                 sbp = list(full.label = "Systolic blood pressure in mmHg",
                            abbreviated.label = "SBP"),
                 hr = list(full.label = "Heart rate",
                            abbreviated.label = "HR"),
                 gcs = list(full.label = "Glasgow coma scale",
                            abbreviated.label = "GCS"),
                 iss = list(full.label = "Injury severity score",
                            abbreviated.label = "ISS"),
                 ttct = list(full.label = "Time from injury to computed tomography in hours",
                            abbreviated.label = "Time to CT"),
                 timeinhospital = list(full.label = "Time in hospital in days",
                            abbreviated.label = ""))
table1 <- bengaltiger::CreateSampleCharacteristicsTable(dcc.order,
                                                        variables = colnames(dcc.order)[!(colnames(dcc.order) %in% c("hos", "seqn", "status"))],
                                                        group = "status",
                                                        codebook = codebook,
                                                        save.to.results = FALSE,
                                                        save.to.disk = TRUE)

## recode status variable
dcc.order$status <- ifelse(dcc.order$status == "Died", 1, 0) #changing the levels of status to 0 & 1
dcc.order$status <- as.numeric(as.character(dcc.order$status)) #converting status to a numeric variable 
## function to create logit models and calculate differences between coefficients in test and validation data
model <- function(model.data, include.centre = FALSE) {
  ## build a simple model with only ttct and survival
  ## transforming timetoct as restricted cubic splines using the rms package
  ttct_rcs <- rcspline.eval(model.data$ttct, nk=3)
  model.data$ttct_rcs <- as.numeric(ttct_rcs)
  ## estimating a crude associated between timetoct and 30 day mortality
  crude.formula <- as.formula("status ~ ttct + ttct_rcs")
  crude.mod <- glm(crude.formula, family = "binomial", data = model.data) # crude logit model 
  crude.coef <- coefficients(crude.mod)[c("ttct", "ttct_rcs")] # get coefficients
  ## build a full model
  ## transforming covariates as restricted cubic splines using the rms package
  age_rcs <- rcspline.eval(model.data$age, nk=3) 
  sbp_rcs <- rcspline.eval(model.data$sbp, nk=3) 
  hr_rcs <- rcspline.eval(model.data$hr, nk=3) 
  ## put them back in data
  model.data$age_rcs <- as.numeric(age_rcs) # age back in data
  model.data$sbp_rcs <- as.numeric(sbp_rcs) # sbp back in data
  model.data$hr_rcs <- as.numeric(hr_rcs) # hr back in data
  ## multivariate mod regression to build a full model
  ## define formula
  covariates <- c("ttct", "ttct_rcs", "age", "age_rcs", "sbp", "sbp_rcs", "hr", "hr_rcs", "sex", "moi", "gcs", "iss")
  if (include.centre)
      covariates <- c(covariates, "hos")
  full.formula <- as.formula(paste0("status ~ ",
                                    paste0(unlist(lapply(covariates,
                                                         function(x) grep(paste0("^", x, "$"),
                                                                          colnames(model.data),
                                                                          value = TRUE))),
                                           collapse = " + ")))
  ## fit full model
  full.mod <- glm(full.formula, family = "binomial", data = model.data) # for the test sample
  full.coef <- coefficients(full.mod)[c("ttct", "ttct_rcs")]
  ## create object to return
  ret <- list(crude.mod = crude.mod,
              full.mod = full.mod)
  ## return
  return (ret)
}
## set seed for the random number generator in sample
set.seed(422)
## create bootstrap resamples of both samples
nbs <- 1000 # number of bootstrap samples 
bootstrap.samples <- lapply(1:nbs, function(i) {
  dcc.order[sample(1:nrow(dcc.order), nrow(dcc.order), TRUE), ]
})
## define function to run analysis and save results
RunAnalysis <- function(original.data, bootstrap.samples, main = FALSE, include.centre = FALSE) {
    ## check parameters
    if (!main & !include.centre)
        stop ("either main or include.centre has to be TRUE")
    if (main & include.centre)
        stop ("both main and include.centre cannot be TRUE")
    ## define identifier
    analysis.id <- "main"
    if (include.centre)
        analysis.id <- "include-centre"
    ## estimate original models
    original.model <- model(original.data, include.centre = include.centre)
    saveRDS(original.model, paste0("original-models-", analysis.id, ".Rds"))
    
    ## generate bootstrap models
    bootstrap.models <- lapply(bootstrap.samples, model, include.centre = include.centre)

    ## get test plot data
    source("./create.plot.data.r") # function to create plot data
    md <- original.model$full.mod # get original full model
    bs.models <- lapply(bootstrap.models, function(x) x$full.mod) # bootstraped models
    evar <- "ttct" # define explanatory variable
    pdt <- create.plot.data(md, bs.models, evar, or = TRUE) # get plot data
    pdt$Sample <- "all"
    x <- pdt$x
    rest <- pdt[, 5:(ncol(pdt) - 1)]
    colnames(rest) <- paste0("t", colnames(rest))
    mpdt <- reshape2::melt(cbind(x, rest), id.vars = "x")
    ## create plot components
    mp <- ggplot() # main plot
    lines <- geom_line(aes(x = x, y = value, col = variable), alpha = 0.1, data = mpdt, show.legend = FALSE)
    scale <- scale_color_manual(values = c(rep("dodgerblue3", length(levels(mpdt$variable)))))
    tci <- geom_ribbon(data = pdt, aes(x = x, ymin = lb, ymax = ub, fill = Sample), alpha = 0.1, inherit.aes = FALSE) # test confidence interval
    tl <- geom_line(data = pdt, aes(x = x, y = y, linetype = Sample, size = Sample))  # test line (point estimate)
    cileg <- scale_fill_manual(values = c("dodgerblue1"))
    lileg <- scale_linetype_manual(values = c("solid"))
    size <- scale_size_manual(values = c(0.5))
    abline <- geom_hline(yintercept = 1, colour = "white", linetype = "dotted")
    labs <- labs(y = "30-day mortality odds ratio", x = "Time to CT from injury in hours") # axis labels
    yticks <- scale_y_continuous(breaks = seq(floor(min(mpdt$value)), ceiling(max(mpdt$value)), 1)) # y axis ticks
    ggp <- mp + tci + lines + abline + tl +  scale + size + cileg + lileg + labs + yticks + theme(legend.position = "none")
    ## print and save plot
    ggsave(filename = paste0("plot-results-", analysis.id, ".pdf"), plot = ggp)
    ## pdf(file = paste0("main.results.", analysis.id, ".pdf")); ggp; dev.off()
}
## Get results
results <- RunAnalysis(original.data = dcc.order, bootstrap.samples = bootstrap.samples, include.centre = TRUE)



