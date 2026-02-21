################################################################################
##
## File:    DS4E-Functions.R
##
## Purpose: Misc functions.
##  
## Created: 2023.03.09
##
## Version: 2023.05.31
##
################################################################################


################################################################################
## Functions specific for "used cars" data
################################################################################

.var.clean <- function(data, var, pattern, replacement)
{
  #### Adjust type
  ind <- grep(x = data$name, pattern = pattern, fixed = TRUE)
  data[ind, var] <- replacement
  data$name <- gsub(x = data$name, pattern = pattern, replacement = "", 
    ignore.case = TRUE)
  #### Answer
  data
}
# ------------------------------------------------------------------------------


.usedcars.data.clean <- function(data)
{
  #### Remove V1
  data <- data[, -1, drop = FALSE]
  #### Price
  data$price <- gsub(x = data$price, pattern = "$", replacement = "", fixed = TRUE)
  data$price <- as.numeric(data$price)
  #### Odometer in thousand 
  data$odometer <- data$odometer / 1000
  
  #### Remove obs with no price
  data <- data[!is.na(data$price), , drop = FALSE]
  
  #### Extract year from name
  data$year <- substr(
    x = data$name, start = 1, stop = 4)
  data$year <- as.numeric(data$year)
  
  #### Store name for future restoring
  name <- data$name
  
  #### Label used for missing values
  na.lab <- "MISS"
  #### Adjust type
  var <- "type"
  pattern <- replacement <- "convertible"
  data <- .var.clean(data = data, var = var, pattern = pattern, 
    replacement = replacement)
  data[is.na(data[, var]), var] <- na.lab
  #### Adjust drive
  var <- "drive"
  pattern <- replacement <- "fwd"
  data <- .var.clean(data = data, var = var, pattern = pattern, 
    replacement = replacement)
  data[is.na(data[, var]), var] <- na.lab
  #### Adjust fuel
  var <- "fuel"
  pattern <- replacement <- "hybrid"
  data <- .var.clean(data = data, var = var, pattern = pattern, 
    replacement = replacement)
  data[is.na(data[, var]), var] <- na.lab
  #### Adjust cylinders
  var <- "cylinders"
  pattern <- "4cyl"
  replacement <- "4 cylinders"
  data <- .var.clean(data = data, var = var, pattern = pattern, 
    replacement = replacement)
  pattern <- "v6"
  replacement <- "6 cylinders"
  data <- .var.clean(data = data, var = var, pattern = pattern, 
    replacement = replacement)
  data[, var] <- gsub(x = data[, var], pattern = " cylinders", replacement = "")
  data[is.na(data[, var]), var] <- na.lab
  #### Other vars
  data$transmission[is.na(data$transmission)] <- na.lab
  data$size[is.na(data$size)] <- na.lab
  data$paintcolor[is.na(data$paintcolor)] <- na.lab
  data$condition[is.na(data$condition)] <- na.lab
  
  #### Clean name to try squeeze further info
  data$name <- tolower(data$name)
  data$name <- gsub(x = data$name, pattern = "1998 1998", replacement = 1998, fixed = TRUE)
  data$name <- gsub(x = data$name, pattern = "1999 1999", replacement = 1999, fixed = TRUE)
  data$name <- gsub(x = data$name, pattern = "2002 2002", replacement = 2002, fixed = TRUE)
  data$name <- gsub(x = data$name, pattern = "2007 2007", replacement = 2007, fixed = TRUE)
  data$name <- gsub(x = data$name, pattern = "[_]|[-]|[.]", replacement = "")
  data$name <- gsub(x = data$name, pattern = "  ", replacement = " ", fixed = TRUE)
  data$name <- gsub(x = data$name, pattern = "toyota camry|sedan|advanced|sport|2016|210hp|4dr|4 dr", 
    replacement = "")
  data$name <- substr(x = data$name, start = 6, stop = 1000)
  #### XLE
  pattern <- "xle"
  x1 <- numeric(NROW(data))
  ind <- grep(x = data$name, pattern = pattern, fixed = TRUE)
  x1[ind] <- 1
  data$xle <- x1
  data$name <- gsub(x = data$name, pattern = pattern, replacement = "", 
    ignore.case = TRUE)
  #### SE
  pattern <- "se"
  x1 <- numeric(NROW(data))
  ind <- grep(x = data$name, pattern = pattern, fixed = TRUE)
  x1[ind] <- 1
  data$se <- x1
  data$name <- gsub(x = data$name, pattern = pattern, replacement = "", 
    ignore.case = TRUE)
  #### LE
  pattern <- "le"
  x1 <- numeric(NROW(data))
  ind <- grep(x = data$name, pattern = pattern, fixed = TRUE)
  x1[ind] <- 1
  data$le <- x1
  data$name <- gsub(x = data$name, pattern = pattern, replacement = "", 
    ignore.case = TRUE)
  #### Solara
  pattern <- "solara"
  x1 <- numeric(NROW(data))
  ind <- grep(x = data$name, pattern = pattern, fixed = TRUE)
  x1[ind] <- 1
  data$solara <- x1
  data$name <- gsub(x = data$name, pattern = pattern, replacement = "", 
    ignore.case = TRUE)
  
  #### Add age
  data$age <- 2018 - data$year
  #### Restore name
  data$name <- name
  
  #### Answer
  data
}
# ------------------------------------------------------------------------------


.usedcars.data.clean2 <- function(data)
{
  #### Remove obs
  ind <- 500 <= data$price & data$price <= 20000 & 
    1 <= data$odometer & data$odometer <= 250 & !is.na(data$odometer) &
    data$condition != "new" & !( data$type %in% c("SUV", "wagon") )
  data <- data[ind, , drop = FALSE]
  
  #### Recode variables
  recodes <- "'sub-compact'='compact'"
  data$size <- car::recode(var = data$size, recodes = recodes)
  recodes <- "c('8','MISS')='other'"
  data$cylinders <- car::recode(var = data$cylinders, recodes = recodes)
  recodes <- "'MISS'='other'"
  data$fuel <- car::recode(var = data$fuel, recodes = recodes)
  
  #### Answer
  data
}
# ------------------------------------------------------------------------------


#### The following cleaning function is needed by gbm() because sometimes gives 
##   errors with character vars: we transform them to factor.
.usedcars.data.clean3 <- function(data)
{
  #### Find character vars
  ind <- mapply(FUN = is.character, x = data)
  #### Exclude "name"
  ind <- ind & !( colnames(data) %in% "name" )
  #### Cycle across vars
  ind <- which(ind)
  for (i in 1 : NROW(ind))
  {
    ind1 <- ind[i]
    data[, ind1] <- factor(data[, ind1])
  }

  #### Answer
  data
}
# ------------------------------------------------------------------------------


################################################################################
## Functions specific for "used cars" data
################################################################################

#### Function to convert character() vars to factor() (all are unordered). 
##   This is needed by gbm() because sometimes gives errors with character vars.
##   Some vars can be excuded; other (non character) can be included.
.character.2.factor <- function(data, exclude = NULL, include = NULL)
{
  #### Find character vars
  ind <- mapply(FUN = is.character, x = data)
  #### Exclude vars in 'exclude'
  ind <- ind & !( colnames(data) %in% exclude )
  #### Include vars in 'include'
  ind <- ind | colnames(data) %in% include
  #### Cycle across vars to convert
  #### Cycle across vars to convert
  ind <- which(ind)
  for (ind1 in ind)
  {
    data[, ind1] <- factor(data[, ind1])
  }
  #### Answer
  data
}
# ------------------------------------------------------------------------------


################################################################################
## Functions for regression based analyses
################################################################################

#### Fit statistics
.fit.stats <- function(fit, y = NULL, x = NULL, ...)
{
  cfit <- class(fit)
  if ("lm" %in% cfit)
  {
    if ( (NROW(y) == 0 && NROW(x) == 0) )
    {
      y <- fit$model[, 1]
      # x <- fit$model[, -1, drop = FALSE]
      fitted <- predict(object = fit)
      AIC <- AIC(fit)
      BIC <- BIC(fit)
    }
    else
    {
      fitted <- predict(object = fit, newdata = x)
      AIC <- NA
      BIC <- NA
    }
    ncoef <- NROW(coefficients(fit)) 
  }
  else if ( "glmnet" %in% cfit )
  {
    AIC <- NA
    BIC <- NA
    fitted <- predict(object = fit, newx = x)
    ncoef <- fit$df + 1 
  }
  else if ( "rpart" %in% cfit )
  {
    AIC <- NA
    BIC <- NA
    fitted <- predict(object = fit, newdata = x)
    ncoef <- max(fit$cptable[, "nsplit"]) + 1 
  }
  else if ( "regbagg" %in% cfit )
  {
    AIC <- NA
    BIC <- NA
    fitted <- predict(object = fit, newdata = x)
    ncoef <- NA 
  }
  else if ( "ranger" %in% cfit )
  {
    AIC <- NA
    BIC <- NA
    fitted <- predict(object = fit, data = x)$predictions
    ncoef <- NA 
  }
  else if ( "gbm" %in% cfit )
  {
    AIC <- NA
    BIC <- NA
    fitted <- predict(object = fit, newdata = x, ... )
    ncoef <- NA 
  }
  nobs <- NROW(y)
  res <- y - fitted
  Rsq <- cor(y, fitted)^2
  Rsqa <- 1 - (1 - Rsq) * (nobs - 1) / (nobs - ncoef)
  RMSE <- sqrt( mean(res^2) )
  MAE <- mean( abs(res) ) 
  
  #### Answer
  c(ncoef = ncoef, Rsq = Rsq, Rsqa = Rsqa, RMSE = RMSE, MAE = MAE, 
    AIC = AIC, BIC = BIC)
}
# ------------------------------------------------------------------------------

#### Error measures
.error.measures <- function(y, fit)
{ 
  #### Errors
  u  <- y - fit
  v  <- y / fit
  
  #### Error measures
  ME   <- mean( u )
  MAE  <- mean( abs(u) )
  RMSE <- sqrt( mean( u^2 ) )
  #### Percentage error measures
  if ( all(y > 0) )
  {
    ur     <- u / y
    MPE    <- mean( ur )
    MAPE   <- mean( abs( ur ) )
    RMSPE  <- sqrt( mean( ur^2 ) )
    LLE    <- mean( v - 1 - log(v) )
  }
  else
  {
    MPE    <- NULL
    MAPE   <- NULL
    RMSPE  <- NULL
    LLE    <- NULL
  }
  
  ####
  c(ME = ME, MAE = MAE, RMSE = RMSE,  
    MPE = MPE, MAPE = MAPE, RMSPE = RMSPE, 
    LLE = LLE)
}
# ------------------------------------------------------------------------------


#### Predictions on the log scale
.predict.log <- function(fit, newx, sigma = 0)
{
  cfit <- class(fit)
  if ("lm" %in% cfit)
  {
    fitted <- predict(object = fit, newdata = newx, type = "response")
  }
  else if ("glmnet" %in% cfit)
  {
    fitted <- predict(object = fit, newx = newx, type = "response")
  }
  ####
  exp(fitted + sigma^2 / 2)
}
# ------------------------------------------------------------------------------

#### glmnet residuals
.glmnet.residuals <- function(fit, y, x)
{
  y - predict(object = fit, newx = x, type = "response")
}
# ------------------------------------------------------------------------------

#### Get sigma
.sigma <- function(fit, y = NULL, x = NULL)
{
  cfit <- class(fit)
  if ( "lm" %in% cfit )
  {
    sigma <- summary(fit1)$sigma
  }
  else if ( "glmnet" %in% cfit )
  {
    res <- .glmnet.residuals(fit = fit, y = y, x = x)
    sigma <- sqrt(mean(res^2))
  }
  sigma
}
# ------------------------------------------------------------------------------


#### Get prediction trying to avoid errors
.predict.lm <- function(object, newdata)
{
  xmat <- model.matrix(object = object, data = newdata)
  coef <- coefficients(object)
  xmat %*% coef[colnames(xmat)]
}
# ------------------------------------------------------------------------------


################################################################################
## rpart
################################################################################

.rpart.plot.01 <- function(fit, data)
{  
  #### Settings
  n <- 400
  #### Variable names
  x1 <- attr(x = fit$terms, which = "factors")
  vy <- rownames(x1)[1] 
  vx <- rownames(x1)[2]
  #### Fitted values on a grid of x values
  rx <- range(data[, vx])
  newdata <- data.frame(x = seq(from = rx[1], to = rx[2], length.out = n))
  colnames(newdata) <- vx
  fitted <- predict(object = fit, newdata = newdata)
  #### Plot
  ylim <- range(fit$y, fitted)
  plot(x = data[, vx], y = fit$y, ylim = ylim, xlab = vx, ylab = vy)
  lines(x = newdata[,1], y = fitted, col = "red")
}
# ------------------------------------------------------------------------------


.rpart.plot.02 <- function(data, formula, maxdepth)
{  
  #### Settings
  n <- 400
  terms <- terms(formula)
  x1 <- attr(x = terms, which = "factors")
  vy <- rownames(x1)[1] 
  vx <- rownames(x1)[2]
  rx <- range(data[, vx])
  newdata <- data.frame(x = seq(from = rx[1], to = rx[2], length.out = n))
  colnames(newdata) <- vx

  #### 1st model: NULL or 0 depth
  control <- rpart.control(minbucket = 10, cp = 1, xval = 10, maxdepth = 1)
  fit <- rpart(formula = formula, data = data, method = "anova", control = control)

  #### Any depth
  plot(x = data[, vx], y = fit$y, xlab = vx, ylab = vy)
  if ( NROW(maxdepth) == 0 )
  {
    return(invisible(NULL))
  }
  
  #### Further settings
  maxdepth <- abs(round(maxdepth[1]))
  col <- rainbow(maxdepth + 1)
  
  #### Depth >= 0
  fitted <- predict(object = fit, newdata = newdata)
  lines(x = newdata[,1], y = fitted, col = col[1])
  
  #### Other depths
  i <- 1
  cond <- i <= maxdepth
  while (cond)
  {
    control <- rpart.control(minbucket = 10, cp = 0, xval = 10, maxdepth = i, maxcompete = i)
    fit <- rpart(formula = formula, data = data, method = "anova", control = control)
    fitted <- predict(object = fit, newdata = newdata)
    lines(x = newdata[,1], y = fitted, col = col[i+1])
    i <- i + 1
    cond <- i <= maxdepth
  }
  #### Append a legend
  leg <- paste0("depth = ", 0 : maxdepth)
  legend(x = "topright", legend = leg, fill = NULL, col = col,
    border = "white", lty = 1, pch = NULL, bty = "o", bg = par("bg"))

  #### Answer
  fit
}
# ------------------------------------------------------------------------------


################################################################################
## bagging
################################################################################

.bagging.plot.02 <- function(data, formula, maxdepth)
{  
  #### Settings
  n <- 400
  terms <- terms(formula)
  x1 <- attr(x = terms, which = "factors")
  vy <- rownames(x1)[1] 
  vx <- rownames(x1)[2]
  rx <- range(data[, vx])
  newdata <- data.frame(x = seq(from = rx[1], to = rx[2], length.out = n))
  colnames(newdata) <- vx

  #### 1st model: NULL or 0 depth
  control <- rpart.control(minbucket = 10, cp = 1, xval = 10, maxdepth = 1)
  fit <- rpart(formula = formula, data = data, method = "anova", control = control)

  #### Any depth
  plot(x = data[, vx], y = fit$y, xlab = vx, ylab = vy)
  if ( NROW(maxdepth) == 0 )
  {
    return(invisible(NULL))
  }
  
  #### Further settings
  maxdepth <- abs(round(maxdepth[1]))
  col <- rainbow(maxdepth + 1)
  
  #### Depth >= 0
  fitted <- predict(object = fit, newdata = newdata)
  lines(x = newdata[,1], y = fitted, col = col[1])
  
  #### Other depths
  i <- 1
  cond <- i <= maxdepth
  while (cond)
  {
    control <- rpart.control(minbucket = 10, cp = 0, xval = 0)
    fit <- bagging(formula = formula, data = data,
      nbagg = 100, coob = TRUE, control = control)
    fitted <- predict(object = fit, newdata = newdata)
    lines(x = newdata[,1], y = fitted, col = col[i+1])
    i <- i + 1
  }
  #### Append a legend
  leg <- paste0("depth = ", 0 : maxdepth)
  legend(x = "topright", legend = leg, fill = NULL, col = col,
    border = "white", lty = 1, pch = NULL, bty = "o", bg = par("bg"))

  #### Answer
  fit
}
# ------------------------------------------------------------------------------


.bagging.varImp <- function(fit, plot = TRUE)
{  
  #### Settings
  tree <- fit$mtrees
  ntree <- NROW(tree)

  #### Cycle
  vimp <- 0 * tree[[1]]$btree$variable.importance
  for (i in 1 : ntree)
  {
    vimp1 <- fit$mtrees[[i]]$btree$variable.importance[names(vimp)]
    vimp <- vimp + vimp1
  }
  #### Average and sort
  vimp <- sort(vimp / ntree, decreasing = TRUE)
  #### Plot
  if (plot)
  {
    par(las = 1)
    barplot(height = rev(vimp), width = 1, space = NULL, horiz = TRUE, 
      ylab = "", xlab = "importance")
  }
  #### Answer
  vimp
}
# ------------------------------------------------------------------------------


.bagging.errorMeasures <- function(fit)
{ 
  #### Settings
  tree <- fit$mtrees
  ntree <- NROW(tree)
  y <- fit$y
  X <- fit$X
  type <- class(fit)
  nobs <- NROW(y)
    
  #### Make
  fun.ce <- function(y, pred)
  {
    ans <- y * log(pred) + (1 - y) * log(1 - pred)
    ans[pred < 1e-10 | pred > 1 - 1e-10] <- 0
    -mean(ans)
  }

  #### Predictions
  pred <- ib <- matrix(NA, nobs, ntree)
  if (type == "classbagg")
  {
    for (i in 1 : ntree)
    {
      tree1 <- fit$mtrees[[i]]$btree
      pred[,i] <- predict(object = tree1, newdata = X, type = "prob")[, "1"]
      ib[, i] <- tabulate(fit$mtrees[[i]]$bindx, nbins = nobs)
    }
  }
  else if (type == "regbagg")
  {
    for (i in 1 : ntree)
    {
      tree1 <- fit$mtrees[[i]]$btree
      pred[,i] <- predict(object = tree1, newdata = X)
      ib[, i] <- tabulate(fit$mtrees[[i]]$bindx, nbins = nobs)
    }
  }
  
  ## IB/OOB
  ob <- ib == 0
  ib <- ib > 0
  ##
  if (type == "classbagg")
  {
    y <- as.numeric(levels(y))[y]
    pred.ib  <- rowSums(pred * ib) / rowSums(ib)
    pred.ob  <- rowSums(pred * ob) / rowSums(ob)
    pred.all <- rowMeans(pred)
    ans <- matrix(NA, 3, 3)
    rownames(ans) <- c("ib", "oob", "all")
    colnames(ans) <- c("me", "ce", "brier")
    ans["ib",  "me"] <- mean( (y != (pred.ib > 0.5) ) )
    ans["oob", "me"] <- mean( (y != (pred.ob > 0.5) ) )
    ans["all", "me"] <- mean( (y != (pred.all > 0.5) ) )
    ans["ib",  "ce"] <- fun.ce(y = y, pred = pred.ib)
    ans["oob", "ce"] <- fun.ce(y = y, pred = pred.ob)
    ans["all", "ce"] <- fun.ce(y = y, pred = pred.all)
    ans["ib",  "brier"] <- mean( (y - pred.ib)^2 )
    ans["oob", "brier"] <- mean( (y - pred.ob)^2 )
    ans["all", "brier"] <- mean( (y - pred.all)^2 )
  }
  else if (type == "regbagg")
  {
    pred.ib  <- rowSums(pred * ib) / rowSums(ib)
    pred.ob  <- rowSums(pred * ob) / rowSums(ob)
    pred.all <- rowMeans(pred)
    ans <- matrix(NA, 3, 3)
    rownames(ans) <- c("ib", "oob", "all")
    colnames(ans) <- c("mse", "rmse", "mae")
    ans["ib",  "mse"] <- mean( (y - pred.ib)^2 )
    ans["oob", "mse"] <- mean( (y - pred.ob)^2 )
    ans["all", "mse"] <- mean( (y - pred.all)^2 )
    ans[, "rmse"] <- sqrt(ans[, "mse"])
    ans["ib",  "mae"] <- mean( abs(y - pred.ib) )
    ans["oob", "mae"] <- mean( abs(y - pred.ob) )
    ans["all", "mae"] <- mean( abs(y - pred.all) )
  }
  else
  {
    stop("Argument 'fit' must have class 'classbagg' or 'regbagg'")
  }
  
  #### Answer
  ans
}
# ------------------------------------------------------------------------------


.bagging.errorPlot <- function(fit,  
  type = c("me", "ce", "brier"))
{ 
  #### Settings
  if (missing(type)) { type <- "me" }
  type <- type[1]
  typeL <- type
  tree <- fit$mtrees
  ntree <- NROW(tree)
  y <- fit$y
  X <- fit$X
  type <- class(fit)
  nobs <- NROW(y)
  #### Make
  fun.ce <- function(y, pred)
  {
    ans <- y * log(pred) + (1 - y) * log(1 - pred)
    ans[pred < 1e-10 | pred > 1 - 1e-10] <- 0
    -ans
  }

  #### Predictions
  pred <- matrix(NA, nobs, ntree)
  if (type == "classbagg")
  {
    for (i in 1 : ntree)
    {
      tree1 <- fit$mtrees[[i]]$btree
      pred[,i] <- predict(object = tree1, newdata = X, type = "prob")[, "1"]
    }
    if (typeL == "me")
    {
      pred <- pred > 0.5
    }
  }
  else if (type == "regbagg")
  {
    for (i in 1 : ntree)
    {
      tree1 <- fit$mtrees[[i]]$btree
      pred[,i] <- predict(object = tree1, newdata = X)
    }
  }
    
  #### Type
  if (type == "classbagg")
  {
    y <- as.numeric(levels(y))[y]
    pred.all <- t(apply(FUN = cumsum, X = pred, MARGIN = 1) / 1 : ntree)
    ans <- if (typeL == "me")
    {
      y != (pred.all > 0.5)
    }
    else if (typeL == "ce")
    {
      fun.ce(y = y, pred = pred.all)
    }
    else if (typeL == "brier")
    {
      (y - pred.all)^2
    }
    ans <- colMeans(ans)
  }  
  else if (type == "regbagg")
  {
    pred.all <- t(apply(FUN = cumsum, X = pred, MARGIN = 1) / 1 : ntree)
    ans <- colMeans( (y - pred.all)^2 )
    typeL <- "mse"
  }
  else
  {
    stop("Argument 'fit' must be of class 'classbagg' or 'regbagg'")
  }

  #### Plot
  ylab <- switch(typeL,
    mse = "Mean Squared Error",
    me = "Misclassification Error",
    ce = "Cross-Entropy",
    brier = "Brier Score" )
  plot(x = ans, type = "l", xlab = "nbagg", ylab = ylab)
  
  #### Answer
  ans
}
# ------------------------------------------------------------------------------


################################################################################
## Random Forests
################################################################################

.rf.varImp <- function(fit, plot = TRUE)
{  
  #### Importance
  vimp <- sort(fit$variable.importance)
  ####
  if (plot)
  {
    par(mfrow = c(1,1), las = 1)
    barplot(height = vimp, width = 1, space = NULL, horiz = TRUE, 
      ylab = "", xlab = "importance")
  }
  #### Answer
  vimp
}
# ------------------------------------------------------------------------------


.rf.errorMeasures <- function(fit, data)
{ 
  #### Extract
  ## General
  type <- fit$treetype
  n.trees <- fit$num.trees
  ## Prediction based
  ib <- do.call(what = cbind, args = fit$inbag.counts)
  ## Dependent var
  form <- update(old = formula(fit), new = ~ 0)
  y <- model.frame(formula = form, data = data)[, 1]

  #### Make
  fun.ce <- function(y, pred)
  {
    ans <- y * log(pred) + (1 - y) * log(1 - pred)
    ans[pred < 1e-10 | pred > 1 - 1e-10] <- 0
    -mean(ans)
  }
    
  ## IB/OOB
  ob <- ib == 0
  ib <- ib > 0
  ##
  if (type == "Probability estimation")
  {
    y <- as.numeric(levels(y))[y]
    pred <- predict(object = fit, data = data, type = "response", 
      predict.all = TRUE)$predictions[, "1",]
    pred.ib  <- rowSums(pred * ib) / rowSums(ib)
    pred.ob  <- rowSums(pred * ob) / rowSums(ob)
    pred.all <- rowMeans(pred)
    ans <- matrix(NA, 3, 3)
    rownames(ans) <- c("ib", "oob", "all")
    colnames(ans) <- c("me", "ce", "brier")
    ans["ib",  "me"] <- mean( (y != (pred.ib > 0.5) ) )
    ans["oob", "me"] <- mean( (y != (pred.ob > 0.5) ) )
    ans["all", "me"] <- mean( (y != (pred.all > 0.5) ) )
    ans["ib",  "ce"] <- fun.ce(y = y, pred = pred.ib)
    ans["oob", "ce"] <- fun.ce(y = y, pred = pred.ob)
    ans["all", "ce"] <- fun.ce(y = y, pred = pred.all)
    ans["ib",  "brier"] <- mean( (y - pred.ib)^2 )
    ans["oob", "brier"] <- mean( (y - pred.ob)^2 )
    ans["all", "brier"] <- mean( (y - pred.all)^2 )
  }
  else if (type == "Regression")
  {
    pred <- predict(object = fit, data = data, type = "response", 
      predict.all = TRUE)$predictions
    pred.ib  <- rowSums(pred * ib) / rowSums(ib)
    pred.ob  <- rowSums(pred * ob) / rowSums(ob)
    pred.all <- rowMeans(pred)
    ans <- matrix(NA, 3, 3)
    rownames(ans) <- c("ib", "oob", "all")
    colnames(ans) <- c("mse", "rmse", "mae")
    ans["ib",  "mse"] <- mean( (y - pred.ib)^2 )
    ans["oob", "mse"] <- mean( (y - pred.ob)^2 )
    ans["all", "mse"] <- mean( (y - pred.all)^2 )
    ans[, "rmse"] <- sqrt(ans[, "mse"])
    ans["ib",  "mae"] <- mean( abs(y - pred.ib) )
    ans["oob", "mae"] <- mean( abs(y - pred.ob) )
    ans["all", "mae"] <- mean( abs(y - pred.all) )
  }
  else
  {
    stop("Argument 'fit' must have the $treetype component as 'Probability estimation' or 'Regression'")
  }

  #### Answer
  ans
}
# ------------------------------------------------------------------------------


.rf.errorPlot <- function(fit, data, 
  type = c("me", "ce", "brier"))
{ 
  #### Extract
  if (missing(type)) { type <- "me" }
  typeL <- type[1]
  ## General
  type <- fit$treetype
  n.trees <- fit$num.trees
  ## Dependent var
  form <- update(old = formula(fit), new = ~ 0)
  y <- model.frame(formula = form, data = data)[, 1]
  
  #### Make
  fun.ce <- function(y, pred)
  {
    ans <- y * log(pred) + (1 - y) * log(1 - pred)
    ans[pred < 1e-10 | pred > 1 - 1e-10] <- 0
    -ans
  }
  
  #### Type
  if (type == "Probability estimation")
  {
    y <- as.numeric(levels(y))[y]
    pred <- predict(object = fit, data = data, type = "response", 
      predict.all = TRUE)$predictions[, "1",]
    #dim(pred)
    pred.all <- t(apply(FUN = cumsum, X = pred, MARGIN = 1) / 1 : n.trees)
    ans <- if (typeL == "me")
    {
      y != (pred.all > 0.5)
    }
    else if (typeL == "ce")
    {
      fun.ce(y = y, pred = pred.all)
    }
    else if (typeL == "brier")
    {
      (y - pred.all)^2
    }
    ans <- colMeans(ans)
  }  
  else if (type == "Regression")
  {
    pred <- predict(object = fit, data = data, type = "response", 
      predict.all = TRUE)$predictions
    pred.all <- t(apply(FUN = cumsum, X = pred, MARGIN = 1) / 1 : n.trees)
    ans <- colMeans( (y - pred.all)^2 )
  }
  else
  {
    stop("Argument 'fit' must have the $treetype component as 'Probability estimation' or 'Regression'")
  }
  
  #### Plot
  ylab <- switch(typeL,
    me = "Misclassification Error",
    ce = "Cross-Entropy",
    brier = "Brier Score" )
  plot(x = ans, type = "l", xlab = "num.trees", ylab = ylab)
  
  #### Answer
  ans
}
# ------------------------------------------------------------------------------


################################################################################
## Functions for binary data
################################################################################

#### Classify probabilities on the basis of the cut-off value
.classify <- function(x, coff) {
  
  # Esegui la classificazione
  as.numeric(x > coff)
}
# ------------------------------------------------------------------------------


#### Compute the cost based cut-off
.cost.cutoff <- function(c01, c10 = 1, c00 = 0, c11 = 0)
{
  1 / (1 + (c01 - c11) / (c10 - c00) )
}
# ------------------------------------------------------------------------------


#### Yuden cut-off
.yuden.coff <- function(x, pred)
{
  pred.r <- range(pred)
  coff <- seq(from = pred.r[1], to = pred.r[2], by = 0.001)
  # coff <- quantile(x = pred, prob = seq(from = 0, to = 1, by = 0.005),
  #   names = FALSE)
  if (pred.r[1] > 0) { coff <- c(0, coff) }
  if (pred.r[2] < 1) { coff <- c(coff, 1) }
  roc <- verification::roc.plot(x = x, pred = pred, thresholds = coff, 
    xlab = "FPR", ylab = "TPR")$plot.data[, , 1]
  yuden <- roc[, 2] - roc[, 3]
  roc <- cbind(roc, yuden)
  ind <- which.max(yuden)
  roc <- as.numeric(roc[ind, ])
  list(coff = roc[1], yuden = roc[6], tpr = roc[2], fpr = roc[3])
}
# ------------------------------------------------------------------------------


#### Fit statistics
.fit.stats.bin <- function(fit, y = NULL, x = NULL, ...)
{
  #### Extract the class of the fitted model
  cfit <- class(fit)
  #### Extract the family
  if ("glm" %in% cfit) { fam <- fit$family$family }
  else if ("glmnet" %in% cfit)
  { 
    if ("classnames" %in% names(fit)) { fam <- "binomial" }
  }
  else 
  {
    fam <- ""
  }
  #### Extract the method
  method <- fit$method
  if (NROW(method) == 0) {method <- ""}
  #### 
  if ("glm" %in% cfit && fam == "binomial")
  {
    if ( (NROW(y) == 0 && NROW(x) == 0) )
    {
      y <- fit$model[, 1]
      #x <- fit$model[, -1, drop = FALSE]
      fitted <- predict(object = fit)
      AIC <- AIC(fit)
      BIC <- BIC(fit)
    }
    else
    {
      x <- data.frame(x, check.names = FALSE)
      fitted <- predict(object = fit, newdata = x, type = "response")
      AIC <- NA
      BIC <- NA
    }
    
    ncoef <- NROW(coefficients(fit)) 
    
  }
  else if ("glmnet" %in% cfit & fam == "binomial")
  {
    AIC <- NA
    BIC <- NA
    fitted <- predict(object = fit, newx = x, type = "response")
    ncoef <- fit$df + 1
  }
  else if ( "rpart" %in% cfit && method == "class")
  {
    AIC <- NA
    BIC <- NA
    fitted <- predict(object = fit, newdata = x, type = "prob")[, "1"]
    ncoef <- max(fit$cptable[, "nsplit"]) + 1
  }
  else if ( "classbagg" %in% cfit )
  {
    AIC <- NA
    BIC <- NA
    fitted <- predict(object = fit, newdata = x, type = "prob")[, "1"]
    # fitted <- as.numeric(levels(fitted))[fitted]
    ncoef <- NA
    if (NROW(y) == 0)
    {
      y <- fit$y
    }
  }
  else if ( "ranger" %in% cfit )
  {
    AIC <- NA
    BIC <- NA
    fitted <- predict(object = fit, data = x)$predictions[, "1"]
    ncoef <- NA
    if (NROW(y) == 0)
    {
      y <- fit$y
    }
  }
  else if ( "gbm" %in% cfit )
  {
    AIC <- NA
    BIC <- NA
    fitted <- predict(object = fit, newdata = x, type = "response", ...)
    fitted.g <<- fitted
    ncoef <- NA
    if (NROW(y) == 0)
    {
      y <- fit$y
    }
  }
  nobs <- NROW(y)
  if (is.factor(y))
  {
    y <- as.numeric(levels(y))[y]
  }
  res <- y - fitted
  Rsq <- cor(y, fitted)^2
  Rsqa <- 1 - (1 - Rsq) * (nobs - 1) / (nobs - ncoef)
  RMSE <- sqrt( mean(res^2) )
  AUROC <- verification::roc.area(obs = y, pred = fitted)$A
  
  #### Answer
  c(ncoef = ncoef, Rsq = Rsq, Rsqa = Rsqa, AUROC = AUROC, Brier = RMSE, 
    AIC = AIC, BIC = BIC)
}
# ------------------------------------------------------------------------------


#### Classification based statistics
.class.stats <- function(true, class, 
  positive = "1", costs = c(c00 = 0, c11 = 0, c10 = 1, c01 = 1))
{
  #### Caret need factors
  true <- as.factor(true)
  class <- as.factor(class)
  #### Stats
  cmat <- caret::confusionMatrix(data = class, reference = true, 
    positive = "1")
  #### Total cost
  costs <- matrix(costs[c("c00", "c10", "c01", "c11")], 2, 2)
  total.cost <- sum(cmat$table * costs)
  #### Join
  list( 
    table = cmat$table,
    stats = c(cmat$overall["Accuracy"],
      TPR = as.numeric(cmat$byClass["Sensitivity"]),
      FNR = 1 - as.numeric(cmat$byClass["Sensitivity"]),
      TNR = as.numeric(cmat$byClass["Specificity"]),
      FPR = 1 - as.numeric(cmat$byClass["Specificity"]),
      TotalCost = total.cost) )
}
# ------------------------------------------------------------------------------
