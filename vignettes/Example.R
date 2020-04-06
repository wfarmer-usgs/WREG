## ----eval = FALSE-------------------------------------------------------------
#  library(WREG)

## ----echo = FALSE-------------------------------------------------------------
suppressWarnings(library(WREG))

## -----------------------------------------------------------------------------
wregDir <- file.path(system.file("exampleDirectory", package = "WREG"),
  "matlabImport")
importedData <- importWREG(wregPath = wregDir)

## ----echo=FALSE---------------------------------------------------------------
names(importedData$Y)

## ----echo=FALSE---------------------------------------------------------------
names(importedData$X)

## -----------------------------------------------------------------------------
Y <- log(importedData$Y$Q100)

## -----------------------------------------------------------------------------
X <- log(importedData$X[, c("A", "P1006")])
X0 <- rep(1,nrow(X))
X <- cbind(X0,X)

## -----------------------------------------------------------------------------
transY <- 'ln'

## -----------------------------------------------------------------------------
Ex.OLS <- WREG.OLS(Y, X, transY)

## ----echo=FALSE---------------------------------------------------------------
names(Ex.OLS)

## -----------------------------------------------------------------------------
Ex.OLS

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(round(Ex.OLS$Coefs,digits=4))

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(round(data.frame(t(unlist(Ex.OLS$PerformanceMetrics))),digits=4))

## ----eval=FALSE, fig.show='hold', fig.width = 6.5, fig.height = 6.5,fig.align='center'----
#  plot(Ex.OLS$fitted.values, Ex.OLS$residuals,
#    main = 'Residuals versus Estimated Flow Characteristics',
#    xlab = 'Estimated Flow Characteristic', ylab = 'Residual',
#    xaxs = 'i', yaxs = 'i', ylim = c(-2, 2), xlim = c(7, 9))
#  grid(nx = 16, ny = 16)

## ----eval=FALSE, fig.show='hold', fig.width = 6.5, fig.height = 6.5,fig.align='center'----
#  plot(Ex.OLS$Y, Ex.OLS$ResLevInf$Leverage,
#    main = 'Leverage Values versus Observations',
#    xlab = 'Observation', ylab = 'Leverage Value',
#    xaxs = 'i', yaxs = 'i', ylim = c(0, 0.5), xlim = c(6, 11))
#  grid(nx = 20, ny = 10)
#  abline(Ex.OLS$LevLim, 0, col = 'red')

## ----eval=FALSE, fig.show='hold', fig.width = 6.5, fig.height = 6.5,fig.align='center'----
#  plot(Ex.OLS$Y, Ex.OLS$ResLevInf$Influence,
#    main = 'Influence Value versus Observation',
#    xlab = 'Observation', ylab = 'Influence Value',
#    xaxs = 'i', yaxs = 'i', ylim = c(0, 1.2), xlim = c(6, 11))
#  grid(nx = 20, ny = 12)
#  abline(Ex.OLS$InflLim, 0, col = 'red')

## -----------------------------------------------------------------------------
RL <- importedData$recLen

## -----------------------------------------------------------------------------
LP3 <- data.frame(S = importedData$LP3f$S, K = importedData$LP3k$Q100, G = importedData$LP3f$G)

## -----------------------------------------------------------------------------
Ex.WLS <- WREG.WLS(Y = Y, X= X, recordLengths = RL, LP3 = LP3, transY = transY)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(round(Ex.WLS$Coefs,digits=4))

## ----echo=FALSE---------------------------------------------------------------
names(Ex.WLS$PerformanceMetrics)

## -----------------------------------------------------------------------------
Ex.WLS

## ----fig.show='hold', fig.width = 6.5, fig.height = 6.5,fig.align='center'----
xcorPlot(object = importedData, alpha = 0.002, 
  theta = 0.98, DistMeth = 2, concurrentMin = 25, plot = FALSE)

## -----------------------------------------------------------------------------
Ex.GLS <- WREG.GLS(Y = Y, X = X, recordLengths = RL, LP3 = LP3,
  transY = transY, basinChars = importedData$BasChars, 
  alpha = 0.002, theta = 0.98, distMeth = 2)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(round(Ex.GLS$Coefs,digits=4))

## -----------------------------------------------------------------------------
Ex.GLS

## -----------------------------------------------------------------------------
LP3$GR <- importedData$LP3f$GR

## -----------------------------------------------------------------------------
Ex.GLSs <- WREG.GLS(Y = Y, X = X, recordLengths = RL, LP3 = LP3,
  transY = transY, basinChars = importedData$BasChars, 
  alpha = 0.002, theta = 0.98, distMeth = 2,
  MSEGR = 0.302, TY = 100, peak = TRUE, regSkew = TRUE)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(round(Ex.GLSs$Coefs,digits=4))

## -----------------------------------------------------------------------------
Ex.GLSs

