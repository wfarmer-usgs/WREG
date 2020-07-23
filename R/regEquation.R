#'Print Resulting Regresstion String
#'
#'@description The \code{regEquation} function takes a WREG object and returns
#'the equation string.
#'  
#'@param object The dependent variable of interest, with any transformations 
#'  already applied.
#'@param decimals An integer or vector of integers specifying how many decimal 
#'  places should be used for each coefficient. If provided as a vector that 
#'  does not match the number of rows in \code{object$Coefs}, then the first 
#'  number provided will be replicated for all coefficients.
#'  
#'@details This function is used via a the print command, but can also be used 
#'  by itself.
#'  
#'@return A character string using the estimated coefficients and the 
#'  coefficient names from the \code{object} 
#'
#' @examples
#' # Import some example data
#' peakFQdir <- paste0(
#'   file.path(system.file("exampleDirectory", package = "WREG"),
#'     "pfqImport"))
#' gisFilePath <- file.path(peakFQdir, "pfqSiteInfo.txt")
#' importedData <- importPeakFQ(pfqPath = peakFQdir, gisFile = gisFilePath)
#' 
#' # Run a simple regression
#' Y <- importedData$Y$AEP_0.5
#' X <- importedData$X[c("Sand", "OutletElev", "Slope")]
#' X <- cbind(data.frame("Intercept" = rep(1, nrow(X))), X)
#' transY <- "none"
#' result <- WREG.OLS(Y, X, transY)
#' print(regEquation(result))
#'
#'@export

regEquation <- function(object, decimals = 2) {
  if (length(decimals) == 1) {
    decimals <- rep(decimals, nrow(object$Coefs))
  }
  if (length(decimals) != nrow(object$Coefs)) {
    decimals <- rep(decimals[1], nrow(object$Coefs))
  }
  eqStr <- "Y ="
  for (i in 1:nrow(object$Coefs)) {
    eqStr <- paste0(eqStr,
      ifelse(object$Coefs[i, 1] >= 0 & i > 1, " + ", 
        ifelse(object$Coefs[i, 1] < 0 & i > 1, " - ", " ")),
      sprintf("%.*f", decimals[i], abs(object$Coefs[i, 1])),
      ifelse(rownames(object$Coefs)[i] == "Intercept", "", paste0(" * ", rownames(object$Coefs)[i]))
    )
  }
  eqStr <- paste0(eqStr, "\n\n\t[NOTE: This does not include any transformations to variables.\n\tThis formula is derived from the columns names.")
  return(eqStr)
}