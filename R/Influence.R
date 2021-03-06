#'Influence Statistics (Cook's D) (WREG)
#'
#'@description The \code{Influence} function calculates the influence statistics
#'(Cooks-D) for any regrerssion.
#'
#'@param e contains the model residuals.
#'@param X contains independent variables.  A leading constant of one is
#'  included if a constant is included in the regression.  Each row represents a
#'  unique obsevation.
#'@param Omega is the weighting matrix used for regression fitting.
#'@param Beta contains the fitted model coefficients.
#'@param ROI is a logical indicating if this is a region-of-influence
#'  regression.
#'@param Lev is a vector with the same length as \code{e} and includes the
#'  leverage of each observation. This input is required for any
#'  region-of-influence regression.
#'  
#'@details Influence is a measure of the impact each observation has on the
#'estimated regression coefficients.  The calculation is based on equation 43 of
#'the WREG v. 1.0 manual.  The critical value of influence is calculated using
#'equation 44. An influence is considered significant if the absolute value of
#'the influence is greater than the critical value.
#'
#'For region-of-influence regressions, the influence calculation is weighted by
#'the leverage of that observation on the target site and the overall leverage 
#'of the observation.  This is a departure from the WREG v. 1.0, but reflects
#'the WREG v. 1.05 code.
#'
#'@return The function returns as list as output.  The list contains: 
#'  \item{Influence}{A vector containing the influence (Cook's D) of each
#'  observation on the estimated regression coefficients.} \item{Limit}{The
#'  critical influence value for this dataset.} \item{Significant}{A logical
#'  vector the same size as \code{Influence}. It indicates if the influence is
#'  significant for each observation.}
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
#' transY <- "none"
#' result <- WREG.OLS(Y, X, transY)
#' 
#' # calculate influence of each point
#' influenceResult <- Influence(e = result$residuals, X = X, 
#'   Omega = result$Weighting, Beta = result$Coefs$Coefficient)
#'
#'@export
Influence <- function(e,X,Omega,Beta,ROI=FALSE,Lev=NA) {
  # William Farmer, USGS, January 02, 2015
  # 01/27/2015, WHF: Added abs on limit and ROI option to match MatLab WREG v 1.05, RoIMetrics, Lines 39-52
  # 11/10/2016  Greg Petrochenkov : Changed validation scheme
  
  # Some upfront error handling
  
  wregValidation((!missing(e)&!missing(X)&!missing(Omega))&&
                   (length(unique(length(e),nrow(X),nrow(Omega),ncol(Omega)))!=1), "eq", FALSE,
                 "length(e), nrow(X), nrow(Omega) and ncol(Omega) must all be equal", warnFlag = TRUE)
  
  wregValidation((!missing(X)&!missing(Beta))&&
                   (length(unique(length(Beta),ncol(X)))!=1), "eq", FALSE,
                 "length(Beta) and ncol(X) must be equal", warnFlag = TRUE)
  
  if(!wregValidation(missing(e), "eq", FALSE,
                     "Residuals (e) must be provided.", warnFlag = TRUE)){
    
    if(!wregValidation(e, "numeric", message =
                       "Residuals (e) must be provided as class numeric.", warnFlag = TRUE)){
      
      if(!wregValidation(sum(is.na(c(e))), "eq", 0,
                         paste0("Some residuals (e) contain missing ",
                                "values.  These must be removed."), warnFlag = TRUE)){
        
        wregValidation(sum(is.infinite(c(e))), "eq", 0,
                       paste0("Some residuals (e) contain infinite ",
                              "values.  These must be removed."), warnFlag = TRUE)
      }
    }
  }
  
  if(!wregValidation(missing(X), "eq", FALSE,
                     "Independent variables (X) must be provided.", warnFlag = TRUE)){
    
    if(!wregValidation((length(unique(apply(X,FUN=class,MARGIN=2)))!=1)|
                       (unique(apply(X,FUN=class,MARGIN=2))!="numeric"), "eq", FALSE,
                       "Independent variables (X) must be provided as class numeric.", warnFlag = TRUE)){
      
      if(!wregValidation(sum(is.na(as.matrix(X))), "eq", 0,
                         paste0("Some independent variables (X) contain missing ",
                                "values.  These must be removed."), warnFlag = TRUE)){
        
        wregValidation(sum(is.infinite(as.matrix(X))), "eq", 0,
                       paste0("Some independent variables (X) contain infinite ",
                              "values.  These must be removed."), warnFlag = TRUE)
      }
    }
  }
  
  if(!wregValidation(missing(Omega), "eq", FALSE,
                     "A weighting matrix (Omega) must be provided.", warnFlag = TRUE)){
    
    if(!wregValidation(!is.matrix(Omega), "eq", FALSE,
                       "The weighting matrix (Omega) must be provided as a matrix.", warnFlag = TRUE)){
      
      if(!wregValidation(Omega, "numeric", message =
                         "The weighting matrix (Omega) must be provided as class numeric.", warnFlag = TRUE)){
        
        # wregValidation(det(Omega), "notEq", 0,
        #                paste0("Some independent variables (X) contain infinite ",
        #                       "values.  These must be removed."), warnFlag = TRUE)
      }
    }
  }
  
  if(!wregValidation(missing(Beta), "eq", FALSE,
                     "Coefficients (Beta) must be provided", warnFlag = TRUE)){
    
    if(!wregValidation(Beta, "numeric", message = 
                       "Coefficients (Beta) must be provided as class numeric", warnFlag = TRUE)){
      
      if(!wregValidation(sum(is.na(c(Beta))), "eq", 0,
                         paste0("Some coefficients (Beta) contain missing ",
                                "values.  These must be removed."), warnFlag = TRUE)){
        
        wregValidation(sum(is.infinite(c(Beta))), "eq", 0,
                       paste0("Some coefficients (Beta) contain infinite ",
                              "values.  These must be removed."), warnFlag = TRUE)
      }
    }
  }
  
  if (ROI) {
    wregValidation(length(Lev)!=length(e), "eq", FALSE,
                   "The lengths of Lev and e must be identical", warnFlag = TRUE)
  }
  if (warn("check")){
    stop("Invalid inputs were provided.  See warnings().", warn("get"))
  }
  
  L <- as.matrix(X)%*%solve(t(X)%*%solve(Omega)%*%as.matrix(X))%*%t(X) # Basic leverage calculation
  if (ROI) {
    # Though not noted in the manual, GLS-ROI uses a different calculation and critical value for influence.
    H <- X%*%solve(t(X)%*%solve(Omega)%*%X)%*%t(X)%*%solve(Omega) # Leverage matrix; need for ROI version of leverage
    D <- (e^2)*diag(L)*abs(Lev/diag(H))/length(Beta)/(diag(Omega)-diag(L))^2 # ROI influence
    D_limit <- 4*2/nrow(X) # ROI critical influence
  } else {
    # Influence formula noted in manual.  Only used for non-ROI.
    D <- (e^2)*diag(L)/length(Beta)/(diag(Omega)-diag(L))^2 # Influence, Eq 43
    D_limit <- 4/nrow(X) # Critical influence, Eq 44
  }
  sig <- abs(D)>abs(D_limit) # Significance test. Logical: Influence is signifianct.
  Output <- list(Influence=D,Limit=D_limit,Significant=sig)
  return(Output)
}