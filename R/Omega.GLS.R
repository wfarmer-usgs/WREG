#'Calculate weighting matrix for GLS regression. (WREG)
#'
#'@description THe \code{Omega.GLS} function calculates the weighting matrix
#'required for generalized least-squares regression, without or without
#'uncertainty in the regional skew.
#'
#'@param alpha A number, required only for \dQuote{GLS} and \dQuote{GLSskew}. 
#'  \code{alpha} is a parameter used in the estimated cross-correlation between
#'  site records.  See equation 20 in the WREG v. 1.05 manual.  The arbitrary,
#'  default value is 0.01.  The user should fit a different value as needed.
#'@param theta A number, required only for \dQuote{GLS} and \dQuote{GLSskew}. 
#'  \code{theta} is a parameter used in the estimated cross-correlation between
#'  site records.  See equation 20 in the WREG v. 1.05 manual.  The arbitrary,
#'  default value is 0.98.  The user should fit a different value as needed.
#'@param independent A dataframe containing three variables: \code{StationID} is
#'  the numerical identifier (without a leading zero) of each site, \code{Lat}
#'  is the latitude of the site, in decimal degrees, and \code{Long} is the
#'  longitude of the site, in decimal degrees.  The sites must be presented in
#'  the same order as \code{Y}.  Required only for \dQuote{GLS} and
#'  \dQuote{GLSskew}.
#'@param X The independent variables in the regression, with any transformations
#'  already applied.  Each row represents a site and each column represents a
#'  particular independe variable.  (If a leading constant is used, it should be
#'  included here as a leading column of ones.)  The rows must be in the same
#'  order as the dependent variables in \code{Y}.
#'@param Y The dependent variable of interest, with any transformations already 
#'  applied.
#'@param recordLengths This input is required. #'  \code{recordLengths} should
#'  be a matrix whose rows and columns are in the same order as \code{Y}.  Each
#'  \code{(r,c)} element represents the length of concurrent record between
#'  sites \code{r} and \code{c}.  The diagonal elements therefore represent each
#'  site's full record length.
#'@param LP3 A dataframe containing the fitted Log-Pearson Type III standard 
#'  deviate, standard deviation and skew for each site.  The names of this data
#'  frame are \code{S}, \code{K} and \code{G}.  For \dQuote{GLSskew}, the
#'  regional skew value must also be provided in a variable called \code{GR}. 
#'  The order of the rows must be the same as \code{Y}.
#'@param MSEGR A number. The mean squared error of the regional skew.  Required
#'  only for \dQuote{GLSskew}.
#'@param TY A number.  The return period of the event being modeled.  Required
#'  only for \dQuote{GLSskew}.  The default value is \code{2}.  (See the
#'  \code{Legacy} details below.)
#'@param peak A logical.  Indicates if the event being modeled is a peak flow
#'  event or a low-flow event.  \code{TRUE} indicates a peak flow, while
#'  \code{FALSE} indicates a low-flow event.
#'@param distMeth Required for \dQuote{GLS} and \dQuote{GLSskew}.  A value of
#'  \code{1} indicates that the "Nautical Mile" approximation should be used to
#'  calculate inter-site distances.  A value of \code{2} designates the
#'  Haversine approximation.  See \code{\link{Dist.WREG}}.  The default value is
#'  \code{2}.  (See the \code{Legacy} details below.)
#'  
#'@details This function is largely a subroutine for \code{\link{WREG.GLS}}.
#'  
#'  The weighting matrix is calculated by iteration, as noted in the manual to 
#'  WREG v. 1.0.  As currently implemented the initial estimate of model error
#'  variance, \code{GSQ}, is taken to range from \code{0} and \code{2*var(Y)}. 
#'  This interval is broken into 30 equally spaced intervals.  The weighting
#'  matrix is calculated for each interval endpoint and the deviation from
#'  equation 21 in the WREG v. 1.0 manual is recorded.  The progam then search
#'  for the interval over which the deviatioon changes sign.  This interval is
#'  then split into 30 finer intervals and the process is repeated. The ine
#'  interval with the smallest positive deviation is selected as the best
#'  estimate.
#'  
#'@return This function returns a list with two elements: \item{GSQ}{The
#'  estimated model error variance.} \item{Omega}{The estimated weighting
#'  matrix.  A square matrix.}
#'  
#' @examples
#' # Import some example data
#' peakFQdir <- paste0(
#'   file.path(system.file("exampleDirectory", package = "WREG"),
#'     "pfqImport"))
#' gisFilePath <- file.path(peakFQdir, "pfqSiteInfo.txt")
#' importedData <- importPeakFQ(pfqPath = peakFQdir, gisFile = gisFilePath)
#' 
#' # Organizing input data
#' lp3Data <- importedData$LP3f
#' lp3Data$K <- importedData$LP3k$AEP_0.5
#' Y <- importedData$Y$AEP_0.5
#' X <- importedData$X[c("Sand", "OutletElev", "Slope")]
#' 
#' # Compute weighting matrix
#' weightingResult <- Omega.GLS(alpha = 0.01, theta = 0.98,
#'   independent = importedData$BasChars, X = X,
#'   Y = Y, recordLengths = importedData$recLen,
#'   LP3 = lp3Data, MSEGR = NA, TY = 20, peak = TRUE, distMeth = 2)
#'   
#' @export
Omega.GLS <- function(alpha=0.01,theta=0.98,independent,X,Y,recordLengths,
  LP3,MSEGR=NA,TY=2,peak=TRUE,distMeth=2) {
  
  # William Farmer, January 22, 2015
  # 11/9/2016  Greg Petrochenkov : Changed validation scheme
  
  # Some basic error checking
  if (!wregValidation(missing(Y), "eq", FALSE,
                      "Dependent variable (Y) must be provided", warnFlag = TRUE)) {
    
    if (!wregValidation(Y, "numeric", message = 
                        "Dependent variable (Y) must be provided as class numeric",
                        warnFlag = TRUE)) {
      
      wregValidation(sum(is.na(Y)), "eq", 0 ,
                     paste0("The depedent variable (Y) contains missing ",
                            "values.  These must be removed."),
                     warnFlag = TRUE)
      
      wregValidation(sum(is.infinite(Y)), "eq", 0 ,
                     paste0("The depedent variable (Y) contains infinite ",
                            "values.  These must be removed."),
                     warnFlag = TRUE)
    }
  }
 
  if (!wregValidation(missing(X), "eq", FALSE,
                      "Independent variables (X) must be provided.", warnFlag = TRUE)) {
    
    if (!wregValidation((length(unique(apply(X,FUN=class,MARGIN=2)))!=1)|
                        (unique(apply(X,FUN=class,MARGIN=2))!="numeric"), "eq", FALSE,
                        "Independent variables (X) must be provided as class numeric.", warnFlag = TRUE)){
      
      wregValidation(sum(is.na(as.matrix(X))), "eq", 0,
                     paste0("Some independent variables (X) contain missing ",
                            "values.  These must be removed."), warnFlag = TRUE)
      
      wregValidation(sum(is.infinite(as.matrix(X))), "eq", 0,
                     paste0("Some independent variables (X) contain infinite ",
                            "values.  These must be removed."), warnFlag = TRUE)
    }
  }

  if(!wregValidation(TY, "numeric", message =
                 "The return period (TY) must be a numeric value.", warnFlag = TRUE)){
    
    wregValidation(length(TY), "eq", 1,
                   "The return period (TY) must be a single value", warnFlag = TRUE)
  }
  
  if(!wregValidation(alpha, "numeric", message =
                     "alpha must be a numeric value.", warnFlag = TRUE)){
    
    wregValidation(length(alpha), "eq", 1,
                   "alpha must be a single value", warnFlag = TRUE)
  }
  
  if(!wregValidation(theta, "numeric", message =
                     "theta be a numeric value.", warnFlag = TRUE)){
    
    wregValidation(length(theta), "eq", 1,
                   "theta must be a single value", warnFlag = TRUE)
  }
 
  wregValidation(!is.logical(peak), "eq", FALSE,
                 paste("The input 'peak' must be either TRUE when estimating a",
                 "maximum event or FALSE when estimatinga minimum event."), warnFlag = TRUE)
  
  wregValidation(length(peak), "eq", 1,
                 "peak must be a single value", warnFlag = TRUE)
  
  wregValidation(!is.element(distMeth,c(1,2)), "eq", FALSE,
                 paste("distMeth must be either 1 for use of a nautical mile ",
                 "approximation or 2 for use of the haversine formula."), warnFlag = TRUE)
  
  if (!wregValidation(missing(independent), "eq", FALSE,
                      "independent must be provided as input.", warnFlag = TRUE)) {
    
    if (!wregValidation(!is.data.frame(independent), "eq", FALSE,
                        paste("'independent' must be provided as a data frame with elements",
                              "named 'Station.ID', 'Lat' and 'Long' for standard deivation,",
                              "deviate and skew, respectively."), warnFlag = TRUE)){
      
      if (!wregValidation(sum(is.element(c("Station.ID","Lat","Long"),names(independent))), "eq", 3,
                          paste("In valid elements: The names of the elements in",
                                "independent are",names(independent),
                                ".  'independent' must be provided as a data frame with elements",
                                "named 'Station.ID', 'Lat' and 'Long"), warnFlag = TRUE)){
        
        if (!wregValidation((length(unique(apply(cbind(independent$Lat,independent$Long),FUN=class,MARGIN=2)))!=1)|
                            (unique(apply(cbind(independent$Lat,independent$Long),FUN=class,MARGIN=2))!="numeric"),
                            "eq", FALSE,
                            "latitudes and longitudes must be provided as class numeric.", warnFlag = TRUE)){
          
          wregValidation(sum(is.na(c(independent$Lat,independent$Long))), "eq", 0,
                         paste0("Some latitudes and longitudes contain missing ",
                                "values.  These must be removed."), warnFlag = TRUE)
          
          wregValidation(sum(is.infinite(c(independent$Lat,independent$Long))), "eq", 0,
                         paste0("Some latitudes and longitudes contain infinite ",
                                "values.  These must be removed."), warnFlag = TRUE)
          
        }
        
      }
    }
  }
  
  ## Determining if skew adjustment is requested
  SkewAdj<-F # default: no skew adjustment
  if (!is.na(MSEGR)) {
    SkewAdj<-T # If user provides a mean squared-error of regional skew, 
    #               then use skew adjustment.
    
    wregValidation(length(MSEGR), "eq", 1, "MSEGR must be a single value",
                      warnFlag = TRUE)
    
    wregValidation(MSEGR, "numeric", FALSE, "MSEGR must be a numeric value",
                   warnFlag = TRUE)
  }
  
  # Error checking LP3
  
  if (!wregValidation(missing(LP3), "eq", FALSE, "LP3 must be provided as input",
                      warnFlag = TRUE)){
    if (!SkewAdj){
      
      if (!wregValidation(!is.data.frame(LP3), "eq", FALSE, 
                          paste("LP3 must be provided as a data frame with elements named",
                                "'S', 'K' and 'G' for standard deivation, deviate and skew,",
                                "respectively."), warnFlag = TRUE)){
        
        wregValidation(sum(is.element(c("S","K","G"),names(LP3))), "eq", 3, 
                       paste("In valid elements: The names of the elements in LP3 are",
                             names(LP3),". LP3 must be provided as a data frame with elements named",
                             "'S', 'K' and 'G' for standard deivation, deviate and skew,",
                             "respectively."), warnFlag = TRUE)
        
        if(wregValidation((length(unique(apply(cbind(LP3$S,LP3$K,LP3$G),FUN=class,MARGIN=2)))!=1)|
                          (unique(apply(cbind(LP3$S,LP3$K,LP3$G),FUN=class,MARGIN=2))!="numeric"), "eq", FALSE,
                          "LP3 must be provided as a numeric array", warnFlag = TRUE)){
          
          wregValidation(sum(is.infinite(LP3$S),is.infinite(LP3$K),is.infinite(LP3$G)), "eq", 0,
                         "LP3 must be provided as a numeric array", warnFlag = TRUE)
          
          wregValidation(sum(is.na(LP3$S),is.na(LP3$K),is.na(LP3$G)), "eq", 0,
                             paste0("Some elements of LP3$S, LP3$K, and LP3$G contain missing ",
                                    "values.  These must be removed."), warnFlag = TRUE)
        }
      }
      
    } else {
      
      if (!wregValidation(!is.data.frame(LP3), "eq", FALSE, 
                          paste("LP3 must be provided as a data frame with elements named",
                                "'S', 'K' and 'G' for standard deivation, deviate and skew,",
                                "respectively."), warnFlag = TRUE)){
        
        wregValidation(sum(is.element(c("S","K","G","GR"),names(LP3))), "eq", 4, 
                       paste("In valid elements: The names of the elements in LP3 are",
                             names(LP3),". LP3 must be provided as a data frame with elements named",
                             "'S', 'K', 'G' and 'GR' for standard deivation, deviate,",
                             "skew and regional skew, respectively."), warnFlag = TRUE)
        
        if(!wregValidation((length(unique(apply(cbind(LP3$S,LP3$K,LP3$G,LP3$GR),FUN=class,MARGIN=2)))!=1)|
                          (unique(apply(cbind(LP3$S,LP3$K,LP3$G,LP3$GR),FUN=class,MARGIN=2))!="numeric"), "eq", FALSE, 
                          "LP3 must be provided as a numeric array", warnFlag = TRUE)){
          
          wregValidation(sum(is.infinite(LP3$S),is.infinite(LP3$K),
                             is.infinite(LP3$G),is.infinite(LP3$GR)), "eq", 0, 
                         paste0("Some elements of LP3$S, LP3$K, LP3$G and LP3$GR contain ",
                                "infinite values.  These must be removed."), warnFlag = TRUE)
          
          wregValidation(sum(is.na(LP3$S),is.na(LP3$K),is.na(LP3$G),is.na(LP3$GR)), "eq", 0, 
                         paste0("Some elements of LP3$S, LP3$K, LP3$G and LP3$GR contain ",
                                "missing values.  These must be removed."), warnFlag = TRUE)
          
        }
      }
    }
  }
  
  if(!wregValidation(missing(recordLengths), "eq", FALSE, 
                     "A matrix of recordLengths must be provided as input.", warnFlag = TRUE)){
    
    wregValidation(missing(recordLengths), "eq", FALSE, 
                   "recordLengths must be provided as a square array", warnFlag = TRUE)
    
    wregValidation(recordLengths, "numeric", message =
                     "recordLengths must be provided as a numeric array", warnFlag = TRUE)
    
  }
  
  if (warn("check")) {
    stop("Invalid inputs were provided.  See warnings().", warn("get"))
  }
  
  #Convert X and Y from dataframes to matrices to work with matrix operations below
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  
  ## Create distance matrix and concurrent record lengths
  ##    (For skew-adjusted GLS: Also calculates the LP3 partial derivatives, mean squared-errors of at-site skew and the variance of at-site skew.)
  Dists <- matrix(NA,ncol=length(Y),nrow=length(Y)) # Empty matrix for intersite distances
  M <- recordLengths # Just renaming input for ease.  (Should probably correct later...)
  if (SkewAdj) { # Make empty vectors for GLS-skew
    dKdG <- vector(length=length(Y)) # Empty vector for LP3 partial derivatives
    MSEg <- vector(length=length(Y)) # Empty vector for mean squared-error of at-site skew
    Varg <- vector(length=length(Y)) # Empty vector for variance of at-site skew
    ### Convert return period into probability
    if (peak) { # if a peak flow is being estimated
      Zp <- -qnorm(1/TY) 
    } else { # if a low flow is being estimated
      Zp <- qnorm(1/TY)
    }
  }
  for (i in 1:length(Y)) {
    for (j in 1:length(Y)) {
      if (i!=j) {
        ### Calculate intersite distance via subroutine.
        ###     distMeth==1 applies the 'Nautical Mile' approximation from WREG v 1.05
        ###     distMeth==2 applies Haversine approximation
        Dists[i,j] <- Dist.WREG(Lat1=independent$Lat[i],Long1=independent$Long[i],Lat2=independent$Lat[j],Long2=independent$Long[j],method=distMeth) # Intersite distance, miles
      }
    }
    if (SkewAdj) { # Additional calculations for skew-adjusted GLS
      ### LP3 Partial Derivative
      dKdG[i] <- (Zp^2-1)/6+LP3$G[i]*(Zp^3-6*Zp)/54-LP3$G[i]^2*(Zp^2-1)/72+Zp*LP3$G[i]^3/324+5*LP3$G[i]^4/23328 # LP3 partial derivative. Eq 23.
      ### Variance of at-site skew
      a <- -17.75/M[i,i]^2+50.06/M[i,i]^3 # Coefficeint for calculation of the variance of at-site skew.  Eq 27.
      b1 <- 3.92/M[i,i]^0.3-31.1/M[i,i]^0.6+34.86/M[i,i]^0.9  # Coefficeint for calculation of the variance of at-site skew.  Eq 28.
      c1 <- -7.31/M[i,i]^0.59+45.9/M[i,i]^1.18-86.5/M[i,i]^1.77  # Coefficeint for calculation of the variance of at-site skew.  Eq 29.
      Varg[i] <- (6/M[i,i]+a)*(1+LP3$GR[i]^2*(9/6+b1)+LP3$GR[i]^4*(15/48+c1)) # Variance of at-site skew. Eq 26.
      ### Mean squared-error of at-site skew
      ###   These equations are not included or described in v1.05 manual.  They are similar to Eq 28 and 29.
      ###   These equations, which appear in the v1.05 code, are documented in Griffis and Stedinger (2009); Eq 3, 4, 5 and 6.
      b <- 3.93/M[i,i]^0.3-30.97/M[i,i]^0.6+37.1/M[i,i]^0.9 # Coefficient for the calculation of mean squared-error of at-site skew.
      c <- -6.16/M[i,i]^0.56+36.83/M[i,i]^1.12-66.9/M[i,i]^1.68 # Coefficient for the calculation of mean squared-error of at-site skew.
      MSEg[i] <- (6/M[i,i]+a)*(1+LP3$G[i]^2*(9/6+b)+LP3$G[i]^4*(15/48+c)) # Mean squared-error of at-site skew.
    }
  }
  Rhos <- theta^(Dists/(alpha*Dists+1)) # Estimated intersite correlation. Eq 20.
  
  if (SkewAdj) { # if skew-adjusted GLS
    ### Calculate skew weights.
    Wg <- MSEGR/(MSEg+MSEGR) # skew weight. Eq 17.
    ### Calculate covariances between at-site skews
    Covgg <- matrix(NA,ncol=length(Y),nrow=length(Y)) # Empty matrix for covariances between at-site skews.
    for (i in 1:length(Y)) {
      for (j in 1:length(Y)) {
        if (i!=j) {
          Covgg[i,j] <- M[i,j]*sign(Rhos[i,j])*abs(Rhos[i,j])^3*sqrt(Varg[i]*Varg[j])/sqrt(M[i,i]*M[j,j]) # Covariance between at-site skews. Eq 24 and 25
        }
      }
    }
  }
  
  ## Baseline OLS sigma regression. Eq 15.
  Omega <- diag(nrow(X)) # OLS weighting matrix (identity)
  B.SigReg <- solve(t(X)%*%solve(Omega)%*%X)%*%t(X)%*%solve(Omega)%*%LP3$S # OLS estimated coefficients for k-variable model of LP3 standard deviation.
  Yhat.SigReg <- X%*%B.SigReg # Estimates from sigma regression
  
  ## NOTE: This iteration procedure is currently implemented in WREG v1.05, though not described in the manual.
  ##        It searches across 30 possible values of model-error variance, looks for a sign-change, and then repeats thirty searches on the identified interval.
  ##        It may be possible to improve performance by stopping th loop after a sign change rather than searching the entire space.
  
  ## Set variables to control iterative procedure
  Target<-length(Y) - ncol(X) # Target value of iterations. RHS of Eq 21.
  GInt <- 30 # Number of intervals to consider (doubled below)
  Gstep <- 2*var(Y)/(GInt-1) # Length of step
  ## Coarse intervals.
  Iterations <- matrix(0,ncol=2,nrow=GInt) # Empty dataframe to store results
  Iterations <- data.frame(Iterations); names(Iterations) <- c('GSQ','Deviation') # Formatting empty dataframe.  Will contain estimated model-error variance (GSQ) and deviation from target.
  for (w in 1:GInt) {
    GSQ <- (w-1)*Gstep; Iterations$GSQ[w]<-GSQ; # guess at model error variance
    for (i in 1:length(Y)) {
      for (j in 1:length(Y)) {
        if (i==j) { # Diagonal elements of weighting matrix
          if (SkewAdj) { # if skew-adjusted GLS, use Eq 22 (correct manual to match Giffis and Stedinger (2007) Eq 5; WREG v1.05 code is correct)
            Omega[i,j] <-  GSQ + 
              Yhat.SigReg[i]^2*(1+LP3$K[i]*LP3$G[i]+
                  0.5*LP3$K[i]^2*(1+0.75*LP3$G[i]^2)+
                  Wg[i]*LP3$K[i]*dKdG[i]*(3*LP3$G[i]+0.75*LP3$G[i]^3)+
                  Wg[i]^2*dKdG[i]^2*(6+9*LP3$G[i]^2+1.875*LP3$G[i]^4))/M[i,j] + 
              (1-Wg[i])^2*Yhat.SigReg[i]^2*MSEGR*dKdG[i]^2
          } else { # if normal GLS, use Eq 19.
            Omega[i,j] <-  GSQ + Yhat.SigReg[i]^2*
              (1+LP3$K[i]*LP3$G[i]+0.5*LP3$K[i]^2*
                  (1+0.75*LP3$G[i]^2))/M[i,j]
          }
        } else { # Off-diagonal elements of weighting matrix
          if (SkewAdj) { # if skew-adjusted GLS, use Eq 22 (correct manual to match Giffis and Stedinger (2007) Eq 5; WREG v1.05 code is correct)
            Omega[i,j] <- Rhos[i,j]*Yhat.SigReg[i]*Yhat.SigReg[j]*M[i,j]*
              (1+0.5*LP3$K[i]*LP3$G[i]+0.5*LP3$K[j]*LP3$G[j]+
                  0.5*LP3$K[i]*LP3$K[j]*(Rhos[i,j]+0.75*LP3$G[i]*LP3$G[j])+
                  0.5*Wg[j]*LP3$K[i]*LP3$G[j]*dKdG[j]*(3*Rhos[i,j]+0.75*LP3$G[i]*LP3$G[j])+
                  0.5*Wg[i]*LP3$K[j]*LP3$G[i]*dKdG[i]*(3*Rhos[i,j]+0.75*LP3$G[i]*LP3$G[j])+
                  Wg[i]*Wg[j]*Yhat.SigReg[i]*Yhat.SigReg[j]*dKdG[i]*dKdG[j]*Covgg[i,j])/M[i,i]/M[j,j]
          } else { # if normal GLS, use Eq 19.
            Omega[i,j] <- Rhos[i,j]*Yhat.SigReg[i]*Yhat.SigReg[j]*M[i,j]*
              (1+0.5*LP3$K[i]*LP3$G[i]+0.5*LP3$K[j]*LP3$G[j]+
                  0.5*LP3$K[i]*LP3$K[j]*(Rhos[i,j]+0.75*LP3$G[i]*LP3$G[j]))/M[i,i]/M[j,j]
          }
        }
      }
    }
    B_hat <- solve(t(X)%*%solve(Omega)%*%X)%*%t(X)%*%solve(Omega)%*%Y # Use weighting matrix to estimate regression coefficients
    Iterations$Deviation[w] <- t(Y-X%*%B_hat)%*%solve(Omega)%*%(Y-X%*%B_hat)-Target # Difference between result and target value. Eq 21.
  }
  ## Finer intervals.  Expands iterval with sign change to get closer to zero.
  Signs <- sign(Iterations$Deviation); LastPos <- which(diff(Signs)!=0) # Finds where sign changes from positive to negative
  if (length(LastPos)==0) { # If no change in sign of deviation
    BestPos <- which(abs(Iterations$Deviation)==min(abs(Iterations$Deviation))) # Use the minimum deviation
    GSQ <- Iterations$GSQ[BestPos] # Best estiamte of model-error variance
  } else { # There is a sign change, so expand the interval with sign change.
    Iterations2 <- matrix(0,ncol=2,nrow=GInt) # Empty matrix to store finer intervals.
    Iterations2 <- data.frame(Iterations); names(Iterations) <- c('GSQ','Deviation') # Formatting empty matrix
    Gstep <- (Iterations$GSQ[LastPos+1]-Iterations$GSQ[LastPos])/(GInt-1) # Step for finer intervals
    for (w in 1:GInt) {
      GSQ <- Iterations$GSQ[LastPos]+(w-1)*Gstep; Iterations2$GSQ[w]<-GSQ; # estimate of model-error variance
      for (i in 1:length(Y)) {
        for (j in 1:length(Y)) {
          if (i==j) { # Diagonal elements of weighting matrix
            if (SkewAdj) { # if skew-adjusted GLS, use Eq 22 (correct manual to match Giffis and Stedinger (2007) Eq 5; WREG v1.05 code is correct)
              Omega[i,j] <-  GSQ + 
                Yhat.SigReg[i]^2*(1+LP3$K[i]*LP3$G[i]+
                    0.5*LP3$K[i]^2*(1+0.75*LP3$G[i]^2)+
                    Wg[i]*LP3$K[i]*dKdG[i]*(3*LP3$G[i]+0.75*LP3$G[i]^3)+
                    Wg[i]^2*dKdG[i]^2*(6+9*LP3$G[i]^2+1.875*LP3$G[i]^4))/M[i,j] + 
                (1-Wg[i])^2*Yhat.SigReg[i]^2*MSEGR*dKdG[i]^2
            } else { # if normal GLS, use Eq 19.
              Omega[i,j] <-  GSQ + Yhat.SigReg[i]^2*
                (1+LP3$K[i]*LP3$G[i]+0.5*LP3$K[i]^2*
                    (1+0.75*LP3$G[i]^2))/M[i,j]
            }
          } else { # Off-diagonal elements of weighting matrix
            if (SkewAdj) { # if skew-adjusted GLS, use Eq 22 (correct manual to match Giffis and Stedinger (2007) Eq 5; WREG v1.05 code is correct)
              Omega[i,j] <- Rhos[i,j]*Yhat.SigReg[i]*Yhat.SigReg[j]*M[i,j]*
                (1+0.5*LP3$K[i]*LP3$G[i]+0.5*LP3$K[j]*LP3$G[j]+
                    0.5*LP3$K[i]*LP3$K[j]*(Rhos[i,j]+0.75*LP3$G[i]*LP3$G[j])+
                    0.5*Wg[j]*LP3$K[i]*LP3$G[j]*dKdG[j]*(3*Rhos[i,j]+0.75*LP3$G[i]*LP3$G[j])+
                    0.5*Wg[i]*LP3$K[j]*LP3$G[i]*dKdG[i]*(3*Rhos[i,j]+0.75*LP3$G[i]*LP3$G[j])+
                    Wg[i]*Wg[j]*Yhat.SigReg[i]*Yhat.SigReg[j]*dKdG[i]*dKdG[j]*Covgg[i,j])/M[i,i]/M[j,j]
            } else { # if normal GLS, use Eq 19.
              Omega[i,j] <- Rhos[i,j]*Yhat.SigReg[i]*Yhat.SigReg[j]*M[i,j]*
                (1+0.5*LP3$K[i]*LP3$G[i]+0.5*LP3$K[j]*LP3$G[j]+
                    0.5*LP3$K[i]*LP3$K[j]*(Rhos[i,j]+0.75*LP3$G[i]*LP3$G[j]))/M[i,i]/M[j,j]
            }
          }
        }
      }
      B_hat <- solve(t(X)%*%solve(Omega)%*%X)%*%t(X)%*%solve(Omega)%*%Y # Use weighting matrix to estimate regression coefficients
      Iterations2$Deviation[w] <- t(Y-X%*%B_hat)%*%solve(Omega)%*%(Y-X%*%B_hat)-Target # Difference between result and target value. Eq 21.
    }
    BestPos <- which(Iterations2$Deviation==min(Iterations2$Deviation[Iterations2$Deviation>0])) # Find minimum positive deviation from Eq 21.
    GSQ <- Iterations2$GSQ[BestPos] # best estimate of model-error variance
  }
  ## Calculate Final Omega (weighting matrix)
  for (i in 1:length(Y)) {
    for (j in 1:length(Y)) {
      if (i==j) { # Diagonal elements of weighting matrix
        if (SkewAdj) { # if skew-adjusted GLS, use Eq 22 (correct manual to match Giffis and Stedinger (2007) Eq 5; WREG v1.05 code is correct)
          Omega[i,j] <-  GSQ + 
            Yhat.SigReg[i]^2*(1+LP3$K[i]*LP3$G[i]+
                0.5*LP3$K[i]^2*(1+0.75*LP3$G[i]^2)+
                Wg[i]*LP3$K[i]*dKdG[i]*(3*LP3$G[i]+0.75*LP3$G[i]^3)+
                Wg[i]^2*dKdG[i]^2*(6+9*LP3$G[i]^2+1.875*LP3$G[i]^4))/M[i,j] + 
            (1-Wg[i])^2*Yhat.SigReg[i]^2*MSEGR*dKdG[i]^2
        } else { # if normal GLS, use Eq 19.
          Omega[i,j] <-  GSQ + Yhat.SigReg[i]^2*
            (1+LP3$K[i]*LP3$G[i]+0.5*LP3$K[i]^2*
                (1+0.75*LP3$G[i]^2))/M[i,j]
        }
      } else { # Off-diagonal elements of weighting matrix
        if (SkewAdj) { # if skew-adjusted GLS, use Eq 22 (correct manual to match Giffis and Stedinger (2007) Eq 5; WREG v1.05 code is correct)
          Omega[i,j] <- Rhos[i,j]*Yhat.SigReg[i]*Yhat.SigReg[j]*M[i,j]*
            (1+0.5*LP3$K[i]*LP3$G[i]+0.5*LP3$K[j]*LP3$G[j]+
                0.5*LP3$K[i]*LP3$K[j]*(Rhos[i,j]+0.75*LP3$G[i]*LP3$G[j])+
                0.5*Wg[j]*LP3$K[i]*LP3$G[j]*dKdG[j]*(3*Rhos[i,j]+0.75*LP3$G[i]*LP3$G[j])+
                0.5*Wg[i]*LP3$K[j]*LP3$G[i]*dKdG[i]*(3*Rhos[i,j]+0.75*LP3$G[i]*LP3$G[j])+
                Wg[i]*Wg[j]*Yhat.SigReg[i]*Yhat.SigReg[j]*dKdG[i]*dKdG[j]*Covgg[i,j])/M[i,i]/M[j,j]
        } else { # if normal GLS, use Eq 19.
          Omega[i,j] <- Rhos[i,j]*Yhat.SigReg[i]*Yhat.SigReg[j]*M[i,j]*
            (1+0.5*LP3$K[i]*LP3$G[i]+0.5*LP3$K[j]*LP3$G[j]+
                0.5*LP3$K[i]*LP3$K[j]*(Rhos[i,j]+0.75*LP3$G[i]*LP3$G[j]))/M[i,i]/M[j,j]
        }
      }
    }
  }
  ## Control output
  GLS.Weights <- list(GSQ=GSQ,Omega=Omega) # Output contains model-error variance and weighting matrix.
  return(GLS.Weights)
}