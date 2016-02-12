#' Import Data from PeakFQ Output
#' 
#' @description
#' The \code{importPeakFQ} function reads output directly from PeakFQ and 
#' prepares it for use in WREG-R.
#' 
#' @param pfqPath A directory that contains all of PeakFQ files for each site 
#' in the GIS file.  Files can be contained in the main path or in 
#' subdirectories of \code{pfqPath}.  Each site should be represented by one
#'  and only one .EXP and .PRT file.
#' @param gisFile A tab-delimited text file that contains a matrix whose rows 
#' represent the sites and contains columns that include \sQuote{Station.ID}, 
#' \sQuote{Lat}, \sQuote{Long} and any other variables to be used for analysis.
#' @param sites (optional) A vetor of sites that should be return.  Allows for 
#' data subsetting.
#' 
#' @details
#' This functions allows users to read output directly from PeakFQ without 
#' the need to manipulate additional files.  The site selection is driven 
#' by the sites in the GIS table.
#' 
#' @return All outputs are returned as part of a list.  The list includes:
#' \item{sites}{A vector of site IDs.}
#' \item{Y}{A matrix whose comlumns represent unique frequency events, 
#' while the row represent particular sites in the same order as \code{sites}.}
#' \item{AEP}{A vector of annual exceedance probabilities associated with the 
#' columns of \code{Y}.}
#' \item{X}{A matrix whose columns represent basin characteristics to be used
#'  as dependent variables and whose rows represent sites corresponding to \code{sites}.}
#' \item{LP3f}{A matrix containing the fitted LP# parameters that are fixed 
#' across exceedence probability.  These include the standard deviation, 
#' skew and regional skew for each site.}
#' \item{LP3k}{A matrix of the fitted kappa parameters of the LP3 distribution
#'  for each \code{AEP}.}
#' \item{BasChars}{A matrix containing the site IDs, latitudes and longitudes.}
#' \item{MSEGR}{The mean-squared-error in the regional skew.}
#' \item{recLen}{A square matrix indicating the number of overlapping years
#'  for each site pair.}
#' \item{recCor}{A matrix of the correlaiton between site paris.}
#' 
#'@export
importPeakFQ <- function(pfqPath,gisFile,sites='') {
  # Developed by William Farmer, 04 February 2016
  
  # pfqPath <- file.path('Example Directory','Peak_FQ_Runs')
  # gisFile <- file.path('Example Directory','FakeSiteInfo.txt')
  # sites <- '
  
  # Load GIS file
  gisData <- read.table(file=gisFile,sep='\t',header=T)
  
  # Determine which sites to search for
  if (sites==''|is.na(sites)) {
    sites <- gisData$Station.ID
  }
  ndx <- which(is.element(gisData$Station.ID,sites))
  
  # Independent variables
  BasChars <- gisData[ndx,is.element(names(gisData),
    c('Station.ID','Lat','Long'))]
  X <- gisData[ndx,!is.element(names(gisData),
    c('Station.ID','Lat','Long'))]
  
  # Search for EXP for each site
  allFiles <- apply(as.matrix(paste0('*',gisData$Station.ID[ndx],'.EXP')),
    MARGIN=1,FUN=list.files,path=pfqPath,recursive=T)
  # NEED  a method to handle duplicates
  allFiles <- file.path(pfqPath,allFiles)
  # Pulls from EXP curtesy of Janet Curran
  ## Could improve by looking for flag rather than a hard-coded skip
  EXP_SiteID <- do.call(rbind,lapply(allFiles,read.table,skip=1,nrows=1))
  EXP_G <- do.call(rbind,lapply(allFiles,read.table,skip=7,nrows=1))
  EXP_S <- do.call(rbind,lapply(allFiles,read.table,skip=9,nrows=1))
  EXP_GR <- do.call(rbind,lapply(allFiles,read.table,skip=14,nrows=1))
  EXP_MSEGR <- do.call(rbind,lapply(allFiles,read.table,skip=15,nrows=1))
  EXP_N <- do.call(rbind,lapply(allFiles,read.table,skip=16,nrows=1))[,2] + 
    do.call(rbind,lapply(allFiles,read.table,skip=17,nrows=1))[,2]
  EXP_AEP <- do.call(rbind,lapply(allFiles,read.table,skip=21,nrows=1))
  EXP_Est <- do.call(rbind,lapply(allFiles,read.table,skip=22,nrows=1))
  EXP_Var <- do.call(rbind,lapply(allFiles,read.table,skip=23,nrows=1))
  EXP_K <- do.call(rbind,lapply(allFiles,read.table,skip=26,nrows=1))
  # Dependent Variables (with names)
  Y <- EXP_Est[,2:ncol(EXP_Est)]
  AEP <- EXP_AEP[,2:ncol(EXP_AEP)]
  # LP3 Variables
  LP3f <- data.frame(S=EXP_S[,2],G=EXP_G[,2],GR=EXP_GR[,2])
  LP3k <- EXP_K[,2:ncol(EXP_K)]
  
  # Search for PRT for each site
  allFiles <- apply(as.matrix(paste0('*',gisData$Station.ID[ndx],'.PRT')),
    MARGIN=1,FUN=list.files,path=pfqPath,recursive=T)
  # NEED  a method to handle duplicates
  allFiles <- file.path(pfqPath,allFiles)
  temp <- function(data) {
    # Just for efficient lapply
    read.table(text=data,fill=T,stringsAsFactors = F)
  }
  siteTS <- lapply(lapply(lapply(allFiles,readLines),grep,
    pattern='^\\s{4}.[0-9]{4}\\s',value=T),temp)
  
  recLen <- recCor <- matrix(NA,ncol=length(siteTS),nrow=length(siteTS))
  
  for (i in 1:length(siteTS)) {
    recLen[i,i] <- nrow(siteTS[[i]])
    idata <- abs(siteTS[[i]][,1:2])
    for (j in 1:i) {
      jdata <- abs(siteTS[[j]][,1:2])
      overlap <- intersect(idata[,1],jdata[,1])
      recLen[i,j] <- recLen[j,i] <- length(overlap)
      if (length(overlap)==0) {next}
      ijdata <- idata[which(is.element(idata[,1],overlap)),2]
      jidata <- jdata[which(is.element(jdata[,1],overlap)),2]
      if (length(unique(ijdata))==1|length(unique(jidata))==1) {next}
      recCor[i,j] <- recCor[j,i] <- cor(ijdata,jidata)
    }
  }
  
  result <- list(
    sites=EXP_SiteID,
    Y=Y,
    AEP=AEP,
    X=X,
    LP3f=LP3f,
    LP3k=LP3k,
    BasChars=BasChars,
    MSEGR=unique(EXP_MSEGR[,2]),
    recLen=recLen,
    recCor=recCor
  )
  return(result)
  
}