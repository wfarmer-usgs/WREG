#' Import Data from PeakFQ Output
#' 
#' @description
#' The \code{importPeakFQ_oneFile} function reads output from PeakFQ when the 
#' output is contained in a a single EXP file and a single PRT file.
#' 
#' @param pfqPath A directory that contains a single EXP file and a single PRT 
#'   file, both from peakFQ and both containing the same gage IDs.
#' @param destination The directory name that will be created to hold all 
#'  of the output from this function.
#' 
#' @details
#' This function allows users to parse the output from peakFQ when all 
#' sites are contained in a single EXP and a single PRT file. This function 
#' will create new files, with a single EXP and a single PRT for each site, 
#' that can then be imported using \code{\link{importPeakFQ}}.
#' 
#' WARNING: This function will create new files on your computer.
#' 
#' @return Creates a directory structure in \code{destination} that contains a 
#'  directory for each station number in the EXP file. Each gage subdirectory
#'  will contain the resulting single-site EXp and PRT files.
#' 
#' @examples
#' peakFQdir <- paste0(
#'   file.path(system.file("exampleDirectory", package = "WREG"),
#'     "pfqImport_oneFile"))
#' importedData <- importPeakFQ_oneFile(pfqPath = peakFQdir)
#' 
#'@export
importPeakFQ_oneFile <- function(pfqPath, destination = "peakFQ_extract") {
  # Developed by William Farmer, 20 August 2019)

dir.create(destination)
# Build multiple EXPs
bigEXP <- list.files("*\\.EXP$",
  path = pfqPath, recursive = FALSE, full.names = TRUE)[1]
con <- file(bigEXP, "r")
allSites <- list()
j <- 0
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  
  if (grepl("Station - ", line)) {
    j <- j + 1
    if (exists("conOut")) {
      close(conOut)
    }
    m <- gregexpr('[0-9]+',line)
    siteNo <- unlist(regmatches(line, m))[1]
    allSites[j] <- siteNo
    dir.create(file.path(destination, siteNo))
    fileName <- file.path(destination, siteNo, paste0(siteNo, ".EXP"))
    conOut <- file(fileName, "w+")
  }
  writeLines(text = line, con = conOut)
}
close(conOut)
close(con)
allSites <- unlist(allSites)
nSites <- length(allSites)

bigPRT <- list.files("*\\.PRT$",
  path = pfqPath, recursive = FALSE, full.names = TRUE)[1]
con <- file(bigPRT, "r")
result <- list()
currentLine <- 1
siteNo <- NA
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  result[[currentLine]] <- line
  currentLine <- currentLine + 1
  if (grepl("Station - ", line)) {
    # Check site number
    m <- gregexpr('[0-9]+',line)
    if (is.na(siteNo)) {
      siteNo <- unlist(regmatches(line, m))[1]
    }
    currentSite <- unlist(regmatches(line, m))[1]
    if (siteNo != currentSite) {
      # If a new site, write it out
      fileName <- file.path(destination, siteNo, paste0(siteNo, ".PRT"))
      conOut <- file(fileName, "w+")
      result <- unlist(result)
      for (i in 1:max(which(result == "1"))) {
        writeLines(text = result[i], con = conOut)
      }
      close(conOut)
      # Initialize next site
      newRes <- list()
      newRes[[1]] <- "1"
      currentLine <- 2
      for (i in (max(which(result == "1")) + 1):length(result)) {
        if (result[i] == "") {
          next
        }
        newRes[[currentLine]] <- result[i]
        currentLine <- currentLine + 1
      }
      result <- newRes
      siteNo <- currentSite
    }
  }
}
# Write last site
fileName <- file.path(destination, siteNo, paste0(siteNo, ".PRT"))
conOut <- file(fileName, "w+")
result <- unlist(result)
for (i in 1:max(which(result == "1"))) {
  writeLines(text = result[i], con = conOut)
}
close(conOut)
close(con)
}