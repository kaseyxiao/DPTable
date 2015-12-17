# load library
load_depends <- function() {
  options(message=-1)

  flag.install.junctiontreealgo = FALSE
  library(R.oo)
  library(methods)
  library(R.matlab)
  library(data.table)
  library(distr)
  library(entropy)
  library(plyr)
  library(FNN)
  library(e1071)
  library(stringr)
  library(caret)
  if(flag.install.junctiontreealgo){
    source("http://bioconductor.org/biocLite.R");
    biocLite(c("graph","RBGL","Rgraphviz"))
  }
  library(gRain)
  library(Rgraphviz)
  library(gRim)
  library(gRbase)
  options(message=0)
}
suppressMessages(load_depends())

# pull all files and folders (including subfolders) into a character vector
# keep ONLY the files that END with ".R" or ".r"
load_source <- function() {
    r.scripts <- list.files( path = "R/"
                         , pattern = ".*\\.[R]$"
                         , recursive = TRUE )
    ignore.files = c("init.R", "preprocess.R", "laplace.R", "objective.R")
    # loop through and source() except the ones in ignore.files
    for ( i in r.scripts ) {
      # if (i != "init.R") {
      if (!(i %in% ignore.files)) {
        print(paste("loading", i))
        source( paste("R", i, sep="/") )        
      }    
    }
}

load_source()
options(scipen = 10)
if (exists("matlab")) {
  close(matlab)
  remove(matlab)
}

