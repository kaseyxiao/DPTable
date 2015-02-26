parse_args <- function(args) {
  data.names <- c('adult', 'kosarak', 'aol', 'MSNBC'
                  , 'Data1', 'Data2', 'Data3', 'Data4'
                  , 'test', 'retail50', 'accidents50'
                  , 'Train2', 'Train4', 'Train1', 'Train3')
  xxx <- paste(unlist(args), collapse = ' ')
  listoptions <- unlist(strsplit(xxx,'-'))[-1]
  options.args <- sapply(listoptions, function(x) {
    unlist(strsplit(x, ' '))[-1]
  })
  options.names <- sapply(listoptions, function(x) {
    option <-  unlist(strsplit(x, ' '))[1]
  })
  names(options.args) <- unlist(options.names)
  #print(options.args)
  specs <- list()
  if("f" %in% names(options.args)) {
    if (options.args['f'] %in% data.names) {
      specs$data.name <- as.character(options.args['f'])   
    }else {
      stop("not valid data file")
    }
  }else{
    stop("no filename")
  }
  if ("e1" %in% names(options.args)) {
    specs$epsilon.1 <- as.numeric(options.args['e1'])    
  }else{
    stop("no epsilon.1")
  }
  if ("e2" %in% names(options.args)) {
    specs$epsilon.2 <- as.numeric(options.args['e2'])    
  }else{
    stop("no epsilon.2")
  }
  if ("CV" %in% names(options.args)) {
    specs$CV <- as.numeric(options.args['CV'])
  }else{
    specs$CV <- 0.2
  }
  if ("dir" %in% names(options.args)) {
    specs$dir <- as.character(options.args['dir'])
  }else{
    specs$dir <- NULL
  }
#   print(specs$CV)
  
  if ("nrun" %in% names(options.args)) {
    specs$nrun <- as.numeric(options.args['nrun'])
  }else{
    specs$nrun <- 5
  }

  if (('s' %in% names(options.args)) && (options.args['s']==TRUE)){
    specs$flag.sample <- TRUE
  }else {
    specs$flag.sample <- FALSE
  }
  
  if (('q' %in% names(options.args)) && (options.args['q'] == FALSE)) {
    print("do not process query")
    specs$flag.process.query <- FALSE
  }else {
    specs$flag.process.query <- TRUE
  }
  
  return(specs)
}
