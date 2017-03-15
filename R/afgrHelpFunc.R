## R functions I commonly use


##summarySE##
# Used to produce group bsed metrics 
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}

##multiplot##
# Used to put multiple graphs on a single pdf 
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

##loadLibraries##
# This function written by afgr will take a list of package names and load and source them if they are not found
#loadLibs <- function(
install_load <- function (package1, ...)  {   

   # convert arguments to vector
   packages <- c(package1, ...)

   # start loop to determine if each package is installed
   for(package in packages){

       # if package is installed locally, load
       if(package %in% rownames(installed.packages()))
          do.call('library', list(package))

       # if package is not installed locally, download, then load
       else {
          chooseCRANmirror(graphics=FALSE, ind = 111)
          install.packages(package,dependencies=TRUE, repos='http://lib.stat.cmu.edu/R/CRAN/')
          do.call("library", list(package))
       }
   } 
}



##Identify Points##
# This function can be used to identify points on a plot
# Needs x (and y if y is plotted) and a n to be called correctly 
identifyPch <- function(x, y = NULL, n = length(x), pch = 19, ...)
{
    xy <- xy.coords(x, y); x <- xy$x; y <- xy$y
    sel <- rep(FALSE, length(x)); res <- integer(0)
    while(sum(sel) < n) {
        ans <- identify(x[!sel], y[!sel], n = 1, plot = FALSE, ...)
        if(!length(ans)) break
        ans <- which(!sel)[ans]
        points(x[ans], y[ans], pch = pch)
        sel[ans] <- TRUE
        res <- c(res, ans)
    }
    res
}


##Cbind to empty dataframe##
# This function can be used to cbind a vecotr to an empty data frame
cbind.all <- function (...) 
{
    nm <- list(...)
    nm <- lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow))
    do.call(cbind, lapply(nm, function(x) rbind(x, matrix(, n - 
        nrow(x), ncol(x)))))
}


## rc.ind ##
# This function mirros matlab's ind2sub function
# Inputs are M = dataframe or matrix, ind = linear index
rc.ind <- function(M, ind) c(row(M)[ind], col(M)[ind] )


## detachAllPackages ##
# This function can be used to rm all loaded packages
# except for those base packages 
# It takes no inputs 
# Simply call:
# detachAllPackages()
detachAllPackages <- function() {

  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")

  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]

  package.list <- setdiff(package.list,basic.packages)

  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)

}

# Declare a function which will return a matrix
# from the output of strsplit
# this will only work for input data that has
# equal number of columns though 
strSplitMatrixReturn <- function(charactersToSplit, splitCharacter){
  # Make sure we are dealing with characters
  classCheck <- class(charactersToSplit)
  if(identical(classCheck, "character")=="FALSE"){
    charactersToSplit <- as.character(charactersToSplit)
  }

  # Now we need to find how many columns our output will have 
  colVal <- length(strsplit(charactersToSplit[1], split=splitCharacter)[[1]])
  
  # Now return the matrix of characters!
  output <- matrix(unlist(strsplit(charactersToSplit, split=splitCharacter)), ncol=colVal, byrow=T)

  # Now return the output
  return(output) 
}

# Create a function which will compute the mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Now create a function which will return the coordinates (x,y) | (x,y,z) from a 
# logical matrix, simulating the which statement- which returns a linear index
multi.which <- function(A){
    if ( is.vector(A) ) return(which(A))
    d <- dim(A)
    T <- which(as.logical(A)) - 1
    nd <- length(d)
    t( sapply(T, function(t){
        I <- integer(nd)
        I[1] <- t %% d[1]
        sapply(2:nd, function(j){
            I[j] <<- (t %/% prod(d[1:(j-1)])) %% d[j]
        })
        I
    }) + 1 )
}


## Create a fucntion which will log and store everything in our log directory
.Logger <- function(){

    # copy local versions of the txtStart,
    locStart  <-  TeachingDemos::txtStart
    locStop  <-  TeachingDemos::txtStop
    locR2txt  <-  TeachingDemos:::R2txt

    # creat a local environment and link it to each function
    .e.  <-  new.env()
    .e.$R2txt.vars <- new.env()
    environment(locStart) <- .e.
    environment(locStop) <- .e.
    environment(locR2txt) <- .e.


    # reference the local functions in the calls to `addTaskCallback`
    # and `removeTaskCallback`
    body(locStart)[[length(body(locStart))-1]] <- 
        substitute(addTaskCallback(locR2txt, name='locR2txt'))
    body(locStop)[[2]] <- 
        substitute(removeTaskCallback('locR2txt'))


    # Now declare our logDir ** This might require some debugging**
    logDir <- paste(system('echo $HOME', intern=T), paste('/adroseHelperScripts/R/logDir/'), sep='')

    list(start=function(logDir){
                op <- options()
                locStart(file.path(logDir,format(Sys.time(), "%Y_%m_%d_%H_%M_%S.txt")),
                         results=FALSE)
                options(op)
    }, stop = function(){
                op <- options()
                locStop()
                options(op)
    })

}()

# Create a fucntion which will find the minimum value needed for a signifiacnt
# correlation given a sample size
getRVal <- function(cohortSize){
  vals <- seq(.0001,1,.0001)
  index <- 1
  pVal <- 1
  while(pVal > .05) {
      i <- vals[index]
      index <- index + 1
      n     <- cohortSize            # length of vector
      rho   <- i                     # desired correlation = cos(angle)
      theta <- acos(rho)             # corresponding angle
      x1    <- rnorm(n, 1, 1)        # fixed given data
      x2    <- rnorm(n, 2, 0.5)      # new random data
      X     <- cbind(x1, x2)         # matrix
      Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)
    
      Id   <- diag(n)                               # identity matrix
      Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
      P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
      x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
      Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
      Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1
    
      x <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
      cor(x1, x)  # check correlation = rho
      pVal <- cor.test(x1, x)$p.value
  }
  return(i)
}
