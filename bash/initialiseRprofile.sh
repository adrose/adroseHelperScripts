#!/bin/bash

# This script is oging to be used to create a .Rprofile file
# which will log and store the output logs in the adroseHelperScripts
# R log directory

# Declare any statics
myHome=`echo $HOME`
outputFile="${myHome}/.Rprofile"


# only run if the output file is not there
if [ -f ${outputFile} ] ; then
  echo "your ${outputFile} already exists"
  exit 0 ; 
fi

# Now write everything to our new file
printf ".First <- function(){\n
  sourceDir <- paste(system('echo @HOME', intern=T), paste('/adroseHelperScripts/R/'), sep='')\n
  source(paste(sourceDir, 'afgrHelpFunc.R', sep=''))\n
  options(repos = c(CRAN = 'http://cran.rstudio.com/'),browserNLdisabled = TRUE,deparse.max.lines = 2)\n
}\n" > ${outputFile}
printf "locStart  <-  TeachingDemos::txtStart\n
locStop  <-  TeachingDemos::txtStop\n
locR2txt  <-  TeachingDemos:::R2txt\n
.e.  <-  new.env()\n
.e.@R2txt.vars <- new.env()\n
environment(locStart) <- .e.\n
environment(locStop) <- .e.\n
environment(locR2txt) <- .e.\n
body(locStart)[[length(body(locStart))-1]] <-\n 
   substitute(addTaskCallback(locR2txt, name=*locR2txt*))\n
body(locStop)[[2]] <- \n
   substitute(removeTaskCallback(*locR2txt*))\n
logDir <- paste(system(*echo @HOME*, intern=T), paste(*/adroseHelperScripts/R/logDir/*), sep=**)\n
start=function(logDir){\n
                op <- options()\n
                locStart(file.path(logDir,format(Sys.time(), *#Y_#m_#d_#H_#M_#S.txt*)),\n
                         results=FALSE)\n
                options(op)}\n
start(logDir)\n" >> ${outputFile}
echo "paste('Logging in', paste(logDir), paste(format(Sys.time(),*#Y_#m_#d_#H_#M_#S.txt*)), sep=**)" >> ${outputFile}

# Now turn our @'s into $'s
sed -i -e "s/@/$/g" ${outputFile}
sed -i -e "s/*/'/g" ${outputFile}
sed -i -e "s/#/%/g" ${outputFile}

# Now create our log directory
mkdir -p "${myHome}/adroseHelperScripts/R/logDir"


# Thats it!
echo "All Done!"
echo "Happy R-ing!"
exit 0 
