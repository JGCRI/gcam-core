# SET THESE TWO VARIABLES
rundir <-  "rgcam-processing-code/Level2/"
myname <-   "L2_All.R"

scriptlist <- list.files( rundir, pattern=".R$" )
for( i in scriptlist ) {
  if( i != myname )
  source( paste( rundir, "/", i, sep="" ) )
}                   
                                         
