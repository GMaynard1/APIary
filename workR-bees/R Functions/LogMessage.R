## ---------------------------
## Script name: LogMessage.R
##
## Purpose of script: Allows easy creation of log files
##
## Date Created: 2023-05-16
##
## Software code created by U.S. Government employees is
## not subject to copyright in the United States
## (17 U.S.C. §105).
##
## Email: george.maynard@noaa.gov
##
## ---------------------------
## Notes:
##
## ---------------------------
LogMessage=function(messageText,data=NULL,logfile){
  newline=paste0(
    Sys.time(),
    " | ",
    messageText,
    " | ",
    data,
    "\n"
  )
  logr::log_print(
    newline
  )
}