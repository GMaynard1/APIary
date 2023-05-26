## ---------------------------
## Script name: HexDecoder.R
##
## Purpose of script: To decode hex messages transmitted from deckboxes
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
HexDecoder=function(data){
  datastring=rawToChar(
    as.raw(
      strtoi(
        wkb::hex2raw(data),
        16L
      )
    )
  )
  return(datastring)
}