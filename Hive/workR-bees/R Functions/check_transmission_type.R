checkTransmissionType=function(data){
  if(nchar(data)<=45){
    return("SHORT_STATUS")
  } else {
    if(nchar(data)>=60){
      return("SUMMARY_DATA")
    } else {
      return("UNKNOWN")
    }
  }
}
