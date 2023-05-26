## ---------------------------
## Script name: old_mobile_parse.R
##
## Purpose of script: parse old mobile gear sets
##
## Date Created: 2023-05-19
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
old_mobile_parse=function(datastring,transmit_time){
  ## Extract latitude
  raw=strsplit(
    x=datastring,
    split=","
  )[[1]][1]
  lat=as.numeric(substr(raw,1,2))+as.numeric(substr(raw,3,nchar(raw)))/60
  
  ## Extract longitude
  raw=strsplit(
    x=datastring,
    split=","
  )[[1]][2]
  lon=(as.numeric(substr(raw,1,2))+as.numeric(substr(raw,3,nchar(raw)))/60)*-1
  
  ## Round latitude and longitude
  lat=round(lat,5)
  lon=round(lon,5)
  
  ### Check transmission type
  message_type=checkTransmissionType(datastring)
  if(message_type!="UNKNOWN"){
    if(message_type=="SHORT_STATUS"){
      return(data.frame(
        latitude=lat,
        longitude=lon,
        type=message_type,
        mean_depth=NA,
        range_depth=NA,
        std_temp=NA,
        mean_temp=NA,
        soak_time=NA,
        mean_time=NA,
        processor="old_mobile",
        message=datastring
      ))
    } else {
      ## Extract mean depth (m)
      mean_depth=as.numeric(substr(strsplit(datastring,",")[[1]][3],1,3))
      
      ## Extract range depth (m)
      range_depth=as.numeric(substr(strsplit(datastring,",")[[1]][3],4,6))
      
      ## Extract soak time (minutes)
      soak_time=as.numeric(substr(strsplit(datastring,",")[[1]][3],7,9))
      
      ## Mean time is the temporal midpoint of the haul and is estimated as the 
      ## transmission time - the soak time / 2
      mean_time=transmit_time-minutes(round(soak_time/2,0))
      
      ## Extract mean temperature
      mean_temp=as.numeric(substr(strsplit(datastring,",")[[1]][3],10,13))/100
      
      ## Extract the standard deviation of temperature
      std_temp=as.numeric(substr(strsplit(datastring,",")[[1]][3],14,17))/100
      return(
        data.frame(
          LATITUDE=lat,
          LONGITUDE=lon,
          type="DATA",
          mean_depth=mean_depth,
          range_depth=range_depth,
          std_temp=std_temp,
          mean_temp=mean_temp,
          soak_time=soak_time,
          mean_time=mean_time,
          processor="old_mobile",
          message=datastring
        )
      )
    }
  } else {
    return(
      data.frame(
        LATITUDE=NA,
        LONGITUDE=NA,
        type=NA,
        mean_depth=NA,
        range_depth=NA,
        std_temp=NA,
        mean_temp=NA,
        soak_time=NA,
        mean_time=NA,
        processor=NA,
        message=datastring
      )
    )
  }
}