## ---------------------------
## Script name: old_mobile_decode.R
##
## Purpose of script: To decode old mobile format rockblock transmissions
##
## Date Created: 2023-05-26
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
old_mobile_decode=function(data,transmit_time){
  ## Decode the datastring
  datastring=rawToChar(
    as.raw(
      strtoi(
        wkb::hex2raw(data),
        16L
      )
    )
  )
  ## Extract Latitude
  raw=strsplit(
    x=datastring,
    split=","
  )[[1]][1]
  lat=as.numeric(substr(raw,1,2))+as.numeric(substr(raw,3,nchar(raw)))/60
  
  ## Extract Longitude
  raw=strsplit(
    x=datastring,
    split=","
  )[[1]][2]
  lon=(as.numeric(substr(raw,1,2))+as.numeric(substr(raw,3,nchar(raw)))/60)*-1
  
  ## Round latitude and longitude
  lat=round(lat,5)
  lon=round(lon,5)
  
  if(nchar(data)==90){
    ## Extract mean depth (m)
    mean_depth=as.numeric(substr(strsplit(datastring,",")[[1]][3],1,3))
    
    ## Extract range depth (m)
    range_depth=as.numeric(substr(strsplit(datastring,",")[[1]][3],4,6))
    
    ## Extract soak time (minutes)
    soak_time=as.numeric(substr(strsplit(datastring,",")[[1]][3],7,9))
    
    ## Mean time is the temporal midpoint of the haul and is estimated as the 
    ## transmission time - the soak time / 2
    mean_time=ymd_hms(transmit_time)-minutes(round(soak_time/2,0))
    
    ## Extract mean temperature
    mean_temp=as.numeric(substr(strsplit(datastring,",")[[1]][3],10,13))/100
    
    ## Extract the standard deviation of temperature
    std_temp=as.numeric(substr(strsplit(datastring,",")[[1]][3],14,17))/100
    
    depth_min_85=mean_depth-range_depth/2
    depth_max_85=mean_depth+range_depth/2
    
    message_type="SUMMARY DATA"
    
  } else {
    mean_depth=NA
    range_depth=NA
    soak_time=NA
    mean_time=transmit_time
    mean_temp=NA
    std_temp=NA
    depth_min_85=NA
    depth_max_85=NA
    message_type="SHORT STATUS"
  }
  
  ## Record all available information and return
  new_messages=data.frame(
    raw=data,
    message_version="OLD MOBILE",
    message_type=message_type,
    latitude=lat,
    longitude=lon,
    timestamp=mean_time,
    haul_duration=soak_time,
    logger_sn=NA,
    logger_mfg=NA,
    logger_batt=NA,
    uptime=NA,
    gps_lock=NA,
    gps_sats=NA,
    last_download=NA,
    last_serial=NA,
    time_since_last_seen=NA,
    time_since_last_internet=NA
  )
  new_data=data.frame(
    raw=data,
    start=ymd_hms(transmit_time)-minutes(soak_time),
    end=transmit_time,
    depth=mean_depth,
    depth_min_85=depth_min_85,
    depth_max_85=depth_max_85,
    depth_std=NA,
    temp=mean_temp,
    temp_min_85=NA,
    temp_max_85=NA,
    temp_std=std_temp
  )
  return(list(new_messages,new_data))
}