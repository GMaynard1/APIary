## ---------------------------
## Script name: MessageProcessor.R
##
## Purpose of script: Calls the series of scripts necessary to process 
##    incoming messages from deckboxes
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
library(logr)
library(lubridate)
library(RMyDataTrash)
library(RMySQL)
library(yaml)
scriptlist=dir("app/utils/R Functions")
for(i in scriptlist){
  if(i!="MessageProcessor.R"){
    source(paste0("app/utils/R Functions/",i))
  }
}
options("logr.notes"=FALSE)
## Create a logfile for this run
filename=paste0("/home/george/logs/apiary/decodes_",format(Sys.time(), "%Y%m%d_%H%M"),".log")
logr::log_open(filename)
## Check to make sure that the serial / imei pairing is real -----
## Connect to database
conn = RMySQL::dbConnect(
  MySQL(),
  user=read_yaml("app/utils/dev_db_all.yml")$Intranet$User,
  password=read_yaml("app/utils/dev_db_all.yml")$Intranet$Password,
  dbname=read_yaml("app/utils/dev_db_all.yml")$Intranet$Database,
  host=read_yaml("app/utils/dev_db_all.yml")$Intranet$Host,
  port=read_yaml("app/utils/dev_db_all.yml")$Intranet$Port
)
## Download all existing pairs from the database
rockblocks=dbGetQuery(
  conn=conn,
  statement="SELECT * FROM vessel_sat"
)
## Download all new transmissions
transmissions=dbGetQuery(
  conn=conn,
  statement="SELECT * FROM fastapi_test"
)
transmissions$real=paste(transmissions$serial,transmissions$imei,sep="_")%in%
  paste(rockblocks$SERIAL_NUMBER,rockblocks$HARDWARE_ADDRESS,sep="_")
transmissions$vessel=ifelse(
  transmissions$real,
  rockblocks$VESSEL_NAME[which(
    paste(rockblocks$SERIAL_NUMBER,rockblocks$HARDWARE_ADDRESS,sep="_") ==
      paste(transmissions$serial,transmissions$imei,sep="_")
  )]
)
old_mobile=c('CHARGER','KAITLYN VICTORIA','LINDA MARIE','MARY K','MORAGH K',
  'SAO PAULO','TOM SLAUGHTER')
old_fixed=c('DEVOCEAN','FREMANTLE DOCTOR','LINA ROSE')
new_emolt=c('CHRISTI CAROLINE','CHRISTY','DONNA MARIE','LINA ROSE','PHYLISS P',
  'RACHEL LEAH','RYAN JOSEPH','VIRGINIA MARIE','VIRGINIA MARISE')
new_standard=c('ADVENTURE','EXCALIBUR','SEA WATCHER I','SEA WATCHER II',
  'TENACIOUS II','TERRI ANN')
## Create an empty dataframe to store parsed messages
new_messages=data.frame(
  latitude=as.numeric(),
  longitude=as.numeric(),
  type=as.character(),
  mean_depth=as.numeric(),
  range_depth=as.numeric(),
  std_temp=as.numeric(),
  mean_temp=as.numeric(),
  soak_time=as.numeric(),
  mean_time=as.numeric()
)
## Loop over real transmissions for processing
for(i in 1:nrow(transmissions)){
  ## If the transmission comes from a real device
  if(transmissions$real[i]==TRUE){
    ## And the data string isn't blank
    if(transmissions$data[i]!=""){
      ## Decode it
      LogMessage(
        messageText="Decoding",
        data=paste(transmissions[i,],collapse=" "),
        logfile=filename
      )
      x=wkb::hex2raw(transmissions$data[i])
      ## and remove embedded nulls
      x=x[!x=="00"]
      datastring=rawToChar(
        as.raw(
          strtoi(
            x,
            16L
          )
        )
      )
      if(datastring!="1111111111111111"){
        ## Attempt to parse it
        LogMessage(
          messageText="Attempting to parse",
          data=datastring
        )
        result=tryCatch(
          {
            parsed=old_fixed_parse(datastring,ymd_hms(transmissions$transmit_time[i]))
          },
          error=function(cond){
            LogMessage(
              messageText="ERROR: parsing failure old_fixed",
              data=cond
            )
            return(NULL)
          },
          warning=function(cond){
            LogMessage(
              messageText="Warning: potential parsing issue old_fixed",
              data=cond
            )
            return(NULL)
          },
          finally={
            message("Parsing attempt complete: old_fixed")
          }
        )
        new_messages=rbind(new_messages,result)
        result=tryCatch(
          {
            parsed=old_mobile_parse(datastring,ymd_hms(transmissions$transmit_time[i]))
          },
          error=function(cond){
            LogMessage(
              messageText="ERROR: parsing failure old_mobile",
              data=cond
            )
            return(NULL)
          },
          warning=function(cond){
            LogMessage(
              messageText="Warning: potential parsing issue old_mobile",
              data=cond
            )
            return(NULL)
          },
          finally={
            message("Parsing attempt complete: old_mobile")
          }
        )
        new_messages=rbind(new_messages,result)
        
      } else {
        LogMessage(
          messageText="Test message identified. No further processing required."
        )
      }
    } else {
      LogMessage(
        messageText="Empty transmission: discarded",
        data=paste(transmissions[i,],collapse=" ")
      )
    }
  } else {
    LogMessage(
      messageText="Unidentified transmission: discarded",
      data=paste(transmissions[i,],collapse=" ")
    )
  }
}
## Discard any messages with a bad longitude value
new_messages=subset(new_messages,new_messages$LONGITUDE<=-65)
## Discard any messages with a bad temperature
new_messages=subset(new_messages,new_messages$mean_temp!=0)
## Disconnect from the database
dbDisconnectAll()
logr::log_close()
