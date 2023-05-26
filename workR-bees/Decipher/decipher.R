## ---------------------------
## Script name: decipher.R
##
## Purpose of script: Replaces MessageProcessor.R in a more concise way to
##    process incoming messages from deckboxes
##
## Date Created: 2023-05-25
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
## Load necessary packages
library(logr)
library(lubridate)
library(RMyDataTrash)
library(RMySQL)
library(yaml)
## Load necessary scripts
scriptlist=c()
for(i in scriptlist){
  source(paste0("directory",i))
}
## Turn off verbose logging
options("logr.notes"=FALSE)
## Create a logfile for this run
filename="directory/decipher.log"
logr::log_open(filename)
## Establish a read-only connection to the database
conn=RMySQL::dbConnect(
  MySQL(),
  user=read_yaml("dev_db_all.yml")$drone$User,
  password=read_yaml("dev_db_all.yml")$drone$Password,
  dbname=read_yaml("dev_db_all.yml")$drone$Database,
  host=read_yaml("dev_db_all.yml")$drone$Host,
  port=read_yaml("dev_db_all.yml")$drone$Port
)
## Download all existing pairs from the database
rockblocks=dbGetQuery(
  conn=conn,
  statement="SELECT * FROM vessel_sat"
)
## Download all new transmissions
transmissions=dbGetQuery(
  conn=conn,
  #statement="SELECT * FROM RAW_TRANSMISSIONS"
  statement="SELECT * FROM fastapi_test"
)
## Close the database connection
dbDisconnect(conn)
## Determine which transmissions are real
rockblocks$key=paste(rockblocks$SERIAL_NUMBER,rockblocks$HARDWARE_ADDRESS,sep="_")
transmissions$key=paste(transmissions$serial,transmissions$imei,sep="_")
transmissions$real=transmissions$key%in%rockblocks$key
## Assign each real transmission to a vessel
transmissions$vessel=NA
for(i in 1:nrow(transmissions)){
  if(transmissions$real[i]==TRUE){
    transmissions$vessel[i]=rockblocks$VESSEL_NAME[which(rockblocks$key==transmissions$key[i])]
  }
}
## Assign each vessel to a decode group based on its deckbox software
## Ultimately, all vessels should end up in the new_standard list
old_mobile=c('CHARGER','KAITLYN VICTORIA','LINDA MARIE','MARY K','MORAGH K',
             'SAO PAULO','TOM SLAUGHTER')
old_fixed=c('DEVOCEAN','FREMANTLE DOCTOR','LINA ROSE')
new_moana=c('CHRISTI CAROLINE','CHRISTY','DONNA MARIE','LINA ROSE','PHYLISS P',
            'RACHEL LEAH','RYAN JOSEPH','VIRGINIA MARIE','VIRGINIA MARISE')
new_standard=c('ADVENTURE','EXCALIBUR','SEA WATCHER I','SEA WATCHER II',
               'TENACIOUS II','TERRI ANN')
## Create empty dataframes to store parsed messages
new_messages=data.frame(
  raw=as.character(),
  message_version=as.character(),
  message_type=as.character(),
  latitude=as.numeric(),
  longitude=as.numeric(),
  timestamp=as.character(),
  haul_duration=as.numeric(),
  logger_sn=as.character(),
  logger_mfg=as.character(),
  logger_batt=as.character(),
  uptime=as.numeric(),
  gps_lock=as.character(),
  gps_sats=as.numeric(),
  last_download=as.character(),
  last_serial=as.character(),
  time_since_last_seen=as.numeric(),
  time_since_last_internet=as.numeric()
)
new_data=data.frame(
  raw=as.character(),
  start=as.character(),
  end=as.character(),
  depth=as.numeric(),
  depth_min_85=as.numeric(),
  depth_max_85=as.numeric(),
  depth_std=as.numeric(),
  temp=as.numeric(),
  temp_min_85=as.numeric(),
  temp_max_85=as.numeric(),
  temp_std=as.numeric()
)
## Loop over the messages and decode them
for(i in 1:nrow(transmissions)){
  if(is.na(transmissions$vessel[i])==FALSE){
    if(transmissions$vessel[i]%in%old_mobile){
      cat("old mobile \n")
    } else {
      if(transmissions$vessel[i]%in%old_fixed){
        cat("old fixed \n")
      } else {
        if(transmissions$vessel[i]%in%new_moana){
          cat("new moana \n")
        } else {
          if(transmissions$vessel[i]%in%new_standard){
            cat("new standard \n")
          }
        }
      }
    }
  }
}
## Create a write connection to the database
conn=RMySQL::dbConnect(
  MySQL(),
  user=read_yaml("dev_db_all.yml")$worker$User,
  password=read_yaml("dev_db_all.yml")$worker$Password,
  dbname=read_yaml("dev_db_all.yml")$worker$Database,
  host=read_yaml("dev_db_all.yml")$worker$Host,
  port=read_yaml("dev_db_all.yml")$worker$Port
)
## Load the new data into the appropriate database tables
## Disconnect from the database
dbDisconnect(conn)
## Create a delete connection to the database
conn=RMySQL::dbConnect(
  MySQL(),
  user=read_yaml("dev_db_all.yml")$queen$User,
  password=read_yaml("dev_db_all.yml")$queen$Password,
  dbname=read_yaml("dev_db_all.yml")$queen$Database,
  host=read_yaml("dev_db_all.yml")$queen$Host,
  port=read_yaml("dev_db_all.yml")$queen$Port
)
## Remove any successfully processed records from the RAW_TRANSMISSIONS table
## Remove any empty or indecipherable records from the RAW_TRANSMISSIONS table
## Disconnect from the database
dbDisconnect(conn)