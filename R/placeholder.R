#--Custom function to call HUD Crosswalk API--
# HUDKey<-readRDS(".\\Geocode\\HUD_API_Key.rds") #API key for making call

HUD_tract2zip<- function(GEOID){ #custom function to convert tracts to zip codes

  tract_san<- paste(str_trim(as.character(GEOID), side = "both")) #sanitizes input track number

  URL<-paste("https://www.huduser.gov/hudapi/public/usps?type=7&query=", tract_san, sep="") #build URL

  call<-try(GET(URL, add_headers(Authorization=paste("Bearer ", as.character(HUDKey$key)))),silent = TRUE) #try to make call

  cont<-try(content(call), silent = TRUE) #parse returned data

  res<-data.frame(tract=integer(),zip=integer(),res_ratio=integer(),bus_ratio=integer(),oth_ratio=integer(),tot_ratio=integer()) #build df

  if (length(cont$data) == 0){ #check to see if results exist - if not fill results with NA

    res<-data.frame(tract=tract_san,
                    zip=NA,
                    res_ratio=NA,
                    bus_ratio=NA,
                    oth_ratio=NA,
                    tot_ratio=NA)

  } else {

    for(i in seq(1,length(cont$data$results),1)){ #itterate over results and append to df

      int<-data.frame(tract=tract_san,
                      zip=cont$data$results[[i]][["geoid"]],
                      res_ratio=cont$data$results[[i]][["res_ratio"]],
                      bus_ratio=cont$data$results[[i]][["bus_ratio"]],
                      oth_ratio=cont$data$results[[i]][["oth_ratio"]],
                      tot_ratio=cont$data$results[[i]][["tot_ratio"]])
      res<-rbind.data.frame(res, int) #appends rows to df

    }
    return(res) #returns df as output of function
  }
}
#------


#--Custom function to call HUD Crosswalk API--
HUDKey<-readRDS(".\\Geocode\\HUD_API_Key.rds") #API key for making call
HUD_zip2tract<- function(ZIP){ #custom function to convert tracts to zip codes
  ZIP_san<- paste(str_trim(as.character(ZIP), side = "both")) #sanitizes input track number
  URL<-paste("https://www.huduser.gov/hudapi/public/usps?type=1&query=", ZIP_san, sep="") #build URL
  call<-try(GET(URL, add_headers(Authorization=paste("Bearer ", as.character(HUDKey$key)))),silent = TRUE) #try to make call
  cont<-try(content(call), silent = TRUE) #parse returned data
  res<-data.frame(zip=integer(),tract=integer(),res_ratio=integer(),bus_ratio=integer(),oth_ratio=integer(),tot_ratio=integer()) #build df
  if (length(cont$data) == 0){ #check to see if results exist - if not fill results with NA
    res<-data.frame(zip=ZIP_san,
                    tract=NA,
                    res_ratio=NA,
                    bus_ratio=NA,
                    oth_ratio=NA,
                    tot_ratio=NA)
  } else{ for(i in seq(1,length(cont$data$results),1)){ #itterate over results and append to df
    int<-data.frame(zip=ZIP_san,
                    tract=cont$data$results[[i]][["geoid"]],
                    res_ratio=cont$data$results[[i]][["res_ratio"]],
                    bus_ratio=cont$data$results[[i]][["bus_ratio"]],
                    oth_ratio=cont$data$results[[i]][["oth_ratio"]],
                    tot_ratio=cont$data$results[[i]][["tot_ratio"]])
    res<-rbind.data.frame(res, int) #appends rows to df
  }
    return(res) #returns df as output of function
  }
}
#------
