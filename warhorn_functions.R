library(jsonlite)
library(httr)

warhorn_creds = read_json("warhorn_creds.json")

submit_warhorn<- function(q,v){
  r <- POST("https://warhorn.net/graphql"
            , add_headers(
              Authorization = paste("bearer", warhorn_creds$user_access_token, sep = " ")
            )
            , body = list(
              query = q,
              variables = v
            )
            , encode = "json")
 return(r)
}



get_event_id<-function(event_slug){
  event_query = "
  query ($slug: String!) {
  event(slug: $slug){
    id
    }
  }"
  
  
  r = submit_warhorn(event_query,list(slug = event_slug))
  
  if(r$status_code==200){
    data = content(r)
    return(data$data$event$id)
  } 
  else{
  return(paste("ERROR: failed to get event id", data$errors[[1]]$message, sep=": "))
  }
}

get_gamesystem<-function(l){
  
  needed_fields = c("Game.System")
  
  if(fields_missing(l, needed_fields)){
    return("ERROR: missing required field for get_gamesystems: ", paste(needed_fields, collapse=", "), sep="")
  }
  
  
  gamesystems_query = '
    query($system: String){
     gameSystems(query: $system){
      nodes{
        abbreviation,
        name,
        id
      }
    } 
}
'



  r = submit_warhorn(gamesystems_query,
                     list(system = l[["Game.System"]])
                     )
  
  if(r$status_code==200){
    data = content(r)
    if(length(data$data$gameSystems$nodes)>0){
      l[["game_id"]]=data$data$gameSystems$nodes[[1]][["id"]]
      return(l)
      
    }else{ 
      
      gamesystems_mutation = '
      mutation ($name: String!){
      createGameSystem(input: {name: $name}){
            gameSystem{
                      id
                    }
                }
            }
      '
      r = submit_warhorn(gamesystems_mutation,
                         list(name = l[["Game.System"]])
      )
      
      if(r$status_code==200){
        data = content(r)
        l[["game_id"]] = data$data$createGameSystem$gameSystem$id
        return(l)
      }else{
      return(paste("ERROR: no matching game system, failed to create game system", l[["Game.System"]], sep=": "))
      }
    }
    
  } 
  else{
    return(paste("ERROR: failed games systems query", data$errors[[1]]$message, sep=": "))
  }
  
}


create_slot_get_id<- function(l){
  
  
  needed_fields = c("Session.Date"
    ,"What.is.your.preferred.start.time..Eastern.Time..GMT.5..for.your.event.."
    ,"Event.Duration..Hours."
    )
  
  if(fields_missing(l, needed_fields)){
    return("ERROR: missing one of required fields for create_slot_get_id: ", paste(needed_fields, collapse=", "), sep="")
  }
  
  start = get_datetime_str(l[[needed_fields[1]]]
                           ,l[[needed_fields[2]]]
                           )
  
  end = get_end_datetime_str(l[[needed_fields[1]]]
                         ,l[[needed_fields[2]]]
                         ,l[[needed_fields[3]]]
                        )
  
  create_slot = "
  mutation (
          $startsat :  ISO8601DateTime!,
          $endsat :  ISO8601DateTime!,
          $eventid : ID!,
          $timezone : String!
        ) {
          createSlot(input: {eventId: $eventid
                    ,endsAt: $endsat
                    ,startsAt: $startsat
                    ,timezone : $timezone}){
                      slot{
                        id
                      }
                  }
          }

"
  variables = list(startsat = start, 
                   endsat = end, 
                   eventid = event_id, 
                   timezone = "America/New_York")
  
  r = submit_warhorn(create_slot, variables)
  
  
  if(r$status_code==200){
    data = content(r)
    l[["slot_id"]] = data$data$createSlot$slot$id
    return(l)
  } 
  else{
    return(paste("ERROR: failed to get create slot id", data$errors[[1]]$message, sep=": "))
  }
  
}

create_scenario_get_id<- function(l){
  
  needed_fields = c(
    "Session.Name..for.Event.Advertising."
    ,"A.Short.Overview.of.Your.Event..Less.than.200.words.please.."
    ,"game_id"
  )
  
  if(fields_missing(l, needed_fields)){
    return("ERROR: missing one of required fields for create_scenario_get_id: ", paste(needed_fields, collapse=", "), sep="")
  }
  
  create_scenario = "
mutation (
          $eventid : ID!
          ,$gamesystemid : ID!
          ,$name : String!
          ,$blurb : String
){
          createEventScenario(input: {eventId: $eventid
                    ,gameSystemId: $gamesystemid
                    ,name : $name
                    ,blurb : $blurb}){
                    scenario{
                    id
                      }
                    }
          
          }
"
  variables = list(eventid = event_id, 
                   gamesystemid = l[[needed_fields[3]]],
                   name = l[[needed_fields[1]]], 
                   blurb = l[[needed_fields[2]]]
                   )
  r = submit_warhorn(create_scenario, variables)
  
  data = content(r)
  if(r$status_code==200){
    l[["scenario_id"]] = data$data$createEventScenario$scenario$id
    return(l)
  } 
  else{
    return(paste("ERROR: failed in create scenario id", data$errors[[1]]$message, sep=": "))
  }
}


create_event_session<- function(l){
  
  
  needed_fields = c("Maximum.Number.of.Players"
                    ,"scenario_id"
                    , "slot_id"
                    )
  
  if(fields_missing(l, needed_fields)){
    return("ERROR: missing one of required fields for create_event_session: ", paste(needed_fields, collapse=", "), sep="")
  }
  
  session_query = "
  mutation ($slotid: ID!
        ,$scenarioid: ID!
        ,$tablecount: Int!
        ,$tablesize: Int!
        ) {
  createEventSession(input: {slotId: $slotId
                    ,scenarioId: $scenarioId
                    ,tableCount: $tableCount
                    ,tableSize: $tableSize
                    }){
                    session{
                    id
                      }
                    }
    }
 "
  variables = list(slotid = l[[needed_fields[3]]], 
                   scenarioid = l[[needed_fields[2]]], 
                   tableSize = l[[needed_fields[1]]], 
                   tablecount = 1)
  r = submit_warhorn(session_query, variables)
  
  
  if(r$status_code==200){
    data = content(r)
    l[["session_id"]]=data$data$createEventSession$session$id
    return(l)
  } 
  else{
    return(paste("ERROR: failed to get create session id", data$errors[[1]]$message, sep=": "))
  }
}

get_datetime_str <- function(raw_date, time){
  
  date = as.character(
    as.Date(
      paste(strsplit(raw_date, ", ")[[1]][2],
            strsplit(raw_date, ", ")[[1]][3], 
            sep= " "
            )
    ,format = "%B %d %Y"
    )
  )
  time = format(strptime(time, "%I:%M:%S %p"), format="%H:%M:%S")
  
  date_time = paste(date, time, sep = "T")
  return(date_time)
}

get_end_datetime_str<- function(date, time, duration){
  start = get_datetime_str(date, time)
  start_pos = as.POSIXct(start, 
                         tz="America/New_York",
                         format = "%Y-%m-%dT%H:%M:%S")
  end_pos = start_pos + 3600*duration
  end = format(end_pos, format = "%Y-%m-%dT%H:%M:%S")
  return(end)
}

add_tz_to_datetime_str<- function(date_time){
  return(paste(date_time, "-05:00", sep=""))
}

fields_missing<- function(l){
  all_present = TRUE
  for(v in vec){
    if(is.null(l[[v]])){
      all_present = FALSE
      break
    }
  } 
  return(!all_present)
}