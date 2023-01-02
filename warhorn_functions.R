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

get_gm_role_id<-function(event_str){
  event_roles= 'query($slug: String!){
     event(slug: $slug){
      id,
      roles{
        id,
        name
      }
    }
    }
'
  data = submit_warhorn(event_roles, list(slug = event_str))
  data = content(data)
  for(d in data$data$event$roles){
    if(d$name =="GM"){
      return(d$id)
    }
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
  
  start = add_tz_to_datetime_str(get_datetime_str(l[[needed_fields[1]]]
                           ,l[[needed_fields[2]]]
                           ))
  end = add_tz_to_datetime_str(get_end_datetime_str(l[[needed_fields[1]]]
                         ,l[[needed_fields[2]]]
                         ,l[[needed_fields[3]]]
                        ))
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
    "Title.of.Your.Event"
    ,"A.Short.Overview.of.Your.Event..Less.than.200.words.please.."
    ,"game_id"
  )
  
  if(fields_missing(l, needed_fields)){
    return("ERROR: missing one of required fields for create_scenario_get_id: ", paste(needed_fields, collapse=", "), sep="")
  }
  
  # TODO need to check if scenario already exists eventScenarioOfferings query, iterate through
  
  event_scenarios = '
  query($slug: String!){
  eventScenarioOfferings(slug: $slug){
    nodes{
      scenario{
        name,
        id
      }
    }
  }
  }
'
  
  e = submit_warhorn(event_scenarios, list(slug = warhorn_creds$event_str))
  e = content(e)
  for(d in e$data$eventScenarioOfferings$nodes){
    if(d$scenario$name == l[[needed_fields[1]]]){
      l[["scenario_id"]] = d$scenario$id
      return(l) 
    }
    
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
    return(paste("ERROR: missing one of required fields for create_event_session: ", paste(needed_fields, collapse=", "), sep=""))
  }
  
  session_query = "
  mutation ($slotid: ID!
        ,$scenarioid: ID!
        ,$tablecount: Int!
        ,$tablesize: Int!
        ) {
  createEventSession(input: {slotId: $slotid
                    ,scenarioId: $scenarioid
                    ,tableCount: $tablecount
                    ,tableSize: $tablesize
                    }){
                    session{
                    id
                      }
                    }
    }
 "
  variables = list(slotid = l[[needed_fields[3]]], 
                   scenarioid = l[[needed_fields[2]]], 
                   tablesize = l[[needed_fields[1]]], 
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




get_registration_id<-function(email, slug){
  get_registration_query='
  query($slug: String!, email: String!){
    eventRegistration(slug: $slug, email: $email){
      id,
      roles{
        id,
        name
        }
      }
    }
  }
'
  data = content(submit_warhorn(get_registration_query, list(slug = slug,
                                                     email = email)
                        )
                 )
  if(is.null(data$data$eventRegistration)){
    return(NULL)
  }else{
    return(data$data$eventRegistration$id)
  }

}

assign_gm_role<-function(l){
  
  needed_fields = c("Preferred.E.mail.Address")
  
  if(fields_missing(l, needed_fields)){
    return(paste("ERROR: missing one of required fields for assign_gm_role: ", paste(needed_fields, collapse=", "), sep=""))
  }
  
  
  registration_id = get_registration_id(l[[needed_fields[1]]], 
                                        warhorn_creds$event_str)
  
  assign_role_query='
  mutation ($registraion_id: ID!,
                    $role_id: ID!){
            assignRegistrationRole(input:{
                registrationId: $registration_id,
                roleId: role_id}){
                  registration{
                  registrant{
                      email
                      },
                  roles{
                      name
                      }
                  }
                
                }
          }
'

  r = submit_warhorn(assign_role_query, list(registration_id = registration_id,
                                             role_id = gm_role_id)
                     )
  
  if(r$status_code==200){
    data = content(r)
    roles=c()
    for(d in data$data$registration$roles){
      roles= append(roles, d$name)
    }
    role_string = paste(data$data$registration$registrant$email, paste(roles, collapse = ", "), sep = "has roles: ")
    l[["roles"]]=role_string
    return(l)
  } 
  else{
    return(paste("ERROR: failed to assign gm role", data$errors[[1]]$message, sep=": "))
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
  pos = as.POSIXct(date_time, 
             tz="America/New_York",
             format = "%Y-%m-%dT%H:%M:%S")
  
  # manual conversion to pacific time
  pos = pos - 3*3600
  date_time = format(pos, format = "%Y-%m-%dT%H:%M:%S")
  
  return(date_time)
}

get_end_datetime_str<- function(date, time, duration){
  start = get_datetime_str(date, time)
  start_pos = as.POSIXct(start, 
                         tz="America/Los_Angeles",
                         format = "%Y-%m-%dT%H:%M:%S")
  end_pos = start_pos + 3600*duration
  end = format(end_pos, format = "%Y-%m-%dT%H:%M:%S")
  return(end)
}

add_tz_to_datetime_str<- function(date_time){
  # warhorn datetime string has a colon in it, easier to just manually add it here
  return(paste(date_time, "-08:00", sep=""))
}

fields_missing<- function(l, vec){
  all_present = TRUE
  for(v in vec){
    if(is.null(l[[v]])){
      all_present = FALSE
      break
    }
  } 
  return(!all_present)
}