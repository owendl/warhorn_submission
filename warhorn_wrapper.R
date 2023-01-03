

source("warhorn_functions.R")
event_id = get_event_id(warhorn_creds$event_str)
gm_role_id = get_gm_role_id(warhorn_creds$event_str)

df = read.csv("gm_responses.csv", stringsAsFactors = FALSE)

df = df[rowSums(is.na(df)) != ncol(df),]

func_list<- list(get_gamesystem
                 ,create_scenario_get_id
                 ,create_slot_get_id
                 ,create_event_session
                 ,assign_gm_role
                 )
func_count = length(func_list)

error_list = list()


for(i in 1:nrow(df)){
  
  entry = as.list(df[i,])
  
  no_errors = TRUE
  func_i = 1
  
  while(func_i <= func_count && no_errors){
    prev_entry=entry
    entry = func_list[[func_i]](entry)
    Sys.sleep(1)
    if(!is.list(entry)){
      no_errors = FALSE
      error_entry = as.list(df[i,])
      error_entry[["error"]] = entry
      error_list = append(error_list, list(error_entry))
    }
    func_i = func_i + 1
  }

  
  
  
}

error_df = as.data.frame(do.call(rbind, error_list))

# TODO need to create event to get role id for gm role to then update all gms to gm role
