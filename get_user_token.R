library(jsonlite)
library(httr)
library(stringi)

warhorn_creds = read_json("warhorn_creds.json")


redirect = "https://github.com/owendl/warhorn_submission"
r <- GET("https://warhorn.net/oauth/authorize", 
          query = list(
             redirect_uri= redirect
            ,response_type = "code"
            ,client_secret = warhorn_creds$client_secret
            ,client_id = warhorn_creds$client_id
            ,state = stri_rand_strings(1,10)
            )
          ,encode = "json")
browseURL(r$request$url)

auth_code<-rstudioapi::askForPassword(prompt = "click allow on Warhorn site then enter code field of redirected url")


p <- POST("https://warhorn.net/oauth/token"
          ,add_headers(
            client_secret = warhorn_creds$client_secret
            ,client_id = warhorn_creds$client_id
            ,code = auth_code
            ,grant_type = "authorization_code"
            ,redirect_uri = redirect 
          ) 
         , body = list(
           client_secret = warhorn_creds$client_secret
           ,client_id = warhorn_creds$client_id
           ,code = auth_code
           ,grant_type = "authorization_code"
           ,redirect_uri = redirect
           )
         ,encode = "form")

d=content(p)

warhorn_creds$user_access_token <- d$access_token

write_json(toJSON(warhorn_creds, auto_unbox = TRUE), "warhorn_creds.json")
