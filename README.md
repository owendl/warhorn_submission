# warhorn_submission

### Basic query

This is the basic format of submitting a graphQL query to the Warhorn graphQL api.

```{r}

new_query = "
query ($slug: String!) {
 event(slug: $slug){
    id,
    isOnline,
    organizer {
      id
    },
    registrations{
      nodes{
        registrant{
          email
        }
      }
    }
    
  }
}

"

r <- POST("https://warhorn.net/graphql"
          , add_headers(
                        Authorization = paste("bearer", warhorn_creds$user_access_token, sep = " ")
                        )
          , body = list(
            query = new_query, 
            variables = list(slug = "name-of-event")
            )
          , encode = "json")
print(content(r))
```

Note: slug is the event specific part of the event url. For example in https://warhorn.net/events/name-of-event, the slug is name-of-event.
