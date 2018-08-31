
#This will give me tracks causing issues.... album, artisturi, trackrui, 
lil_big <- ArtistTrack1[,2]
lil_big1 <- left %>% select(track, artist)

track_error_test <- map_df(lil_big1, function(x) {
  track_info <- get_tracks(x, return_closest_track = TRUE)
  if (nrow(track_info) > 0) {
    df <- track_info %>% 
      select(track_name,artist_uri, track_uri, album_name,album_id)
  } else {
    print(paste("No tracks found for query:", x))
    df <- tibble() # return empty dataframe in case there are no results
  }
  return(df)
})

#Make a data frome with only track and track uri
tracks_scale <- select(copy_uri_TRT, "track_name", "track_uri")
colnames(tracks_scale)<- c("Track", "track_uri")


# TRACK POPULARITY 

track_pop <- function(x) {
  output <- get_track_popularity(x)
  return(output) 
}


plz<- track_pop(tracks_scale)
plz <- plz %>% select(track_uri,track_popularity)

Scale_collect_1 <- full_join(Scale_collect,plz, by="track_uri")
Scale_collect_1 <- filter(!duplicated(Scale_collect_1$track))
Scale_collect_1 <- Scale_collect_1[-3174,]
#^^^^^^This worked.... but is a smaller number than where I started. 

#Artist followers
artist_genre_followers <- function(x){ 
  output<- get_artists(x, return_closest_artist =  TRUE)
  output <- output %>% select(artist_name,artist_uri, artist_genres,artist_num_followers)
  return(output)}

genre_followers <- map_df(Scale_collect_1$artist.x, artist_genre_followers)


new_list_genre <- sapply(list_genre, function (x) x[length(x)])
list_genre <- genre_followers$artist_genres

copy_genre_followers <- genre_followers

copy_genre_followers$artist_genres <- new_list_genre
copy_genre_followers$artist_genres <-flatten(copy_genre_followers$artist_genres)
# to save as CSV have to get rid of list. 
copy_genre_followers <- apply(copy_genre_followers,2,as.character)
write.csv(copy_genre_followers,file="copy_genre_followers.csv")
copy_genre_followers <- data.frame(copy_genre_followers)






# track audio features. 
Audio_Features <- function(x) {
  output <- get_track_audio_features(x) 
  return(output)
}


scaleup_af <- FOR_ANALYSIS %>% select(track,track_uri.x)
colnames(scaleup_af) <- c("track", "track_uri")

map_df(scaleup_af$scaleup_af,Audio_Features)

now_merge$artist_uri.x <- coalesce(now_merge$artist_uri.x, now_merge$artist_uri.y)
now_merge$track_uri.x <- coalesce(now_merge$track_uri.x, now_merge$track_uri.y)
now_merge$Album <- coalesce(now_merge$Album, now_merge$album_name)




#vector of names to for filtering. 
Alb_search <- c(alb_1$trackalbum)

alb_list <- c(Analysis_dataset$Album)

#This function uses spotifyr to search for release date of albums from a list of albums each track is found on. I filtered to keep the album names listed. 
ok <- function(x){
  output <- get_artist_albums(x, return_closest_artist = TRUE) %>%
    filter(album_name %in% Alb_search) %>%
    select(artist_name,album_release_date,album_name)
  return(output)
}

# iterate over aritsts with ok function  to get all release dates of albums we need, result isnt in order. 
release_date2 <- map_df(Analysis_dataset$artist,ok)


final_artist <- Analysis_dataset$track
the_Rest <- need_RD$track

getting_albID <- map_df(the_Rest[101:188], function(x) {
  track_info <- get_tracks(x, return_closest_track = TRUE)
  if (nrow(track_info) > 0) {
    df <- track_info %>% 
      select(album_name,album_id)
  } else {
    print(paste("No tracks found for query:", x))
    df <- tibble() # return empty dataframe in case there are no results
  }
  return(df)})

#getting the rest
R1<- 1-20
R2<- 21-50
R3 <- 50-100
R4 <- 101-188

R3[]
albumID_only <- getting_albID$album_id

release_album<-  function(x){
  album_release <- get_albums(x) %>%
    select(label,release_date)
}


#
get_albums(albumID_only)
Error: Duplicate identifiers for rows (23, 29), (24, 30)


alb_research <- getting_albID %>% select(album_id) %>% filter(!duplicated(getting_albID$album_id))



album_release_date <- function(x) {
  tryCatch({
    outty<- get_albums(x) 
    outty <- outty %>% select(name,label,release_date,id)
    return(outty)}, 
    error=function(e){
      return(data.frame())
    })}

}

#release dates 
RD1 <- album_release_date(albumID_only[1:20])
RD2 <- map_df(no_dup_release$album_id[21:30], get_albums)
RD3 <- map_df(no_dup_release$album_id[31:60], get_albums)
RD4 <- map_df(no_dup_release$album_id[61:100], get_albums)
RD5 <- map_df(no_dup_release$album_id[101:250], get_albums)
RD6 <- map_df(no_dup_release$album_id[251:300], get_albums)
RD7 <- map_df(no_dup_release$album_id[301:400],album_release_date)
RD8 <- map_df(no_dup_release$album_id[401:500],album_release_date)
RD9 <- map_df(no_dup_release$album_id[501:650],album_release_date)
RD10 <- map_df(no_dup_release$album_id[651:800],album_release_date)
RD11 <- map_df(no_dup_release$album_id[800:1200],album_release_date)
RD12 <- map_df(no_dup_release$album_id[800:1200],album_release_date)
RD13 <- map_df(no_dup_release$album_id[1200:1431],album_release_date)



album_releases <- bind_rows(RD1,RD2)
longe_release_date <- bind_rows(RD7, RD8, RD9,RD10,RD11,RD12,RD13)

release_dates_alb <- release_dates_alb %>% select(name,label, release_date)
FINAL_ALB_RELEASE <- bind_rows(release_date_final,longe_release_date)



#neeed to use the df "rest" to add the missing info to analysis dataset cuz missing 188 

lil_big1 <- lil_big1 %>% select(track)
lil_big1 <- as.data.frame(lil_big1)


trackie <- function(x){
  get_tracks(x, return_closest_track = TRUE)
}



                             
NEW_analysis %>% select("Rank","track","artist","type","Album","label","release_date","totalstreams","Track Genre 1", "Track Genre 2","danceability","energy","loudness","speechiness","acousticness", "instrumentalness","liveness","valence","tempo","duration_ms","time_signature","mode","key", "key_numeric","Numeric_totalstreams" ,"log_totalstreams", "group")                                
   
        
