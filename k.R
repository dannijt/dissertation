

# Frank's Track 1836
# I need a forest fire 2846
# Waste a moment 2162
# runnin down a dream 2582
# sexy back 2754
# the explanation 2116

Frank <-  New_analysis1 %>% filter(New_analysis1$Rank=="1836")
Forest <-  New_analysis1 %>% filter(New_analysis1$Rank=="2846")
Forest$`Track Genre 1` <- "Indie"
Waste <-  New_analysis1 %>% filter(New_analysis1$Rank=="2162")
Run <-  New_analysis1 %>% filter(New_analysis1$Rank=="2582")
Run$year <- c("1989")
Run$year <- as.numeric(as.character(Run$year))
sexy <- New_analysis1 %>% filter(New_analysis1$Rank=="3218")
THEex <- New_analysis1 %>% filter(New_analysis1$Rank=="2116")
silver <- New_analysis1 %>% filter(New_analysis1$Rank=="2754")
Discover<- bind_rows(Frank, Forest, Waste, Run, sexy, THEex, silver)                                 

table_Disc <- Discover %>% select(track,artist,Album,label,`Track Genre 1`, totalstreams,artist_num_followers,type,year,group) %>%
colname1 <- c("Track","Artist","Album", "Label","Track Genre", "Total streams","Artist Followers","Type","Release Year","Cluster")
colnames(table_Disc) <- colname1
table_Disc$`Artist Followers` <- c("8,123,190","705,855","3,385,777","672,703","6,605,715","5,101,755","8,123,116")
  kable(table_Disc)  %>%
    kable_styling("striped", full_width = T)%>%
    save_kable(file = "weird.html", self_contained = T)

#visualizarion of songs 
  library(ggplot2)
  theme_set(theme_bw())
  
  # Plot
  ggplot(Kab, aes(x=track, y=danceability)) + 
    geom_point(size=3) + 
    geom_segment(aes(x=track, 
                     xend=track, 
                     y=0, 
                     yend=danceability)) + 
    labs(title="Danceability Comparison", 
         subtitle="unusual tracks") + 
    theme(axis.text.x = element_text(angle=50, vjust=0.6))
  #diverging 
  library(ggplot2)
  theme_set(theme_bw())
  
  ggplot(Kab, aes(x=track, y=valence, label=valence)) + 
    geom_point(fill="black", size=4)  +
    geom_segment(aes(y = 0, 
                     x = track, 
                     yend = valence, 
                     xend = track), 
                 color = "black") +
    labs(title="Valence Comparison", 
         subtitle="unusual tracks") + 
    ylim(-2.5, 2.5) +
    coord_flip()
  
  
  
  
  #visualization of AF 
Kab <- Discover %>% select(track,acousticness,danceability,energy,instrumentalness,key_numeric,liveness,mode,speechiness,tempo,time_signature,valence)
  
  library(formattable)
  Kab %>%
    mutate(
      Track = row.names(.),
      acousticness = color_tile("white", "orange")(acousticness),
      danceability = cell_spec(danceability, background = "blue", color = "white", align = "center"),
      energy = color_bar("blue")(energy),
      instrumentalness = color_bar("lightgreen")(instrumentalness),
      key= cell_spec(key_numeric, color = "lightgreen", italic = T),
      liveness= color_bar("blue")(liveness),
      mode= color_tile("blue","white")(mode),
      speechiness= color_tile("white", "lightgreen")(speechiness),
      tempo= cell_spec(tempo, color = "red", bold = T),
      timesig= color_text("blue")(time_signature),
      valence= color_bar("blue")(valence))  %>%
    select(track, everything()) %>%
    kable(escape = F) %>%
    kable_styling("hover", full_width = F) %>%
    column_spec(5, width = "3cm") %>%
    add_header_above(c(" ", "Hello" = 2, "World" = 2))
