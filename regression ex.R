regression1 <- lm(log_totalstreams~ as.factor(group) , data=New_analysis1)
regression_fixedFX <- lm(log_totalstreams~ as.factor(group) +  as.factor(artist) , data=New_analysis1)

#with controls
New_analysis1$type_scale <- NA
New_analysis1$type_scale <- scale(New_analysis1$type_numeric)
regression_controls <- lm(log_totalstreams~ as.factor(group) +  type_scale + superstar_num + artist_num_followers + year + as.factor(label), data=New_analysis1)
regression_less <-  lm(log_totalstreams~ as.factor(group) +  type_scale + superstar_num + artist_num_followers + year, data=New_analysis1)

setwd("/Users/Danni/Desktop") #will create word file 
stargazer::stargazer(regression1, regression_fixedFX, out="table5.txt", type = "text",  omit = c("artist"))
stargazer::stargazer(regression_controls, out="controls.txt", type = "text", omit = c("label"))
stargazer::stargazer(regression1, regression_fixedFX,regression_controls,out="ALL.txt", type = "text",  omit = c("artist","label"))

#html 
stargazer::stargazer(regression1, regression_fixedFX,regression_controls, type="html", omit = c("artist","label"),
          dep.var.labels=c("Total Streams (ln)"),
          covariate.labels=c("Cluster 1","Cluster 2","Artist Type","Superstar Status","Release Year"), out="models.html")





#plotting coeficcients
dwplot(list(regression1,regression_less),
vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 4)) %>% # plot line at zero _behind_ coefs
  relabel_predictors(c( `as.factor(group)2` = "Cluster 2",                       
                        `as.factor(group)3` = "Cluster 3",
                        type_scale = "Artist Type", 
                       superstar_num = "Superstar Status" , 
                      artist_num_followers = "Spotify Followers", 
                       year = "Release Year")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("Predicting Track Performance by Cluster ") +
  theme(plot.title = element_text(face="bold"),
        legend.position = c(0.78, 0.81),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 


#regressions for each AF
regression_Dance <- lm(log_totalstreams~ danceability, data=New_analysis1)
regression_valence <- lm(log_totalstreams~ valence, data=New_analysis1)
regression_Acoustic <- lm(log_totalstreams~ acousticness, data=New_analysis1)
regression_energy <-  lm(log_totalstreams~ energy, data=New_analysis1)
regression_instrumentall <- lm(log_totalstreams~ instrumentalness, data=New_analysis1)
regression_liveness <- lm(log_totalstreams~ liveness, data=New_analysis1)
regresion_mode <- lm(log_totalstreams~mode, data=New_analysis1)
regression_speech <- lm(log_totalstreams~ speechiness, data=New_analysis1)
regression_tempo <- lm(log_totalstreams~ tempo, data=New_analysis1)
regression_key <- lm(log_totalstreams~ key_numeric, data=New_analysis1)
regression_timesig <- lm(log_totalstreams~ time_signature, data=New_analysis1)

sum#fixed fx 
regression_Dance1 <- lm(log_totalstreams~ danceability + as.factor(artist), data=New_analysis1)
regression_valence1 <- lm(log_totalstreams~ valence + as.factor(artist), data=New_analysis1)
regression_Acoustic1 <- lm(log_totalstreams~ acousticness + as.factor(artist), data=New_analysis1)
regression_energy1 <-  lm(log_totalstreams~ energy + as.factor(artist), data=New_analysis1)
regression_instrumentall1 <- lm(log_totalstreams~ instrumentalness + as.factor(artist), data=New_analysis1)
regression_liveness1 <- lm(log_totalstreams~ liveness + as.factor(artist), data=New_analysis1)
regresion_mode1 <- lm(log_totalstreams~mode + as.factor(artist), data=New_analysis1)
regression_speech1 <- lm(log_totalstreams~ speechiness + as.factor(artist), data=New_analysis1)
regression_tempo1 <- lm(log_totalstreams~ tempo + as.factor(artist), data=New_analysis1)
regression_key1 <- lm(log_totalstreams~ key_numeric + as.factor(artist) , data=New_analysis1)
regression_timesig1 <- lm(log_totalstreams~ time_signature + as.factor(artist), data=New_analysis1)
#html 
stargazer::stargazer(regression_Dance1,regression_valence1,regression_Acoustic1,regression_energy1,regression_instrumentall1,
                     regression_liveness1,regresion_mode1,regression_speech1,regression_tempo1,regression_key1,regression_timesig1, type = "html", omit =c("artist"),
dep.var.labels=c("Total Streams (ln)"), out="AF.CONT.html")


AF_L <- dwplot(list(regression_Dance,regression_valence,regression_Acoustic,regression_energy,regression_instrumentall,
            regression_liveness,regresion_mode,regression_speech,regression_tempo,regression_key,regression_timesig),
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) %>% # plot line at zero _behind_ coefs
  relabel_predictors(c( danceability = "Danceability",
                        valence= "Valence",
                        acousticness= "Acousticness",
                        energy = "Energy",
                        instrumenallness= "Instrumentalness",
                        liveness= "Liveness",
                        mode= "Mode",
                        speechiness= "Speechiness",
                        temp= "Tempo",
                         key_numeric = "Key")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("Predicting Track Performance by Audio Feature") 


  AF_L + scale_colour_hue(name= "Models by Feature")



dwplot(list(regression1,regression_less),
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 4)) %>% # plot line at zero _behind_ coefs
  relabel_predictors(c( `as.factor(group)2` = "Cluster 2",                       
                        `as.factor(group)3` = "Cluster 3",
                        type_scale = "Artist Type", 
                        superstar_num = "Superstar Status" , 
                        artist_num_followers = "Spotify Followers", 
                        year = "Release Year")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("Predicting Track Performance") +
  theme(plot.title = element_text(face="bold"),
        legend.position = c(0.007, 0.01),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 


#descriptive stats
stargazer(Scaled_AF_Only, type = "html", title="Descriptive statistics", digits=1, out="table9.html")

stargazer(New_analysis1[c("artist_num_followers","totalwk_num","year","Numeric_totalstreams")], type = "html", title="Descriptive Statistics", digits=1, out="where.html",
covariate.labels=c("Artist Followers", "Total Weeks", "Release Year", "Total Streams"))

stargazer(Nice_numbers_AF, type = "html", title="Descriptive statistics", digits=1, out="nice.html")

#total weeks regression maybe use 
regression_weeks <- lm(totalwk_num~ as.factor(group),  data=New_analysis1)
regression_weeks_c <- lm(totalwk_num~ as.factor(group)+ log_totalstreams,  data=New_analysis1)
stargazer::stargazer(regression_weeks, regression_weeks_c, type = "html", out= "totalweeks.html")

exp2<- function (x) {
  thig <- exp(x)-1
  thig2 <- thig *100
  return(thig2)}
}


COZ <- c(0.329,-0.025,-0.070, -0.096,-0.154, -0.065, -0.164, 0.068, -0.003, 0.007)

