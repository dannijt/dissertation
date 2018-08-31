#Data set of audio features only 

#Distance Matrix 
distance <- dist(Scaled_AF_Only, method = "euclidean") 
distance <- as.matrix(distance)

#seeing number for K-means 
#elbow method
              fviz_nbclust(distance, kmeans, method = "wss") +
                geom_vline(xintercept = 4, linetype = 2)+
                labs(subtitle = "Elbow method")
#silhouett method
              fviz_nbclust(distance, kmeans, method = "silhouette")+
                labs(subtitle = "Silhouette method")
#gap_stat -  
      set.seed(123)
        fviz_nbclust(distance, kmeans, nstart = 25,  method = "gap_stat", nboot = 500,verbose = TRUE)+
         labs(subtitle = "Gap statistic method")
        
#still deciding number 
        library("NbClust")
        nb <- NbClust(Scaled_AF_Only, distance = "euclidean", min.nc = 2,
                      max.nc = 10, method = "kmeans")
        
        library("factoextra") #results of above function 
        fviz_nbclust(nb)
       
        
        Scaled_AF_R <- New_analysis1 %>% select(track,acousticness,danceability,energy,instrumentalness,key_numeric,liveness,mode,speechiness,tempo,time_signature,valence)
        row.names(Scaled_AF_R) <- Scaled_AF_R$track  
       Scaled_AF_R$track <- NULL
  ##visualize where things are clustered. 
        fviz_pca_ind(prcomp(Scaled_AF_R), title = "PCA ", 
          palette = "jco",
        geom = "text", ggtheme = theme_classic(),
        legend = "bottom")
        
        #get ellipses around clusters  
        km.res1 <- kmeans(Scaled_AF_R, 3)
       f<-  fviz_cluster(list(data = Scaled_AF_R, cluster = km.res1$cluster),
                     ellipse.type = "norm", geom = "text", stand = FALSE,xlab = "PC1", ylab = "PC2",
                     palette = "jco", ggtheme = theme_classic())
       f + theme(text = element_text(size= 2))
       #hierarchical clustering dend 
        fviz_dend(hclust(dist(Scaled_AF_Only)), k = 3, k_colors = "jco",  
                  as.ggplot = TRUE, show_labels = FALSE)
        
    #hopkins statistic for my dataset needs to be above .5
        res <- get_clust_tendency(Scaled_AF_Only, n = nrow(Scaled_AF_Only)-1)
 #$hopkins_stat A value for H higher than 0.75 indicates a clustering tendency at the 90% confidence level.
  # [1] 0.84515
 #Heatmap
        fviz_dist(dist(Scaled_AF_Only), show_labels = FALSE)+
          labs(title = "Audiofeatures")
        
#actual clustering 
fit1 <- kmeans(Scaled_AF_Only, 3, nstart = 25)
groups <- fit1$cluster
NEW_analysis$group <- groups

fit4 <- kmeans(Scaled_AF_Only, 4, nstart = 25)
print(fit2)

#chisquared
Cluter_table <- table(NEW_analysis$type,NEW_analysis$group)
chisquared <-chisq.test(Cluter_table) # this is with 3 clusters 
              #data:  Cluter_table
              # X-squared = 731.99, df = 112, p-value < 2.2e-16
#compute mean original data :  ) 
MEAN_NONSCALE <- aggregate(Nice_numbers_AF, by=list(cluster=fit1$cluster), mean) # 3 clusters

