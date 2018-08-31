
pca4 <- prcomp(t(Scaled_AF_Only))
plot(pca4$x[,1],pca4$x[,2])
pca.data4 <- data.frame(Sample=rownames(pca4$x),
                       X=pca4$x[,1],
                       Y=pca4$x[,2])
#variation
pca.var4 <- pca4$sdev^2
pca.var.per4 <- round(pca.var4/sum(pca.var4)*100, 1)
barplot(pca.var.per4, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

#for plotting 
vvvvppp <- Analysis_dataset_1 %>% select(Rank)
colnames(vvvvppp) <- c("Sample")
pca.data4 <- merge(pca.data4,vvvvppp,by="Sample")
pca.data$group <- as.factor(pca.data$group)
PCA_PLOTTTT <- ggplot(data=pca.data4, aes(x=X, y=Y, label=Sample)) +
  geom_point() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("PCA of Tracks")

# see which songs create most variation 
loading_scores4 <- pca4$rotation[,1]
track_scores4 <- abs(loading_scores4) ## get the magnitudes
track_score_ranked4 <- sort(track_scores4, decreasing=TRUE)
top_10_track4 <- names(track_score_ranked4[1:10])
#positive- push to the right of the graph these have the larges effect of  where songs are plotted. 
