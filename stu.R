plot(Analysis_dataset_1$group,Analysis_dataset_1$Numeric_totalstreams)
abline(lm(Numeric_totalstreams~ as.factor(group) , data=Analysis_dataset_1))

plot(Analysis_dataset_1$group,Analysis_dataset$log_totalstreams)
summ(regression1, confint = TRUE, digits = 3)

plot_summs(regression2, fit=TRUE, omit.coefs = TRUE)


regression1 <- lm(log_totalstreams~ as.factor(group) , data=Analysis_dataset)
regression2 <- lm(log_totalstreams~ as.factor(group) +  as.factor(artist) , data=Analysis_dataset)

plot(Analysis_dataset$group,resid(regression1) ,pch=.02, abline(h=0), xlab= "Cluster", ylab= "Residuals", main= "Residuals Against The Independent Variable")

plot(Analysis_dataset$group,resid(regression2) ,pch=.02, abline(h=0), xlab= "Cluster", ylab= "Residuals", main= "Residuals Against The Independent Variable With Control")

as.data.frame(okkkkkkkk)
ggplot(data=okkkkkkkk, aes(x=X, y=Y)) +
  geom_point()
xlab("Var2")
ylab("Var3")
  theme_bw() +
  ggtitle("HI")