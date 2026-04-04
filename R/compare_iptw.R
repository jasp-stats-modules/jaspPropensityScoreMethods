# libraries
pacman::p_load(dplyr,ipw,ggplot2)
# comparison
df=read.csv("C:\\Users\\P095206\\OneDrive - Amsterdam UMC\\Shared material with Elia\\PhD\\Presentations\\Confounding\\example.csv")
# perform matching
weights=ipwpoint(exposure=trt,
              family='binomial',
              link='logit',
              numerator=~1,
              denominator=~age+sex+chol,
              data=df)
# associate weight to dataset
df$weight=weights$ipw.weights
# plot
ggplot(df,aes(x=weight,group=trt,fill=trt))+
  geom_density()+
  theme_bw()
