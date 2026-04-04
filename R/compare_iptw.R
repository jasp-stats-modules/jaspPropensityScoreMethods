# libraries
pacman::p_load(dplyr,ipw,ggplot2)
# comparison
df=read.csv("C:\\Users\\P095206\\OneDrive - Amsterdam UMC\\Shared material with Elia\\PhD\\Presentations\\Confounding\\example.csv")
# perform matching
weights=ipwpoint(exposure=trt,
              family='binomial',
              link='logit',
              numerator=~1,
              denominator=~age,
              data=df)
# associate weight to dataset
df$weight=weights$ipw.weights
# plot
ggplot(df,aes(x=weight,group=trt,fill=factor(trt)))+
  geom_density()+
  #guides(fill='none')+
  theme_bw()
#
df$weight <- weights$ipw.weights
bal <- cobalt::bal.tab(
  x          = trt ~ age,
  data       = df,
  weights    = df$weight,
  binary     = "std",
  un         = TRUE,
  s.d.denom  = "pooled"
)

print(bal$Balance)
