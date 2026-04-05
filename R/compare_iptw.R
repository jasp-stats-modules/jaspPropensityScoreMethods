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
# calculate weights
## assign weights
df$weight=weights$ipw.weights
## table it
bal=cobalt::bal.tab(
  x          = trt ~ age,
  data       = df,
  weights    = df$weight,
  binary     = "std",
  un         = TRUE,
  s.d.denom  = "pooled")
## print results
print(bal$Balance)
# see problems with plotting
df$trt1=as.character(df$trt)
# plot
treat_col=df$trt1
ggplot(df,aes(x=weight,fill=treat_col,alpha=0.3))+
  geom_density()+
  guides(alpha='none')+
  scale_fill_manual(values=c('red','navy'))+
  theme_bw()

