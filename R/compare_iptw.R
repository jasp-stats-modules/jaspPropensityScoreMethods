# libraries
pacman::p_load(dplyr,ipw)
# comparison
df=read.csv("C:\\Users\\P095206\\OneDrive - Amsterdam UMC\\Shared material with Elia\\PhD\\Presentations\\Confounding\\example.csv")
# perform matching
weights=ipwpoint(exposure=trt,
              family='binomial',
              link='logit',
              numerator=~1,
              denominator=~age+sex+chol,
              data=df)
# summary
summary(match)
# treatment model formula
f=match$model$formula
attr(terms(match$model$formula), "term.labels")
