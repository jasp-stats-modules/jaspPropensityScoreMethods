# libraries
pacman::p_load(dplyr,MatchIt)
# comparison
df=read.csv("C:\\Users\\P095206\\OneDrive - Amsterdam UMC\\Shared material with Elia\\PhD\\Presentations\\Confounding\\example.csv")
# perform matching
match=matchit(trt~age+I(age^2)+sex+chol,
              data=df,
              method='nearest',
              distance='glm',
              caliper=NULL,
              ratio=2,
              replace=F)
# sample size
dim(match.data(match))[1]
# summary
summary(match)
# treatment model formula
f=match$model$formula
attr(terms(match$model$formula), "term.labels")
