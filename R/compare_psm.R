# libraries
pacman::p_load(dplyr,MatchIt)
# comparison
df=read.csv("C:\\Users\\P095206\\OneDrive - Amsterdam UMC\\Shared material with Elia\\PhD\\Presentations\\Confounding\\example.csv")
# perform matching
match=matchit(trt~age+sex+chol,
              data=df,
              method='optimal',
              distance='mahalanobis',
              #caliper=0.5,
              ratio=2,
              replace=F)
# summary
summary(match); matcheddf=match.data(match)
# sample size
dim(match.data(match))[1]
# treatment model formula
f=match$model$formula
attr(terms(match$model$formula), "term.labels")
