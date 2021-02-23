## Byssinosis Prevalance Analysis
## STA 138 FINAL Project
##


df = read.csv('Byssinosis.csv')


for(i in 1:72)
{
  if(df$Workspace[i]==1){
    df$Workspace[i] = 'most dusty'
  }
  if(df$Workspace[i]==2){
    df$Workspace[i] = 'less dusty'
  }
  if(df$Workspace[i]==3){
    df$Workspace[i] = 'least dusty'
  }
}

df$rate <- df$Byssinosis / (df$Byssinosis + df$Non.Byssinosis)

for(i in 1:72)
{
  if(df$rate[i] == 'NaN'){
    df$rate[i] = 0
  }
}

df$SigRate <- (df$rate > .001)
for(i in 1:72)
{
  if(df$SigRate[i] == 'TRUE'){
    df$SigRate[i] = 1
  }
  if(df$SigRate[i] == 'FALSE'){
    df$SigRate[i] = 0
  }
}

df = df[df$rate!='NaN',]

plot(df$rate, ylab="Rate")

## Referencing closely HW8 solutions and disc8.R
## Logistic Model Fitting
## Discussion 8 code
## Using bestForwardAIc fit model
library(bestglm)
## Loading required package: leaps

fullModel = glm(SigRate ~ Employment + Smoking + Sex + Race + Workspace,data=df,family = 'binomial')
nullModel = glm(SigRate ~ 1,data=df,family = 'binomial')

bestForwardAIC=step(nullModel,scope = list(lower = nullModel, upper = fullModel),direction = "forward")
## Start:  AIC=101.59
## SigRate ~ 1
## 
##              Df Deviance     AIC
## + Race        1   95.994  99.994
## + Employment  2   94.680 100.680
## <none>            99.591 101.591
## + Sex         1   98.697 102.697
## + Smoking     1   98.697 102.697
## + Workspace   2   98.810 104.810
## 
## Step:  AIC=99.99
## SigRate ~ Race
## 
##              Df Deviance     AIC
## + Employment  2   90.815  98.815
## <none>            95.994  99.994
## + Smoking     1   95.053 101.053
## + Sex         1   95.053 101.053
## + Workspace   2   95.172 103.172
## 
## Step:  AIC=98.81
## SigRate ~ Race + Employment
## 
##             Df Deviance     AIC
## <none>           90.815  98.815
## + Smoking    1   89.802  99.802
## + Sex        1   89.802  99.802
## + Workspace  2   89.929 101.929
bestBackwardAIC=step(fullModel,scope = list(lower = nullModel, upper = fullModel),direction = "backward")
## Start:  AIC=103.86
## SigRate ~ Employment + Smoking + Sex + Race + Workspace
## 
##              Df Deviance    AIC
## - Workspace   2   88.774 100.77
## - Smoking     1   88.903 102.90
## - Sex         1   88.903 102.90
## <none>            87.862 103.86
## - Employment  2   93.255 105.25
## - Race        1   91.890 105.89
## 
## Step:  AIC=100.77
## SigRate ~ Employment + Smoking + Sex + Race
## 
##              Df Deviance     AIC
## - Smoking     1   89.802  99.802
## - Sex         1   89.802  99.802
## <none>            88.774 100.774
## - Employment  2   94.100 102.100
## - Race        1   92.751 102.751
## 
## Step:  AIC=99.8
## SigRate ~ Employment + Sex + Race
## 
##              Df Deviance     AIC
## - Sex         1   90.815  98.815
## <none>            89.802  99.802
## - Employment  2   95.053 101.053
## - Race        1   93.722 101.722
## 
## Step:  AIC=98.81
## SigRate ~ Employment + Race
## 
##              Df Deviance     AIC
## <none>            90.815  98.815
## - Employment  2   95.994  99.994
## - Race        1   94.680 100.680
best.model = glm(SigRate ~ Race + Employment, data= df, family=binomial)

## Test interaction of Race and Employment
inter.model = glm(SigRate ~ Race + Employment + Race*Employment, data=df, family = 'binomial') 
best.model = glm(SigRate ~ Race + Employment , data=df, family = 'binomial')
LLA = logLik(inter.model) 
LL0 = logLik(best.model)
LR = round(-2*(LL0-LLA),4); 
d.f = length(inter.model$coefficients) - length(best.model$coefficients) 
p.val = pchisq(LR,d.f, lower.tail=F)

## Analyze Pearson Residuals and DF's

library(LogisticDx) 
good.stuff = dx(best.model) 

## Pearson Residuals
## https://newonlinecourses.science.psu.edu/stat504/node/86/
pear.r = good.stuff$Pr 
hist(pear.r,main = "Pearson's residuals", xlab = "Residuals") 

## DF BETA
## https://www.albany.edu/faculty/kretheme/PAD705/SupportMat/DFBETA.pdf
df.beta = good.stuff$dBhat 
plot(df.beta,ylab = "DFBeta",main = "Plot of Change in Beta")
