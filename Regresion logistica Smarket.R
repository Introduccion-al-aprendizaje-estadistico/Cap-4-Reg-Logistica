library(ISLR)

#Hacer el Lab de el stock market, pag 156


head(Smarket)

#This data set consists of percentage returns for the S&P 500 stock index over 1, 250 days, from the
#beginning of 2001 until the end of 2005. For each date, we have recorded the percentage returns for each 
#of the five previous trading days, Lag1 through Lag5. We have also recorded Volume
#(the number of shares traded on the previous day, in billions), Today (the percentage return on the date
#in question) and Direction (whether the market was Up or Down on this                                                                                                                


#Generalized linear models
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume , data=Smarket ,family =binomial )

summary(glm.fits)

#The smallest p-value here is associated with Lag1. The negative coefficient
#for this predictor suggests that if the market had a positive return yesterday,
#then it is less likely to go up today. However, at a value of 0.15, the p-value
#is still relatively large, and so there is no clear evidence of a real association
#between Lag1 and Direction.                                                                                                                                                                                                                                                                    date).



coef(glm.fits)


glm.probs=predict(glm.fits,type="response")
 glm.probs[1:10]
 
 
 contrasts(Direction)
 
 glm.pred=rep("Down",1250)
 glm.pred[glm.probs>.5]="Up"

 table(glm.pred,Direction) 

 (507+145)/1250
 
 mean(glm.pred==Direction)
 
 glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,
              data=Smarket ,family =binomial ,subset =train )
  glm.probs=predict(glm.fits,Smarket.2005,type="response")
 
  glm.pred=rep("Down",252)
  glm.pred[glm.probs>.5]="Up"  
  table(glm.pred,Direction.2005)  
  mean(glm.pred==Direction.2005)  
  mean(glm.pred!=Direction.2005)  

  glm.fits=glm(Direction~Lag1+Lag2 ,data=Smarket ,family =binomial ,
               subset =train)
  glm.probs=predict(glm.fits,Smarket.2005,type="response")
  glm.pred=rep("Down",252)
  glm.pred[glm.probs>.5]="Up"
  table(glm.pred,Direction.2005)
  mean(glm.pred==Direction.2005)
  
  106/(106+76)
  
  predict(glm.fits,newdata=data.frame(Lag1=c(1.2,1.5),
                                      Lag2=c(1.1 , -0.8) ),type =" response ")
  