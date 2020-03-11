library(ISLR)

head(Default)

str(Default)
#Generalized linear models
glm.fits=glm(default ~ balance , data=Default ,family =binomial )
test<-summary(glm.fits)
test

#el p-valor es extremadamente chico, rechazamos H_0 !
#Concluimos que hay una asociacion entre el balance y el default
#The estimated intercept in Table 4.1
#is typically not of interest; its main purpose is to adjust the average fitted
#probabilities to the proportion of ones in the data.
#Para revisar las probabilidades asignadas a cada elemento hay dos formas

head(glm.fits$fitted.values) 
glm.probs=predict(glm.fits,type="response") 
glm.probs[1:10] 

#sink("lm.txt")  #Para guardar la tabla del summary como txt
#print(test)
#sink() 

####PAra predecir##########
#A mano
beta0<-test$coefficients[1]
beta1<-test$coefficients[2]

exp(beta0+beta1*1000)/(1+exp(beta0+beta1*1000))

exp(beta0+beta1*2000)/(1+exp(beta0+beta1*2000))

#Mas pro !

balance <- c(1000, 2000)
nuevo <- data.frame(balance)
nuevo[1]
glm.probs=predict(glm.fits,nuevo[1],type="response") #No jalo
glm.probs

##############Usando predictora cuantitativa

glm.fits=glm(default ~ student , data=Default ,family =binomial )
test<-summary(glm.fits)
test

#estudiante Yes=1=X
#estudiante NO=0=X
#Asi
#A mano

beta0<-test$coefficients[1]
beta1<-test$coefficients[2]

exp(beta0+beta1*1)/(1+exp(beta0+beta1*1))
exp(beta0+beta1*0)/(1+exp(beta0+beta1*0))

student <- c(1, 0)
student <- factor(student)
str(student)
student

nuevo <- data.frame(student)
nuevo[1]
str(nuevo)
glm.probs=predict(glm.fits,nuevo,type="response") #No jalo
glm.probs

################################################Regresion logistica multiple#########
glm.fits=glm(default ~ balance+income+student , data=Default ,family =binomial )
summary( glm.fits)

#obtener las probabilidades de dafault para un estudiante con
# $1,500 de balance e income $40,000
