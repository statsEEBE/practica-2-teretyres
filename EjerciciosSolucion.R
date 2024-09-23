#Ejercicio 1

altura <- c(178, 181, 168, 183, 164, 181, 174, 176, 174, 176, 181, 168, 164, 174, 171)
peso <- c(82, 89, 68, 91, 65, 80, 79, 81, 80, 79, 82, 69, 67, 80, 78)

plot(peso~altura, pch=16,col="red",cex=1.5)

#¿Qúe tipo de relación ves?
  #a) Relación lineal simple creciente

model1 <- lm(peso~altura); model1


plot(peso~altura, pch=16,col="red",cex=1.5)
abline(model1,col="blue",lwd=5)


#peso = 1.153*altura-122.899


summary(model1)

cor(peso,altura)

altura_pred <- 173
peso_pred <- predict(model1,data.frame(altura=altura_pred))
peso_pred


plot(peso~altura, pch=16,col="red",cex=1.5)
abline(model1,col="blue",lwd=5)
points(altura_pred,peso_pred, col = "black", pch= 20, cex = 4)


model2 <- lm(altura~peso)
model2
peso_pred2 <- 72
altura_pred2 <- predict(model2,data.frame(peso=peso_pred2)); altura_pred2

#Ejercicio 2

Datos = read.table("anscombe.txt",header=TRUE, sep=";",dec=",")

head(Datos)

model1 <- lm(Datos$y1~Datos$x1)

x_hat <- mean(Datos$x1)
x_hat

y_hat <- mean(Datos$y1)
y_hat

sum_x <- sum((Datos$x1-mean(Datos$x1))^2)
sum_x

sum_y <- sum((Datos$y1-mean(Datos$y1))^2)
sum_y

m <- model1$coefficients[2]
m

b <- model1$coefficients[1]
b

RR <- cor(Datos$x1,Datos$y1)
RR

R2 <- summary(model1)$r.squared
R2

y_pred <- m*8.5+b
y_pred

model2 <- lm(Datos$y2~Datos$x2)

x_hat <- mean(Datos$x12)
x_hat

y_hat <- mean(Datos$y2)
y_hat

sum_x <- sum((Datos$x2-mean(Datos$x2))^2)
sum_x

sum_y <- sum((Datos$y2-mean(Datos$y2))^2)
sum_y

m <- model2$coefficients[2]
m

b <- model2$coefficients[1]
b

RR <- cor(Datos$x2,Datos$y2)
RR

R2 <- summary(model2)$r.squared
R2

y_pred <- m*8.5+b
y_pred


model3 <- lm(Datos$y3~Datos$x3)

x_hat <- mean(Datos$x3)
x_hat

y_hat <- mean(Datos$y3)
y_hat

sum_x <- sum((Datos$x3-mean(Datos$x3))^2)
sum_x

sum_y <- sum((Datos$y3-mean(Datos$y3))^2)
sum_y

m <- model3$coefficients[2]
m

b <- model3$coefficients[1]
b

RR <- cor(Datos$x3,Datos$y3)
RR

R2 <- summary(model3)$r.squared
R2

y_pred <- m*8.5+b
y_pred

model4 <- lm(Datos$y4~Datos$x4)

x_hat <- mean(Datos$x4)
x_hat

y_hat <- mean(Datos$y4)
y_hat

sum_x <- sum((Datos$x4-mean(Datos$x4))^2)
sum_x

sum_y <- sum((Datos$y4-mean(Datos$y4))^2)
sum_y

m <- model4$coefficients[2]
m

b <- model4$coefficients[1]
b

RR <- cor(Datos$x4,Datos$y4)
RR

R2 <- summary(model4)$r.squared
R2

y_pred <- m*8.5+b
y_pred

#En general, ¿qué puedes decir de los coeficientes de correlación (R) y de determinación (R^2)?
# Son iguales y altos en todos los casos (correcta)

#¿Cuál de los 4 modelos se ajusta mejor a los datos?
# Caso I (correcta)
#Ejercicio 3



par(mfrow=c(2,2))    # set the plotting area into a 2*2 array
plot(Datos$y1~Datos$x1, pch=16,col="red",cex=1.5)
abline(model1,col="blue",lwd=3)
points(8.5,y_pred, col = "black", pch= 20, cex = 4)

plot(Datos$y2~Datos$x2, pch=16,col="red",cex=1.5)
abline(model2,col="blue",lwd=3)
points(8.5,y_pred, col = "black", pch= 20, cex = 4)

plot(Datos$y3~Datos$x3, pch=16,col="red",cex=1.5)
abline(model3,col="blue",lwd=3)
points(8.5,y_pred, col = "black", pch= 20, cex = 4)

plot(Datos$y4~Datos$x4, pch=16,col="red",cex=1.5)
abline(model4,col="blue",lwd=3)
points(8.5,y_pred, col = "black", pch= 20, cex = 4)


#¿Qué puedes decir de cada conjunto de datos?
# I: (x1,y1)
# Relación lineal de primer orden (correcta)

# II: (x2,y2)
# Relación lineal de orden superior (correcta)
# Relación lineal de primer orden con un valor atípico  (correcta)

# IV: (x4,y4)
# Relación no lineal con un valor atípico   (correcta)

# ¿Cómo mejorarías cada modelo de regresión?
# I: (x1,y1)
# No haría nada, está bien como está (correcta)

# II: (x2,y2)
# Probaría un modelo lineal de orden o grado superior (correcta)

# III: (x3,y3)
# Eliminaría el dato atípico y volvería a calcular la recta de regresión (correcta)

# IV: (x4,y4)
# Eliminaría el dato atípico y así se ve claramente la no linealidad de los datos (correcta)


#Finalmente, ¿Cuál de las siguientes conclusiones crees que es válida?
# Todas las anteriores (correcta)

#Ejercicio 4

Lluvia <- c(97, 27, 93, 175, 38	, 192, 28, 182, 61, 77)
Incendios <- c(521, 863, 712, 163, 138, 811, 534, 442, 963, 313)

summary(lm(Incendios ~ Lluvia))

#Ejercicio 4

Datos4 = read.table("ester.txt",header=TRUE, sep=";",dec=".")
head(Datos4)

par(mfrow=c(1,2))    # set the plotting area into a 2*2 array
plot(Datos4$conc~Datos4$t, pch=16,col="red",cex=1.5)
plot(log(Datos4$conc)~Datos4$t, pch=16,col="red",cex=1.5)


model_ester <- lm(log(Datos4$conc)~Datos4$t)
summary(model_ester)


t_pred <- 70
conc_pred <- exp(model_ester$coefficients[2]*t_pred+model_ester$coefficients[1])
conc_pred


par(mfrow=c(1,1))    # set the plotting area into a 1*1 array
plot(Datos4$conc~Datos4$t, pch=16,col="red",cex=1.5)
curve(exp(model_ester$coefficients[2]*x+model_ester$coefficients[1]), add=T, col="blue",lwd=3)
points(t_pred,conc_pred, col = "black", pch= 20, cex = 4)
