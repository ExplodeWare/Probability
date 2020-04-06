

#PRIMER PUNTO
auto_hora= 210
cap_minuto = 10

#La v.a de Poisson es el numero de ocurrencias en un rango de tiempo
#Por lo que el problema nos dice que tenemos que usar una v.a de Poisson

#observe que lambda es igual a 210 gracias a que el valor esperado de poisson es lambda
#y tenemos que la media de carros en un minuto es 210



q = seq(0,10, by=1)
func <- ppois(q, 210/60, lower.tail = FALSE) #Variable de poisson que nos da la acumulada
                #El lower tail en FALSE nos permite omitir hacer el cambio de P(X>=10) a 1-P(X<=10)
plot(func)        # Nos dibuja los valores que nos da la funcion acumulada, nos fijamos en el último valor

print(func[11])   # Valor que nos pide el problema(probabilidad P(X<=10))




#SEGUNDO PUNTO

#La proporcion de individuos de una población con renta superior a 2 millones de dolares
#es de 0.005%. Determine la probabilidad de que entre 5000 individuos haya como mucho 2 
#con ese nivel de renta usando la variable aleatoria binomial y la aprox de Poisson.



pois <- function(x,lambda){
  1/exp(lambda) * (lambda^x / factorial(x))
}

print(Bin(5000,2))

pbinom(2, size = 5000, prob = 0.005)
pois(2,25)

#Cómo nuestro n es muy grande y p es muy pequeño, podemos aproximar el valor esperado de la
#variable aleatoria de Poisson cómo lambda = np por lo que pbinom nos da la probabilidad de la
#variable binomial y pois nos da la probabilidad de Poisson con lambda igual a np que en este caso es 2,25


#TERCER PUNTO

# Despejamos Sn en la hoja escrita.


#Esta función lo que hace es crear variables con valores entre 0 y 1 y si el valor de la variable 
#está dentro del conjunto que queremos obtener su probabilidad es igual a 1, de lo contrario la probabilidad
#es igual a 0
funcion <- function(){  
  a <- runif(1)
  les <- sqrt(1-a^2)
  if(a^2 + les^2 == 1){
    resultado = 1
  }else{
    resultado = 0
  }
  return(resultado)
}
#Esta función recursiva lo que hace es calcular el valor de Sn a partir de sus valores anteriores
recursive <- function(n){
  if(n<=1){
    q<- funcion()
  }else{
    q<-(((n-1)*recursive(n-1)+funcion())/n)
  }
  return(q)
}
#Cuando imprimimos el resultado tenemos que multiplicarlo por 4 ya que sólamante estamos en 1/4 de
#la circunferencia entonces sólo estamos encontrando pi/4 por lo que tenemos que multiplicar por 4
#para que podamos aproximar el valor de pi
print(4*recursive(4000))


# Sn = ((n-1)Sn-1 + Xn )/n




