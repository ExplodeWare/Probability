est = 44.7/100 #estudiantes que usan wiki

numest = 41 # estudiantes de macc

x= 0:numest # Variable aleatoria

# Como se distribuye x?
# X como v.a. es una función del espacio muestral
# a un subconjunto de los números reales tenemos que 
# la función es 1 a 1, tomando subconjuntos del espacio muestral, por ejemplo:
# Nicolas usa wikipedia pero los demás no, entonces la variable nos daría 1,
# entre más estudiantes usen wikipedia, más grande es el número que nos da la imagen.


# Función de masa de probabilidad Binomial
PXx <- function(n,k,p){
  PXx = factorial(n)/(factorial(n-k)*factorial(k)) * p^k * (1-p)^(n-k)
}

vec <- vector("numeric",42) # Vector que recoge los datos de la PMF

for(i in 0:41){
  vec[i] = PXx(41,i,est) #Obtenemos los valores para graficar la PMF
}

plot(x,vec) # Graafica de la PMF Binomial

#--------------------------------------

# Probabilidad de que x = 17
# vec[18] Esto nos dice la probabilidad de que 17 hayan ido
# la probabilidad sería 0.12411

#-------------------------------------

#Probabilidad de que X sea como máximo 13
suma <- sum(vec[0:14])

#------------------------------

#Probabilidad de que X sea mayor que 11.

suma2 <- sum(vec[12:42])

#-----------------------------

#Probabilidad de que X esté entre 16 y 19

suma3 <- sum(vec[17:20])

#------------------------------


#            PUNTO 2

#       FUNCIONES DE VARIABLES ALEATORIAS

#Binomial

Bin <- function(n,k,p){
  Bin = factorial(n)/(factorial(n-k)*factorial(k)) * p^k * (1-p)^(n-k)
}
#______________________________________________________________________

#Geométrica

Geo <- function(k,p){
  Bin = p*(1-p)^(k-1)
}
#______________________________________________________________________

#Poisson

Poi <- function(k,lambda){
  Poi = exp(-lambda) * (lambda^k)/factorial(k) 
}
#______________________________________________________________________







