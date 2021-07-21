# Questão 1
matrix(c(12, 14, 35, 7, 6, 12, 5, 22, 7, 17, 9, 11), 3, 4)
matrix(c(12, 14, 35, 7, 6, 12, 5, 22, 7, 17, 9, 11), 3, 4, byrow = TRUE)


# Questão 2
vec <- c(0, 4, 2, 1, 0, 4, 0, 3, 0, 3, 3, 3, 4, 4, 2, 0)
vec
vec[(vec < 3)]


# Questão 3
fact <- function(num){
  
  factorial = 1
  
  if(num < 0){
    return("Não há fatorial para números negativos")
  } else if(num == 0){
    return(factorial)
  } else{
    for(i in 1:num){
      factorial = factorial * i
    }
  }
  
  return(factorial)
}

fact(-1)
fact(0)
fact(5)


# Questão 4
par_impar <- function(num){
  if(num %% 2 == 0){
    return("Par")
  } else{
    return("Ímpar")
  }
}

par_impar(2)
par_impar(3)


# Questão 5
# Letra a
soma_a = 0

for(i in 1:100){
  soma_a = soma_a + (1/ i)
}

soma_a


# Letra b
soma_b = 1
denominador = 2

for(i in 2:100){
  denominador = denominador + 20
  soma_b = soma_b + (1/ denominador)
}

soma_b


# Letra c
soma_c = 0

for(i in 1:100){
  soma_c = soma_c + (1/ ((1 + 1/ fact(i)) * 2))
}

soma_c


# Letra d
soma_par = 0
soma_impar = 0

for(i in seq(2, 100, by = 2)){
  soma_par = soma_par - (1/ i)
}

for(i in seq(1, 100, by = 2)){
  soma_impar = soma_impar + (1/ i)
}

soma_d = soma_par + soma_impar
soma_d


# Questão 6
delta <- function(a, b, c){
  return((b^2) - (4 * a * c))
}

raizes <- function(a, b, c){
  
  raiz1 = (b * (-1) - sqrt(delta(a, b, c)))/ (2 * a)
  raiz2 = (b * (-1) + sqrt(delta(a, b, c)))/ (2 * a)
  
  resposta = c(raiz1, raiz2)
  return(resposta)
}

a = 1
b = -5
c = 6
delta(a, b, c)
raizes(a, b, c)