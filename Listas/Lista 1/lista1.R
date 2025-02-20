library(ggplot2)
library(agricolae)
library(dplyr)

# Quest�o 7
df_7 <- data.frame(
  estado = c("Rio de Janeiro", "Goi�s", "R. G. do Sul", "Paran�",
          "M. G. do Sul", "Sta. Catarina", "Pernambuco", "Para�ba", "Piau�",
          "Minas Gerais", "Mato Grosso", "R. G. do Norte", "Acre", "S�o Paulo",
          "Maranh�o", "E. Santo", "Bahia", "Cear�", "D. Federal", "Tocantins",
          "Rond�nia", "Alagoas", "Par�", "Sergipe", "Amazonas", "Roraima", 
          "Amap�"),
  leito = c(341.30, 344.83, 354.61, 362.32, 364.96, 375.94, 395.26, 398.41,
            404.86, 414.94, 418.41, 418.41, 421.94, 436.68, 440.53, 446.43,
            456.62, 467.29, 469.48, 471.70, 497.51, 507.61, 520.83, 552.49,
            641.03, 653.59, 800.00)
)

df_7

# Letra a
mediana_df_7 <- median(df_7$leito)
desvio_df_7 <- sd(df_7$leito)
dist_interq_df_7 <- IQR(df_7$leito)


# Letra b
boxplot(df_7$leito, ylab = "Habitantes por leito")


# Letra c
df_7_c <- subset(df_7, estado != "Amap�")
df_7_c

mediana_df_7_c <- median(df_7_c$leito)
desvio_df_7_c <- sd(df_7_c$leito)
dist_interq_df_7c <- IQR(df_7_c$leito)


# Letra d
# Observamos na letra b que o Amap� � um outlier com 800 habitantes por leito
# Por essa raz�o, quando calculamos a mediana, desvio padr�o 
# e dist�ncia interquartil sem ele, os valores diminuem


# Quest�o 8
# Podemos observar que a corretora B possuem porcentagens de lucro mais
# sim�tricas, e por isso � melhor investir nela do que na A.


# Quest�o 9
df_9 <- read.table("C:/Users/Usu�rio/Downloads/Livros (Mat�ria)/Probabilidade e Estat�stica/Listas/Lista 1/MBindEcom.txt",
                 sep = " ", fill = TRUE, header = TRUE)

# Letra a
# - Secao: ?
# - Administr: quantitativa discreta
# - Direito: quantitativa discreta
# - Redacao: quantitativa continua
# - Estatist: quantitativa discreta
# - Ingles: qualitativa ordinal
# - Metodologia: qualitativa nominal
# - Politica: quantitativa continua
# - Economia: quantitativa continua


# Letra b
mediana_direito <- median(df_9$Direito)
desvio_direito <- sd(df_9$Direito)
diq_direito <- IQR(df_9$Direito)

mediana_politica <- median(df_9$Politica)
desvio_politica <- sd(df_9$Politica)
diq_politica <- IQR(df_9$Politica)


mediana_estatistica <- median(df_9$Estatist)
desvio_estatistica <- sd(df_9$Estatist)
diq_estatistica <- IQR(df_9$Estatist)


# Letra c
hist(df_9$Redacao, main = "Histograma notas Reda��o",
     xlab = "Notas", ylab = "Frequ�ncia")


# Letra d
barplot(table(df_9$Metodologia), main = "Distribui��o da Metodologia",
        xlab = "Metodologias", ylab = "Frequ�ncia")


# Letra e
df_9_e <- df_9 %>%
  group_by(Secao, Estatist) %>%
  summarise(Media = mean(df_9$Estatist))

df_9_e
barplot(df_9_e$Estatist)

# barplot(
#  main = "Distribui��o das Notas em Estat�stica",
#        xlab = "Notas Estat�sitca", ylab = "Frequ�ncia")

media_estatistica <- mean(df_9$Estatist)


# Quest�o 10

# Letra a
# Z d� uma ideia do qu�o longe da m�dia um dado est�.


# Letra b
notas <- vector()

for(i in 1:nrow(df_9)){
  
  nota = (df_9$Estatist[i] - mean(df_9$Estatist))/ sd(df_9$Estatist)
  notas <- append(notas, nota)
  
}

df_9$notas_padrozinadas_estatist <- notas


# Letra c
mean_z <- mean(df_9$notas_padrozinadas_estatist)
sd_z <- sd(df_9$notas_padrozinadas_estatist)


# Letra d
df_9_atipico_acima <- subset(df_9, notas_padrozinadas_estatist > 2 * sd_z)
df_9_atipico_acima

# ?
# df_9_atipico_abaixo <- subset(df_9, notas_padrozinadas_estatist < 2 * sd_z)
# df_9_atipico_abaixo


# Letra e
func_direito <- 9 - mean(df_9$Direito)
func_estatist <- 9 - mean(df_9$Estatist)
func_politica <- 9 - mean(df_9$Politica)
# O funcion�rio teve melhor desempenho relativo em Pol�tica


# Quest�o 11
df_11 <- read.csv("C:/Users/Usu�rio/Downloads/Livros (Mat�ria)/Probabilidade e Estat�stica/Listas/Lista 1/ICUData.csv")

# Letra b
table(df_11$outcome)
table(df_11$heart.rate)


# Letra c
stem(df_11$age)
stripchart(df_11$age)

plot(df_11$age, main = "Gr�fico de Dispers�o da Idade",
     xlab = "�ndice", ylab = "Idade")


# Letra d
barplot(table(df_11$sex), xlab = "Sexo", ylab = "Frequ�ncia")
barplot(table(df_11$surgery), xlab = "Tipo de cirurgia", ylab = "Frequ�ncia")
barplot(table(df_11$outcome), xlab = "Forma de sa�da do hospital", ylab = "Frequ�ncia")


# Letra e
quantile(df_11$heart.rate)
mean(df_11$heart.rate)
sd(df_11$heart.rate)
IQR(df_11$heart.rate)
mad(df_11$heart.rate)
var(df_11$heart.rate)

assimetria_bowley <- function(df){
  
  q1 = quantile(df, 0.25)
  q2 = quantile(df, 0.5)
  q3 = quantile(df, 0.75)
  iqr = IQR(df)
  
  numerador = (q3 - q2) - (q2 - q1)
  denominador = q3 - q1
  
  return(numerador/ denominador)
}

assimetria_bowley(df_11$heart.rate)


# Letra f
boxplot(df_11$heart.rate)

ggplot(df_11, aes(x=heart.rate)) + geom_histogram()


# Letra g
curtose <- function(df){
  
  dq = IQR(df)
  q1 = quantile(df, 0.1)
  q9 = quantile(df, 0.9)
  
  return((dq/ (2 * (q9 - q1)))/ 100)
}

curtose(df_11$temperature)
# Como C � menor do que 0.2631538, temperatura n�o segue uma distribui��o normal


# Letra h
ggplot(df_11, aes(x=heart.rate)) + 
  geom_histogram(bins = 30, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(heart.rate)), 
             linetype = "dashed", size = 0.6)

ggplot(df_11, aes(x=SAPS.II)) + 
  geom_histogram(bins = 30, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(SAPS.II)), 
             linetype = "dashed", size = 0.6)

boxplot(df_11$heart.rate)
boxplot(df_11$SAPS.II)

assimetria_bowley(df_11$heart.rate)
assimetria_bowley(df_11$SAPS.II)

# Letra i
plot.ecdf(df_11$age)

ogiva <- graph.freq(df_11$age)
pontos <- ogive.freq(ogiva)


# Letra k
assimetria_bowley(df_11$heart.rate)
curtose(df_11$heart.rate)

ggplot(df_11, aes(x=heart.rate)) + 
  geom_histogram(bins = 30, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(heart.rate)), 
             linetype = "dashed", size = 0.6)
