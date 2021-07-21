 # Questão 7
df_7 <- data.frame(
  estado = c("Rio de Janeiro", "Goiás", "R. G. do Sul", "Paraná",
          "M. G. do Sul", "Sta. Catarina", "Pernambuco", "Paraíba", "Piauí",
          "Minas Gerais", "Mato Grosso", "R. G. do Norte", "Acre", "São Paulo",
          "Maranhão", "E. Santo", "Bahia", "Ceará", "D. Federal", "Tocantins",
          "Rondônia", "Alagoas", "Pará", "Sergipe", "Amazonas", "Roraima", 
          "Amapá"),
  leito = c(341.30, 344.83, 354.61, 362.32, 364.96, 375.94, 395.26, 398.41,
            404.86, 414.94, 418.41, 418.41, 421.94, 436.68, 440.53, 446.43,
            456.62, 467.29, 469.48, 471.70, 497.51, 507.61, 520.83, 552.49,
            641.03, 653.59, 800.00)
)

df_7

# Letra a
mediana_a <- median(df_7$leito)
desvio_a <- sd(df_7$leito)
dist_interq_a_ <- IQR(df_7$leito)


# Letra b
boxplot(df_7$leito, ylab = "Habitantes por leito")


# Letra c
df_7_c <- subset(df_7, estado != "Amapá")
df_7_c

mediana_c <- median(df_7_c$leito)
desvio_c <- sd(df_7_c$leito)
dist_interq_c <- IQR(df_7_c$leito)


# Letra d
# Observamos na letra b que o Amapá é um outlier com 800 habitantes por leito
# Por essa razão, quando calculamos a mediana, desvio padrão 
# e distância interquartil sem ele, os valores diminuem


# Questão 8
# Podemos observar que a corretora B possuem porcentagens de lucro mais
# simétricas, e por isso é melhor investir nela do que na A.


# Questão 9
df_9 <- read.table("C:/Users/Usuário/Downloads/Livros (Matéria)/Probabilidade e Estatística/Listas/Lista 1/MBindEcom.txt",
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
hist(df_9$Redacao, main = "Histograma notas Redação",
     xlab = "Notas", ylab = "Frequência")


# Letra d
barplot(table(df_9$Metodologia), main = "Distribuição da Metodologia",
        xlab = "Metodologias", ylab = "Frequência")


# Letra e
# ???


# Questão 10

# Letra a
# Z dá uma ideia do quão longe da média um dado está.


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

# Símbolo à esquerda de 2dp(Z) ???
# df_9_atipico_abaixo <- subset(df_9, notas_padrozinadas_estatist < 2 * sd_z)
# df_9_atipico_abaixo


# Letra e
func_direito <- 9 - mean(df_9$Direito)
func_estatist <- 9 - mean(df_9$Estatist)
func_politica <- 9 - mean(df_9$Politica)
# O funcionário teve melhor desempenho relativo em Política


# Questão 11
df_11 <- read.csv("C:/Users/Usuário/Downloads/Livros (Matéria)/Probabilidade e Estatística/Listas/Lista 1/ICUData.csv")

# Letra b
table(df_11$outcome)
table(df_11$heart.rate)


# Letra c
plot(df_11$age, main = "Gráfico de Dispersão da Idade",
     xlab = "Índice", ylab = "Idade")


# Letra d
barplot(table(df_11$sex), xlab = "Sexo", ylab = "Frequência")
barplot(table(df_11$surgery), xlab = "Tipo de cirurgia", ylab = "Frequência")
barplot(table(df_11$outcome), xlab = "Forma de saída do hospital", ylab = "Frequência")


# Letra e
quantile(df_11$heart.rate)
mean(df_11$heart.rate)
sd(df_11$heart.rate)
IQR(df_11$heart.rate)
mad(df_11$heart.rate)
var(df_11$heart.rate)


# Letra f
boxplot(df_11$heart.rate)
hist(df_11$heart.rate)


# Letra g
C <- IQR(df_11$temperature)/ 2 * (quantile(df_11$temperature, 0.9) - quantile(df_11$temperature, 0.1))
C/ 100
# Como C é menor do que 0.2631538, temperatura não segue uma distribuição normal


# Letra h
boxplot(df_11$heart.rate)
boxplot(df_11$SAPS.II)

assimetria_heart <- ((quantile(df_11$heart.rate, 0.75) - quantile(df_11$heart.rate, 0.5)) - (quantile(df_11$heart.rate, 0.5) - quantile(df_11$heart.rate, 0.25)))/ IQR(df_11$heart.rate) 
assimetria_saps <- ((quantile(df_11$SAPS.II, 0.75) - quantile(df_11$SAPS.II, 0.5)) - (quantile(df_11$SAPS.II, 0.5) - quantile(df_11$SAPS.II, 0.25)))/ IQR(df_11$SAPS.II) 


# Letra i
plot(cumsum(df_11$age))


# Letra j ???


# Letra k
assimetria_heart <- ((quantile(df_11$heart.rate, 0.75) - quantile(df_11$heart.rate, 0.5)) - (quantile(df_11$heart.rate, 0.5) - quantile(df_11$heart.rate, 0.25)))/ IQR(df_11$heart.rate) 
C_heart <- IQR(df_11$heart.rate)/ 2 * (quantile(df_11$heart.rate, 0.9) - quantile(df_11$heart.rate, 0.1))
C_heart/ 100
