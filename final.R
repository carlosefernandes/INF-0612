########################################
# Trabalho Final- INF-0612          
# Nome(s): Carlos Eduardo Fenandes
#          Marina Tachibana            
########################################

consecutive <- function(vector , k = 1) {
  n <- length(vector)
  result <- logical(n)

  for (i in (1+k):n)
    if (all(vector[(i-k):(i-1)] == vector[i]))
      result[i] <- TRUE
  return(result)
}

consecutive_horarios <- function (my_vector, k = 1){
  n <- length(my_vector)
  result <- c(rep(FALSE, n))
    for (i in 2:n) {
    result[i] <- difftime(my_vector[i],my_vector[i-1],units="mins") == 10
  }
  return(result)
}

names <- c("Horario", "Temperatura", "Vento", "Umidade", "Sensacao")
con <- url("https://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv")      
cepagri <- read.csv(con, header = FALSE, sep = ";", col.names = names)

# Remove linhas faltando dados
cepagri <- cepagri[!is.na(cepagri[ , 5]), ]

# Cria nova coluna Horario2 com horário formatado como data
cepagri$Horario2 <- as.POSIXct(as.character(cepagri$Horario), format="%d/%m/%Y-%H:%M")

# Cria data frame com dados filtrados de 01/01/2014 a 31/12/2017
cepagri2 <- cepagri[cepagri$Horario2 >= "2015/01/01" & cepagri$Horario2 < "2018/01/01",]

# Força colunas Temperatura, Vento, Umidade e Sensação a serem numericas
cepagri2$Temperatura <- as.numeric(as.character(cepagri2$Temperatura))
cepagri2$Vento <- as.numeric(as.character(cepagri2$Vento))
cepagri2$Umidade <- as.numeric(as.character(cepagri2$Umidade))
cepagri2$Sensacao <- as.numeric(as.character(cepagri2$Sensacao))

# Cria novas colunas Ano e Mês
cepagri2$Ano <- as.numeric(format(strptime(as.character(cepagri2$Horario), "%d/%m/%Y-%H:%M"), "%Y"))
cepagri2$Mes <- as.numeric(format(strptime(as.character(cepagri2$Horario), "%d/%m/%Y-%H:%M"), "%m"))
cepagri2$Dia <- as.numeric(format(strptime(as.character(cepagri2$Horario), "%d/%m/%Y-%H:%M"), "%d"))

library(ggplot2)

# Fonte: https://www.cpa.unicamp.br/outras-informacoes/clima-de-campinas.html
temp_medias_historicas <- c(24.7, 24.9, 24.7, 23.05, 20, 18.8, 18.5, 20.5, 21.8, 23.3, 23.8, 24.3)
dados_medios_historicos <-  data.frame(Mês = c(1:12), Temperatura = temp_medias_historicas)

################################################################################################
### Comparação Temperaturas Médias Mensais com a Temperatura Média Mensal Histórica
################################################################################################

#  TABELA 1: Cria dataframe de temperaturas médias por mês durante os 3 anos
dados_medios_agrupados <- aggregate(cepagri2[ , 2:6], list(cepagri2$Ano, cepagri2$Mes), mean)
colnames(dados_medios_agrupados) <- c("Ano", "Mês", "Temperatura", "Vento", "Umidade", "Sensação")
# Remove colunas Umidade, Vento e Sensaçao do dataframe
dados_medios_agrupados <- dados_medios_agrupados[, (colnames(dados_medios_agrupados) %in% c("Ano","Mês","Temperatura"))]

# GRÁFICO 1
# Plota dados médios em barras
g <- ggplot(dados_medios_historicos, aes(x = as.factor(dados_medios_historicos$Mês), 
                                         y = dados_medios_historicos$Temperatura))
g <- g + geom_bar(stat="identity")

# Plota dados mensais em linhas (1 para cada ano)
g <- g + geom_line(data = dados_medios_agrupados,aes(x = dados_medios_agrupados$Mês , 
                                                     y = dados_medios_agrupados$Temperatura,
                                                     group = as.factor(dados_medios_agrupados$Ano),
                                                     colour = as.factor(dados_medios_agrupados$Ano)))
g <- g + scale_x_discrete(name = "Mês", 
                            breaks = c(1:12)) 

g <- g + ylab("Temperatura") + labs(colour = "Anos",
                                    caption = "Fonte Dados Médios: https://orion.cpa.unicamp.br/outras-informacoes/clima-de-campinas.html",
                                    title = "Comparação Temperaturas Médias Mensais com Médias Históricas")
g

################################################################################################
### Análise de extremos de Temperatura Mínima e Sensação Térmica
################################################################################################

# TABELA 2: Tabela com temperatura mínima e sensação térmica mínima diária
min_agregado <- aggregate(cepagri2[ , c(2:5)], list(cepagri2$Dia, cepagri2$Mes, cepagri2$Ano), min)
colnames(min_agregado) <- c("dia", "mes", "ano", "Temperatura", "Vento", "Umidade","Sensação")

# TABELA 3:
## Temperaturas mínimas em geral são altas em Campinas. Apenas 9 vezes em 3 anos a temperatura ficou abaixo de 8
## Nenhum dia teve minima inferior a 5
sum(min_agregado$Temperatura < 5)
sum(min_agregado$Temperatura >= 5 & min_agregado$Temperatura < 8)
sum(min_agregado$Temperatura >= 8 & min_agregado$Temperatura < 10)
sum(min_agregado$Temperatura >= 10 & min_agregado$Temperatura < 15)
sum(min_agregado$Temperatura >= 15 & min_agregado$Temperatura < 20)
sum(min_agregado$Temperatura >= 20)

## Se olharmos a sensação térmica: Temos 46 casos de sensação térmica negativa.
sum(min_agregado$Sensação < -5)
sum(min_agregado$Sensação >= -5 & min_agregado$Sensação < 0)
sum(min_agregado$Sensação >= 0 & min_agregado$Sensação < 5)
sum(min_agregado$Sensação >= 5 & min_agregado$Sensação < 8)
sum(min_agregado$Sensação >= 8 & min_agregado$Sensação < 10)
sum(min_agregado$Sensação >= 10 & min_agregado$Sensação < 15)
sum(min_agregado$Sensação >= 15 & min_agregado$Sensação < 20)
sum(min_agregado$Sensação >= 20)

summary(min_agregado$Sensação)
summary(min_agregado$Temperatura)

# Data da menor temperatura no período - 12/06/2016
min_agregado[min_agregado$Temperatura == min(min_agregado$Temperatura),]

# Data da menor sensaçao termica no período  - 13/06/2016
min_agregado[min_agregado$Sensação == min(min_agregado$Sensação),]

g_temp_min <- ggplot()
# Olhando os extremos:
# Temp Min x Sensação Minima : 5.1 e -8.00 ... Uma diferença muito grande e curiosamente são dias em sequencia
# 12/06/2016 - 6:40 - 16.2 - 98.4
# 13/06/2016 - 7:00 - 66.8 - 57.4

mean(cepagri2$Umidade[cepagri2$Horario2 >= "2016-06-12" & cepagri2$Horario2 < "2016-06-14"])
mean(cepagri2$Vento[cepagri2$Horario2 >= "2016-06-12" & cepagri2$Horario2 < "2016-06-14"])
# Umidade media de 60,43%
# Vento medio de 40,13 km

# GRAFICO 2
df_12_13 <- cepagri2[cepagri2$Horario2 >= "2016-06-12" & cepagri2$Horario2 < "2016-06-14",]
g_min <- ggplot(df_12_13) + geom_line(aes(x = df_12_13$Horario2, y = df_12_13$Vento, colour="Vento"))
g_min <- g_min + geom_line(aes(x = df_12_13$Horario2, y = df_12_13$Umidade, colour="Umidade"))
g_min <- g_min + geom_line(aes(x = df_12_13$Horario2, y = df_12_13$Temperatura, colour="Temperatura"))
g_min <- g_min + geom_line(aes(x = df_12_13$Horario2, y = df_12_13$Sensacao, colour="Sensação térmica"))
g_min <- g_min  + xlab("Horário") + ylab("")
g_min

################################################################################################
### Análise Micro-Explosão em 05/06/2016
### http://g1.globo.com/sp/campinas-regiao/noticia/2016/06/entenda-o-que-e-microexplosao-que-atingiu-campinas-veja-trajetoria-dela.html
### Vento Máximo em Campinas
### http://g1.globo.com/sp/campinas-regiao/noticia/2015/12/temporal-tem-ventos-de-ate-143-kmh-na-regiao-de-campinas-diz-cepagri.html
### Segundo vento máximo em Campinas
### http://g1.globo.com/sp/campinas-regiao/noticia/2015/09/temporal-tem-ventos-ate-1425-kmh-na-regiao-de-campinas-diz-cepagri.html
################################################################################################

# Dataframe de Junho/2016
vento_junho_2016 <- cepagri2[cepagri2$Horario2 > "2016-06-01" & cepagri2$Horario2 < "2016-07-01",]

# Ordena o vento de Campinas em ordem decrescente
vento_ordenado_periodo <- sort(cepagri2$Vento, decreasing = TRUE)

# Vento Máximo em Junho de 2016 (88.6)
vento_maximo_junho_2016 <- max(vento_junho_2016$Vento)

# Horário do Vento Máximo em Campinas (05/06/2016-00:50)
horario_vento_maximo_junho_2016 <- vento_junho_2016$Horario2[vento_junho_2016$Vento == vento_maximo_junho_2016]

# Vento Máximo nos anos de 2015, 2016 e 2017 (143.6)
vento_maximo_periodo <- max(cepagri2$Vento)

# Horário do Vento Máximo em Campinas (2015-12-12 14:50:00)
horario_vento_maximo_periodo <- cepagri2$Horario2[cepagri2$Vento == vento_maximo_periodo]

# Segundo Vento Máximo nos anos de 2015, 2016 e 2017 (142.5)
segundo_vento_maximo_periodo <- vento_ordenado_periodo[2]

# Horário do Segundo Vento Máximo em Campinas (2015-09-28 00:50:00)
horario_segundo_vento_maximo_periodo <- cepagri2$Horario2[cepagri2$Vento == segundo_vento_maximo_periodo]

# GRAFICO 3 - Grafico de linha com os valores de vento nos anos de 2015-2017
g_vento <- ggplot(cepagri2) + geom_line(aes(x = cepagri2$Horario2, y = cepagri2$Vento ))
g_vento <- g_vento + ylab("Vento (Km/H") + 
                     xlab("Horários") +
                     labs(title = "Vento em Campinas nos anos 2015-2017")
g_vento

# Qual a ordem do vento da micro-explosão em relação ao vento do período 2015-2017 (106)
match(vento_maximo_junho_2016, vento_ordenado_periodo)

# Análise de vento = 0
vento_zero <- cepagri2$Vento == 0
horarios_vento_zero <- cepagri2$Horario2[vento_zero]
sum(vento_zero)
# Verifica se existe medições seguidas com vento 0
sum(consecutive_horarios(horarios_vento_zero))
# GRAFICO 4
# Sequencia de zero: 26/08/2015
## Vento e  umidade diminuíram, temperatura aumentou.
## Vento e  umidade aumentaram, temperatura caiu
seq_zero1 <- cepagri2[cepagri2$Horario2 >= "2015-08-26" & cepagri2$Horario2 < "2015-08-27",]
g_seq_zero1 <- ggplot(seq_zero1, aes(x = seq_zero1$Horario2, y = value))
g_seq_zero1 <- g_seq_zero1 + geom_line(aes(y = seq_zero1$Temperatura, colour="Temperatura"))
g_seq_zero1 <- g_seq_zero1 + geom_line(aes(y = seq_zero1$Vento, colour="Vento"))
g_seq_zero1 <- g_seq_zero1 + geom_line(aes(y = seq_zero1$Umidade, colour="Umidade"))
g_seq_zero1 <- g_seq_zero1 + geom_line(aes(y = seq_zero1$Sensacao, colour="Sensação"))
g_seq_zero1 <- g_seq_zero1 + xlab("Horário") + ylab("") +
                             labs(title = "Variação Vento/Umidade/Temperatura em 26/08/2015")
g_seq_zero1 <- g_seq_zero1 + scale_colour_manual("", 
                                                 values = c("Temperatura"="green", "Vento"="red", 
                                                            "Umidade"="blue", "Sensação"="yellow"))
g_seq_zero1