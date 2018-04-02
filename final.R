########################################
# Trabalho Final- INF-0612          
# Nome(s): Carlos Eduardo Fernandes
#          Marina Tachibana            
########################################

# Limpa as variáveis do workspace do R
rm(list=ls())

consecutive_horarios <- function (my_vector, k = 1){
  n <- length(my_vector)
  result <- c(rep(FALSE, n))
  for (i in 2:n) {
    result[i] <- difftime(my_vector[i],my_vector[i-1],units="mins") == 10
  }
  return(result)
}

# Leitura dos dados
names <- c("Horario", "Temperatura", "Vento", "Umidade", "Sensacao")
con <- url("https://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv")      
cepagri <- read.csv(con, header = FALSE, sep = ";", col.names = names)

# Remove linhas faltando dados
cepagri <- cepagri[!is.na(cepagri[ , 5]), ]

# Remove linhas com outlier de Sensacao de 99.9
cepagri <- cepagri[cepagri[ , 5] != 99.9, ]

# Cria nova coluna Horario2 com horário formatado como data
cepagri$Horario2 <- as.POSIXct(as.character(cepagri$Horario), format="%d/%m/%Y-%H:%M")

# Cria data frame com dados filtrados de 01/01/2015 a 31/12/2017
cepagri2 <- cepagri[cepagri$Horario2 >= "2015/01/01" & cepagri$Horario2 < "2018/01/01",]

# Força colunas Temperatura, Vento, Umidade e Sensação a serem numericas
cepagri2$Temperatura <- as.numeric(as.character(cepagri2$Temperatura))
cepagri2$Vento <- as.numeric(as.character(cepagri2$Vento))
cepagri2$Umidade <- as.numeric(as.character(cepagri2$Umidade))
cepagri2$Sensacao <- as.numeric(as.character(cepagri2$Sensacao))

# Cria novas colunas Ano, Mês e Dia
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
# Plota dados médios em barras para temperatura
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

# Centraliza o título do gráfico
g <- g  + theme(plot.title = element_text(hjust = 0.5))

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
g_min <- g_min + labs(title = "Variação das Métricas nos dias 12 e 13/Junho/2016")
# Centraliza o título do gráfico
g_min <- g_min + theme(plot.title = element_text(hjust = 0.5))
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

# GRÁFICO 3
# Grafico de linha com os valores de vento nos anos de 2015-2017
g_vento <- ggplot(cepagri2) + geom_line(aes(x = cepagri2$Horario2, y = cepagri2$Vento ))
g_vento <- g_vento + ylab("Vento (Km/H") + 
  xlab("Horários") +
  labs(title = "Vento em Campinas nos anos 2015-2017")
# Centraliza o título do gráfico
g_vento <- g_vento + theme(plot.title = element_text(hjust = 0.5))
g_vento

# GRÁFICO 4
# Plota bloxplot do vento com facewarp por ano
g_bloxplot_Vento <- ggplot(cepagri2, aes(x = Mes,
                                         y = Vento,
                                         group = Mes,
                                         fill = Mes))
g_bloxplot_Vento <- g_bloxplot_Vento + geom_boxplot()
g_bloxplot_Vento <- g_bloxplot_Vento + facet_wrap (~ Ano)
g_bloxplot_Vento <- g_bloxplot_Vento + labs(title = "BloxPlot do Vento com FaceWarp Por Ano")
# Centraliza o título do gráfico
g_bloxplot_Vento <- g_bloxplot_Vento + theme(plot.title = element_text(hjust = 0.5))
g_bloxplot_Vento

# Qual a ordem do vento da micro-explosão em relaçao ao vento do período 2015-2017 (106)
match(vento_maximo_junho_2016, vento_ordenado_periodo)

# Análise de vento = 0
vento_zero <- cepagri2$Vento == 0
horarios_vento_zero <- cepagri2$Horario2[vento_zero]
sum(vento_zero)
# Verifica se existe medições seguidas com vento 0
sum(consecutive_horarios(horarios_vento_zero))

# Sequencia de zero: 26/08/2015
## Vento e  umidade diminuíram, temperatura aumentou.
## Vento e  umidade aumentaram, temperatura caiu
# GRÁFICO 5
seq_zero1 <- cepagri2[cepagri2$Horario2 >= "2015-08-26" & cepagri2$Horario2 < "2015-08-27",]
g_seq_zero1 <- ggplot(seq_zero1, aes(x = seq_zero1$Horario2, y = value))
g_seq_zero1 <- g_seq_zero1 + geom_line(aes(y = seq_zero1$Temperatura, colour="Temperatura"))
g_seq_zero1 <- g_seq_zero1 + geom_line(aes(y = seq_zero1$Vento, colour="Vento"))
g_seq_zero1 <- g_seq_zero1 + geom_line(aes(y = seq_zero1$Umidade, colour="Umidade"))
g_seq_zero1 <- g_seq_zero1 + geom_line(aes(y = seq_zero1$Sensacao, colour="Sensação"))
g_seq_zero1 <- g_seq_zero1 + xlab("Horário") + ylab("") +
                             labs(title = "Variação das Métricas em 26/08/2015")
g_seq_zero1 <- g_seq_zero1 + scale_colour_manual("", 
                                                 values = c("Temperatura"="green", "Vento"="red", 
                                                            "Umidade"="blue", "Sensação"="yellow"))
# Centraliza o título do gráfico
g_seq_zero1 <- g_seq_zero1 + theme(plot.title = element_text(hjust = 0.5))
g_seq_zero1

## Análise:
## 1 - 2 outliers são dados válidos
## 2 - Apesar do estrago, o vento da micro-explosão foi apenas 106o mais forte do período
## 3 - 202 medições com Vento = 0, 2 análises de vento = 0

################################################################################################
### Análise da correlação entre as métricas
################################################################################################

# GRÁFICO 6
# Plota a correlacao entre temperatura, sensacao, vento e umidade
correlacao <- c(cor(cepagri2$Temperatura, cepagri2$Sensacao),
                cor(cepagri2$Vento, cepagri2$Sensacao),
                cor(cepagri2$Umidade, cepagri2$Sensacao),
                cor(cepagri2$Temperatura, cepagri2$Vento),
                cor(cepagri2$Temperatura, cepagri2$Umidade),
                cor(cepagri2$Vento, cepagri2$Umidade))
variaveis <- c("Temperatura e Sensação",
               "Vento e Sensação",
               "Umidade e Sensação",
               "Temperatura e Vento",
               "Temperatura e Umidade",
               "Vento e Umidade")
df <- data.frame(eixox = variaveis, eixoy = correlacao)
g_cor <- ggplot(df, aes(x = eixox, y = eixoy));
g_cor <- g_cor + geom_bar(stat = "identity");
g_cor <- g_cor + ylab("Correlação") + xlab("Pares de Métricas") +
               labs(title = "Correlação entre Métricas")
# Centraliza o título do gráfico
g_cor <- g_cor + theme(plot.title = element_text(hjust = 0.5))
# Rotaciona de 90 graus a legenda do eixo x para evitar sobreposição de textos
g_cor <- g_cor + theme(axis.text.x = element_text(angle = 90))
g_cor

# GRÁFICO 7 sobre variação das métricas em 13/05/2017
seq_zero2 <- cepagri2[cepagri2$Horario2 >= "2017-05-13" & cepagri2$Horario2 < "2017-05-14",]
g_seq_zero2 <- ggplot(seq_zero2, aes(x = seq_zero2$Horario2, y = value))
g_seq_zero2 <- g_seq_zero2 + geom_line(aes(y = seq_zero2$Temperatura, colour="Temperatura"))
g_seq_zero2 <- g_seq_zero2 + geom_line(aes(y = seq_zero2$Vento, colour="Vento"))
g_seq_zero2 <- g_seq_zero2 + geom_line(aes(y = seq_zero2$Umidade, colour="Umidade"))
g_seq_zero2 <- g_seq_zero2 + geom_line(aes(y = seq_zero2$Sensacao, colour="Sensação"))
g_seq_zero2 <- g_seq_zero2 + xlab("Horário") + ylab("") +
  labs(title = "Variação Métricas em 13/05/2017")
g_seq_zero2 <- g_seq_zero2 + scale_colour_manual("",
                                                 values = c("Temperatura"="green", "Vento"="red",
                                                            "Umidade"="blue", "Sensação"="yellow"))
# Centraliza o título do gráfico
g_seq_zero2 <- g_seq_zero2 + theme(plot.title = element_text(hjust = 0.5))
g_seq_zero2