########################################
# Teste 4 - INF-0612          
# Nome(s): 
########################################


names <- c("Horario", "Temperatura", "Vento", "Umidade", "Sensacao")
con <- url("https://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv")      
cepagri <- read.csv(con, header = FALSE, sep = ";", col.names = names)
cepagri <- cepagri[!is.na(cepagri[ , 5]), ]

cepagri$Horario2 <- as.POSIXct(as.character(cepagri$Horario), format="%d/%m/%Y-%H:%M")
cepagri2 <- cepagri[cepagri$Horario2 >= "2015/01/01" & cepagri$Horario2 < "2018/01/01",]

cepagri2$Temperatura <- as.numeric(as.character(cepagri2$Temperatura))

cepagri2$Ano <- format(strptime(as.character(cepagri2$Horario), "%d/%m/%Y-%H:%M"), "%Y")
cepagri2$Mes <- format(strptime(as.character(cepagri2$Horario), "%d/%m/%Y-%H:%M"), "%m")

library(ggplot2)

# Fonte: https://www.cpa.unicamp.br/outras-informacoes/clima-de-campinas.html
temp_medias_historicas <- c(24.7, 24.9, 24.7, 23.05, 20, 18.8, 18.5, 20.5, 21.8, 23.3, 23.8, 24.3)
dados_medios_historicos <-  data.frame(Mês = c(1:12), Temperatura = medias_historicas)

cepagri2$Ano <- format(strptime(as.character(cepagri2$Horario), "%d/%m/%Y-%H:%M"), "%Y")
cepagri2$Mes <- format(strptime(as.character(cepagri2$Horario), "%d/%m/%Y-%H:%M"), "%m")

################################################################################################
### Comparação Temperaturas Médias Mensais com a Temperatura Média Mensal Histórica
################################################################################################

#  Cria dataframe de temperaturas médias por mês durante os 3 anos
dados_medios_agrupados <- aggregate(cepagri2[ , 2:5], list(cepagri2$Ano, cepagri2$Mes), mean)
colnames(dados_medios_agrupados) <- c("Ano", "Mês", "Temperatura", "Vento", "Umidade", "Sensação")
# Remove colunas Umidade, Vento e Sensaçao do dataframe
dados_medios_agrupados <- dados_medios_agrupados[, (colnames(dados_medios_agrupados) %in% c("Ano","Mês","Temperatura"))]

# Plota dados médios em barras
g <- ggplot(dados_medios_historicos, aes(x = dados_medios_historicos$Mês, 
                                         y = dados_medios_historicos$Temperatura))
g <- g + geom_bar(stat="identity")

# Plota dados mensais em linhas (1 para cada ano)
g <- g + geom_line(data = dados_medios_agrupados,aes(x = dados_medios_agrupados$Mês , 
                                                     y = dados_medios_agrupados$Temperatura,
                                                     group = dados_medios_agrupados$Ano,
                                                     colour = dados_medios_agrupados$Ano))
g <- g + scale_x_discrete(name = "Mês", 
                            breaks = c(1:12)) 

g <- g + ylab("Temperatura") + labs(colour = "Anos",
                                    caption = "Fonte Dados Médios: https://orion.cpa.unicamp.br/outras-informacoes/clima-de-campinas.html",
                                    title = "Comparação Temperaturas Médias Mensais com Médias Históricas")
g

















