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

medias_historicas <- c(24.7, 24.9, 24.7, 23.05, 20, 18.8, 18.5, 20.5, 21.8, 23.3, 23.8, 24.3)

#  Cria dataframe e gráfico de temperaturas médias por mês durante os 3 anos.
cepagri2$Ano <- format(strptime(as.character(cepagri2$Horario), "%d/%m/%Y-%H:%M"), "%Y")
cepagri2$Mes <- format(strptime(as.character(cepagri2$Horario), "%d/%m/%Y-%H:%M"), "%m")
teste <- aggregate(cepagri2[ , 2:5], list(cepagri2$Ano, cepagri2$Mes), mean)
g <- ggplot(teste , aes(x = teste$Mês , y = teste$Temperatura, group=teste$Ano, color=teste$Ano)) + geom_line()