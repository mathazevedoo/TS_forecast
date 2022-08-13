################################################################
####   AULA - SÉRIES TEMPORAIS - Fabiano - MBA USP ESALQ
####                   DATA SCIENCE e ANALYTICS
################################################################

# Instalação e Carregamento de Todos os Pacotes ---------------------------
# Rotina prof. Rafael Souza e Prof Fávero

pacotes <- c("readxl","plotly","tidyverse","gridExtra","forecast","TTR",
             "smooth","tidyverse", "tsibble", "fable","tsibbledata", "fpp3",
             "urca",)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
        instalador <- pacotes[!pacotes %in% installed.packages()]
        for(i in 1:length(instalador)) {
                install.packages(instalador, dependencies = T)
                break()}
        sapply(pacotes, require, character = T)
} else {
        sapply(pacotes, require, character = T)
}

#-------------------------------------------------------------------------

###### Importando dados do Excel

##### Importando uma base de dados com 7 colunas de dados

###### Buscar o caminho do diretório onde está salvo a base de dados

basepetr4 <- read_excel("basepetr4.xlsx")

###### Lendo a base de dados

View(basepetr4)

##### Lendo apenas as primeiras linhas das variáveis

head(basepetr4)

basepetr4
# - Note: é um tibble de 456 x 7

global_economy
# - Note: é um tsibble: permite o armazenamento e manipulação de múltiplas
# séries temporais no R.
# Ele contêm: um index (informação de tempo, normalmente o período de tempo)
# Possui as variáveis medidas e as variáveis chave
# coluna que contém a variável chave *ou as chaves (identificadores únicos
# opcionais para cada série)
# base disponível:


##### Buscando a cotação de fechamento apenas da PETR4 e chamando os dados em petr4

petr4=basepetr4[4]

##### Lendo os dados iniciais brutos

head(petr4)

##### se quiser usar o comando gráfico veja o que irá acontecer

plot(petr4)

# primeiro tenho que definir que o conjunto de dados petr4 como uma série temporal, pelo
# comando time series (ts)

petr4=ts(petr4)

##### Agora sim, o gráfico como série de tempo
ggplotly(
        basepetr4 %>%
                mutate(Data = as.Date(Data)) %>%
                ggplot() +
                geom_line(aes(x = Data, y = Fechamento, color = "série")) +
                scale_color_viridis_d() +
                scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
                      panel.background = element_rect(fill = "white", color = "black"),
                      panel.grid = element_line(color = "grey90"),
                      panel.border = element_rect(color = "black", fill = NA),
                      legend.position = "none")
)

# Observe que tenho 456 observações; posso informar

petr4=ts(petr4, start=1, end=456)

plot(petr4, main="Cotações da PETR4 - jan/2020 a nov/2021",
     xlab="Tempo em dias (02/01/20 a 01/11/21)", ylab="R$/ação" )

# Considerando as demais informações da base de dados basepetr4

q_neg=ts(basepetr4[2])
volume=ts(basepetr4[3])
abertura=ts(basepetr4[5])

# colocando os dados em forma de uma matriz com todos os dados no conjunto dados1

dados1=ts(matrix(1,456,4))
dados1[,1]=petr4
dados1[,2]=q_neg
dados1[,3]=volume
dados1[,4]=abertura

colnames(dados1)[1]='Fechamento R$'
colnames(dados1)[2]='Quant. Neg'
colnames(dados1)[3]='Volume Financ. Neg'
colnames(dados1)[4]='Abertura R$'

plot(dados1, main="Informações da ação PETR4",
xlab="Tempo em dias (02/01/20 a 01/11/21) -
     Fonte: Economática")

# Construindo uma janela com 4 gráficos

par(mfrow=c(2,2))
plot(petr4, main="Cotação de Fechamento PETR4 - jan/20 a nov/21",
     xlab="tempo em dias - 02/01/20 a 01/11/21", ylab="R$")
options(scipen = 999)
plot(q_neg, main="Quantidade de Negócios Realizados - PETR4",
     xlab="tempo em dias - 02/01/20 a 01/11/21", ylab="Núm.Negócios")
options(scipen = 999)
plot(volume, main="Volume Financeiro Negociado PETR4 - jan/20 a nov/21",
     xlab="tempo em dias - 02/01/20 a 01/11/21", ylab="R$")
plot(abertura, main="Cotação de Abertura PETR4 - jan/20 a nov/21",
     xlab="tempo em dias - 02/01/20 a 01/11/21", ylab="R$")

# preciso encerrar o comparticionamento dos gráficos

par(mfrow=c(1,1))


############################################################################
### Trabalhando com a base de dados de Passageiros no Transporte Aéreo - BR
############################################################################

###### Importando a base de dados

passageiros <- read_excel("passageiros.xlsx")

###### Lendo a base de dados

View(passageiros)

##### A base de dados tem duas colunas, e os passageiros transportados est?o na coluna 2

passageiros = passageiros[2]

##### Lendo apenas as primeiras linhas das vari?veis

head(passageiros)

# definindo o conjunto de dados passageiros como uma série temporal, pelo
# comando time series (ts)

passageiros=ts(passageiros)

# fazendo o gráfico

plot(passageiros, main="Total de Passageiros no Transporte Aéreo BR",
     xlab="Jan/2011 a set/2021", ylab="Total de Passageiros Mensal" )


#####################################################################################
### Trabalhando com uma Série Temporal da Receita Trimestral da AMBEV
#####################################################################################

###### Importando a base de dados

ambev <- read_excel("ambev.xlsx")

###### Lendo a base de dados
View(ambev)

###### Ajustando a base de dados trimestrais da Receita Líquida Operacional da AMBEV

receita=ts(ambev[2], start=c(2000,1), end=c(2021,3), frequency=4)

###### plotando o grafico
# options(scipen = 999)
plot(receita, main="Faturamento Trimestral da AMBEV SA - 1T/2000 ao 3T/2021",
     xlab="Trimestres - jan/2000 a set/2021", ylab="R$ mil")


############################################################################
### Trabalhando com a base de dados das Manchas Solares
############################################################################

###### Importando a base de dados

manchas <- read_excel("manchas.xlsx")


# Transformar o data frame em série de tempo

sol=ts(manchas$manchas,start=c(1749,1), end=c(2021,10), frequency = 12)

plot(sol, main="Número Médio Mensal de Manchas Solares",
     xlab="Mensal - jan/1749 a out/2021")

# Medidas das Estat?sticas Descritivas da S?rie Temporal

summary(sol)
sd(sol)
length(sol)

# Definir uma janela de tempo da Série Temporal

sol1=window(sol,c(1749,1), c(1990,12))

plot(sol1)


############################################################################
### Trabalhando com uma Série Temporal de Números Aleatórios
############################################################################


# gerando números aleatórios

aleat=ts(rnorm(500))

View(aleat)

plot(aleat, main="Série Temporal de Números Aleatórios com Distribuição Normal Padrão",
     xlab="quantidade de números aleatórios")

mean(aleat)
sd(aleat)

# gerar uma série de números aleatórios com média 2 e dp = 0.1

aleat1=ts(rnorm(500, 2,0.1))
plot(aleat1)
mean(aleat1)
sd(aleat1)

############################################################################
### Trabalhando com uma Série Temporal de um Passeio Aleatório
############################################################################

passeio=cumsum(aleat)


plot(passeio, type='l', main="Passeio Aleatório", xlab = "Núm. Observações")

############################################################################
### Comparando a Série PETR4(jan/2020 a nov/2021) com o Passeio Aleatório
############################################################################

##### criando a série petropasseio com mesmo tamanho da petr4, inicialmente todos os valores
##### com zero

petropasseio=ts(0, start=1, end=456)

##### séries começam no mesmo ponto
petropasseio[1] = petr4[1]

##### gerando um passeio aleatório

for(i in 2:456){petropasseio[i]=petropasseio[i-1]+rnorm(1)}

##### plotando no mesmo gráfico as duas séries

plot(petr4, main="Cotação Original de Fechamento e Random Walk",
     xlab="tempo em dias - jan/20 a nov/21",
     ylab="R$", ylim=c(min(petr4),max(petropasseio)))
par(new=TRUE)
lines(petropasseio, type="l", lty=2, ylim=c(min(petr4),max(petropasseio)))
legend("bottomright", c('Cotação Original','Random Walk'), lty = 1:2, bty='n')




############################################################################
#####  DECOMPOSIÇÃO DE SÉRIES TEMPORAIS
############################################################################

##### TendÊncia por médias móveis

# Analisando a série de casos diários de COVID 19 Brasil

# lendo a base de dados: Fonte: https://covid.saude.gov.br/

covid <- read_excel("covid.xlsx")

# visualizando a série com as médias móveis
ggplotly(
        covid %>%
                mutate(Data = as.Date(Data),
                       media_movel = ma(por_dia, order=14)) %>%
                ggplot() +
                geom_line(aes(x = Data, y = por_dia, color = "Por Dia")) +
                geom_line(aes(x = Data, y = media_movel, color = "Média Móvel"), size = 1) +
                labs(color = "Legenda:",
                     x = "Data",
                     y = "Comportamento da Covid-19") +
                scale_color_viridis_d() +
                scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
                      panel.background = element_rect(fill = "white", color = "black"),
                      panel.grid = element_line(color = "grey90"),
                      panel.border = element_rect(color = "black", fill = NA),
                      legend.position = "bottom")
) %>% layout(showlegend = TRUE,
             legend = list(orientation = "h"))
# Podemos notar a presença de outliers

# Gráfico Box-Plot para verificar se temos outliers

covid %>%
        mutate(Data = as.Date(Data),
               media_movel = ma(por_dia, order=14)) %>%
        ggplot() +
        geom_boxplot(aes(x = Data, y = por_dia, color = "Por Dia")) +
        labs(x = "Data",
             y = "Comportamento da Covid-19") +
        scale_color_viridis_d() +
        scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
              panel.background = element_rect(fill = "white", color = "black"),
              panel.grid = element_line(color = "grey90"),
              panel.border = element_rect(color = "black", fill = NA),
              legend.position = "none")

#### Limpando os dados com comando TSCLEAN

covid %>%
        mutate(Data = as.Date(Data),
               covid_suave = tsclean(por_dia),
               media_movel = ma(por_dia, order=14)) %>%
        ggplot() +
        geom_line(aes(x = Data, y = por_dia, color = "Série Original"), size = 1) +
        geom_line(aes(x = Data, y = covid_suave, color = "Série Suavizada")) +
        geom_line(aes(x = Data, y = media_movel, color = "Média Móvel"), size = 1) +
        labs(color = "Legenda:",
             x = "Data",
             y = "Comportamento da Covid-19") +
        scale_color_viridis_d() +
        scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
              panel.background = element_rect(fill = "white", color = "black"),
              panel.grid = element_line(color = "grey90"),
              panel.border = element_rect(color = "black", fill = NA),
              legend.position = "bottom")

# note que o gráfico amarelo removeu os outliers.
# para saber quais pontos foram removidos e quais entraram no lugar

tsoutliers(covid$por_dia)


## Cálculo da mádia móvel Não centralizada

ggplotly(
        covid %>%
                mutate(Data = as.Date(Data),
                       media_movel_nao_centralizada = SMA(por_dia, 14)) %>%
                ggplot() +
                geom_line(aes(x = Data, y = por_dia, color = "Por Dia")) +
                geom_line(aes(x = Data, y = media_movel_nao_centralizada,
                              color = "Média Móvel Não Centralizada"), size = 1) +
                labs(color = "Legenda:",
                     x = "Data",
                     y = "Comportamento da Covid-19") +
                scale_color_viridis_d() +
                scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
                      panel.background = element_rect(fill = "white", color = "black"),
                      panel.grid = element_line(color = "grey90"),
                      panel.border = element_rect(color = "black", fill = NA),
                      legend.position = "bottom")
) %>% layout(showlegend = TRUE,
             legend = list(orientation = "h"))


##### Vamos aqui decompor uma serie temporal em suas componentes Tedência, Sazonal e Erros

##### Assumindo inicialmente uma série temporal simples - vide excel

s=ts(c(10,14,8,25,16,22,14,35,15,27,18,40,28,40,25,65), start=c(2016,1),
     end=c(2019,4), frequency = 4)
plot(s)

# Decomposição pelo modelo ADITIVO

decompa=decompose(s,type = "additive")
plot(decompa)

decompa$trend
decompa$seasonal
decompa$random

# Decomposição pelo modelo MULTIPLICATIVO

decompm=decompose(s,type = "multiplicative")
plot(decompm)

decompm$trend
decompm$seasonal
decompm$random

###############################################################################
##### Aplicando a decomposição das séries temporais
# usando o pacote viridis
###### decomposição da série temporal do PIB Mensal BR

load("pib.RData")

#Transformando a base de dados em um objeto de classe ts
pib_ts <- ts(data = pib[, 2],
             start = c(2004, 1),
             end = c(2021, 9),
             frequency = 12)

#Fazendo a plotagem da série temporal
pib %>%
        ggplot() +
        geom_line(aes(x = Data, y = PIB, group = TRUE, color = "PIB"), size = 1) +
        scale_color_viridis_d() +
        scale_y_continuous(labels = scales::comma) +
        scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
              panel.background = element_rect(fill = "white", color = "black"),
              panel.grid = element_line(color = "grey90"),
              panel.border = element_rect(color = "black", fill = NA),
              legend.position = "none")

# decompondo o PIB pelo modelo aditivo

decpib <- decompose(x = pib_ts,
                    type = "additive")



#Transformando o objeto decpib num data frame
decpib_df <- data.frame(tempo = pib$Data,
                        serie = unlist(decpib$x),
                        tendencia = unlist(decpib$trend),
                        sazonalidade = unlist(decpib$seasonal),
                        dessazonalizada = pib_ts - decpib$seasonal,
                        erro = unlist(decpib$random)) %>%
        rename(tempo = 1,
               serie = 2,
               tendencia = 3,
               sazonalidade = 4,
               dessazonalizada = 5,
               erro = 6)

#Plotando a decomposição de forma conjunta
decpib_df %>%
        ggplot() +
        geom_line(aes(x = tempo, y = serie, color = "Série"), size = 1.2) +
        geom_line(aes(x = tempo, y = tendencia, color = "Tendência"), size = 1) +
        geom_line(aes(x = tempo, y = sazonalidade, color = "Sazonalidade"), size = 1.2) +
        geom_line(aes(x = tempo, y = erro, color = "Erro"), size = 1) +
        scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
        scale_y_continuous(labels = scales::comma) +
        labs(color = "Legenda:",
             x = NULL,
             y = NULL) +
        scale_color_manual(values = c("#440154FF", "#3CBB75FF", "#39568CFF", "#DCE319FF")) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
              panel.background = element_rect(fill = "white", color = "black"),
              panel.grid = element_line(color = "grey90"),
              panel.border = element_rect(color = "black", fill = NA),
              legend.position = "bottom")

# observando cada componente - vide excel
decpib$trend
decpib$seasonal
decpib$random

#Plotando a decomposição individualmente

#a) Série
decpib_df %>%
        ggplot() +
        geom_line(aes(x = tempo, y = serie, color = "Série")) +
        scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Série",
             x = NULL,
             y = NULL) +
        scale_color_manual(values = c("#39568CFF")) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 7),
              panel.background = element_rect(fill = "white", color = "black"),
              panel.grid = element_line(color = "grey90"),
              panel.border = element_rect(color = "black", fill = NA),
              legend.position = "none") -> decomp_serie

#b) Sazonalidade
decpib_df %>%
        ggplot() +
        geom_line(aes(x = tempo, y = sazonalidade, color = "Sazonalidade")) +
        scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Sazonalidade",
             x = NULL,
             y = NULL) +
        scale_color_manual(values = c("#3CBB75FF")) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 7),
              panel.background = element_rect(fill = "white", color = "black"),
              panel.grid = element_line(color = "grey90"),
              panel.border = element_rect(color = "black", fill = NA),
              legend.position = "none") -> decomp_sazonalidade

#c) Tendência
decpib_df %>%
        ggplot() +
        geom_line(aes(x = tempo, y = tendencia, color = "Tendência")) +
        scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Tendência",
             x = NULL,
             y = NULL) +
        scale_color_manual(values = c("#DCE319FF")) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 7),
              panel.background = element_rect(fill = "white", color = "black"),
              panel.grid = element_line(color = "grey90"),
              panel.border = element_rect(color = "black", fill = NA),
              legend.position = "none") -> decomp_tendencia

#d) Erro
decpib_df %>%
        ggplot() +
        geom_line(aes(x = tempo, y = erro, color = "Erro")) +
        scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Erro",
             x = NULL,
             y = NULL) +
        scale_color_manual(values = c("#440154FF")) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 7),
              panel.background = element_rect(fill = "white", color = "black"),
              panel.grid = element_line(color = "grey90"),
              panel.border = element_rect(color = "black", fill = NA),
              legend.position = "none") -> decomp_erro

#Dê zoom para uma visualização mais agradável
grid.arrange(decomp_serie,
             decomp_sazonalidade,
             decomp_tendencia,
             decomp_erro,
             ncol = 1)

#Plotando a série dessazonalizada
decpib_df %>%
        ggplot() +
        geom_line(aes(x = tempo, y = serie, color = "Série"), size = 1.2) +
        geom_line(aes(x = tempo, y = dessazonalizada, color = "Dessazonalizada"), size = 1) +
        scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") +
        scale_y_continuous(labels = scales::comma) +
        labs(color = "Legenda:",
             x = NULL,
             y = NULL) +
        scale_color_viridis_d() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
              panel.background = element_rect(fill = "white", color = "black"),
              panel.grid = element_line(color = "grey90"),
              panel.border = element_rect(color = "black", fill = NA),
              legend.position = "bottom")


# decompondo o PIB pelo modelo multiplicativo

decpib = decompose(pib_ts, type = "multiplicative")
plot(decpib)


# observando cada componente - vide excel
decpib$trend
decpib$seasonal
decpib$random



###############################################################################
###### SES - Suavização Exponencial Simples
###############################################################################

###############################################################################
## SES - Suavização Exponencial Simples
###############################################################################

# Inicialmente, uma base bem simples

base=ts(c(3,5,9,20,12,17,22,23,51,41,56,75,60,75,88))
autoplot(base)

# criando o modelo de suavização exponencial com previsão de 3 passos (h) a frente
modeloses=ses(base,h=3)

# valores previstos

modeloses

# modelo gerado
modeloses$model

# valores estimados

modeloses$fitted

# visualização dos dados e das previões com intervalos de confiança

autoplot(modeloses)


###############################################################################
## Modelo de Holt com Tendência
###############################################################################

base=ts(c(10,14,8,25,16,22,14,35,15,27,18,40,28,40,25,65),start = c(2010,1),
         end = c(2013,4), frequency = 4)

autoplot(base)

modeloholt =holt(base,h=3)

# valores previstos

modeloholt

# modelo gerado
modeloholt$model

# valores estimados

modeloholt$fitted

# visualização dos dados e das previ?es com intervalos de confiança

autoplot(modeloholt)


###############################################################################
## Modelo de Holt com Tendência e Amortecimento (damped)
###############################################################################

modholtamort =holt(base,damped = T, phi = 0.9, h=3)

# valores previstos

modholtamort

# modelo gerado
modholtamort$model

# visualização dos dados e das previ?es com intervalos de confiança

autoplot(modholtamort)

###############################################################################
## Modelo de Holt com Tendência e Sazonalidade
###############################################################################

modholtsazonalad=hw(base,h=3,seasonal="additive")

# valores previstos

modholtsazonalad

# modelo gerado
modholtsazonalad$model

# visualização dos dados e das previ?es com intervalos de confiança

autoplot(modholtsazonalad)

 ###############################################################################
## Modelo de Holt com Tendência e Sazonalidade
###############################################################################

modholtsazonalm=hw(base,h=3,seasonal="multiplicative")

# valores previstos

modholtsazonalm

# modelo gerado
modholtsazonalm$model

# visualização dos dados e das previ?es com intervalos de confiança

autoplot(modholtsazonalm)


###############################################################################
### Comparando os modelos de previsão
###############################################################################

### carregando a base de dados da receita líquida da Ambev SA

ambev <- read_excel("ambev.xlsx")

ambev=ambev[2]

ambev=ts(ambev,start=c(2000,1), end=c(2021,3), frequency=4)
ambev


# total de observações

length(ambev)

# separar a base de dados em uma janela para criar o modelo e outra para prever

# base para rodar o modelo

bambev=window(ambev,start=c(2000,1), end=c(2019,4))
plot(bambev)

reais=window(ambev,start=c(2020,1), end=c(2021,3))
plot(reais)
length(reais)

#### fazendo as previsões e calculando a estatística MAPE de qualidade das previzões

# fazendo a previsão pelo alisamento exponencial simples

ses=ses(bambev,h=7)
prevses=ses$mean
prevses
pses=ts(prevses,start=c(2020,1),end=c(2021,3), frequency = 4)
qualises=accuracy(pses,reais)
qualises=accuracy(pses,reais)[5]
qualises

# fazendo a previsão pelo HoltWinters com Tendência

holttend=holt(bambev,h=7)

prevholttend=holttend$mean

pholt=ts(prevholttend, start=c(2020,1),end=c(2021,3),frequency=4)
qualiholt=accuracy(pholt,reais)[5]
qualiholt

# fazendo a previsão pelo HoltWinters Sazonal Aditivo

hwaditivo=hw(bambev,h=7,seasonal = "additive")
hwaditivo
phwaditivo=hwaditivo$mean
phwaditivo
pha=ts(phwaditivo,start=c(2020,1),end=c(2021,3), frequency = 4)
qualihwa=accuracy(pha,reais)[5]
qualihwa

# fazendo a previsão pelo HoltWinters Sazonal Multiplicativo

hwmult=hw(bambev,h=7,seasonal = "multiplicative")
hwmult
phwmult=hwmult$mean
phwmult
phm=ts(phwmult,start=c(2020,1),end=c(2021,3), frequency = 4)
qualihwm=accuracy(phm,reais)[5]
qualihwm

# Analisando as acurácias das previsões

modelos=c("SES","HOLT_T", "HW_ADIT", "HW_MULT")
mape=c(qualises,qualiholt,qualihwa,qualihwm)
tabela=data.frame(modelos,mape)
tabela

# Usando modelo ETS
# EXPONENTIAL SMOOTHING

# E (Error) T (Trend) S (Season)
# Erro: aditivo (A) ou multiplicativo (M)
# Tendência: nenhuma (N), aditiva (A), multiplicativa (M) ou amortecida (Ad ou Md)
# Sazonalidade: nenhuma (N), aditiva (A) ou multiplicativa (M)


bambev=window(ambev,start=c(2000,1), end=c(2019,4))

reais=window(ambev,start=c(2020,1), end=c(2021,3))

autoplot(ambev)+
        autolayer(bambev,series="Treino") +
        autolayer(reais,series = "Reais") +
        scale_color_viridis_d() +
        scale_y_continuous(labels = scales::comma) +
        theme_bw()

 # Holt-Winters Ambev

ambev.hw <- forecast::hw(bambev, h = 7, seasonal = "additive")
summary(ambev.hw)

autoplot(ambev) +
        forecast::autolayer(ambev.hw,
                            series = "Holt-Winters adit",
                            PI = FALSE) +
        xlab("Ano") +
        ylab("Receita Líquida") +
        ggtitle("Forecasts para AMBEV") +
        guides(colour = guide_legend(title = "Forecast")) +
        scale_color_viridis_d(option = "cividis") +
        scale_y_continuous(labels = scales::comma) +
        theme_bw()

accuracy(ambev.hw$mean,reais)

# Usando ETS
# N=none, A=additive, M=multiplicative e Z=automatic

ambev.ets <- ets(bambev, model = "ZZZ")
summary(ambev.ets)

ambev.ets.forecasts <- forecast.ets(ambev.ets, h = 7)
summary(ambev.ets.forecasts)

accuracy(ambev.ets.forecasts$mean,reais)

# Analisando os resíduos (erros) das previsões
# Condições:
# não podem ser correlacionados; se forem correlacionados ficaram informações
# nos resíduos que deveriam estar no modelo
# devem possui média zero, caso não seja então as previsões são viesadas

autoplot(ambev.ets$residuals)

acf(ambev.ets$residuals)

# Teste de Ljung-box
# H0: os resíduos são iid (modelo não exibe falhas de ajustes)
# H1: os resíduos não são iid (modelo exibe falhas de ajustes)
# não quero rejeitar H0 (quero um pvalor grande)


Box.test(ambev.ets$residuals, lag=1,type=c("Ljung-Box"))


#############################################################################
# Caso Prático: Previsão Consumo Energia Elétricana região sudeste (Gwh)
# Fonte: http://www.ipeadata.gov.br/Default.aspx
#############################################################################

energia <- read_excel("energia.xlsx")

energia=energia[2]

energia=ts(energia,start=c(1979,1), end=c(2021,9), frequency=12)
energia

# total de observações

length(energia)

# separar a base de dados em uma janela para criar o modelo e outra para prever

# base para rodar o modelo

benergia=window(energia,start=c(1979,1), end=c(2019,12))
plot(benergia)

reaisenergia=window(energia,start=c(2020,1), end=c(2021,9))
plot(reaisenergia)
length(reaisenergia)

energia.ets <- ets(benergia)
summary(energia.ets)

energia.ets.forecasts <- forecast.ets(energia.ets, h = 21)
summary(energia.ets.forecasts)

autoplot(energia.ets.forecasts)
accuracy(energia.ets.forecasts$mean,reaisenergia)

autoplot(energia.ets$residuals)

acf(energia.ets$residuals)

Box.test(energia.ets$residuals, lag=1,type=c("Ljung-Box"))

