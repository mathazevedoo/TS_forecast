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
