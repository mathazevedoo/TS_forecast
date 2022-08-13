# Instalação e Carregamento de Todos os Pacotes ---------------------------
# Rotina prof. Rafael Souza e Prof Fávero

pacotes <- c("readxl","plotly","tidyverse","gridExtra","forecast","TTR",
             "smooth", "tsibble", "fable","tsibbledata", "fpp3",
             "urca", "dygraphs", "quantmod","BETS","tseries","FinTS")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}
