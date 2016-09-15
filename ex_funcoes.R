
# este script requer os seguintes pacotes carregados e atualizados:
require("broom")
require("purrr")
require("tidyr")
require("dplyr")

# carregar funcoes:
source("fit_mod.R")
source("group_lm.R")

# carregar dados
ex <- read.csv2("dados_eq_chave.csv", header = T, sep = ";", dec = ",") %>% 
  filter(!is.na(CAP)) %>% 
  mutate( DAP=CAP/pi, INV_DAP=1/DAP, LN_HT=log(HT), LN_HD=log(HD) )
# criamos as variaveis que seram utilizadas no modelo

# visualizar exemplo:
head(ex)

# group_lm ajusta um modelo por grupos (fatores)
# e retorna o dataframe, 
# os coeficientes deste modelo
# variaveis de qualidade (R2 ajustado e erro)
# residuos do ajuste
# todos agrupados em colunas-lista
# vejamos com este exemplo,
#  onde iremos utilizar o modelo de Curtis:
group_lm(ex, c("TALHAO", "PARCELA"), "LN_HT ~ INV_DAP" )

# podemos ver que cada lista tem definido a sua dimensao, e classe
# podemos assim, com o comando tidyr:unnest, 
# extrair qualquer uma destas listas. por exemplo:
group_lm(ex, c("TALHAO", "PARCELA"), "LN_HT ~ INV_DAP" ) %>% 
  unnest(Coefs)
# note que as outras colunas-listas sao mantidas, para uso futuro

# se quisermos remove-las, temos o comando .drop
group_lm(ex, c("TALHAO", "PARCELA"), "LN_HT ~ INV_DAP" ) %>% 
  unnest(Coefs, .drop = T)

# agora temos apenas os coeficientes

# este procedimento ja foi inserido na funcao group_lm, par conveniencia
# para isso utilizamos o comando coef_only
group_lm(ex, c("TALHAO", "PARCELA"), "LN_HT ~ INV_DAP", coef_only = T )


# da mesma forma foi inserido um comando que une os dados originais com os coeficientes
# isto e util quando se deseja estimar o y do modelo futuramente
# o comando e merge_coef:
group_lm(ex, c("TALHAO", "PARCELA"), "LN_HT ~ INV_DAP", merge_coef = T )

# para estimarmos o y do modelo, podemos utilizar a funcao fit_mod
# nela inserimos o modelo utilizado, seguido do nome para a nova variavel
# como desejamos estimar a altura apena das arvores nao medidas
# indicamos na funcao que EANM = T, e o nome da variavel altura (entre aspas)

ex %>% 
  group_lm(c("TALHAO"), "LN_HT ~ INV_DAP + LN_HD", merge_coef = T ) %>% 
  fit_mod("LN_HT ~ INV_DAP + LN_HD", name = "HT_EST", EANM=T, Y = "HT")

# como podemos perceber, apenas as arvores nao medidas tiveram suas alturas estimadas
# a funcao remove as variaveis utilizadas nos calculos, para deixar o dataframe mais "limpo"

# agora seguem exemplos com o modelo de Campos & Leite

# resultado padrao: retorno com colunas-lista
ex %>% 
  group_lm(c("TALHAO"), "LN_HT ~ INV_DAP + LN_HD" )

# retornar apenas os coeficientes
ex %>% 
  group_lm(c("TALHAO"), "LN_HT ~ INV_DAP + LN_HD", coef_only = T )

# retornar o dataframe unido aos coeficientes
ex %>% 
  group_lm(c("TALHAO"), "LN_HT ~ INV_DAP + LN_HD", merge_coef = T )

# retornar o dataframe unido aos coeficientes
# e em seguida estimar a altura para as arvores nao medidas
ex %>% 
  group_lm(c("TALHAO"), "LN_HT ~ INV_DAP + LN_HD", merge_coef = T ) %>% 
  fit_mod("LN_HT ~ INV_DAP + LN_HD", name = "HT_EST")


# o procedimento de estimar as alturas pode ser feito da seguinte forma,
# caso nao se deseje utilizar a funcao fit_mod:
ex %>% 
  group_lm(c("TALHAO"), "LN_HT ~ INV_DAP + LN_HD", merge_coef = T ) %>% 
  mutate( HT_EST = ifelse(is.na(HT), exp(b0 + b1*INV_DAP + b2*LN_HD ) , HT )   )%>% 
  select(-b0, -b1, -b2, -LN_HT, -INV_DAP, -LN_HD)
