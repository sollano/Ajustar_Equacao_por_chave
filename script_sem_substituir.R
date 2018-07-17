# Importando Dados ####

dados <- read.csv2("Dados.csv")

# Carregando Pacotes ####

library(dplyr)
library(minpack.lm)

# Definicao da idade de inflexao
i.inflexao <- 35 

# Definicao dos chutes iniciais
chute_ini <- list(
  b0 = max(dados$Volume.Ha),
  b1 = max(dados$Volume.Ha) / min(dados$Volume.Ha) - 1 ,
  b2 = log(max(dados$Volume.Ha) / min(dados$Volume.Ha) - 1) / i.inflexao )
chute_ini



# Depois realiza-se um ajuste por grupo. Quando o ajuste por grupo nao convergir, 
# iremos substiruir o valor de beta por NA
tab.site <- dados %>%
  group_by(Fazenda, Ciclo, Procedencia) %>%
  do(Reg = try(# try garante que a funcao continue rodando quando encontrar erros
    nlsLM( #funcao que ajusta nao linear com algoritmo bom
      HD ~ b0 / (1 + b1 * exp(-b2 * Idade)),
      data = .,
      start = chute_ini
    ), 
    silent=TRUE # esconde as mensagens de erro
  )) %>% 
  mutate(b0 = ifelse(is(Reg,"try-error"),NA,coef(Reg)[1]), # se a chave nao convergiu, NA, se convergir, coloca o valor do coeficiente
         b1 = ifelse(is(Reg,"try-error"),NA,coef(Reg)[2]),
         b2 = ifelse(is(Reg,"try-error"),NA,coef(Reg)[3])) %>%
  select(-Reg)
tab.site

# inserir os coeficientes nos dados
dados_coef <- left_join(dados, tab.site, by = c("Fazenda", "Ciclo", "Procedencia") )


write.csv2(dados_coef,"dados_coef.csv", row.names = FALSE)



