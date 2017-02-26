## ---

## title: Script para ajuste de regressão linear simples por grupos (chaves) no R

## author:
## - Sollano Rabelo Braga 
## - Marcio Leles Romarco de Oliveira

## date: março, 2016

## output:

##    pdf_document:

##      toc: true

##      toc_depth: 4

##      highlight: tango

##    word_document:

##      toc: true

##      toc_depth: 4

##      highlight: tango

##    html_document:

##      toc: true

##      toc_float: true

##      toc_depth: 4

##      highlight: tango

## ---
## \pagebreak
##
#+ include=FALSE
knitr::opts_chunk$set(tidy.opts=list(width.cutoff=55),tidy=TRUE)

## # 1) Baixar e/ou carregar pacotes necessários ####

# install.packages("nlme", dependencies = TRUE)
# install.packages("tidyverse", dependencies = TRUE)
# Para se realizar a instalação, basta remover o # e rodar o comando.

## Para carregar os pacotes, pode-se utilizar do comando library:
library(nlme)
library(tidyverse)
library(broom)

## # 2) Carregar os dados ####

dados_orig <- read.csv2("dados.csv")

## Salva-se os dados em um objeto separado, removendo seus NAs
dados <- na.omit(dados_orig)

## # 3) Cálculo das variáveis necessárias para a regressão ####

## Este passo pode ser feito utilizando o R base ou o pacote dplyr

## R base
dados$DAP <- dados$CAP/pi
dados$INV_DAP <- 1/dados$DAP
dados$LN_HT <- log(dados$HT)
dados$LN_HD <- log(dados$HD)
head(dados)
## dplyr
## Com o dplyr pode-se criar diversas variáveis com um unico comando
dados <- mutate(dados, DAP=CAP/pi, INV_DAP=1/DAP, LN_HT=log(HT), LN_HD=log(HD))
head(dados)

## # 4) Ajuste de uma equação por uma chave ####
## Ln(Ht) = b0 + b1*((1/DAP)  + b2 * Ln(Hd)
##
## ## 4.1) R base ####

## Utilizando a função by, executa-se a regressão linear
## no primeiro argumento insere-se as variáveis,
## no segundo argumento uma variável classificatoria,
## no terceiro argumento a função a ser executada (lm),
## e em seguida indica-se o argumento "formula", e insere-se
## o modelo que será utilizado:
reg_rbase_talh <- by(dados, dados$TALHAO, lm, formula = LN_HT ~ INV_DAP + LN_HD )

## O arquivo gerado pela função by comporta-se como uma lista.
## note que só é possível pedir o sumario individual de cada observação:
summary(reg_rbase_talh)
summary(reg_rbase_talh$`3654`)

## Criação da tabela de coeficientes

## utiliza-se a função vapply, que e composta por 3 argumentos
## 1: matriz ou vetor em que se deseja aplicar uma função em cada elemento;
## 2: função a ser aplicada na matriz ou vetor;
## neste caso, cria-se uma função que extrái os coeficientes,
## o r quadrado e o erro do ajuste;
## 3: template que fornece os nomes das colunas a serem criadas pela função
## esta função gera uma matriz com duas linhas e n colunas, dependendo
## do numero de coeficientes. Entao transpoe-se o seu resultado com t()
tab_rbase_talh <- t(vapply(reg_rbase_talh, function(x) c(coef(x), summary(x)$adj.r.squared, summary(x)$sigma), c("b0"=0, "b1"=0, "b2"=0, "Rsqr"=0, "Std.Error"=0)))

## A função by salva a chave na linha do objeto; devido a isso,
## precisavos uni-lo novamente ao data frame:
## obs: e importante que um dos elementos desta uniao seja um dataframe,
## caso contrario todas as variáveis do objeto seram transformadas em fatores
## Isso ocorre provavelmente pelo fato de utilizar-se os nomes da linhas na função
tab_rbase_talh <- cbind(TALHAO = rownames(tab_rbase_talh),as.data.frame(tab_rbase_talh))

## Remoção dos rownames
rownames(tab_rbase_talh) <- NULL

tab_rbase_talh

## ## 4.2) dplyr  ####

## Esta e uma forma mais direta de se realizar este procedimento,
## pois utiliza apenas um pacote, e nao cria nenhum objeto adicional
## com os dados originais, pode-se criar a tabela de coeficientes diretamente

## obs: em pipes do dplyr, "." representa o dataframe ate aquele ponto do codigo

reg_dplyr_talh <- dados %>% # definição do df a ser utilizado
  group_by(TALHAO) %>% # definição da chave. Uma vantagem e que pode-se utilizar mais de um grupo na função
  do(Reg = lm(LN_HT ~ INV_DAP + LN_HD, data =.)) # modelo linear
  
## o objeto gerado pelo dplyr e mais organizado,
## e contém a sua chave como primeira coluna, e as regressoes como segunda coluna
## note que so e possivel pedir o sumario individual de cada observação
reg_dplyr_talh
reg_dplyr_talh$Reg
summary(reg_dplyr_talh$Reg)
summary(reg_dplyr_talh$Reg[[1]])

## Criação da tabela de coeficientes

tab_dplyr_talh <- reg_dplyr_talh %>%
  mutate(b0=coef(Reg)[1], # a variável reg, criada anteriormente, possui os coeficientes na ordem,
         b1=coef(Reg)[2], # por isso os extrai-se com [], na ordem b0(1), b1(2)...bn(n+1)
         b2=coef(Reg)[3],
         Rsqr=summary(Reg)[[9]], # extrai-se r quadrado ajustado do summario de Reg
         Std.Error=summary(Reg)[[6]]) %>% # extrai-se o erro do summario de Reg
  select(-Reg) # agora que extrai-se as variáveis de interesse, remove-se a variável com os ajustes
tab_dplyr_talh

## porém, a grande vantagem do dplyr e que este processo pode ser feita de forma direta, 
## por meio dos pipes

tab_dplyr_talh <- dados %>% # definição do df a ser utilizado
  group_by(TALHAO) %>% # definição da chave
  do(Reg = lm(LN_HT ~ INV_DAP + LN_HD, data =.)) %>% # modelo linear
  mutate(b0=coef(Reg)[1], # a variável reg, criada anteriormente, possui os coeficientes na ordem,
         b1=coef(Reg)[2], # por isso os extrai-se com [], na ordem b0(1), b1(2)...bn(n+1)
         b2=coef(Reg)[3],
         Rsqr=summary(Reg)[[9]], # extrai-se r quadrado ajustado do summario de Reg
         Std.Error=summary(Reg)[[6]]) %>% # extrai-se o erro do summario de Reg
  select(-Reg) # agora que extrai-se as variáveis de interesse, remove-se a variável com os ajustes
tab_dplyr_talh

## ## 4.3) nlme ####

## a função lnList comporta-se de maneira similiar a lm,
## porém pode-se inserir uma chave apos a fórmula;
## a fórmula e a chave são separadas por | 
## no proximo argumento insere-se  o dado a ser utilizado.
reg_nlme_talh <- lmList(LN_HT ~ INV_DAP + LN_HD|TALHAO, dados)

## Note que o objeto gerado aqui e o mais organizado dentre as 3 alternativas;
## Se apenas o objeto for chamado, ele fornece várias informações adicionais,
## como modelo ajustado, dados, graus de liberdade, e erro do ajuste
## é possível pedir o sumário do objeto como um todo, ou de observações:

reg_nlme_talh
summary(reg_nlme_talh)
summary(reg_nlme_talh$`3654`)

## Obtem-se estas informações a mais pois o objeto criado pela função lmList
## possui uma classe especial:
class(reg_nlme_talh)

## Caso o objetivo seja obter apenas os coeficientes, somente a função lmList ja e suficiente
##
## caso o objetivo seja uma tabela com informações de r2 e erro por grupo, pode-se obter da seguinte forma
##
## Criação da tabela de coeficientes
##
## aqui a procedimento se repete como nos metodos anteriores:
tab_nlme_talh <- t(vapply(reg_nlme_talh, function(x) c(coef(x), summary(x)$adj.r.squared, summary(x)$sigma), c("b0"=0, "b1"=0, "b2"=0, "Rsqr"=0, "Std.Error"=0)))
tab_nlme_talh <- cbind(TALHAO = rownames(tab_nlme_talh),as.data.frame(tab_nlme_talh))

rownames(tab_nlme_talh) <- NULL

tab_nlme_talh

## ## 4.4) Comparar as tabelas geradas ####

## Pode-se testar se as tabelas geradas são iguais
## Primeiro argumento Target, segundo argumento Current
all.equal(tab_nlme_talh, tab_rbase_talh)
## Aqui recebos um comando TRUE, ou seja, as tabelas são idênticas

all.equal(tab_nlme_talh, tab_dplyr_talh)
## Aqui recebe-se o aviso de que a variável TALHAO possui a classe diferente
## Os demais dados são os mesmos, ou seja, os valores estimados são idênticos
##

## # 5) Ajuste de uma equação por duas ou mais chaves ####
## Ln(Ht) = b0 + b1*((1/DAP)
##
## ## 5.1) R base ####

## O procedimento é o mesmo do anterior, porém 
## deve-se unir as chaves em uma unica variável
dados2 <- dados
dados2$TALHAO_PAR <- paste(dados2$TALHAO, dados2$PARCELA, sep='_')

reg_rbase_talh_par <- by(dados2, dados2$TALHAO_PAR, lm, formula = LN_HT ~ INV_DAP)
summary(reg_rbase_talh_par)
summary(reg_rbase_talh_par$`3654_101`)


## Criação da tabela de coeficientes
## Como agora utiliza-se um modelo com 2 coeficientes, o terceiro argumento
## o função vapply e alterado, contendo 2 coeficientes tambem
tab_rbase_talh_par <- t(vapply(reg_rbase_talh_par, function(x) c(coef(x), summary(x)$adj.r.squared, summary(x)$sigma), c("b0"=0, "b1"=0, "Rsqr"=0, "Std.Error"=0)))
tab_rbase_talh_par <- cbind(TALHAO_PAR = rownames(tab_rbase_talh_par),as.data.frame(tab_rbase_talh_par))

## Remoção dos nomes das linhas
row.names(tab_rbase_talh_par) <- NULL

head(tab_rbase_talh_par)

## ## 5.2) dplyr ####

## Nao ha a necessidade de criar uma chave adicional,
## basta informar as chaves desejadas na função group_by()
## Isto torna este metodo bem pratico

tab_dplyr_talh_par <- dados %>%
  group_by(TALHAO, PARCELA) %>%
  do(Reg = lm(LN_HT ~ INV_DAP, data =.)) %>%
  mutate(b0=coef(Reg)[1], # a variável reg, criada anteriormente, possui os coeficientes na ordem,
         b1=coef(Reg)[2], # por isso os extrai-se com [], na ordem b0(1), b1(2)...bn(n+1)
         Rsqr=summary(Reg)[[9]], # extrai-se r quadrado ajustado do summario de Reg
         Std.Error=summary(Reg)[[6]]) %>% # extrai-se o erro do summario de Reg
  select(-Reg) # agora que extrai-se as variáveis de interesse, remove-se a variável com os ajustes
tab_dplyr_talh_par

## ## 5.3) nlme ####

## Assim como no R base, deve-se criar uma chave que represente as demais
dados2 <- dados
dados2$TALHAO_PAR <- paste(dados2$TALHAO, dados2$PARCELA, sep='_')

reg_nlme_talh_par <- lmList(LN_HT ~ INV_DAP|TALHAO_PAR, dados2)

reg_nlme_talh_par
summary(reg_nlme_talh_par)

## Criação da tabela de coeficientes
tab_nlme_talh_par <- t(vapply(reg_nlme_talh_par, function(x) c(coef(x), summary(x)$adj.r.squared, summary(x)$sigma), c("b0"=0, "b1"=0, "Rsqr"=0, "Std.Error"=0)))
tab_nlme_talh_par <- cbind(TALHAO_PAR = rownames(tab_nlme_talh_par),as.data.frame(tab_nlme_talh_par))

head(tab_nlme_talh_par)

## ## 5.4) Comparar as tabelas geradas ####

## Pode-se testar se as tabelas geradas são iguais
## Primeiro argumento Target, segundo argumento Current
all.equal(tab_nlme_talh_par, tab_rbase_talh_par)
## Aqui recebos um comando TRUE, ou seja, as tabelas são idênticas

## Neste caso como a tabela gerada pelo dplyr possui uma coluna a mais,
## Deve-se especificar as colunas que deseja-se comparar:
all.equal(tab_nlme_talh_par[,c(2,3,4,5)], tab_dplyr_talh_par[,c(3,4,5,6)])
## Recebe-se o aviso de a classe dos nomes das colunas são diferentes
## porém não há nenhum outro aviso, ou seja, os dados gerados são idênticos.
##

## # 6) Exportar tabelas de coeficientes ####

write.csv2(tab_rbase_talh, "tabelas/tab_rbase_talh.csv")
write.csv2(tab_rbase_talh_par, "tabelas/tab_rbase_talh_par.csv")

write.csv2(tab_dplyr_talh, "tabelas/tab_dplyr_talh.csv")
write.csv2(tab_dplyr_talh_par, "tabelas/tab_dplyr_talh_par.csv")

write.csv2(tab_nlme_talh, "tabelas/tab_nlme_talh.csv")
write.csv2(tab_nlme_talh_par, "tabelas/tab_nlme_talh_par.csv")

## # 7) Junção dos dados originais e dados de regressão ####

## ## 7.1) Importar dados ####

#tab_reg <- read.csv(file.choose(), header = T)

## ## 7.2) R base ####

## Caso os dados nao possuam a chave utilizada na regressão,
## deve-se adicioná-la:
dados_orig_mod <- dados_orig
dados_orig_mod$TALHAO_PAR <- paste(dados_orig_mod$TALHAO, dados_orig_mod$PARCELA, sep='_')

## Utiliza-se a função Merge, que se comporta de forma similar ao PROCV;
## Os primeiros dois argumentos informam os dados a serem assimilados;
## O terceiro argumento informa sera o fator de uniao.
## Utiliza-se o argumento all.x = TRUE, para garantir que os dados originais sejam preservados
tab_final_rbase_talh <- merge(dados_orig, tab_rbase_talh, by = "TALHAO", all.x = TRUE)
tab_final_rbase_talh_par <- merge(dados_orig_mod, tab_rbase_talh_par, by = "TALHAO_PAR", all.x = TRUE)

## Substitui-se NAs por 0, caso desejado
tab_final_rbase_talh[is.na(tab_final_rbase_talh)] <- 0
tab_final_rbase_talh_par[is.na(tab_final_rbase_talh_par)] <- 0

head(tab_final_rbase_talh)
head(tab_final_rbase_talh_par)

## ## 7.3) dplyr ####

## Com o pacote dplyr nao e necessário unir as chaves:
## caso elas nao existam, devem ser adicionadas 
## com a função mutate() ou cbind() separadamente, e entao,
## Utiliza-se full join (ou left join)
## para garantir que os dados originais nao sejam alterados


tab_final_dplyr_talh <- dados_orig %>%
  full_join(tab_dplyr_talh)

tab_final_dplyr_talh_par <- dados_orig %>%
  full_join(tab_dplyr_talh_par, by = c("TALHAO", "PARCELA"))

## Substituir NAs por 0, caso desejado
tab_final_dplyr_talh[is.na(tab_final_dplyr_talh)] <- 0
tab_final_dplyr_talh_par[is.na(tab_final_dplyr_talh_par)] <- 0

head(tab_final_dplyr_talh)
head(tab_final_dplyr_talh_par)


## Pode-se verificar se as tabelas finais são iguais:
all.equal(tab_final_dplyr_talh, tab_final_rbase_talh)
## Recebe-se a resposta TRUE, ou seja, as tabelas são idênticas
##

## # 8) Exportar tabelas finais ####

write.csv(tab_final_rbase_talh, "tabelas/tab_final_rbase_talh.csv" )
write.csv(tab_final_rbase_talh_par, "tabelas/tab_final_rbase_talh_par.csv")

write.csv(tab_final_dplyr_talh, "tabelas/tab_final_dplyr_talh.csv")
write.csv(tab_final_dplyr_talh_par, "tabelas/tab_final_dplyr_talh_par.csv")





