
### Script criado por: Sollano Rabelo Braga e Marcio Leles R. de Oliveira

# Ajuste de equacoes Lineares por Grupos (Chaves)

# Obs: Este ajuste pode ser feito utilizando o R base, dplyr ou nlme;
#  O resultado final e o mesmo,
# Diferenciando-se apenas na classe do objeto gerado pela regressao.

# Este tutorial esta dividido em topicos, para melhor visualizacao, utilize:
# ctrl + o : fecha todas as foldings (topicos)
# ctrl + shift + o : abre todas as foldings (topicos)
# ou command + o e command + shift + o, caso utilize o Mac

# 1) Baixar e/ou carregar pacotes necessarios ####

# Este script contem secoes que podem ser rodadas sem o uso de nenhum pacote adicional
# no entanto, e interessante se conhecer os metodos alternativos, pois sao de
# melhor entendimento e facil computacao
# para isso precisamos dos pacotes instalados e carregados
# iremos usar uma pequena funcao que ira carregar os pacotes e,
# caso algum nao esteja instalado na maquina, este sera intalado e carregado

pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Pacote nao encontrado")
  }
}

# Agora que carregamos a funcao, verificamos e carregamos
# os pacotes que seram utilizados neste script

pkgTest("nlme")
pkgTest("broom")
pkgTest("purrr")
pkgTest("tidyr")
pkgTest("dplyr")

# 2) Carregar os dados ####

dados_orig <- read.csv2("dados_eq_chave.csv", header = T, sep = ";", dec = ",")

# Salva-se os dados em um objeto separado, removendo seus NAs
dados <- na.omit(dados_orig)

# 3) Calculo das variaveis necessarias para a regressao ####

# Este passo pode ser feito utilizando o R base ou o pacote dplyr

# R base
dados$DAP <- dados$CAP/pi
dados$INV_DAP <- 1/dados$DAP
dados$LN_HT <- log(dados$HT)
dados$LN_HD <- log(dados$HD)

# dplyr
# Com o dplyr pode-se criar diversas variaveis com um unico comando
dados <- mutate(dados, DAP=CAP/pi, INV_DAP=1/DAP, LN_HT=log(HT), Ln_HD=log(HD))

# 4) Ajuste de uma equacao por uma chave ####
# Ln(Ht) = b0 + b1*((1/DAP)  + b2 * Ln(Hd)

# 4.1) R base ####

# Cria-se um objeto que contem apenas as variaveis da regressao
# Primeiramente deve se inserir o y, e entao, x1, x2... xn
aux1 <- dados[,c("LN_HT", "INV_DAP", "LN_HD")]

# Utilizando a funcao by, executa-se a regressao linear
# no primeiro argumento insere-se as variaveis,
# no segundo argumento uma variavel classificatoria,
# e no terceiro argumento a funcao a ser executada (lm)
reg_rbase_talh <- by(aux1, dados$TALHAO, lm)

# O arquivo gerado pela funcao by comporta-se como uma lista,
# por isso e necessario utilziar o cbind para unir os seus dados
# note que so e possivel pedir o sumario individual de cada observacao
aux2 <- data.frame(cbind(reg_rbase_talh))
summary(aux2)
summary(aux2[1,1]$`3654`)

# Criacao da tabela de coeficientes

# utiliza-se a funcao vapply, que e composta por 3 argumentos
# 1: matriz ou vetor em que se deseja aplicar uma funcao em cada elemento;
# 2: funcao a ser aplicada na matriz ou vetor;
# neste caso, criamos uma funcao que extrai os coeficientes,
# o r quadrado e o erro do ajuste;
# 3: template que fornece os nomes das colunas a serem criadas pela funcao
# esta funcao gera uma matriz com duas linhas e n colunas, dependendo
# do numero de coeficientes. Entao transpoe-se o seu resultado com t()
tab_rbase_talh <- t(vapply(reg_rbase_talh, function(x) c(coef(x), summary(x)$adj.r.squared, summary(x)$sigma), c("b0"=0, "b1"=0, "b2"=0, "Rsqr"=0, "Std.Error"=0)))

# A funcao by salva a chave na linha do objeto; devido a isso,
# precisavos uni-lo novamente ao data frame:
# obs: e importante que um dos elementos desta uniao seja um dataframe,
# caso contrario todas as variaveis do objeto seram transformadas em fatores
# Isso ocorre provavelmente pelo fato de utilizarmos os nomes da linhas na funcao
tab_rbase_talh <- cbind(TALHAO = rownames(tab_rbase_talh),as.data.frame(tab_rbase_talh))

# Remocao dos rownames
rownames(tab_rbase_talh) <- NULL

head(tab_rbase_talh)

# 4.2) dplyr  ####

# Esta e uma forma mais direta de se realizar este procedimento,
# pois utiliza apenas um pacote, e nao cria nenhum objeto adicional
# com os dados originais, pode-se criar a tabela de coeficientes diretamente

# obs: em pipes do dplyr, "." representa o dataframe ate aquele ponto do codigo

reg_dplyr_talh <- dados %>% # definicao do df a ser utilizado
  group_by(TALHAO) %>% # definicao da chave. Uma vantagem e que pode-se utilizar mais de um grupo na funcao
  do(Reg = lm(LN_HT ~ INV_DAP + LN_HD, data =.)) # modelo linear
  
# o objeto gerado pelo dplyr e mais organizado,
# e contem a sua chave como primeira coluna, e as regressoes como segunda coluna
# note que so e possivel pedir o sumario individual de cada observacao
reg_dplyr_talh
reg_dplyr_talh$Reg
summary(reg_dplyr_talh$Reg)
summary(reg_dplyr_talh$Reg[[1]])

# Criacao da tabela de coeficientes

tab_dplyr_talh <- reg_dplyr_talh %>%
  mutate(b0=coef(Reg)[1], # a variavel reg, criada anteriormente, possui os coeficientes na ordem,
         b1=coef(Reg)[2], # por isso os extraimos com [], na ordem b0(1), b1(2)...bn(n+1)
         b2=coef(Reg)[3],
         Rsqr=summary(Reg)[[9]], # extraimos r quadrado ajustado do summario de Reg
         Std.Error=summary(Reg)[[6]]) %>% # extraimos o erro do summario de Reg
  select(-Reg) # agora que extraimos as variaveis de interesse, removemos a variavel com os ajustes


# Porem, a grande vantagem do dplyr e que este processo pode ser feita de forma direta, 
# por meio dos pipes

tab_dplyr_talh <- dados %>% # definicao do df a ser utilizado
  group_by(TALHAO) %>% # definicao da chave
  do(Reg = lm(LN_HT ~ INV_DAP + LN_HD, data =.)) %>% # modelo linear
  mutate(b0=coef(Reg)[1], # a variavel reg, criada anteriormente, possui os coeficientes na ordem,
         b1=coef(Reg)[2], # por isso os extraimos com [], na ordem b0(1), b1(2)...bn(n+1)
         b2=coef(Reg)[3],
         Rsqr=summary(Reg)[[9]], # extraimos r quadrado ajustado do summario de Reg
         Std.Error=summary(Reg)[[6]]) %>% # extraimos o erro do summario de Reg
  select(-Reg) # agora que extraimos as variaveis de interesse, removemos a variavel com os ajustes

head(tab_dplyr_talh)

# 4.3) nlme ####

# a funcao lnList comporta-se de maneira similiar a lm,
# porem podemos inserir uma chave apos a formula;
# a formula e a chave sao separadas por | 
# no proximo argumento insere-se  o dado a ser utilizado.
reg_nlme_talh <- lmList(LN_HT ~ INV_DAP + LN_HD|TALHAO, dados)

# Note que o objeto gerado aqui e o mais organizado dentre as 3 alternativas;
# Se chamarmos apenas o objeto, ele fornece varias informacoes adicionais.
# Como modelo ajustado, dados, graus de liberdade, e erro do ajuste
# e possivel pedir o sumario do objeto como um todo, ou de observacoes

reg_nlme_talh
summary(reg_nlme_talh)
summary(reg_nlme_talh$`3654`)

# Obtemos estas informacoes a mais pois o objeto criado pela funcao lmList
# possui uma classe especial
class(reg_nlme_talh)

# Caso o objetivo seja obter apenas os coeficientes, somente a funcao lmList ja e suficiente

# caso o objetivo seja uma tabela com informacoes de r2 e erro por grupo, podemos obter da seguinte forma

# Criacao da tabela de coeficientes
# aqui a procedimento se repete como nos metodos anteriores
tab_nlme_talh <- t(vapply(reg_nlme_talh, function(x) c(coef(x), summary(x)$adj.r.squared, summary(x)$sigma), c("b0"=0, "b1"=0, "b2"=0, "Rsqr"=0, "Std.Error"=0)))
tab_nlme_talh <- cbind(TALHAO = rownames(tab_nlme_talh),as.data.frame(tab_nlme_talh))

rownames(tab_nlme_talh) <- NULL

head(tab_nlme_talh)

# 4.4) dplyr, purrr, broom, tidyr ####

# Iremos utilizar as funcoes nest, unnest e map, para criar um tibble, ou table data frame,
# que contem colunas formadas por listas,
# isto é pratico pois podemos unir objetos de diferentes dimensoes em um único tibble,
# e este pode ser separado depois, para criacao de diversos objetos

tibble_talh <- dados %>% # definicao do df
  group_by(TALHAO) %>% # definicao dos grupos
  nest  %>% # com tidyr::nest agrupamos os dados a mais em uma lista, resumindo os dados ( a funcao unnest desfaz este ato)
  mutate(Reg = map(data, ~lm(LN_HT ~ INV_DAP + LN_HD, data =.)), # a funcao purrr::map aplica uma funcao para cada elemento da lista
  Coefs = map(Reg, tidy) , # a funcao broom:tidy nos da os coeficientes e algumas estatisticas adicionais
         Glance = map(Reg, glance) ) # a funcao broom::glance oferece as estatisticas R2 ajustato e sigma, entre outras. Iremos remover as outras, mas sinta-se livre pra manter as variaveis desejadas

# Porem, glance e tidy geram muitas variaveis que geralmente nao sao utilizadas
# se quisermos apenas os coeficientes, R2 ajustado e o erro-padrao, podemos obtelos da seguinte forma:

tibble_talh <- dados %>% # definicao do df
  group_by(TALHAO) %>% # definicao dos grupos
  nest  %>% # com tidyr::nest agrupamos os dados a mais em uma lista, resumindo os dados ( a funcao unnest desfaz este ato)
  mutate(Reg = map(data, ~lm(LN_HT ~ INV_DAP + LN_HD, data =.)), # a funcao purrr::map aplica uma funcao para cada elemento da lista
         Coefs = map(Reg, 
                     function(x) tibble(term=tidy(x)$term,  # coluna que contem os nomes dos coeficientes
                                        estimate=tidy(x)$estimate ) ), # coluna que contem os valores dos coeficientes
         Glance = map(Reg, 
                      function(x) tibble( Rsqr=glance(x)$adj.r.squared, # coluna do R2 ajustado
                                          Std.Error=glance(x)$sigma )    )      ) # coluna do erro-padrao


# Porem, ainda temos o problema d eque tidy nos da os coeficientes organizados por fator
# e quisermos os coeficientes organizados por coluna, podemos utilizar spread dentro da funcao criada anteriormente

tibble_talh <- dados %>% # definicao do df
  group_by(TALHAO) %>% # definicao dos grupos
  nest  %>% # com tidyr::nest agrupamos os dados a mais em uma lista, resumindo os dados ( a funcao unnest desfaz este ato)
  mutate(Reg = map(data, ~lm(LN_HT ~ INV_DAP + LN_HD, data =.)), # a funcao purrr::map aplica uma funcao para cada elemento da lista
         Coefs = map(Reg, 
                     function(x) 
                       tibble(b=tidy(x)$term, 
                              estimate=tidy(x)$estimate ) %>% 
                       mutate(b = factor(b, labels=0:(length(b)-1) ) ) %>% # mudamos os nomes dos coeficientes para bn
                       spread(                    # com spread deixamos a tabela horizontal, colocando cada coeficiente em uma coluna
                         b, estimate,sep="" # variaveis que seram utilizadas no spread
                       )  ),
         Glance = map(Reg, 
                      function(x) tibble( Rsqr=glance(x)$adj.r.squared, 
                                          Std.Error=glance(x)$sigma )    ),
         Res = map(Reg, resid) ) # a funcao resid cria um vetor com os valores residuais do ajuste

# ou, se salvarmos esta funcao em um objeto separado

tidys <- function(x) 
{
  tibble(b=tidy(x)$term, 
         estimate=tidy(x)$estimate ) %>% 
    mutate(b = factor(b, labels=0:(length(b)-1) ) ) %>% # mudamos os nomes dos coeficientes para bn
    spread(                    # com spread deixamos a tabela horizontal, colocando cada coeficiente em uma coluna
      b, estimate,sep="" # variaveis que seram utilizadas no spread
    ) 
}


tibble_talh <- dados %>% # definicao do df
  group_by(TALHAO) %>% # definicao dos grupos
  nest  %>% # com tidyr::nest agrupamos os dados a mais em uma lista, resumindo os dados ( a funcao unnest desfaz este ato)
  mutate(Reg = map(data, ~lm(LN_HT ~ INV_DAP + LN_HD, data =.)), # a funcao purrr::map aplica uma funcao para cada elemento da lista
         Coefs = map(Reg, tidys ),
         Glance = map(Reg, 
                      function(x) tibble( Rsqr=glance(x)$adj.r.squared, 
                                          Std.Error=glance(x)$sigma )    ),
         Res = map(Reg, resid) ) # a funcao resid cria um vetor com os valores residuais do ajuste

# a vantagem deste metodo e que nao e necessaria a definicao dos nomes de cada coeficiente
# deixando a funcao automatizada para qualquer modelo, com n coeficientes

tibble_talh
# Observe como cada coluna é uma lista que agrupa diferentes variaveis, com dimensoes diferentes
# a informacao de cada parcela esta agrupada neste objeto
# data tem as informacoes basicas das parcelas (DAP, HT, etc) comprimidos em uma coluna,
# Reg tem objetos de regressao de cada ajuste de cada talhao,
# Coefs tem os coeficientes e mais algumas variaveis de qualidade do ajuste de cada talhao.
# Glance tem variaveis de qualidade do ajuste de cada talhao,
# e uma coluna dos residuos de cada ajuste

# Assim, se quisermos obter a tabela de coeficientes e qualidade de ajuste,
# podemos utilizar unnest
tibble_talh %>%
  unnest(Coefs, Glance, .drop=T) # O argumento .drop controla se as colunas lista adicionais devem ser mantidas ou nao

# ou uma tabela contendo os dados originais e os residuos
tibble_talh %>%
  unnest(data, Res)


# 4.5) Comparar as tabelas geradas ####

# Podemos testar se as tabelas geradas sao iguais
# Primeiro argumento Target, segundo argumento Current
all.equal(tab_nlme_talh, tab_rbase_talh)
# Aqui recebos um comando TRUE, ou seja, as tabelas sao identicas

all.equal(tab_nlme_talh, tab_dplyr_talh)
# Aqui recebemos o aviso de que a variavel TALHAO possui a classe diferente
# Os demais dados sao os mesmos, ou seja, os valores estimados sao identicos

# 5) Ajuste de uma equacao por duas ou mais chaves ####
# Ln(Ht) = b0 + b1*((1/DAP)

# 5.1) R base ####

# O procedimento e o mesmo do anterior, porem 
# Deve-se unir as chaves em uma unica variavel
dados2 <- within(dados, TALHAO_PAR <- paste(TALHAO, PARCELA, sep='_'))

aux1 <- dados[,c("LN_HT", "INV_DAP")]
reg_rbase_talh_par <- by(aux1, dados2$TALHAO_PAR, lm)
aux2 <- data.frame(cbind(reg_rbase_talh_par))

# Criacao da tabela de coeficientes
# Como agora utiliza-se um modelo com 2 coeficientes, o terceiro argumento
# o funcao vapply e alterado, contendo 2 coeficientes tambem
tab_rbase_talh_par <- t(vapply(reg_rbase_talh_par, function(x) c(coef(x), summary(x)$adj.r.squared, summary(x)$sigma), c("b0"=0, "b1"=0, "Rsqr"=0, "Std.Error"=0)))
tab_rbase_talh_par <- cbind(TALHAO_PAR = rownames(tab_rbase_talh_par),as.data.frame(tab_rbase_talh_par))

# Remocao dos nomes das linhas
row.names(tab_rbase_talh_par) <- NULL

head(tab_rbase_talh_par)

# 5.2) dplyr ####

# Nao ha a necessidade de criar uma chave adicional,
# basta informar as chaves desejadas na funcao group_by()
# Isto torna este metodo bem pratico

reg_dplyr_talh_par <- dados %>%
  group_by(TALHAO, PARCELA) %>%
  do(Reg = lm(LN_HT ~ INV_DAP, data =.)) %>%
  mutate(b0=coef(Reg)[1], # a variavel reg, criada anteriormente, possui os coeficientes na ordem,
         b1=coef(Reg)[2], # por isso os extraimos com [], na ordem b0(1), b1(2)...bn(n+1)
         Rsqr=summary(Reg)[[9]], # extraimos r quadrado ajustado do summario de Reg
         Std.Error=summary(Reg)[[6]]) %>% # extraimos o erro do summario de Reg
  select(-Reg) # agora que extraimos as variaveis de interesse, removemos a variavel com os ajustes

head(tab_dplyr_talh_par)

# 5.3) nlme ####

# Assim como no R base, deve-se criar uma chave que represente as demais
dados2 <- within(dados, TALHAO_PAR <- paste(TALHAO, PARCELA, sep='_'))

reg_nlme_talh_par <- lmList(LN_HT ~ INV_DAP|TALHAO_PAR, dados2)

reg_nlme_talh_par
summary(reg_nlme_talh_par)

# Criacao da tabela de coeficientes
tab_nlme_talh_par <- t(vapply(reg_nlme_talh_par, function(x) c(coef(x), summary(x)$adj.r.squared, summary(x)$sigma), c("b0"=0, "b1"=0, "Rsqr"=0, "Std.Error"=0)))
tab_nlme_talh_par <- cbind(TALHAO_PAR = rownames(tab_nlme_talh_par),as.data.frame(tab_nlme_talh_par))

head(tab_nlme_talh_par)

# 5.4) dplyr, purrr, broom, tidyr ####

# Iremos utilizar as funcoes nest, unnest e map, para criar um tibble, ou table data frame,
# que contem colunas formadas por listas,
# isto é pratico pois podemos unir objetos de diferentes dimensoes em um único tibble,
# e este pode ser separado depois, para criacao de diversos objetos

tibble_talh_par <- dados %>% # definicao do df
  group_by(TALHAO, PARCELA) %>% # definicao dos grupos
  nest  %>% # com tidyr::nest agrupamos os dados a mais em uma lista, resumindo os dados ( a funcao unnest desfaz este ato)
  mutate(Reg = map(data, ~lm(LN_HT ~ INV_DAP, data =.)),
         Coefs = map(Reg, tidy) , # a funcao broom:tidy nos da os coeficientes e algumas estatisticas adicionais
         Glance = map(Reg, glance) ) # a funcao broom::glance oferece as estatisticas R2 ajustato e sigma, entre outras. Iremos remover as outras, mas sinta-se livre pra manter as variaveis desejadas

# Porem, glance e tidy geram muitas variaveis que geralmente nao sao utilizadas
# se quisermos apenas os coeficientes, R2 ajustado e o erro-padrao, podemos obtelos da seguinte forma:

tibble_talh_par <- dados %>% # definicao do df
  group_by(TALHAO, PARCELA) %>% # definicao dos grupos
  nest  %>% # com tidyr::nest agrupamos os dados a mais em uma lista, resumindo os dados ( a funcao unnest desfaz este ato)
  mutate(Reg = map(data, ~lm(LN_HT ~ INV_DAP, data =.)),
         Coefs = map(Reg, 
                     function(x) tibble(term=tidy(x)$term,  # coluna que contem os nomes dos coeficientes
                                        estimate=tidy(x)$estimate ) ), # coluna que contem os valores dos coeficientes
         Glance = map(Reg, 
                      function(x) tibble( Rsqr=glance(x)$adj.r.squared, # coluna do R2 ajustado
                                          Std.Error=glance(x)$sigma )    )      ) # coluna do erro-padrao

# Porem, ainda temos o problema d eque tidy nos da os coeficientes organizados por fator
# e quisermos os coeficientes organizados por coluna, podemos utilizar spread dentro da funcao criada anteriormente

tibble_talh_par <- dados %>% # definicao do df
  group_by(TALHAO, PARCELA) %>% # definicao dos grupos
  nest  %>% # com tidyr::nest agrupamos os dados a mais em uma lista, resumindo os dados ( a funcao unnest desfaz este ato)
  mutate(Reg = map(data, ~lm(LN_HT ~ INV_DAP, data =.)),
         Coefs = map(Reg, 
                     function(x) 
                       tibble(b=tidy(x)$term, 
                              estimate=tidy(x)$estimate ) %>% 
                       mutate(b = factor(b, labels=0:(length(b)-1) ) ) %>% # mudamos os nomes dos coeficientes para bn
                       spread(                    # com spread deixamos a tabela horizontal, colocando cada coeficiente em uma coluna
                         b, estimate,sep="" # variaveis que seram utilizadas no spread
                       )  ),
         Glance = map(Reg, 
                      function(x) tibble( Rsqr=glance(x)$adj.r.squared, 
                                          Std.Error=glance(x)$sigma )    ),
         Res = map(Reg, resid) ) # a funcao resid cria um vetor com os valores residuais do ajuste

# ou, se salvarmos esta funcao mais complexa separadamente,

tidys <- function(x) 
{
  tibble(b=tidy(x)$term, 
         estimate=tidy(x)$estimate ) %>% 
    mutate(b = factor(b, labels=0:(length(b)-1) ) ) %>% # mudamos os nomes dos coeficientes para bn
  spread(                    # com spread deixamos a tabela horizontal, colocando cada coeficiente em uma coluna
          b, estimate,sep="" # variaveis que seram utilizadas no spread
                             ) 
  }


tibble_talh_par <- dados %>% # definicao do df
  group_by(TALHAO, PARCELA) %>% # definicao dos grupos
  nest  %>% # com tidyr::nest agrupamos os dados a mais em uma lista, resumindo os dados ( a funcao unnest desfaz este ato)
  mutate(Reg = map(data, ~lm(LN_HT ~ INV_DAP, data =.)),
         Coefs = map(Reg, tidys ),
         Glance = map(Reg, 
                      function(x) tibble( Rsqr=glance(x)$adj.r.squared, 
                                          Std.Error=glance(x)$sigma )    ),
         Res = map(Reg, resid) ) # a funcao resid cria um vetor com os valores residuais do ajuste

# a vantagem deste metodo e que nao e necessaria a definicao dos nomes de cada coeficiente
# deixando a funcao automatizada para qualquer modelo, com n coeficientes


tibble_talh_par
# Observe como cada coluna é uma lista que agrupa diferentes variaveis, com dimensoes diferentes
# a informacao de cada parcela esta agrupada neste objeto
# data tem as informacoes basicas das parcelas (DAP, HT, etc) comprimidos em uma coluna,
# Reg tem objetos de regressao de cada ajuste de cada parcela,
# Coefs tem os coeficientes e mais algumas variaveis de qualidade do ajuste de cada parcela.
# Glance tem variaveis de qualidade do ajuste de cada parcela,
# e uma coluna dos residuos de cada ajuste

# Assim, se quisermos obter a tabela de coeficientes e qualidade de ajuste,
# podemos utilizar unnest
tibble_talh_par %>%
unnest(Coefs, Glance, .drop=T) # O argumento .drop controla se as colunas lista adicionais devem ser mantidas ou nao

# ou uma tabela contendo os dados originais e os residuos
tibble_talh_par %>%
  unnest(data, Res)

# 5.5) Comparar as tabelas geradas ####

# Podemos testar se as tabelas geradas sao iguais
# Primeiro argumento Target, segundo argumento Current
all.equal(tab_nlme_talh_par, tab_rbase_talh_par)
# Aqui recebos um comando TRUE, ou seja, as tabelas sao identicas

# Neste caso como a tabela gerada pelo dplyr possui uma coluna a mais,
# Devemos especificar as colunas que desejamos comparar:
all.equal(tab_nlme_talh_par[,c(2,3,4,5)], tab_dplyr_talh_par[,c(3,4,5,6)])
# Recebemos o aviso de a classe dos nomes das colunas sao diferentes
# Porem nao recebemos nenhum outro aviso, ou seja, os dados gerados sao identicos

# 6) Exportar tabelas de coeficientes ####

write.csv2(tab_rbase_talh, file.choose())
write.csv2(tab_rbase_talh_par, file.choose())

write.csv2(tab_dplyr_talh, file.choose())
write.csv2(tab_dplyr_talh_par, file.choose())

write.csv2(tab_nlme_talh, file.choose())
write.csv2(tab_nlme_talh_par, file.choose())

write.csv2(tab_dplyr_broom_talh, file.choose())
write.csv2(tab_dplyr_broom_talh_par, file.choose())

# 7) Juncao dos dados originais e dados de regressao ####

# 7.1) Importar dados ####

#tab_reg <- read.csv(file.choose(), header = T)

# 7.2) R base ####

# Caso os dados nao possuam a chave utilizada na regressao,
# devemos adiciona-la
dados_orig_mod <- within(dados_orig, TALHAO_PAR <- paste(TALHAO, PARCELA, sep='_'))

# Utilizamos a funcao Merge, que se comporta de forma similar ao PROCV;
# Os primeiros dois argumentos informam os dados a serem assimilados;
# O terceiro argumento informa sera o fator de uniao.
# Utilizamos o argumento all.x = TRUE, para garantir que os dados originais sejam preservados
tab_final_rbase_talh <- merge(dados_orig, tab_rbase_talh, by = "TALHAO", all.x = TRUE)
tab_final_rbase_talh_par <- merge(dados_orig_mod, tab_rbase_talh_par, by = "TALHAO_PAR", all.x = TRUE)

# Substitui-se NAs por 0, caso desejado
tab_final_rbase_talh[is.na(tab_final_rbase_talh)] <- 0
tab_final_rbase_talh_par[is.na(tab_final_rbase_talh_par)] <- 0

head(tab_final_rbase_talh)
head(tab_final_rbase_talh_par)

# 7.3) dplyr ####

# Com o pacote dplyr nao e necessario unir as chaves:
# caso elas nao existam, devem ser adicionadas 
# com a funcao mutate() ou cbind() separadamente, e entao,
# Utilizamos full join (ou left join)
# para garantir que os dados originais nao sejam alterados


tab_final_dplyr_talh <- dados_orig %>%
  full_join(tab_dplyr_talh)

tab_final_dplyr_talh_par <- dados_orig %>%
  full_join(tab_dplyr_talh_par, by = c("TALHAO", "PARCELA"))

# Substituir NAs por 0, caso desejado
tab_final_dplyr_talh[is.na(tab_final_dplyr_talh)] <- 0
tab_final_dplyr_talh_par[is.na(tab_final_dplyr_talh_par)] <- 0

head(tab_final_dplyr_talh)
head(tab_final_dplyr_talh_par)


# Podemos verificar se as tabelas finais sao iguais:
all.equal(tab_final_dplyr_talh, tab_final_rbase_talh)
# Recebos a resposta TRUE, ou seja, as tabelas sao identicas

# 8) Exportar tabelas finais ####

write.csv(tab_final_rbase_talh, file.choose())
write.xlsx2(tab_final_rbase_talh, file.choose())
write.csv(tab_final_rbase_talh_par, file.choose())
write.xlsx2(tab_final_rbase_talh_par, file.choose())

write.csv(tab_final_dplyr_talh, file.choose())
write.xlsx2(tab_final_dplyr_talh, file.choose())
write.csv(tab_final_dplyr_talh_par, file.choose())
write.xlsx2(tab_final_dplyr_talh_par, file.choose())





# Chamar residuos (tidyr, dplyr, purrr) ####

dados %>%  # definicao do df
  group_by(TALHAO) %>% # definicao dos grupos
  nest  %>% # com tidyr::nest agrupamos os dados a mais em uma lista, resumindo os dados ( a funcao unnest desfaz este ato)
  mutate(Reg = map(data, ~lm(LN_HT ~ INV_DAP + LN_HD, data =.)),  # map gera objetos dentro de um nest, ou seja, em forma de lista, por grupo
         Res = map(Reg, resid ) ) %>%
  unnest(data, Res, .drop=F) # reverte os dados ao estado original; .drop=F Mantem a coluna com o ajuste feito anteriormente

dados %>%  # definicao do df
  group_by(TALHAO) %>% # definicao dos grupos
  nest  %>% # com tidyr::nest agrupamos os dados a mais em uma lista, resumindo os dados ( a funcao unnest desfaz este ato)
  mutate(Reg = map(data, ~lm(LN_HT ~ INV_DAP + LN_HD, data =.)),  # map gera objetos dentro de um nest, ou seja, em forma de lista, por grupo
         Coefs = map(Reg, 
                     function(x) tibble(
                       
                       b0=coef(x)[1],
                       b1=coef(x)[2],
                       b2=coef(x)[3],
                       Rsqr=summary(x)[[9]],
                       Std.Error=summary(x)[[6]]  ) 
         ),
         Res = map(Reg, resid ) ) %>%
  unnest(Coefs)


