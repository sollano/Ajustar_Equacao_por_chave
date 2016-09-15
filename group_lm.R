group_lm <- function(df, groups, modelo, coef_only = F, merge_coef = F){

require("broom")
require("purrr")
require("tidyr")
require("dplyr")
  
mod <- formula(modelo)

tidy_ <- function(x){
  tibble(b=tidy(x)$term, # criamos um data frame que tem apenas as colunas dos betas e seus valores
         estimate=tidy(x)$estimate ) %>% 
    mutate(b = factor(b, labels=0:(length(b)-1) ) ) %>% # mudamos os nomes dos coeficientes para bn
    spread( b, estimate,sep="" )     # com spread deixamos a tabela horizontal, colocando cada coeficiente em uma coluna
  
}

glance_ <- function(x){ 
  
  tibble( Rsqr=glance(x)$adj.r.squared, # criamos um data table que tem apenas as variaveis R quadrado ajustado e erro padrao
          Std.Error=glance(x)$sigma ) }


x <- df %>% 
  ungroup %>% 
  group_by_(.dots = groups ) %>% 
  nest  %>% 
  mutate(Reg = map(data, ~lm(mod, data =.) ),
         Coefs  = map(Reg, tidy_   ),
         Qualid = map(Reg, glance_ ),
         Res = map(Reg, resid) ) 

if(coef_only==T && merge_coef==T) stop("please select only one option")

if(coef_only==T){
  
  
  x <- unnest(x, Coefs, .drop = T) 


}else if(merge_coef==T){
  
  x <- x %>% 
    unnest(data, .drop=F) %>% 
    unnest(Coefs) %>% 
    select(-Reg, -Qualid, -Res)
}

return(x)

}