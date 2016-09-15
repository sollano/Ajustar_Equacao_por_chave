group_lm <- function(df, groups, modelo, coef_only = F, merge_coef = F){

require("broom")
require("purrr")
require("tidyr")
require("dplyr")
  
mod <- formula(modelo)

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