


dadosex <- dados_orig %>% 
  filter(!is.na(CAP)) %>% 
  mutate( DAP=CAP/pi, INV_DAP=1/DAP, LN_HT=log(HT), LN_HD=log(HD) )



group_lm(dadosex, c("TALHAO", "PARCELA"), "LN_HT ~ INV_DAP" )

group_lm(dadosex, c("TALHAO", "PARCELA"), "LN_HT ~ INV_DAP", coef_only = T )

group_lm(dadosex, c("TALHAO", "PARCELA"), "LN_HT ~ INV_DAP", merge_coef = T )



group_lm(dadosex, c("TALHAO"), "LN_HT ~ INV_DAP + LN_HD" )

group_lm(dadosex, c("TALHAO"), "LN_HT ~ INV_DAP + LN_HD", coef_only = T )

group_lm(dadosex, c("TALHAO"), "LN_HT ~ INV_DAP + LN_HD", merge_coef = T )





dadosex %>% 
  group_lm(c("TALHAO"), "LN_HT ~ INV_DAP + LN_HD", merge_coef = T )


dadosex %>% 
  group_lm(c("TALHAO"), "LN_HT ~ INV_DAP + LN_HD", merge_coef = T ) %>% 
  fit_mod("LN_HT ~ INV_DAP + LN_HD", name = "HT_EST")


dadosex %>% 
  group_lm(c("TALHAO"), "LN_HT ~ INV_DAP + LN_HD", merge_coef = T ) %>% 
  fit_mod("LN_HT ~ INV_DAP + LN_HD", name = "HT_EST") %>% 
  as.data.frame %>% 
  head(15)

dadosex %>% 
  group_lm(c("TALHAO"), "LN_HT ~ INV_DAP + LN_HD", merge_coef = T ) %>% 
  mutate( HT_EST = ifelse(is.na(HT), exp(b0 + b1*INV_DAP + b2*LN_HD ) , HT )   )%>% 
  select(-b0, -b1, -b2, -LN_HT, -INV_DAP, -LN_HD) %>% 
  as.data.frame %>% 
  head(15)


dadosex %>% 
  group_lm(c("TALHAO", "PARCELA"), "LN_HT ~ INV_DAP", merge_coef = T )

dadosex %>% 
  group_lm(c("TALHAO", "PARCELA"), "LN_HT ~ INV_DAP", merge_coef = T ) %>% 
  fit_mod("LN_HT ~ INV_DAP", name = "HT_EST") %>% 
  as.data.frame %>% 
  head
