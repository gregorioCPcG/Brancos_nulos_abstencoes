#seleção #####
library(haven)
ESEB2018 <- read_sav("C:/Users/grego/Dropbox/doutorado/2021.1 disciplinas/Metodologia Quantitativa avançada/paper/04622.sav")
library (shiny)
library(tidyverse)
options(scipen = 1000)
library(haven)
library(memisc)
library(sjPlot)
library(Zelig)
library(knitr)
library(car)
library(kableExtra)
BASET <- subset(ESEB2018, select = c(D1A_FAIXAID, D2_SEXO, D3_ESCOLA, Q12P1_B, Q12P2_B, D12A, localidade,REG))
BASET$MULTI_T1<- memisc::recode(as.factor(BASET$Q12P1_B), 1 <- c (9), 2 <- c(5), 
                                3 <- c(3), 4 <- c(6), 5 <-c(10),
                                6 <-c(1,2,4,7,8,12,13,14), 7 <-c(50,60, 97,98, 99))
table(BASET$Q12P2_B)
# 10 amoedo - 35 pessoas
# https://rpubs.com/gregogregogregogregogrego/819255 - amoedo 1.99 %

BASET <- subset(BASET, select = c(D1A_FAIXAID, D2_SEXO, D3_ESCOLA, MULTI_T1, Q12P1_B, Q12P2_B, D10, REG, D12A)) %>% na.omit() 

BASET$BOLS_HADD_T2<- memisc::recode(as.factor(BASET$Q12P2_B), 1 <- c (1,2), 2 <- c(50,60,97,98,99))
BASET$BOLS_HADD_T2_<- memisc::recode(as.factor(BASET$Q12P2_B), 1 <- c (2), 2 <-c(1), 3 <- c(50,60,97,98,99))
table(BASET$BOLS_HADD_T2)
table(BASET$Q12P2_B) #para comparar
BASET <- within(BASET, {
  turno2 <- Recode(BOLS_HADD_T2, '1 = "valid"; 2 = "invalid"', as.factor=FALSE)
})
BASET <- within(BASET, {
  turno2_ <- Recode(BOLS_HADD_T2_, '1 = "Bolsonaro"; 2 = "Haddad"; 3 = "invalid"', as.factor=FALSE)
})
table(BASET$BOLS_HADD_T2) #para conferir
table(BASET$turno2_) # para conferir
BASET$faixa_idade <- as.numeric(BASET$D1A_FAIXAID) # necessário em todos os dados
BASET$escolaridade <- as.numeric(BASET$D3_ESCOLA)
BASET$sexo <- as.numeric(BASET$D2_SEXO)
BASET <- BASET %>%
  mutate(Feminino = case_when(sexo == "2" ~ 1,
                              TRUE ~0)) 
BASET <- BASET %>%
  dplyr::mutate(invalid = case_when(MULTI_T1 == "7" ~ 1,
                                   TRUE ~ 0)) 
table(BASET$invalid)#para conferir
BASET$D10 <- as.numeric(BASET$D10)

BASET$D12A <- as.numeric(BASET$D12A)

BASET <- BASET %>%
  mutate(Branco = case_when(D12A == "3" ~ 1,
                            TRUE ~0)) 
BASET$REG <- as.numeric(BASET$REG)
BASET <- BASET %>%
  mutate(Sudeste = case_when(REG == "3" ~ 1,
                             TRUE ~ 0))%>%
  mutate(Norte = case_when(REG == "1" ~ 1,
                           TRUE ~ 0))%>%
  mutate(Centro_Oeste = case_when(REG == "5" ~ 1,
                                  TRUE ~ 0))%>%
  mutate(Nordeste = case_when(REG == "2" ~1,TRUE ~ 0))%>%
  mutate(Sul = case_when(REG == "4" ~1,TRUE ~ 0))


dados <- subset(BASET, select = c(faixa_idade, Feminino, escolaridade, invalid, 
                                  localidade, Branco, Sudeste, Norte, Centro_Oeste,
                                  Nordeste,Sul, turno2, turno2_))

rm(BASET, ESEB2018)
detach("package:memisc", unload = TRUE)


### analisis ####
table(dados$invalid)
by(dados$escolaridade, dados$invalid, mean)
by(dados$faixa_idade, dados$invalid, mean)
by(dados$Feminino, dados$invalid, mean)*100
by(dados$Branco, dados$invalid, mean)*100
prop.table(table(dados$invalid, dados$localidade),1)*100
social1 <- glm(invalid ~ escolaridade + localidade, data = dados, family =
                 binomial(link=logit))
tab_model(social1, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
(0.33-1)*100

social2 <- glm(invalid ~ escolaridade + faixa_idade +
                Branco + Feminino + localidade + Sudeste + Centro_Oeste +
                Nordeste + Norte, data = dados,
                   family=binomial(link=logit))
tab_model(social2, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
library(coefplot)
coefplot(social2, intercept = FALSE)


# explicando o segundo turno

prop.table(table(dados$invalid, dados$turno2_),1)*100
library(nnet)
dados$turno2_ <- as.factor(dados$turno2_)
dados$turno2_ <- relevel(dados$turno2_, "invalid")
voto1 <- multinom(turno2_ ~ escolaridade + localidade, data = dados)
tab_model(voto1, show.ci = F, auto.label = T, rm.terms= "t_name", show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
voto2 <-multinom(turno2_ ~ escolaridade + localidade+Sudeste+Centro_Oeste+Nordeste+Norte+Feminino+Branco+faixa_idade, data = dados)
tab_model(voto2, show.ci = F, auto.label = T, rm.terms= "t_name", show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
voto3 <-multinom(turno2_ ~ invalid, data = dados)
tab_model(voto3, show.ci = F, auto.label = T, rm.terms= "t_name", show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
voto4 <-multinom(turno2_ ~ invalid + escolaridade + localidade+Sudeste+Centro_Oeste+Nordeste+Norte+Feminino+Branco+faixa_idade, data = dados)
tab_model(voto4, show.ci = F, auto.label = T, rm.terms= "t_name", show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
