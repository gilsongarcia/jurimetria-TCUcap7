library(readxl)
package(ellipsis)
install.packages("ellipsis")
install.packages("RTools")
install.packages("tidyverse")
library(tidyverse)

View(tcu_licitacao_140720)
r<- tcu
r<- tcu_licitacao_140720
r1<-r

r<-r %>% filter(`Tipo de processo`=="REPRESENTAÇÃO (REPR)")
r2<-r
r <- r %>% filter(str_detect(Sumário,"(?i)(cautelar)"))
r3<-r
r <- r %>% filter(str_detect(Sumário,"(?i)(PROCEDENTE|PROCEDÊNCIA|
IMPROCEDENTE|IMPROCEDÊNCIA)"))
r<-r %>% mutate(Ano_processo=str_sub(Processo,9,12))
r<-r %>% mutate(mp=case_when(str_detect
                             (d$`Representante do Ministério Público`,
                               "não atuou|Não atuou|não atuou.|Não atuou.|não há")~
                               "nao atuou",TRUE~"atuou"))
r<-r %>% mutate(mp=case_when(str_detect
                             (r$`Representante do Ministério Público`,
                               "não atuou|Não atuou|não atuou.|Não atuou.|não há")~
                               "nao atuou",TRUE~"atuou"))
r <- r %>% mutate(decisao=case_when(str_detect(Sumário,
                                               "(?i)(IMPROCEDÊNCIA|improcedente|não procedente|não procedência)")~
                                      "improcedente",TRUE~"procedente"))
r <- r %>% mutate(decisao1=case_when(str_detect(decisao,
                                                "(?i)(improcedente)")~"0",TRUE~"1"))
r$cautelar <- str_detect(r$Sumário, "(?i)(pregão)")
r$pregao <- str_detect(r$Sumário, "(?i)(pregão)")
r$concorrencia <- str_detect(p$Sumário, "(?i)(concorrência)")
View(r)
r$concorrencia <- str_detect(p$Sumário, "(?i)(concorrência)")
r$concorrencia <- str_detect(r$Sumário, "(?i)(concorrência)")
table(r$Ano_processo)
table(r$Relator)
table(r$mp)
table(r$pregao)
table(r$concorrencia)
table(r$decisao)
table(r$decisao1)
glm.fit=glm(decisao1~r$Ano_processo+r$pregao+r$concorrencia+r$Relator+r$mp,
            data = r , family = binomial)
r <- r %>% mutate(decisao1=as.factor(decisao1))
glm.fit=glm(decisao1~r$Ano_processo+r$pregao+r$concorrencia+r$Relator+r$mp,
            data = r , family = binomial)
coef(glm.fit)
table(glm.fit)
summary(glm.fit)

   
exp(coef(glm.fit))
    r <- r  %>% mutate(Ano_processo=as.factor(Ano_processo))
    glm.fit=glm(decisao1~r$Ano_processo+r$pregao+r$concorrencia+r$Relator+r$mp,
                data = r , family = binomial)
    coef(glm.fit)
    summary(glm.fit)
    glm.fit=glm(decisao1~r$Ano_processo+r$pregao+r$concorrencia+r$Relator+r$mp+
                  r$cautelar,
                data = r , family = binomial)
    summary(glm.fit)
   
    
    View(per_tcu)
   
    r<-per_tcu
    r1<-r
    r <- r %>% filter(str_detect(Sumário,"(?i)(PROCEDENTE|PROCEDÊNCIA|
IMPROCEDENTE|IMPROCEDÊNCIA)"))
    table(r$MODALIDADE)
    r2<-r
    r <- r %>% mutate(decisao1=case_when(str_detect(Sumário,
                                                    "(?i)(IMPROCEDÊNCIA|improcedente|não procedente|não procedência)")~
                                           "improcedente",TRUE~"procedente"))
    r <- r %>% mutate(decisao0=case_when(str_detect(decisao,
                                                    "(?i)(improcedente)")~"0",TRUE~"1"))
    r <- r %>% mutate(decisao=case_when(str_detect(decisao,
                                                   "(?i)(improcedente)")~"0",TRUE~"1"))
    r <- r %>% mutate(decisao=case_when(str_detect(decisao1,
                                                   "(?i)(improcedente)")~"0",TRUE~"1"))
    View(r)
    r <- r %>% mutate(OUTRAS=case_when(str_detect(MODALIDADE,
                                                  "(?i)(0)")~"TRUE",TRUE~"FALSE"))
    table(r$OUTRAS)
    glm.fit=glm(decisao~r$Ano_processo+r$PREGAO+r$CONCORRENCIA+r$OUTRAS+
                  r$Relator+r$mp,
                data = r , family = binomial)
    r<-r %>% mutate(mp=case_when(str_detect
                                 (r$`Representante do Ministério Público`,
                                   "não atuou|Não atuou|não atuou.|Não atuou.|não há")~
                                   "nao atuou",TRUE~"atuou"))
    glm.fit=glm(decisao~r$Ano_processo+r$PREGAO+r$CONCORRENCIA+r$OUTRAS+
                  r$Relator+r$mp,
                data = r , family = binomial)
    r <- r %>% mutate(decisao1=as.factor(decisao1))
    glm.fit=glm(decisao~r$Ano_processo+r$PREGAO+r$CONCORRENCIA+r$OUTRAS+
                  r$Relator+r$mp,
                data = r , family = binomial)
    r <- r %>% mutate(decisao=as.factor(decisao))
    glm.fit=glm(decisao~r$Ano_processo+r$PREGAO+r$CONCORRENCIA+r$OUTRAS+
                  r$Relator+r$mp,
                data = r , family = binomial)
    coef(glm.fit)
    glm.fit=glm(decisao~r$Ano_processo+r$PREGAO+r$CONCORRENCIA+
                  r$Relator+r$mp,
                data = r , family = binomial)
    coef(glm.fit)
    summary(glm.fit)
    glm.fit=glm(decisao1~r$Ano_processo+r$pregao+r$concorrencia+r$Relator+r$mp+
                  r$cautelar,
                data = r , family = binomial)
    summary(glm.fit)
    summary(glm.fit)
    glm.fit=glm(decisao~r$Ano_processo+r$PREGAO+r$CONCORRENCIA+
                  r$Relator+r$mp,
                data = r , family = binomial)
    glm.fit=glm(decisao~r$Ano_processo+r$PREGAO+r$CONCORRENCIA+
                  r$Relator+r$mp,
                data = r , family = binomial)
    glm.fit=glm(decisao~r$Ano_processo+r$PREGAO+
                  r$CONCORRENCIA+r$Relator+r$mp,
                data = r , family = binomial)
   
    