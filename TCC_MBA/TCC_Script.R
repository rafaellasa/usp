#
# USP/ESALQ | MBA em Data Science e Analytics
#
# Discente: Rafaella Silva Almeida
# Orientadora: Patrícia Belfiore Fávero
#
# Previsão do risco de inadimplência em uma construtora utilizando regressão logística
#


# 0. INSTALAÇÃO E CARREGAMENTO DE PACOTES

pacotes <- c("Rcpp", "ggpubr", "dplyr","plotly","tidyverse","knitr","kableExtra","fastDummies","rgl","car",
             "reshape2","jtools","lmtest","caret","pROC","ROCR","nnet","magick",
             "cowplot","readxl","imbalance","sjPlot")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

library(lmtest)
library(jtools)

# 1. CARREGAMENTO DA BASE

dados_brutos <- read_excel("TCC_Dataset_Final.xlsx")

# 2. ANÁLISE EXPLORATÓRIA

# 2.1. Visualização dos dados

dados_brutos %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 16)

# 2.2. Conhecendo a estrutura do dataset
str(dados_brutos)

# 2.3. Resumo dos dados
summary(dados_brutos)

# 2.4. Examinando os dados
dplyr::glimpse(dados_brutos)

# 2.5. Quantidade de valores ausentes por variável
colSums(is.na(dados_brutos))

# 2.6. Quantidade de valores distintos por variável
aggregate(values ~ ind, unique(stack(dados_brutos)), length)

# 2.7. Conhecendo os valores distintos das variáveis

table(dados_brutos$empreendimento)
table(dados_brutos$origem)
table(dados_brutos$sexo)
table(dados_brutos$escolaridade)
table(dados_brutos$tipo_fonte_renda)
table(dados_brutos$tem_segundo_comprador)
table(dados_brutos$possui_renda_extra)
table(dados_brutos$estado_civil)
table(dados_brutos$possui_dependentes)
table(dados_brutos$composicao_renda)
table(dados_brutos$modalidade)
table(dados_brutos$amortizacao)
table(dados_brutos$fgts_utilizado)

# 3. TRATAMENTO DOS DADOS

# 3.1. Descarte da variável 'estado_civil'

dados_brutos$estado_civil <- NULL 

dados_brutos$parcela_caixa <- NULL

dados_brutos$parcela_empresa <- NULL

# 3.2. Preencher valores ausentes das variáveis categóricas com valores padrão 
#      do negócio

dados_brutos$escolaridade[which(is.na(dados_brutos$escolaridade))] <- "Não informado"

dados_brutos$tipo_fonte_renda[which(is.na(dados_brutos$tipo_fonte_renda))] <- "Não informado"

dados_brutos$possui_renda_extra[which(is.na(dados_brutos$possui_renda_extra))] <- "Não informado"

dados_brutos$possui_dependentes[which(is.na(dados_brutos$possui_dependentes))] <- "Não informado"

dados_brutos$composicao_renda[which(is.na(dados_brutos$composicao_renda))] <- "Não informado"

dados_brutos$modalidade[which(is.na(dados_brutos$modalidade))] <- "Não informado"

dados_brutos$amortizacao[which(is.na(dados_brutos$amortizacao))] <- "Não informado"

dados_brutos$fgts_utilizado[which(is.na(dados_brutos$fgts_utilizado))] <- "Não informado"

dados_brutos$tem_segundo_comprador[which(is.na(dados_brutos$tem_segundo_comprador))] <- "Não informado"

# 3.3. Alterar o tipo das variáveis métricas e preencher as ausências com a
#      mediana correspondente

dados_brutos <- dados_brutos %>% dplyr::mutate(renda_bruta_mensal=as.numeric(renda_bruta_mensal),
                                                     porc_comprador_1=as.numeric(porc_comprador_1),
                                                     porc_comprador_2=as.numeric(porc_comprador_2),
                                                     renda_extra=as.numeric(renda_extra),
                                                     renda_segundo_comprador=as.numeric(renda_segundo_comprador),
                                                     desc_promocional=as.numeric(desc_promocional),
                                                     subsidio_estadual=as.numeric(subsidio_estadual),
                                                     subsidio_federal=as.numeric(subsidio_federal),
                                                     entrada=as.numeric(entrada),
                                                     fgts=as.numeric(fgts),
                                                     financiamento=as.numeric(financiamento),
                                                     recursos_proprios=as.numeric(recursos_proprios),
                                                     prazo_caixa=as.numeric(prazo_caixa))

dados_brutos <- dados_brutos %>% mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))

# 3.4. Atribuir à variável 'escolaridade' o tipo factor com os respectivos níveis

dados_brutos <- dados_brutos %>% mutate(escolaridade=factor(escolaridade,
                                                                  levels = c("Não informado","Ens. Fundamental Incompleto","Ens. Fundamental Completo","Ens. Médio Incompleto","Ens. Médio Completo","Ens. Superior Incompleto","Ens. Superior Completo","Ens. Pós-graduação Incompleto","Ens. Pós-graduação Completo"),
                                                                  ordered=TRUE))

# 3.5. Transformando a variável alvo e visualizando a proporção de inadimplentes na base

ds_inadimplente <- dados_brutos %>% mutate(inadimplente = ifelse(inadimplente == "Sim",
                                                                    yes = 1,
                                                                    no = 0),
                                              inadimplente = factor(inadimplente)) 

#colSums(is.na(ds_inadimplente))

#ds_inadimplente$escolaridade[which(is.na(ds_inadimplente$escolaridade))] <- "Não informado"

#ds_inadimplente <- ds_inadimplente %>% mutate(escolaridade=factor(escolaridade,
#                                                            levels = c("Não informado","Ens. Fundamental Incompleto","Ens. Fundamental Completo","Ens. Médio Incompleto","Ens. Médio Completo","Ens. Superior Incompleto","Ens. Superior Completo","Ens. Pós-graduação Incompleto","Ens. Pós-graduação Completo"),
#                                                            ordered=TRUE))

as.data.frame(table(ds_inadimplente$inadimplente)) %>% 
  ggplot(aes(y = Freq, x = Var1)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  geom_col() +
  geom_text(aes(label = Freq), vjust = 1.5, colour = "white")+
  theme_classic(base_size = 18) +
  xlab("Inadimplência") + 
  ylab("Número de clientes") 

# 3.6. Plotando a distibuição de inadimplentes de acordo com cada variável categórica

# Inadimplência x escolaridade

plot_escolaridade <- ds_inadimplente %>% ggplot(aes(y = escolaridade)) + 
  geom_bar(aes(fill = inadimplente, colour = inadimplente), alpha = 0.5) +
  ylab("Escolaridade") + 
  xlab("Número de clientes")+
  theme(legend.position = "top")+
  scale_fill_manual("Inadimplente", values = c("1" = "red", "0" = "green"))+
  scale_colour_manual("Inadimplente", values = c("1" = "red", "0" = "green"))

# Inadimplência x empreendimento

plot_empreendimento <- ds_inadimplente %>% ggplot(aes(y = empreendimento)) + 
  geom_bar(aes(fill = inadimplente, colour = inadimplente), alpha = 0.5) +
  ylab("Empreendimento") + 
  xlab("Número de clientes")+
  theme(legend.position = "top")+
  scale_fill_manual("Inadimplente", values = c("1" = "red", "0" = "green"))+
  scale_colour_manual("Inadimplente", values = c("1" = "red", "0" = "green"))

# Inadimplência x origem

plot_origem <- ds_inadimplente %>% ggplot(aes(y = origem)) + 
  geom_bar(aes(fill = inadimplente, colour = inadimplente), alpha = 0.5) +
  ylab("Origem") + 
  xlab("Número de clientes")+
  theme(legend.position = "top",axis.text.y = element_text(size = 6))+
  scale_fill_manual("Inadimplente", values = c("1" = "red", "0" = "green"))+
  scale_colour_manual("Inadimplente", values = c("1" = "red", "0" = "green"))

# Inadimplência x Tipo de fonte de renda

plot_tipo_fonte_renda <- ds_inadimplente %>% ggplot(aes(y = tipo_fonte_renda)) + 
  geom_bar(aes(fill = inadimplente, colour = inadimplente), alpha = 0.5) +
  ylab("Tipo de fonte de renda") + 
  xlab("Número de clientes")+
  theme(legend.position = "top")+
  scale_fill_manual("Inadimplente", values = c("1" = "red", "0" = "green"))+
  scale_colour_manual("Inadimplente", values = c("1" = "red", "0" = "green"))

#Inadimplência x Tem segundo comprador

plot_tem_segundo_comprador <- ds_inadimplente %>% ggplot(aes(y = tem_segundo_comprador)) + 
  geom_bar(aes(fill = inadimplente, colour = inadimplente), alpha = 0.5) +
  ylab("Tem segundo comprador") + 
  xlab("Número de clientes")+
  theme(legend.position = "top")+
  scale_fill_manual("Inadimplente", values = c("1" = "red", "0" = "green"))+
  scale_colour_manual("Inadimplente", values = c("1" = "red", "0" = "green"))

# Inadimplência x Sexo

plot_sexo <- ds_inadimplente %>% ggplot(aes(y = sexo)) + 
  geom_bar(aes(fill = inadimplente, colour = inadimplente), alpha = 0.5) +
  ylab("Sexo") + 
  xlab("Número de clientes")+
  theme(legend.position = "top")+
  scale_fill_manual("Inadimplente", values = c("1" = "red", "0" = "green"))+
  scale_colour_manual("Inadimplente", values = c("1" = "red", "0" = "green"))

# Inadimplência x Possui renda extra

plot_possui_renda_extra <- ds_inadimplente %>% ggplot(aes(y = possui_renda_extra)) + 
  geom_bar(aes(fill = inadimplente, colour = inadimplente), alpha = 0.5) +
  ylab("Possui renda extra") + 
  xlab("Número de clientes")+
  theme(legend.position = "top")+
  scale_fill_manual("Inadimplente", values = c("1" = "red", "0" = "green"))+
  scale_colour_manual("Inadimplente", values = c("1" = "red", "0" = "green"))

# Inadimplência x Possui dependentes

plot_possui_dependentes <- ds_inadimplente %>% ggplot(aes(y = possui_dependentes)) + 
  geom_bar(aes(fill = inadimplente, colour = inadimplente), alpha = 0.5) +
  ylab("Possui dependentes") + 
  xlab("Número de clientes")+
  theme(legend.position = "top")+
  scale_fill_manual("Inadimplente", values = c("1" = "red", "0" = "green"))+
  scale_colour_manual("Inadimplente", values = c("1" = "red", "0" = "green"))

# Inadimplência x Composição de renda

plot_composicao_renda <- ds_inadimplente %>% ggplot(aes(y = composicao_renda)) + 
  geom_bar(aes(fill = inadimplente, colour = inadimplente), alpha = 0.5) +
  ylab("Composição de renda") + 
  xlab("Número de clientes")+
  theme(legend.position = "top")+
  scale_fill_manual("Inadimplente", values = c("1" = "red", "0" = "green"))+
  scale_colour_manual("Inadimplente", values = c("1" = "red", "0" = "green"))

# Inadimplência x Modalidade

plot_modalidade <- ds_inadimplente %>% ggplot(aes(y = modalidade)) + 
  geom_bar(aes(fill = inadimplente, colour = inadimplente), alpha = 0.5) +
  ylab("Modalidade") + 
  xlab("Número de clientes")+
  theme(legend.position = "top")+
  scale_fill_manual("Inadimplente", values = c("1" = "red", "0" = "green"))+
  scale_colour_manual("Inadimplente", values = c("1" = "red", "0" = "green"))

# Inadimplência x Amortização

plot_amortizacao<- ds_inadimplente %>% ggplot(aes(y = amortizacao)) + 
  geom_bar(aes(fill = inadimplente, colour = inadimplente), alpha = 0.5) +
  ylab("Amortizacao") + 
  xlab("Número de clientes")+
  theme(legend.position = "top")+
  scale_fill_manual("Inadimplente", values = c("1" = "red", "0" = "green"))+
  scale_colour_manual("Inadimplente", values = c("1" = "red", "0" = "green"))

# Inadimplência x FGTS utilizado

plot_fgts_utilizado <- ds_inadimplente %>% ggplot(aes(y = fgts_utilizado)) + 
  geom_bar(aes(fill = inadimplente, colour = inadimplente), alpha = 0.5) +
  ylab("FGTS utilizado") + 
  xlab("Número de clientes")+
  theme(legend.position = "top")+
  scale_fill_manual("Inadimplente", values = c("1" = "red", "0" = "green"))+
  scale_colour_manual("Inadimplente", values = c("1" = "red", "0" = "green"))

# Visualizando a influência das variáveis categóricas
ggarrange(plot_origem, plot_escolaridade,plot_tipo_fonte_renda,
          plot_empreendimento,plot_modalidade, plot_possui_dependentes,
          plot_possui_renda_extra,plot_tem_segundo_comprador,plot_composicao_renda, 
          plot_sexo, plot_fgts_utilizado,plot_amortizacao,
          ncol = 3, nrow = 4)

# 3.7. Estatísticas descritivas das variáveis métricas

tapply(ds_inadimplente$renda_bruta_mensal, ds_inadimplente$inadimplente, summary)
tapply(ds_inadimplente$porc_comprador_1, ds_inadimplente$inadimplente, summary)
tapply(ds_inadimplente$porc_comprador_2, ds_inadimplente$inadimplente, summary)
tapply(ds_inadimplente$renda_extra, ds_inadimplente$inadimplente, summary)
tapply(ds_inadimplente$renda_segundo_comprador, ds_inadimplente$inadimplente, summary)
tapply(ds_inadimplente$desc_promocional, ds_inadimplente$inadimplente, summary)
tapply(ds_inadimplente$subsidio_estadual, ds_inadimplente$inadimplente, summary)
tapply(ds_inadimplente$subsidio_federal, ds_inadimplente$inadimplente, summary)
tapply(ds_inadimplente$entrada, ds_inadimplente$inadimplente, summary)
tapply(ds_inadimplente$fgts, ds_inadimplente$inadimplente, summary)
tapply(ds_inadimplente$financiamento, ds_inadimplente$inadimplente, summary)
tapply(ds_inadimplente$recursos_proprios, ds_inadimplente$inadimplente, summary)
tapply(ds_inadimplente$prazo_caixa, ds_inadimplente$inadimplente, summary)

# 4. MODELAGEM

# 4.1. Convertendo a base em dataframe e transformando variáveis categóricas em dummy

df_inadimplente_dummies <- fastDummies::dummy_columns(.data = as.data.frame(ds_inadimplente),
                                                      select_columns = c("origem", 
                                                                         "sexo",
                                                                         "escolaridade", 
                                                                         "empreendimento",
                                                                         "tipo_fonte_renda",
                                                                         "tem_segundo_comprador",
                                                                         "possui_renda_extra",
                                                                         "possui_dependentes",
                                                                         "composicao_renda",
                                                                         "modalidade",
                                                                         "amortizacao",
                                                                         "fgts_utilizado"),
                                                      remove_selected_columns = T,
                                                      remove_first_dummy = T) 

# 4.2. Balanceamento da base

df_inadimplente_sim <- dplyr::filter(df_inadimplente_dummies, inadimplente == 1)
df_inadimplente_nao <- dplyr::filter(df_inadimplente_dummies, inadimplente == 0)

complemento_sim <- dplyr::sample_n(df_inadimplente_sim, 817, replace = TRUE, prob = NULL)

df_inadimplente_balanceado <- rbind(df_inadimplente_nao, df_inadimplente_sim, complemento_sim)

summary(df_inadimplente_balanceado)

dplyr::glimpse(df_inadimplente_balanceado)

as.data.frame(table(df_inadimplente_balanceado$inadimplente)) %>% 
  ggplot(aes(y = Freq, x = Var1)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  geom_col() +
  geom_text(aes(label = Freq), vjust = 1.5, colour = "white")+
  theme_classic(base_size = 18) +
  xlab("Inadimplência") + 
  ylab("Número de clientes") 

# 4.3. Separando base balanceada em treino e teste

indice_treino = createDataPartition(y=df_inadimplente_balanceado$inadimplente, p=0.7, list=FALSE)
treino = df_inadimplente_balanceado[indice_treino, ]
teste = df_inadimplente_balanceado[-indice_treino, ] 

# 4.4. Modelo balanceado e comparação com modelo nulo

modelo_inadimplentes_balanceado <- glm(formula = inadimplente ~ . - id, 
                                   data = treino, 
                                   family = "binomial")

summary(modelo_inadimplentes_balanceado)

logLik(modelo_inadimplentes_balanceado)

summ(modelo_inadimplentes_balanceado, confint = T, digits = 3, ci.width = .95)

modelo_nulo <- glm(formula = inadimplente ~ 1, 
                   data = treino, 
                   family = "binomial")

comparacao_balanceado <- lrtest(modelo_nulo, modelo_inadimplentes_balanceado)

comparacao_balanceado

# 4.4. Modelo balanceado com procedimento stepwise e comparação com modelo nulo

step_inadimplentes_balanceado <- step(object = modelo_inadimplentes_balanceado,
                                   k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

summary(step_inadimplentes_balanceado)

logLik(step_inadimplentes_balanceado)

summ(step_inadimplentes_balanceado, confint = T, digits = 3, ci.width = .95)

comparacao_step <- lrtest(modelo_nulo, step_inadimplentes_balanceado)

comparacao_step

# 5. RESULTADOS

# 5.1. Matriz de confusão

matriz_step <- confusionMatrix(
  table(predict(step_inadimplentes_balanceado, type = "response") >= 0.5, 
        treino$inadimplente == 1)[2:1, 2:1]
)

matriz_step

df_matriz <- as.data.frame(matriz_step$table)

ggplot(df_matriz, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile()+
  geom_text(aes(label = Freq), colour = "black", size = 5)+
  scale_fill_gradient2(low = "lightblue", 
                       mid = "white", 
                       high = "red",
                       midpoint = 500) +
  labs(x = NULL, y = NULL) 

# 5.2. Gerando a curva ROC

predicoes <- prediction(predictions = step_inadimplentes_balanceado$fitted.values,
                        labels = treino$inadimplente)

dados_curva_roc <- performance(predicoes, measure = "sens")
sensitividade <- dados_curva_roc@y.values[[1]] 
especificidade <- performance(predicoes, measure = "spec") 
especificidade <- especificidade@y.values[[1]]
cutoffs <- dados_curva_roc@x.values[[1]] 

dados_plotagem <- cbind.data.frame(cutoffs, especificidade, sensitividade)

dados_plotagem %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F,
                font_size = 22)

ggplotly(dados_plotagem %>%
           ggplot(aes(x = cutoffs, y = especificidade)) +
           geom_line(aes(color = "Especificidade"),
                     size = 1) +
           geom_point(color = "#CC6666",
                      size = 1.9) +
           geom_line(aes(x = cutoffs, y = sensitividade, color = "Sensitividade"),
                     size = 1) +
           geom_point(aes(x = cutoffs, y = sensitividade),
                      color = "#66CC99",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#CC6666", "#66CC99")) +
           theme_bw())

ROC <- roc(response = treino$inadimplente, 
           predictor = step_inadimplentes_balanceado$fitted.values)

ggplotly(
  ggroc(ROC, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((ROC$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

# 5.3. Matriz de confusão com dados de teste para cutoffs 0.55, 0.75 e 0.25

teste1 <- confusionMatrix(table(predict(object = step_inadimplentes_balanceado, newdata = teste, type = "response") >= 0.55,teste$inadimplente==1))

teste2 <- confusionMatrix(table(predict(object = step_inadimplentes_balanceado, newdata = teste, type = "response") >= 0.75,teste$inadimplente==1))

teste3 <- confusionMatrix(table(predict(object = step_inadimplentes_balanceado, newdata = teste, type = "response") >= 0.25,teste$inadimplente==1))

df_teste1 <- as.data.frame(teste1$table)

plot_teste1 <- ggplot(df_teste1, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile()+
  geom_text(aes(label = Freq), colour = "black", size = 5)+
  scale_fill_gradient2(low = "lightblue", 
                       mid = "white", 
                       high = "red",
                       midpoint = 196) +
  labs(x = NULL, y = NULL, title = "Cutoff = 0.55")

df_teste2 <- as.data.frame(teste2$table)

plot_teste2 <- ggplot(df_teste2, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile()+
  geom_text(aes(label = Freq), colour = "black", size = 5)+
  scale_fill_gradient2(low = "lightblue", 
                       mid = "white", 
                       high = "red",
                       midpoint = 196) +
  labs(x = NULL, y = NULL, title = "Cutoff = 0.75")

df_teste3 <- as.data.frame(teste3$table)

plot_teste3 <- ggplot(df_teste3, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile()+
  geom_text(aes(label = Freq), colour = "black", size = 5)+
  scale_fill_gradient2(low = "lightblue", 
                       mid = "white", 
                       high = "red",
                       midpoint = 196) +
  labs(x = NULL, y = NULL, title = "Cutoff = 0.25")

ggarrange(plot_teste1, plot_teste2, plot_teste3,
          ncol = 3, nrow = 1)

teste1

teste2

teste3