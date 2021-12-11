setwd("C:/Users/Rodrigo/Documents/Audiovisual/AudiovisualMG")
rm(list = ls())

library("basedosdados")
library(DBI)
library(bigrquery)
library(ggplot2)
library(tidyverse)
library(Rcpp)
library(sf)
library(raster)
library(dplyr)
library(spData)
install.packages('spDataLarge',
                 repos='https://nowosad.github.io/drat/', type='source')
library(spDataLarge)
library(tmap)   
library(leaflet)
library(rgdal)
library(maptools)
library(ggridges)
library(viridis)
library(hrbrthemes)
#############CONEXÃO NO BIG QUERY - BASE DOS DADOS###################

# PASSO 1: criar usuário e projeto no BigQuery
# PASSO 2: criar arquivo de credenciais e salvar numa pasta
# https://console.cloud.google.com/apis/credentials/serviceaccountkey?project=<project_id>
# service account name: admin
# role: project owner
# Defina o seu projeto no Google Cloud

# PASSO 3: apontar a autenticação para o arquivo json

bq_auth(path = "C:/Users/Rodrigo/Documents/Audiovisual/AudiovisualMG/audiovisualmg-b48bffae933a.json")

# PASSO 4: criar conexão com o BigQuery

con <- dbConnect(
  bigrquery::bigquery(),
  billing = "audiovisualmg",
  project = "basedosdados"
)

set_billing_id("audiovisualmg")

#####################RAIS VÍNCULOS PARA MG#########################

queryMG_core <- "SELECT  
ano,
sigla_uf,
id_municipio,
tipo_vinculo,
faixa_tempo_emprego,
tempo_emprego,
quantidade_horas_contratadas,
faixa_horas_contratadas,
id_municipio_6_trabalho,
faixa_remuneracao_media_sm,
valor_remuneracao_media_sm,
valor_remuneracao_dezembro_nominal,
subsetor_ibge,
cbo_2002,
cnae_1,
cnae_2,
cnae_2_subclasse,
idade,
grau_instrucao_apos_2005,
grau_instrucao_1985_2005,
sexo,
raca_cor,
natureza_juridica,
tipo_vinculo
FROM `basedosdados.br_me_rais.microdados_vinculos` 

WHERE sigla_uf = 'MG'
AND ano > 2013
and (cnae_2_subclasse = '1830002' or
cnae_2_subclasse = '1830002' or
cnae_2_subclasse = '5911101' or
cnae_2_subclasse = '5911102' or
cnae_2_subclasse = '5911199' or
cnae_2_subclasse = '5912001' or
cnae_2_subclasse = '5912002' or
cnae_2_subclasse = '5912099' or
cnae_2_subclasse = '5913800' or
cnae_2_subclasse = '5914600' or
cnae_2_subclasse = '6203100')
"

raismg_core <- dbGetQuery(con, queryMG_core)

#raisbr <- dbGetQuery(con, querybr)

##############BASE COM CÓDIGOS DOS MUNCÍPIOS MG###########
cod_munic <- read.csv2("cod_munic.csv", encoding = "UTF-8")

cod_munic <- cod_munic[,c(1, 2,3,4,5,6,7,8)]

cod_municMG <- filter(cod_munic, X.U.FEFF.UF == 31)

cod_municMG <- cod_munic[,c(2,3,4,5,6,7,8)]

#############JUNTANDO AS BASES PARA INCLUIR NOME DOS MUNICS E REGIÕES##############
raismg_core <- merge(raismg_core, cod_municMG, by.x="id_municipio", by.y="Código.Município.Completo")

write.csv2(raismg_core, 'raismg_CORE.csv')
options(scipen = 1000000)

########ANÁLISES GERAIS DOS DADOS###############
glimpse(raismg_core)

summary(raismg_core)

raismg_core <- raismg_core%>%
  mutate(
    sexo = case_when(
      sexo == "1" ~ "Masculino",
      sexo == "2" ~ "Feminino"))

raismg_core$valor_remuneracao_media_sm = as.double(raismg_core$valor_remuneracao_media_sm)

raismg_core$ano = as.character(raismg_core$ano)


table(raismg_core$sexo, as.character(raismg_core$ano))

table(raismg_core$raca_cor, as.character(raismg_core$ano))

table(raismg_core$grau_instrucao_apos_2005, as.character(raismg_core$ano))

table(raismg_core$tipo_vinculo_1, as.character(raismg_core$ano))

table(raismg_core$cnae_2_subclasse, as.character(raismg_core$ano))

table(raismg_core$Nome_Mesorregião)

ggplot(raismg_core, mapping=aes(valor_remuneracao_dezembro_nominal, as.character(ano)), color=as.character(ano))+
  geom_boxplot()

ggplot(raismg_core, mapping=aes(valor_remuneracao_media_sm, as.character(ano)), color=as.character(ano))+
  geom_boxplot()

ggplot(raismg_core, mapping=aes(valor_remuneracao_dezembro_nominal))+
  geom_histogram()+          
  geom_histogram(color="black", fill="white")+
  geom_density(alpha=.2) 

ggplot(raismg_core, mapping=aes(x= valor_remuneracao_dezembro_nominal, fill = sexo, color=sexo))+
  geom_histogram(alpha=0.5, position="identity")

ggplot(filter(raismg_core, valor_remuneracao_dezembro_nominal < 50000), mapping=aes(x= valor_remuneracao_dezembro_nominal, fill = sexo, color=sexo))+
  geom_histogram(alpha=0.5, position="identity")

ggplot(filter(raismg_core, valor_remuneracao_dezembro_nominal < 30000), mapping=aes(x= valor_remuneracao_dezembro_nominal, fill = sexo, color=sexo))+
  geom_histogram(alpha=0.5, position="identity")

ggplot(raismg_core, aes(x=valor_remuneracao_dezembro_nominal))+
  geom_histogram(color="black", fill="white")+
  facet_grid(ano ~ .)

ggplot(raismg_core, 
       mapping=aes(x= idade, color=sexo, 
                   fill =sexo))+
  geom_histogram(position="identity", alpha=0.3)+
  theme_classic()+
  theme(legend.position="bottom")+
  labs(x = "Idade", y="")+
  scale_fill_grey()+
  scale_color_grey()+
  facet_wrap(~ano)

tapply(raismg_core$idade, raismg_core$ano, min)

tapply(raismg_core$idade, as.character(raismg_core$cnae_2_subclasse), mean)

tapply(raismg_core$idade, raismg_core$ano, mean)


mean(raismg_core$idade)

ggplot(raismg_core, 
       mapping=aes(x= idade, y = ano, color=ano, 
                   fill =ano))+
  geom_boxplot(position="identity", alpha=0.3)+
  theme_classic()+
  theme(legend.position="")+
  labs(x = "Idade", y="")+
  scale_fill_grey(start=0.0, end=0.6)+
  scale_color_grey(start=0.0, end=0.6)

ggplot(filter(raismg_core, ano == '2014' | ano == '2019'), 
       mapping=aes(x= idade, y = as.character(cnae_2_subclasse), 
                   color=as.character(cnae_2_subclasse), 
                   fill =as.character(cnae_2_subclasse)))+
  geom_boxplot(position="identity", alpha=0.3)+
  theme_classic()+
  theme(legend.position="")+
  labs(x = "Idade", y="")+
  scale_fill_grey(start=0.0, end=0.6)+
  scale_color_grey(start=0.0, end=0.6)+
  facet_wrap(~ano)


ggplot(filter(raismg_core, ano == '2014' | ano == '2019'), 
       mapping=aes(x= valor_remuneracao_dezembro_nominal, y = as.character(cnae_2_subclasse), 
                   color=as.character(cnae_2_subclasse), 
                   fill =as.character(cnae_2_subclasse)))+
  geom_boxplot(position="identity", alpha=0.3)+
  theme_classic()+
  theme(legend.position="")+
  labs(x = "Idade", y="")+
  scale_fill_grey(start=0.0, end=0.6)+
  scale_color_grey(start=0.0, end=0.6)+
  facet_wrap(~ano)

ggplot(filter(raismg_core, ano == '2014' | ano == '2019'), 
       mapping=aes(x= valor_remuneracao_dezembro_nominal, y = as.character(cnae_2_subclasse), 
                   color=as.character(cnae_2_subclasse), 
                   fill =as.character(cnae_2_subclasse)))+
  geom_violin(position="identity", alpha=0.3)+
  theme_classic()+
  theme(legend.position="")+
  labs(x = "Valor remuneração média em Dezembro", y="")+
  scale_fill_grey(start=0.0, end=0.6)+
  scale_color_grey(start=0.0, end=0.6)+
  facet_wrap(~ano)

ggplot(filter(raismg_core, ano == '2014' | ano == '2019'), 
       mapping=aes(x= valor_remuneracao_dezembro_nominal, y = sexo), 
       color=sexo, 
       fill =sexo)+
  geom_boxplot(position="identity", alpha=0.3)+
  theme_classic()+
  theme(legend.position="")+
  labs(x = "Valor remuneração média em Dezembro", y="")+
  scale_fill_grey(start=0.0, end=0.6)+
  scale_color_grey(start=0.0, end=0.6)+
  facet_wrap(~ano)

ggplot(filter(raismg_core, ano == '2014' | ano == '2019'), 
       mapping=aes(x= valor_remuneracao_media_sm, y = sexo,
                   fill=sexo))+
  geom_violin(position="identity", alpha=0.3)+
  theme_classic()+
  theme(legend.position="")+
  labs(x = "Valor remuneração média em SM", y="")+
  scale_fill_grey(start=0.2, end=0.8)+
  facet_wrap(~ano)

ggplot(filter(raismg_core, ano == '2014' | ano == '2019'), 
       mapping=aes(x= valor_remuneracao_dezembro_nominal, y = sexo,
                   fill=sexo))+
  geom_violin(position="identity", alpha=0.3)+
  theme_classic()+
  theme(legend.position="")+
  labs(x = "Valor remuneração média em nominal", y="")+
  scale_fill_grey(start=0.2, end=0.8)+
  facet_wrap(~ano)


ggplot(filter(raismg_core, ano == '2014' | ano == '2019', valor_remuneracao_media_sm < 20), 
       mapping=aes(x= valor_remuneracao_media_sm, y = cnae_2_subclasse, fill = cnae_2_subclasse))+
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")+
  scale_fill_grey(start=0.2, end=0.8)+
  facet_wrap(~ano)

tapply(raismg_core$valor_remuneracao_dezembro_nominal, raismg_core$ano, mean)

tapply(raismg_core$valor_remuneracao_media_sm, raismg_core$ano, mean)

raismg_core%>%
  group_by(ano, sexo)%>%
  summarise(mean_remun  = mean(valor_remuneracao_dezembro_nominal), mean_sm = mean(valor_remuneracao_media_sm))

ggplot(filter(raismg_core, ano == '2014' | ano == '2019'), 
       mapping=aes(x= valor_remuneracao_media_sm, y = as.character(cnae_2_subclasse), 
                   color=as.character(cnae_2_subclasse), 
                   fill =as.character(cnae_2_subclasse)))+
  geom_violin(position="identity", alpha=0.3)+
  theme_classic()+
  theme(legend.position="")+
  labs(x = "Valor remuneração média em SM", y="")+
  scale_fill_grey(start=0.0, end=0.6)+
  scale_color_grey(start=0.0, end=0.6)+
  facet_wrap(~ano)

ggplot(filter(raismg_core, ano == '2014'), 
       mapping=aes(x= valor_remuneracao_media_sm, y = as.character(cnae_2_subclasse), 
                   color=as.character(cnae_2_subclasse), 
                   fill =as.character(cnae_2_subclasse)))+
  geom_boxplot(position="identity", alpha=0.3)+
  theme_classic()+
  theme(legend.position="")+
  labs(x = "Valor remuneração média em SM", y="")+
  scale_fill_grey(start=0.0, end=0.6)+
  scale_color_grey(start=0.0, end=0.6)+
  facet_wrap(~ano)



ggplot(raismg_core, 
       mapping=aes(x= idade, y = ano, color=ano, 
                   fill =ano))+
  geom_violin(position="identity", alpha=0.3)+
  theme_classic()+
  theme(legend.position="")+
  labs(x = "Idade", y="")+
  scale_fill_grey()+
  scale_color_grey()

ggplot(raismg_core, mapping=aes(x= quantidade_horas_contratadas, fill = sexo, color=sexo))+
  geom_histogram(alpha=0.5, position="identity")

ggplot(raismg_core, mapping=aes(x= idade, as.character(sexo), color =as.character(sexo), fill =as.character(sexo)))+
  geom_boxplot(position="identity", alpha=0.3)+
  theme_minimal()+theme(legend.position="right")


ggplot(raismg_core, mapping=aes(x= valor_remuneracao_media_sm, as.character(ano), color =as.character(ano), fill =as.character(ano)))+
  geom_violin(position="identity", alpha=0.3)+
  theme_minimal()+theme(legend.position="right")


ggplot(raismg_core, mapping=aes(x= valor_remuneracao_media_sm, as.character(ano), color =as.character(ano), fill =as.character(ano)))+
  geom_violin(position="identity", alpha=0.3)+
  theme_minimal()+theme(legend.position="right")+
  facet_wrap(~sexo)

ggplot(raismg_core, mapping=aes(x= valor_remuneracao_media_sm, as.character(raca_cor), color =as.character(raca_cor), fill =as.character(raca_cor)))+
  geom_violin(position="identity", alpha=0.3)+
  theme_minimal()+theme(legend.position="right")+
  facet_wrap(~sexo)

table(raismg_core$raca_cor, raismg_core$sexo)

tabela1 <- table(raismg_core$cnae_2_subclasse, raismg_core$ano)

plot(tabela1)


##################RAIS ESTABELECIMENTOS PARA MG###################


query_estabmg <- "SELECT  
ano,
id_municipio,
qtde_vinculos_ativos,
qtde_vinculos_clt,
natureza_estabelecimento,
natureza_juridica,
tamanho_estabelecimento,
indicador_cei_vinculado,
cnae_2_subclasse
FROM `basedosdados.br_me_rais.microdados_estabelecimentos`

WHERE sigla_uf = 'MG'
AND ano > 2013
and (cnae_2_subclasse = '1830002' or
cnae_2_subclasse = '1830002' or
cnae_2_subclasse = '5911101' or
cnae_2_subclasse = '5911102' or
cnae_2_subclasse = '5911199' or
cnae_2_subclasse = '5912001' or
cnae_2_subclasse = '5912002' or
cnae_2_subclasse = '5912099' or
cnae_2_subclasse = '5913800' or
cnae_2_subclasse = '5914600' or
cnae_2_subclasse = '5920100' or
cnae_2_subclasse = '6010100' or
cnae_2_subclasse = '6141800' or
cnae_2_subclasse = '6143400' or
cnae_2_subclasse = '6319400' or
cnae_2_subclasse = '7420001' or
cnae_2_subclasse = '7420004' or
cnae_2_subclasse = '9001906' or
cnae_2_subclasse = '6203100')"

estabMG <- dbGetQuery(con, query_estabmg)

glimpse(estabMG)

write.csv2(estabMG, 'rais_estab.csv')
estabMG <- read.csv2('rais_estab.csv')

table(estabMG$indicador_cei_vinculado, estabMG$ano)

table(estabMG$ano, estabMG$natureza_juridica)

table(estabMG$cnae_2_subclasse, estabMG$ano)

table(estabMG$cnae_2_subclasse, estabMG$qtde_vinculos_ativos)

######################### ANÁLISE ESPACIAL ########################################


rais_munic_core <- raismg_core[,c(1, 2, 11, 12, 17, 25, 26, 27, 28, 29, 30)]

rais_meso_core <- raismg_core[,c(1, 2, 11, 12, 17, 25, 26, 27, 28, 29, 30)]

rais_munic_core <- rais_munic_core%>%
  group_by(id_municipio, ano)%>%
  summarise(n=n())
write.csv2(rais_munic_core, 'rais_munic_core.csv')

rais_meso <- rais_meso%>%
  group_by(Nome_Mesorregião, ano)%>%
  summarise(n=n())
write.csv2(rais_meso, 'rais_meso.csv')

breaks <- c(0, 5, 50, 500, 5000, 10000)  

mapamg = shapefile("Shapes/MG_Municipios_2020.shp")
mapa_meso <- shapefile("Shapes/MG_Mesorregioes_2020.shp")
############2019##############

#####MUNIC#######
rais_munic19_core <- filter(raismg_core, ano == '2019')

rais_munic19_core <- rais_munic19_core%>%
  group_by(id_municipio)%>%
  summarise(trab = n(),
            remun = mean(valor_remuneracao_dezembro_nominal),
            massa_salarial = trab*remun)

names(rais_munic19_core)[1]="CD_MUN"

rais_munic19_core <- merge(mapamg,rais_munic19_core,by="CD_MUN", all.x=T)
names(rais_munic19_core)

rais_munic19_core[is.na(rais_munic19_core$trab)] <- 0
rais_munic19_core[is.na(rais_munic19_core$remun)] <- 0

breaks_core <-  c(0, 1, 10, 100, 1000, Inf)

tm_shape(rais_munic19_core)+
  tm_fill("trab", breaks = breaks_core, palette= "Greys", title = "Vínculos-2019")+
  tm_legend(position=c("left","bottom"))

breaks_salario = c(0, 1000, 10000, 100000, 1000000, 2000000, Inf)

tm_shape(rais_munic19_core)+
  tm_fill("massa_salarial",breaks = breaks_salario, palette= "Greys", title = "Massa Salarial-2019")+
  tm_legend(position=c("left","bottom"))

#########MESO#####

rais_meso19_core <- filter(raismg_core, ano == '2019')

rais_meso19_core <- rais_meso19_core%>%
  group_by(Mesorregião.Geográfica)%>%
  summarise(trab = n(),
            remun = mean(valor_remuneracao_dezembro_nominal),
            massa_salarial = trab*remun)

rais_meso19_core <- rais_meso19_core%>%
  mutate(
    Mesorregião.Geográfica = case_when(
      Mesorregião.Geográfica == "1" ~ "3101",
      Mesorregião.Geográfica == "2" ~ "3102",
      Mesorregião.Geográfica == "3" ~ "3103",
      Mesorregião.Geográfica == "4" ~ "3104", 
      Mesorregião.Geográfica == "5" ~ "3105",
      Mesorregião.Geográfica == "6" ~ "3106", 
      Mesorregião.Geográfica == "7" ~ "3107", 
      Mesorregião.Geográfica == "8" ~ "3108", 
      Mesorregião.Geográfica == "9" ~ "3109", 
      Mesorregião.Geográfica == "10" ~"3110",
      Mesorregião.Geográfica == "11" ~"3111",
      Mesorregião.Geográfica == "12" ~"3112",))

names(rais_meso19_core)[1]="CD_MESO"

rais_meso19_core <- merge(mapa_meso,rais_meso19_core, by="CD_MESO", all.x=T)
names(rais_meso19_core)

rais_meso19_core[is.na(rais_meso19_core$trab)] <- 0
rais_meso19_core[is.na(rais_meso19_core$remun)] <- 0

table(rais_meso19_core$CD_MESO, rais_meso19_core$trab)
table(mapa_meso$NM_MESO)

table(rais_meso19_core$CD_MESO, rais_meso19_core$massa_salarial)


breaks_meso_core <- c(0, 100, 1000, 5000, 10000)

breaks_salario_meso_core <- c(0, 100000, 500000, 1000000, 10000000, 50000000)


tm_shape(rais_meso19_core)+
  tm_fill("trab", breaks = breaks_meso, palette= "Greys", title = "Vínculos-2019")+
  tm_legend(position=c("left","bottom"))

tm_shape(rais_meso19_core)+
  tm_fill("massa_salarial",breaks = breaks_salario_meso, palette= "Greys", title = "Massa Salarial-2019")+
  tm_legend(position=c("left","bottom"))


####################2014##################

rais_munic14_core <- filter(raismg_core, ano == '2014')

rais_munic14_core <- rais_munic14_core%>%
  group_by(id_municipio)%>%
  summarise(trab = n(),
            remun = mean(valor_remuneracao_dezembro_nominal),
            massa_salarial = trab*remun)
names(rais_munic14_core)[1]="CD_MUN"

rais_munic14_core <- merge(mapamg,rais_munic14_core, by="CD_MUN", all.x=T)
names(rais_munic14_core)

rais_munic14_core[is.na(rais_munic14_core$trab)] <- 0
rais_munic14_core[is.na(rais_munic14_core$remun)] <- 0


tm_shape(rais_munic14_core)+
  tm_fill("trab", breaks = breaks_core, palette= "Greys", title = "Vínculos-2014")+
  tm_legend(position=c("left","bottom"))

tm_shape(rais_munic14_core)+
  tm_fill("massa_salarial",breaks = breaks_salario, palette= "Greys", title = "Massa Salarial-2014")+
  tm_legend(position=c("left","bottom"))

#########MESO#####
rais_meso14_core <- filter(raismg_core, ano == '2014')

rais_meso14_core <- rais_meso14_core%>%
  group_by(Mesorregião.Geográfica)%>%
  summarise(trab = n(),
            remun = mean(valor_remuneracao_dezembro_nominal),
            massa_salarial = trab*remun)

rais_meso14_core <- rais_meso14_core%>%
  mutate(
    Mesorregião.Geográfica = case_when(
      Mesorregião.Geográfica == "1" ~ "3101",
      Mesorregião.Geográfica == "2" ~ "3102",
      Mesorregião.Geográfica == "3" ~ "3103",
      Mesorregião.Geográfica == "4" ~ "3104", 
      Mesorregião.Geográfica == "5" ~ "3105",
      Mesorregião.Geográfica == "6" ~ "3106", 
      Mesorregião.Geográfica == "7" ~ "3107", 
      Mesorregião.Geográfica == "8" ~ "3108", 
      Mesorregião.Geográfica == "9" ~ "3109", 
      Mesorregião.Geográfica == "10" ~"3110",
      Mesorregião.Geográfica == "11" ~"3111",
      Mesorregião.Geográfica == "12" ~"3112",))

names(rais_meso14_core)[1]="CD_MESO"

rais_meso14_core <- merge(mapa_meso,rais_meso14_core, by="CD_MESO", all.x=T)
names(rais_meso14_core)

rais_meso14_core[is.na(rais_meso19_core$trab)] <- 0
rais_meso14_core[is.na(rais_meso19_core$remun)] <- 0

table(rais_meso14_core$CD_MESO, rais_meso14_core$trab)
table(mapa_meso_core$NM_MESO)

table(rais_meso14_core$CD_MESO, rais_meso14_core$massa_salarial)


breaks_meso <- c(0, 100, 500, 2000, 5000)

breaks_salario_meso <- c(0, 100000, 500000, 1000000, 10000000, 50000000)


tm_shape(rais_meso14_core)+
  tm_fill("trab", breaks = breaks_meso, palette= "Greys", title = "Vínculos-2014")+
  tm_legend(position=c("left","bottom"))
  

tm_shape(rais_meso14_core)+
  tm_fill("massa_salarial",breaks = breaks_salario_meso, palette= "Greys", title = "Massa Salarial-2014")+
  tm_legend(position=c("left","bottom"))


################ESTABELECIMENTOS#############

estab_munic <- estabMG[,c(1, 2, 3, 7, 9)]
estab_munic14 <- filter(estab_munic, ano =='2014')
estab_munic <- estab_munic%>%
  group_by(id_municipio, ano)%>%
  summarise(n=n())

breaks_estab <- c(0, 1, 5, 50, 500, 1000)  


############2019##############

estab_munic19 <- filter(estabMG, ano == '2019')

estab_munic19 <- estab_munic19%>%
  group_by(id_municipio)%>%
  summarise(estab = n(),
            vinculos = sum(qtde_vinculos_ativos))

names(estab_munic19)[1]="CD_MUN"

estab_munic19 <- merge(mapamg,estab_munic19,by="CD_MUN", all.x=T)
names(estab_munic19)

estab_munic19[is.na(estab_munic19$estab)] <- 0
estab_munic19[is.na(estab_munic19$vinculos)] <- 0


tm_shape(estab_munic19)+
  tm_fill("estab", breaks = breaks_estab, palette= "Greys", title = "Estabelecimentos-2019")+
  tm_legend(position=c("left","bottom"))

############2014##############

estab_munic14 <- filter(estabMG, ano == '2014')

estab_munic14 <- estab_munic14%>%
  group_by(id_municipio)%>%
  summarise(estab = n(),
            vinculos = sum(qtde_vinculos_ativos))

names(estab_munic14)[1]="CD_MUN"

estab_munic14 <- merge(mapamg,estab_munic14,by="CD_MUN", all.x=T)
names(estab_munic14)

estab_munic19[is.na(estab_munic14$estab)] <- 0
estab_munic14[is.na(estab_munic14$vinculos)] <- 0


tm_shape(estab_munic14)+
  tm_fill("estab", breaks = breaks_estab, palette= "Greys", title = "Estabelecimentos-2014")+
  tm_legend(position=c("left","bottom"))

######################QL####################################
mapamunic = shapefile("Shapes/MG_Municipios_2020.shp")
mapameso <- shapefile("Shapes/MG_Mesorregioes_2020.shp")

names(QL1)[1]="CD_MUN"

QL2019 <- QL1[,c(1,2,3, 17, 23)]

QLmunic <- merge(mapamunic,QL2019,by="CD_MUN", all.x=T)

names(QLmunic)

QLmunic[is.na(QLmunic$`2019QL`)] <- 0
QLmunic[is.na(QLmunic$`2019QLg`)] <- 0

breaksQL <- c(0, 1, 4, Inf)  

tm_shape(QLmunic)+
  tm_fill("2019QL", breaks=breaksQL, palette= "Greys", title = "Quociente Locacional 2019")+
  tm_legend(position=c("left","bottom"))

QL2014 <- QL1[,c(1,2,3, 22, 28)]

QLmunic14 <- merge(mapamunic,QL2014,by="CD_MUN", all.x=T)

names(QLmunic14)

QLmunic14[is.na(QLmunic14$`2014QL`)] <- 0
QLmunic14[is.na(QLmunic14$`2014QLg`)] <- 0

breaksQL <- c(0, 1, 4, Inf)  

tm_shape(QLmunic14)+
  tm_fill("2014QL", breaks=breaksQL, palette= "Greys", title = "Quociente Locacional 2014")+
  tm_legend(position=c("left","bottom"))

#############################DADOS DA FAZENDA#############################3

####################MESO#############################################3
codigo_sef <- read.csv2('codigo_SEF_munic.csv')

names(codigo_sef)[3] <- 'cod_serpro'

base_receita <- read.csv2('base_receita.csv')
names(base_receita)[2] <- 'cnae'
base_receita_mg <- filter(base_receita, UF == "MG")

names(base_receita_mg)[5] <- 'cod_serpro'

base_receita_mg_av <- filter(base_receita_mg, 
                             cnae == '1830002' |
                             cnae == '5911101' |
                             cnae == '5911102' |
                             cnae == '5911199' |
                             cnae == '5912001' |
                             cnae == '5912002' |
                             cnae == '5912099' |
                             cnae == '5913800' |
                             cnae == '5914600' |
                             cnae == '6203100' )

base_receita_mg_av <- merge(base_receita_mg_av, codigo_sef, by="cod_serpro")

tapply(base_receita_mg_av$Capital.Social, base_receita_mg_av$Nome_MesorregiÃ.o, mean)

table(base_receita_mg_av$Nome_MesorregiÃ.o)

tapply(base_receita_mg_av$Capital.Social, base_receita_mg_av$Nome_MesorregiÃ.o, sum)


base_receita_mg_av_meso <- base_receita_mg_av%>%
  group_by(MesorregiÃ.o.GeogrÃ.fica)%>%
  summarise(empresas = n(),
            capital_social_medio = mean(Capital.Social),
            capital_social_total = sum(Capital.Social))

names(base_receita_mg_av_meso)[1] <- 'Mesorregião.Geográfica'


base_receita_mg_av_meso <- base_receita_mg_av_meso%>%
  mutate(
    Mesorregião.Geográfica = case_when(
      Mesorregião.Geográfica == "1" ~ "3101",
      Mesorregião.Geográfica == "2" ~ "3102",
      Mesorregião.Geográfica == "3" ~ "3103",
      Mesorregião.Geográfica == "4" ~ "3104", 
      Mesorregião.Geográfica == "5" ~ "3105",
      Mesorregião.Geográfica == "6" ~ "3106", 
      Mesorregião.Geográfica == "7" ~ "3107", 
      Mesorregião.Geográfica == "8" ~ "3108", 
      Mesorregião.Geográfica == "9" ~ "3109", 
      Mesorregião.Geográfica == "10" ~"3110",
      Mesorregião.Geográfica == "11" ~"3111",
      Mesorregião.Geográfica == "12" ~"3112",))

names(base_receita_mg_av_meso)[1]="CD_MESO"

base_receita_mg_av_meso_mapa <- merge(mapa_meso,base_receita_mg_av_meso, by="CD_MESO", all.x=T)
names(rais_meso14_core)

base_receita_mg_av_meso_mapa[is.na(base_receita_mg_av_meso_mapa$empresas)] <- 0
base_receita_mg_av_meso_mapa[is.na(base_receita_mg_av_meso_mapa$capital_social_medio)] <- 0
base_receita_mg_av_meso_mapa[is.na(base_receita_mg_av_meso_mapa$capital_social_total)] <- 0

class(base_receita_mg_av_meso_mapa$capital_social_total)


breaks_meso_empresas <- c(0, 50, 100, 500, 1000, Inf)

breaks_csm_meso <- c(0, 10000, 100000, 1000000, 2000000, Inf)

breaks_cst_meso <- c(-Inf, 300000, 100000000, Inf)


tm_shape(base_receita_mg_av_meso_mapa)+
  tm_fill("empresas", breaks = breaks_meso_empresas, palette= "Greys", title = "Empresas Audiovisual")+
  tm_legend(position=c("left","bottom"))


tm_shape(base_receita_mg_av_meso_mapa)+
  tm_fill("capital_social_medio", breaks=breaks_csm_meso, palette= "Greys", title = "Capital Social Médio")+
  tm_legend(position=c("left","bottom"))


tm_shape(base_receita_mg_av_meso_mapa)+
  tm_fill("capital_social_total", palette= "Greys", title = "Capital Social Total")+
  tm_legend(position=c("left","bottom"))

########################################mnunic###########################3

base_receita_mg_av_munic <- base_receita_mg_av%>%
  group_by(base_receita_mg_av$`CÃ³digo.MunicÃ.pio.Completo`)%>%
  summarise(empresas = n(),
            capital_social_medio = mean(Capital.Social),
            capital_social_total = sum(Capital.Social))

names(base_receita_mg_av_munic)[1] <- 'CD_MUN'



base_receita_mg_av_munic_mapa <- merge(mapamg,base_receita_mg_av_munic, by="CD_MUN", all.x=T)


base_receita_mg_av_munic_mapa[is.na(base_receita_mg_av_munic_mapa$empresas)] <- 0
base_receita_mg_av_munic_mapa[is.na(base_receita_mg_av_munic_mapa$capital_social_medio)] <- 0
base_receita_mg_av_munic_mapa[is.na(base_receita_mg_av_munic_mapa$capital_social_total)] <- 0

class(base_receita_mg_av_munic_mapa$capital_social_total)


breaks_munic_empresas <- c(0, 5, 10, 100, 500, 1000, Inf)

breaks_csm_munic <- c(0, 1000, 10000, 100000, 200000, Inf)

breaks_cst_munic <- c(0, 10000, 100000, 1000000, 10000000, Inf)


tm_shape(base_receita_mg_av_munic_mapa)+
  tm_fill("empresas", breaks = breaks_munic_empresas, palette= "Greys", title = "Empresas Audiovisual")+
  tm_legend(position=c("left","bottom"))


tm_shape(base_receita_mg_av_munic_mapa)+
  tm_fill("capital_social_medio", breaks=breaks_csm_munic, palette= "Greys", title = "Capital Social Médio")+
  tm_legend(position=c("left","bottom"))


tm_shape(base_receita_mg_av_munic_mapa)+
  tm_fill("capital_social_total",breaks=breaks_cst_munic,  palette= "Greys", title = "Capital Social Total")+
  tm_legend(position=c("left","bottom"))


write.csv2(rais_munic, 'rais_minc.csv')

write.csv2(base_receita_mg_av_munic, 'base_receita_munic.csv')


table(base_receita_mg_av$Nome_MunicÃ.pio, base_receita_mg_av$cnae)


write.csv2(base_receita_mg_av, 'base_receita_mg_av.csv')
###########################EMPRESAS RAIS##############33

query_estabmg <- "SELECT  
ano,
id_municipio,
natureza_juridica,
indicador_cei_vinculado,
cnae_2_subclasse
FROM `basedosdados.br_me_rais.microdados_estabelecimentos`

WHERE sigla_uf = 'MG'
AND ano > 2013
and (cnae_2_subclasse = '1830002' or
cnae_2_subclasse = '1830002' or
cnae_2_subclasse = '5911101' or
cnae_2_subclasse = '5911102' or
cnae_2_subclasse = '5911199' or
cnae_2_subclasse = '5912001' or
cnae_2_subclasse = '5912002' or
cnae_2_subclasse = '5912099' or
cnae_2_subclasse = '5913800' or
cnae_2_subclasse = '5914600' or
cnae_2_subclasse = '6203100')"

estabMG <- dbGetQuery(con, query_estabmg)

table(estabMG$ano)

mean(base_receita_mg_av_munic$capital_social_medio)
