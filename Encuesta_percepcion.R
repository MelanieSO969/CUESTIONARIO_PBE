
#Librerias

library(readr)
library(dplyr) #mutate #case_when
library(readxl)
library(writexl)
library(tidyr) #unite 
library(maditr) #dcast
library(DescTools) #CramerV
library("car") #Para la funcion recode
library(ggplot2)
library(gridExtra)

library("factoextra")
library("FactoClass")
library("ade4")
library("FactoMineR")


#Carga de base de datos
setwd("C:/Users/usuario/Downloads")
base_enc<- read_xlsx("Base para estadistica.xlsx",sheet = "DEFINITIVA ORGANIZADA (2)")
attach(base_enc)

##### Categorización ####
#Resultados de las dimensiones
#escenario 1
base_enc <- base_enc %>%
  mutate(
    Cat_Dim1 = case_when(
      base_enc$`1. PUNTAJE CREENCIAS Y ACTITUDES` <= 34                          ~ "Low score",
      dplyr::between(base_enc$`1. PUNTAJE CREENCIAS Y ACTITUDES`,left = 35,right = 55)  ~ "Medium-Low score",
      dplyr::between(base_enc$`1. PUNTAJE CREENCIAS Y ACTITUDES`,left = 56,right = 77)  ~ "Medium score",
      dplyr::between(base_enc$`1. PUNTAJE CREENCIAS Y ACTITUDES`,left = 78,right = 98)  ~ "Medium-High score",
      dplyr::between(base_enc$`1. PUNTAJE CREENCIAS Y ACTITUDES`,left = 99,right = 120) ~ "High score"
    )
  )

base_enc <- base_enc %>%
  mutate(
    Cat_Dim2 = case_when(
      base_enc$`2. PUNTAJE RESULTADOS PROVENIENTES DE LA INVESTIGACION CIENTIFICA` <= 39                           ~ "Low score",
      dplyr::between(base_enc$`2. PUNTAJE RESULTADOS PROVENIENTES DE LA INVESTIGACION CIENTIFICA`,left = 40,right = 64)   ~ "Medium-Low score",
      dplyr::between(base_enc$`2. PUNTAJE RESULTADOS PROVENIENTES DE LA INVESTIGACION CIENTIFICA`,left = 65,right = 90)   ~ "Medium score",
      dplyr::between(base_enc$`2. PUNTAJE RESULTADOS PROVENIENTES DE LA INVESTIGACION CIENTIFICA`,left = 91,right = 115)  ~ "Medium-High score",
      dplyr::between(base_enc$`2. PUNTAJE RESULTADOS PROVENIENTES DE LA INVESTIGACION CIENTIFICA`,left = 116,right = 140) ~ "High score"
    )
  )

base_enc <- base_enc %>%
  mutate(
    Cat_Dim3 = case_when(
      base_enc$`3. PUNTAJE DESARROLLO DE LA PRÁCTICA PROFESIONAL` <= 28                           ~ "Low score",
      dplyr::between(base_enc$`3. PUNTAJE DESARROLLO DE LA PRÁCTICA PROFESIONAL`,left = 29,right = 46)   ~ "Medium-Low score",
      dplyr::between(base_enc$`3. PUNTAJE DESARROLLO DE LA PRÁCTICA PROFESIONAL`,left = 47,right = 64)   ~ "Medium score",
      dplyr::between(base_enc$`3. PUNTAJE DESARROLLO DE LA PRÁCTICA PROFESIONAL`,left = 65,right = 82)   ~ "Medium-High score",
      dplyr::between(base_enc$`3. PUNTAJE DESARROLLO DE LA PRÁCTICA PROFESIONAL`,left = 83,right = 100)  ~ "High score"
    )
  )

base_enc <- base_enc %>%
  mutate(
    Cat_Dim4 = case_when(
      base_enc$`4. PUNTAJE EVALUACIÓN DE RESULTADOS` <= 34                          ~ "Low score",
      dplyr::between(base_enc$`4. PUNTAJE EVALUACIÓN DE RESULTADOS`,left = 35,right = 55)  ~ "Medium-Low score",
      dplyr::between(base_enc$`4. PUNTAJE EVALUACIÓN DE RESULTADOS`,left = 56,right = 77)  ~ "Medium score",
      dplyr::between(base_enc$`4. PUNTAJE EVALUACIÓN DE RESULTADOS`,left = 78,right = 98)  ~ "Medium-High score",
      dplyr::between(base_enc$`4. PUNTAJE EVALUACIÓN DE RESULTADOS`,left = 99,right = 120) ~ "High score"
    )
  )

base_enc <- base_enc %>%
  mutate(
    Cat_Dim5 = case_when(
      base_enc$`5. PUNTAJE BARRERAS Y FACILITADORES` <= 34                          ~ "Low score",
      dplyr::between(base_enc$`5. PUNTAJE BARRERAS Y FACILITADORES`,left = 35,right = 55)  ~ "Medium-Low score",
      dplyr::between(base_enc$`5. PUNTAJE BARRERAS Y FACILITADORES`,left = 56,right = 77)  ~ "Medium score",
      dplyr::between(base_enc$`5. PUNTAJE BARRERAS Y FACILITADORES`,left = 78,right = 98)  ~ "Medium-High score",
      dplyr::between(base_enc$`5. PUNTAJE BARRERAS Y FACILITADORES`,left = 99,right = 120) ~ "High score"
    )
  )





# #escenario 5
# base_enc <- base_enc %>%
#   mutate(
#     Cat_Dim1_1 = case_when(
#       base_enc$`1. PUNTAJE CREENCIAS Y ACTITUDES` <= 48                                 ~ "Low score",
#       dplyr::between(base_enc$`1. PUNTAJE CREENCIAS Y ACTITUDES`,left = 49,right = 84)  ~ "Medium score",
#       dplyr::between(base_enc$`1. PUNTAJE CREENCIAS Y ACTITUDES`,left = 85,right = 120) ~ "High score"
#     )
#   )
# 
# base_enc <- base_enc %>%
#   mutate(
#     Cat_Dim2_1 = case_when(
#       base_enc$`2. PUNTAJE RESULTADOS PROVENIENTES DE LA INVESTIGACION CIENTIFICA` <= 56                                  ~ "Low score",
#       dplyr::between(base_enc$`2. PUNTAJE RESULTADOS PROVENIENTES DE LA INVESTIGACION CIENTIFICA`,left = 57,right = 98)   ~ "Medium score",
#       dplyr::between(base_enc$`2. PUNTAJE RESULTADOS PROVENIENTES DE LA INVESTIGACION CIENTIFICA`,left = 99,right = 140) ~ "High score"
#     )
#   )
# 
# base_enc <- base_enc %>%
#   mutate(
#     Cat_Dim3_1 = case_when(
#       base_enc$`3. PUNTAJE DESARROLLO DE LA PRÁCTICA PROFESIONAL` <= 40                                  ~ "Low score",
#       dplyr::between(base_enc$`3. PUNTAJE DESARROLLO DE LA PRÁCTICA PROFESIONAL`,left = 41,right = 70)   ~ "Medium score",
#       dplyr::between(base_enc$`3. PUNTAJE DESARROLLO DE LA PRÁCTICA PROFESIONAL`,left = 71,right = 100)  ~ "High score"
#     )
#   )
# 
# base_enc <- base_enc %>%
#   mutate(
#     Cat_Dim4_1 = case_when(
#       base_enc$`4. PUNTAJE EVALUACIÓN DE RESULTADOS` <= 48                                 ~ "Low score",
#       dplyr::between(base_enc$`4. PUNTAJE EVALUACIÓN DE RESULTADOS`,left = 49,right = 84)  ~ "Medium score",
#       dplyr::between(base_enc$`4. PUNTAJE EVALUACIÓN DE RESULTADOS`,left = 85,right = 120) ~ "High score"
#     )
#   )
# 
# base_enc <- base_enc %>%
#   mutate(
#     Cat_Dim5_1 = case_when(
#       base_enc$`5. PUNTAJE BARRERAS Y FACILITADORES` <= 48                                 ~ "Low score",
#       dplyr::between(base_enc$`5. PUNTAJE BARRERAS Y FACILITADORES`,left = 49,right = 84)  ~ "Medium score",
#       dplyr::between(base_enc$`5. PUNTAJE BARRERAS Y FACILITADORES`,left = 85,right = 120) ~ "High score"
#     )
#   )

#Edad
edad_c<-cut(base_enc$Edad, breaks = c(13,26,59,100))
edad_c<-recode(edad_c," '(13,26]'='Joven' ;'(26,59]'='Adulto'; '(59,100]'='Persona Mayor' ")
       
base_enc<-cbind(edad_c,base_enc)
        
######## Analisis exploratorio #########

#Tablas
dcast(base_enc,base_enc$`Pais de residencia`~base_enc$Cat_Dim1)

#Suplementaria
#Evidencia asociación estadistica entre los resultados de las dimensiones y el pais
#Prueba chi-cuadrados - $expected < 5 entonces se realiza prueba de fisher
#alpha=0.05

# chisq.test(base_enc$Cat_Dim1,base_enc$`Pais de residencia`,simulate.p.value=T) #0.2919 
# chisq.test(base_enc$Cat_Dim2,base_enc$`Pais de residencia`,simulate.p.value=T) #0.4678
# chisq.test(base_enc$Cat_Dim3,base_enc$`Pais de residencia`,simulate.p.value=T) #0.3813
# chisq.test(base_enc$Cat_Dim4,base_enc$`Pais de residencia`,simulate.p.value=T) #0.2759
# chisq.test(base_enc$Cat_Dim5,base_enc$`Pais de residencia`,simulate.p.value=T) #0.1759

fisher.test(base_enc$Cat_Dim1,base_enc$`Pais de residencia`,alternative = "two.sided",simulate.p.value=TRUE) #0.02199*
fisher.test(base_enc$Cat_Dim2,base_enc$`Pais de residencia`,alternative = "two.sided",simulate.p.value=TRUE) #0.2234
fisher.test(base_enc$Cat_Dim3,base_enc$`Pais de residencia`,alternative = "two.sided",simulate.p.value=TRUE) #0.1324
fisher.test(base_enc$Cat_Dim4,base_enc$`Pais de residencia`,alternative = "two.sided",simulate.p.value=TRUE) #0.04648*
fisher.test(base_enc$Cat_Dim5,base_enc$`Pais de residencia`,alternative = "two.sided",simulate.p.value=TRUE) #0.1104

#V de Cramer - debil si <=0.2 - moderados 0.2<VC<=0.6 * - fuerte si >=0.6 **   
#Este método es adecuado para muestras grandes y se utiliza para evaluar la relación entre variables categóricas con tres o más niveles.
#Se da prioridad al test de Fisher
#Test de fisher - existe o no asociación / V de Cramer - fuerza de asociacion

CramerV(base_enc$Cat_Dim1,base_enc$`Pais de residencia`) #0.1949362
CramerV(base_enc$Cat_Dim2,base_enc$`Pais de residencia`) #0.1857666
CramerV(base_enc$Cat_Dim3,base_enc$`Pais de residencia`) #0.185979
CramerV(base_enc$Cat_Dim4,base_enc$`Pais de residencia`) #0.2069946*
CramerV(base_enc$Cat_Dim5,base_enc$`Pais de residencia`) #0.2141415*

#Se incluye por estudios previos
#Evidencia asociación estadistica entre los resultados de las dimensiones y la titulacion
fisher.test(base_enc$Cat_Dim1,base_enc$`Titulación de mayor nivel académico`,alternative = "two.sided",simulate.p.value=TRUE) #0.1434
fisher.test(base_enc$Cat_Dim2,base_enc$`Titulación de mayor nivel académico`,alternative = "two.sided",simulate.p.value=TRUE) #0.0004998*
fisher.test(base_enc$Cat_Dim3,base_enc$`Titulación de mayor nivel académico`,alternative = "two.sided",simulate.p.value=TRUE) #0.2814
fisher.test(base_enc$Cat_Dim4,base_enc$`Titulación de mayor nivel académico`,alternative = "two.sided",simulate.p.value=TRUE) #0.004498*
fisher.test(base_enc$Cat_Dim5,base_enc$`Titulación de mayor nivel académico`,alternative = "two.sided",simulate.p.value=TRUE) #0.001499*

CramerV(base_enc$Cat_Dim1,base_enc$`Titulación de mayor nivel académico`) #0.1012846
CramerV(base_enc$Cat_Dim2,base_enc$`Titulación de mayor nivel académico`) #0.2181431*
CramerV(base_enc$Cat_Dim3,base_enc$`Titulación de mayor nivel académico`) #0.08559809
CramerV(base_enc$Cat_Dim4,base_enc$`Titulación de mayor nivel académico`) #0.1451924
CramerV(base_enc$Cat_Dim5,base_enc$`Titulación de mayor nivel académico`) #0.1520579

#Posible suplementaria
#Evidencia asociación estadistica entre los resultados de las dimensiones y la profesion
fisher.test(base_enc$Cat_Dim1,base_enc$Profesión,alternative = "two.sided",simulate.p.value=TRUE) #0.3618
fisher.test(base_enc$Cat_Dim2,base_enc$Profesión,alternative = "two.sided",simulate.p.value=TRUE) #0.0004998*
fisher.test(base_enc$Cat_Dim3,base_enc$Profesión,alternative = "two.sided",simulate.p.value=TRUE) #0.4763
fisher.test(base_enc$Cat_Dim4,base_enc$Profesión,alternative = "two.sided",simulate.p.value=TRUE) #0.05897
fisher.test(base_enc$Cat_Dim5,base_enc$Profesión,alternative = "two.sided",simulate.p.value=TRUE) #0.2379

CramerV(base_enc$Cat_Dim1,base_enc$Profesión) #0.1057197
CramerV(base_enc$Cat_Dim2,base_enc$Profesión) #0.1872276
CramerV(base_enc$Cat_Dim3,base_enc$Profesión) #0.087843
CramerV(base_enc$Cat_Dim4,base_enc$Profesión) #0.1448374
CramerV(base_enc$Cat_Dim5,base_enc$Profesión) #0.1100806

#Se incluye por estudios previos
#Evidencia asociación estadistica entre los resultados de las dimensiones y los entornos
fisher.test(base_enc$Cat_Dim1,base_enc$`¿En cuál de los siguientes entornos realiza usted la mayor parte de su actividad profesional?`,alternative = "two.sided",simulate.p.value=TRUE) #0.2269
fisher.test(base_enc$Cat_Dim2,base_enc$`¿En cuál de los siguientes entornos realiza usted la mayor parte de su actividad profesional?`,alternative = "two.sided",simulate.p.value=TRUE) #0.0009995*
fisher.test(base_enc$Cat_Dim3,base_enc$`¿En cuál de los siguientes entornos realiza usted la mayor parte de su actividad profesional?`,alternative = "two.sided",simulate.p.value=TRUE) #0.2859
fisher.test(base_enc$Cat_Dim4,base_enc$`¿En cuál de los siguientes entornos realiza usted la mayor parte de su actividad profesional?`,alternative = "two.sided",simulate.p.value=TRUE) #0.08646
fisher.test(base_enc$Cat_Dim5,base_enc$`¿En cuál de los siguientes entornos realiza usted la mayor parte de su actividad profesional?`,alternative = "two.sided",simulate.p.value=TRUE) #0.0004998*

CramerV(base_enc$Cat_Dim1,base_enc$`¿En cuál de los siguientes entornos realiza usted la mayor parte de su actividad profesional?`) 
CramerV(base_enc$Cat_Dim2,base_enc$`¿En cuál de los siguientes entornos realiza usted la mayor parte de su actividad profesional?`) 
CramerV(base_enc$Cat_Dim3,base_enc$`¿En cuál de los siguientes entornos realiza usted la mayor parte de su actividad profesional?`) 
CramerV(base_enc$Cat_Dim4,base_enc$`¿En cuál de los siguientes entornos realiza usted la mayor parte de su actividad profesional?`) 
CramerV(base_enc$Cat_Dim5,base_enc$`¿En cuál de los siguientes entornos realiza usted la mayor parte de su actividad profesional?`) 

#Se incluye
#Evidencia asociación estadistica entre los resultados de las dimensiones y los cursos realizados
fisher.test(base_enc$Cat_Dim1,base_enc$`¿Realiza usted cursos de formación continuada para su mejora profesional?`,alternative = "two.sided",simulate.p.value=TRUE) #0.001499*
fisher.test(base_enc$Cat_Dim2,base_enc$`¿Realiza usted cursos de formación continuada para su mejora profesional?`,alternative = "two.sided",simulate.p.value=TRUE) #0.0004998*
fisher.test(base_enc$Cat_Dim3,base_enc$`¿Realiza usted cursos de formación continuada para su mejora profesional?`,alternative = "two.sided",simulate.p.value=TRUE) #0.1599
fisher.test(base_enc$Cat_Dim4,base_enc$`¿Realiza usted cursos de formación continuada para su mejora profesional?`,alternative = "two.sided",simulate.p.value=TRUE) #0.01799*
fisher.test(base_enc$Cat_Dim5,base_enc$`¿Realiza usted cursos de formación continuada para su mejora profesional?`,alternative = "two.sided",simulate.p.value=TRUE) #0.4573

CramerV(base_enc$Cat_Dim1,base_enc$`¿Realiza usted cursos de formación continuada para su mejora profesional?`) #0.2223588
CramerV(base_enc$Cat_Dim2,base_enc$`¿Realiza usted cursos de formación continuada para su mejora profesional?`) #0.2772138
CramerV(base_enc$Cat_Dim3,base_enc$`¿Realiza usted cursos de formación continuada para su mejora profesional?`) 
CramerV(base_enc$Cat_Dim4,base_enc$`¿Realiza usted cursos de formación continuada para su mejora profesional?`) 
CramerV(base_enc$Cat_Dim5,base_enc$`¿Realiza usted cursos de formación continuada para su mejora profesional?`) 

#Se incluye
#Evidencia asociación estadistica entre los resultados de las dimensiones y formacion en PBE
fisher.test(base_enc$Cat_Dim1,base_enc$`¿Ha realizado usted algún tipo de formación específica en Práctica Basada en la Evidencia?`,alternative = "two.sided",simulate.p.value=TRUE) #0.0004998*
fisher.test(base_enc$Cat_Dim2,base_enc$`¿Ha realizado usted algún tipo de formación específica en Práctica Basada en la Evidencia?`,alternative = "two.sided",simulate.p.value=TRUE) #0.0004998*
fisher.test(base_enc$Cat_Dim3,base_enc$`¿Ha realizado usted algún tipo de formación específica en Práctica Basada en la Evidencia?`,alternative = "two.sided",simulate.p.value=TRUE) #0.006497*
fisher.test(base_enc$Cat_Dim4,base_enc$`¿Ha realizado usted algún tipo de formación específica en Práctica Basada en la Evidencia?`,alternative = "two.sided",simulate.p.value=TRUE) #0.0004998*
fisher.test(base_enc$Cat_Dim5,base_enc$`¿Ha realizado usted algún tipo de formación específica en Práctica Basada en la Evidencia?`,alternative = "two.sided",simulate.p.value=TRUE) #0.0004998*

CramerV(base_enc$Cat_Dim1,base_enc$`¿Ha realizado usted algún tipo de formación específica en Práctica Basada en la Evidencia?`) #0.2053879
CramerV(base_enc$Cat_Dim2,base_enc$`¿Ha realizado usted algún tipo de formación específica en Práctica Basada en la Evidencia?`) #0.3582133
CramerV(base_enc$Cat_Dim3,base_enc$`¿Ha realizado usted algún tipo de formación específica en Práctica Basada en la Evidencia?`) #0.159263
CramerV(base_enc$Cat_Dim4,base_enc$`¿Ha realizado usted algún tipo de formación específica en Práctica Basada en la Evidencia?`) #0.3084294
CramerV(base_enc$Cat_Dim5,base_enc$`¿Ha realizado usted algún tipo de formación específica en Práctica Basada en la Evidencia?`) #0.2718971

#Se incluye
#Evidencia asociación estadistica entre los resultados de las dimensiones y articulos cientificos
fisher.test(base_enc$Cat_Dim1,base_enc$`¿Usted lee articulos cientificos?`,alternative = "two.sided",simulate.p.value=TRUE) #0.0004998*
fisher.test(base_enc$Cat_Dim2,base_enc$`¿Usted lee articulos cientificos?`,alternative = "two.sided",simulate.p.value=TRUE) #0.0004998*
fisher.test(base_enc$Cat_Dim3,base_enc$`¿Usted lee articulos cientificos?`,alternative = "two.sided",simulate.p.value=TRUE) #0.0004998*
fisher.test(base_enc$Cat_Dim4,base_enc$`¿Usted lee articulos cientificos?`,alternative = "two.sided",simulate.p.value=TRUE) #0.0004998*
fisher.test(base_enc$Cat_Dim5,base_enc$`¿Usted lee articulos cientificos?`,alternative = "two.sided",simulate.p.value=TRUE) #0.0004998*

CramerV(base_enc$Cat_Dim1,base_enc$`¿Usted lee articulos cientificos?`) 
CramerV(base_enc$Cat_Dim2,base_enc$`¿Usted lee articulos cientificos?`) #0.3513646
CramerV(base_enc$Cat_Dim3,base_enc$`¿Usted lee articulos cientificos?`) 
CramerV(base_enc$Cat_Dim4,base_enc$`¿Usted lee articulos cientificos?`) #0.2556923
CramerV(base_enc$Cat_Dim5,base_enc$`¿Usted lee articulos cientificos?`) #0.2135854

#Se incluye
#Evidencia asociación estadistica entre los resultados de las dimensiones y el género
fisher.test(base_enc$Cat_Dim1,base_enc$Género,alternative = "two.sided",simulate.p.value=TRUE) #0.02099*
fisher.test(base_enc$Cat_Dim2,base_enc$Género,alternative = "two.sided",simulate.p.value=TRUE) #0.01699*
fisher.test(base_enc$Cat_Dim3,base_enc$Género,alternative = "two.sided",simulate.p.value=TRUE) #0.7581
fisher.test(base_enc$Cat_Dim4,base_enc$Género,alternative = "two.sided",simulate.p.value=TRUE) #0.5112
fisher.test(base_enc$Cat_Dim5,base_enc$Género,alternative = "two.sided",simulate.p.value=TRUE) #0.4683

CramerV(base_enc$Cat_Dim1,base_enc$Género) #0.1199904
CramerV(base_enc$Cat_Dim2,base_enc$Género) #0.1475876
CramerV(base_enc$Cat_Dim3,base_enc$Género) #0.05447897
CramerV(base_enc$Cat_Dim4,base_enc$Género) #0.091747
CramerV(base_enc$Cat_Dim5,base_enc$Género) #0.08910321

#Se incluye
#Evidencia asociación estadistica entre los resultados de las dimensiones y las edades categoricas
fisher.test(base_enc$Cat_Dim1,base_enc$edad_c,alternative = "two.sided",simulate.p.value=TRUE) #0.2389
fisher.test(base_enc$Cat_Dim2,base_enc$edad_c,alternative = "two.sided",simulate.p.value=TRUE) #0.3498
fisher.test(base_enc$Cat_Dim3,base_enc$edad_c,alternative = "two.sided",simulate.p.value=TRUE) #0.2499
fisher.test(base_enc$Cat_Dim4,base_enc$edad_c,alternative = "two.sided",simulate.p.value=TRUE) #0.3858
fisher.test(base_enc$Cat_Dim5,base_enc$edad_c,alternative = "two.sided",simulate.p.value=TRUE) #0.2869

CramerV(base_enc$Cat_Dim1,base_enc$edad_c) #0.09296542
CramerV(base_enc$Cat_Dim2,base_enc$edad_c) #0.1016263
CramerV(base_enc$Cat_Dim3,base_enc$edad_c) #0.08471897
CramerV(base_enc$Cat_Dim4,base_enc$edad_c) #0.09456906
CramerV(base_enc$Cat_Dim5,base_enc$edad_c) #0.1035593





##### ACM #####

base_acm<-cbind(base_enc[,c(1,3,5:11,77:81)])
# base_acm_e5<-cbind(base_enc[,c(1,3,5:11,82:86)])

colnames(base_acm)<-c("edad_c","género","país","titulación","profesión","entornos","cursos","formacion_PBE","lee_articulos","Dim1","Dim2","Dim3","Dim4","Dim5")
# colnames(base_acm_e5)<-c("edad_c","género","país","titulación","profesión","entornos","cursos","formacion_PBE","lee_articulos","D1","D2","D3","D4","D5")

for (i in 1:ncol(base_acm)) {
  base_acm[,i] <- as.factor(base_acm[,i])
}

# for (i in 1:ncol(base_acm_e5)) {
#   base_acm_e5[,i] <- as.factor(base_acm_e5[,i])
# }


save.image("C:/Users/usuario/Downloads/Encuesta_percepcion.RData")

#Variables activas e ilustrativas
dat<-as.data.frame(base_acm[,-c(3)])
sup<-as.data.frame(base_acm[,c(3)])

#Tabla disyuntiva completa
#View(acm.disjonctif(dat$tab))
#Tabla de Burt
#View(acm.burt(dat$tab,dat$tab))

#para escoger el # de ejes a retener
#ACM CON ADE4
acm<-dudi.acm(dat,scannf = T)

#acm<-dudi.acm(dat$tab,scannf = F,nf=3) #Los primeros cuatro ejes recogen cerca del 29% de la inercia

# windows()
# screeplot(acm, main = "", ylim=c(0, 0.5))

summary(acm)

acm$eig #valores propios
acm$li  #psi: Comp. principales
acm$co  #phi: Cordenadas variables

#Gráficas de sedimentos
windows()
plot(acm$eig, type="b", ylab="Proporción de varianza explicada")

windows()
plot(cumsum(acm$eig), ylab="Proporción de varianza explicada acumulada",type="b")

#Descomposicion de la inercia
DesI<-inertia.dudi(acm,row.inertia=TRUE,col.inertia=TRUE)
DesI$tot.inertia #valores y sus porcentajes
DesI$row.abs #contribuciones absolutas de las filas a la inercia
DesI$row.rel #contribuciones relativas para las filas (cosenos cuadrado = calidad de la representación)
DesI$row.cum #contribucion relativa de las filas a la inercia total
DesI$col.abs #contribuciones absolutas de las columnas a la inercia
DesI$col.rel #contribuciones relativas para las columnas (cosenos cuadrado = calidad de la representación)
DesI$col.cum #contribucion relativa de las columnas a la inercia total

#Subnubes de las categorias
windows()
scatter(acm,col=c(1,2,3,4,5,6))

#Nube de variables(categorias)
windows()
s.label(acm$co)

#Nube de individuos
windows()
s.label(acm$li)

#circulo de correlaciones (variables = categorias)
windows(); s.corcircle(acm$co,1,2,clabel = 0.7)

#boxplot
windows()
boxplot(acm)

#biplot
windows()
s.label(acm$co)
s.label(acm$li,add.p=T,clab=1)

windows()
s.label(acm$co)
s.class(acm$li,datos$fun,cellipse = 0,add.p=T)

##### Analisis de conglomerados (clusters) ######
#Facto Class
FC<-FactoClass(dfact = dat, metodo = dudi.acm, dfilu = sup) #39 ejes y se ven claramente 4 cluster
inertia.dudi(FC$dudi)

FC$indices                   #índices de nivel para clústeres jerárquicos (WARD)
sum(FC$indices[4])           #Suma de los indices de nivel
FC$clus.summ                 #cambios de partición debido al proceso de consolidación
FC$cluster;table(FC$cluster) #Numero del cluster para cada elemento
sort(FC$cluster)             #Numero ordenado del cluster para cada elemento
FC$carac.cate                #caracterizacion clases segun variables cualitativas

#Cluster con FactoClass
windows()
plotFactoClass(FC,roweti = F,col.col = "gray")
#supcol(FC$dudi,as.matrix(sup))
# points(SE.coor,pch=23)
# text(SE.coor,"SE",pos=3)




#ACM con factoextra
#1. Calculo  ACM
options(ggrepel.max.overlaps = Inf) #para q grafique todas las etiquetas
acm_1<-MCA(base_acm, ncp = 4, graph = TRUE, quali.sup = c(3)) #retiene 29% los 4 ejes
acm_1$eig #datos de la inercia

#2. Resultados por variables e individuos (coord, cos2, contrib)

get_mca_var(acm_1)
get_mca_ind(acm_1)

# 3. Contribuciones de var e ind a los ejes ppales
# variable a eje 1
fviz_contrib(acm_1, choice ="var", axes = 2, top = 20)
fviz_contrib(acm_1, choice ="ind", axes = 1, top = 20)

#4. Grafica de individuos
grp1 <- as.factor(base_acm$Dim1)
grp2 <- as.factor(base_acm$Dim2)
grp3 <- as.factor(base_acm$Dim3)
grp4 <- as.factor(base_acm$Dim4)
grp5 <- as.factor(base_acm$Dim5) 

#repel T para evitar sobretrazar las lineas
windows()
grid.arrange(fviz_mca_ind(acm_1,  habillage = grp1,addEllipses = TRUE, repel = TRUE, geom = "point", subtitle="Dimension 1", palette = "lancet"), 
             fviz_mca_ind(acm_1,  habillage = grp2,addEllipses = TRUE, repel = TRUE, geom = "point", subtitle="Dimension 2", palette = "lancet"), 
             fviz_mca_ind(acm_1,  habillage = grp3,addEllipses = TRUE, repel = TRUE, geom = "point", subtitle="Dimension 3", palette = "lancet"), 
             fviz_mca_ind(acm_1,  habillage = grp4,addEllipses = TRUE, repel = TRUE, geom = "point", subtitle="Dimension 4", palette = "lancet"), 
             fviz_mca_ind(acm_1,  habillage = grp5,addEllipses = TRUE, repel = TRUE, geom = "point", subtitle="Dimension 5", palette = "lancet"), 
             ncol=2,nrow=3)


#5. Nube de categorias
fviz_mca_var(acm_1, repel = TRUE)

#6. Biplot
fviz_mca_biplot(acm_1, repel = TRUE, geom.var = "text", geom.ind = "point",
                palette = "jco",label = "all")

#Grafica (nube de ind, nube de var, biplot)
#grid.arrange(fviz_mca_var(acm_1), fviz_mca_ind(acm_1), fviz_mca_biplot(acm_1), ncol=3)


#Indíces de nivel
fviz_screeplot(acm_1, addlabels = TRUE,ylab="Porcentaje de varianza explicada",
               xlab="Dimensiones",main="")

#Subnubes por cada categoria
windows()
fviz_ellipses(acm_1, c("edad_c","género","titulación","entornos",
                       "cursos","formacion_PBE","lee_articulos","Dim1","Dim2","Dim3","Dim4","Dim5"),
              geom = "point",addEllipses = TRUE,ellipse.type="norm",
              ggtheme = theme_gray(),palette = c("5,8,40,25,17,23,12,18,34,22,27,13,20,14,32,1,11,43,24,10,39,15,19,26,4,37,46,7,33,38,35,42,16,36,28,2,29,45,3,6,41,30,44,31,21,9"))



#Factoextra
#Cluster jerárquico

# Dissimilarity matrix a partir de los resultados del analisis factorial  ACM
# d <- dist((1-acm_1$ind$cos2), method = "euclidean") #pearson mirar

# Hierarchical clustering using metodo de WARD
# hc1 <- hclust(d, method = "ward.D") #intentra con ward2

#1
windows()
res.hcpc <- HCPC(acm_1,metric="euclidean", method="ward")

#dendrograma
# windows()
# plot(hc1, cex = 0.6, hang = -1)

# Cantidad de ind por cluster
#res <- hcut(dat, k = 5, stand = TRUE)
# sub_grp <- cutree(hc1, k = 3)
# table(sub_grp)

#clusters en el dendograma
# windows()
# plot(hc1, cex = 0.6)

#clusters en el dendograma con colores
# windows()
# fviz_dend(res.hcpc, rect = TRUE, cex = 0.5,
#           k_colors = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07","#FC4E88")) #

#2
windows()
fviz_dend(res.hcpc, rect = F, cex = 0.5)

#clusters
# windows()
# fviz_cluster(list(data = acm_1$ind$cos2, cluster = sub_grp))

#3
windows()
fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map",
             geom = c("point"))

windows()
fviz_mca_biplot(acm_1, label="var", habillage=10,addEllipses=TRUE) + 
  theme_minimal()

#4
#carcaterización de los grupos

#Description of each cluster by the categories 
res.hcpc$desc.var #valores test de cada categoria en cada cluster - incluye suplementarias
res.hcpc$desc.var$category$`1` #caracteristicas cluster 1

#Cantidad individuos por cluster
table(res.hcpc$data.clust$clust)

write_xlsx(as.data.frame(res.hcpc$desc.var$category$`1`),"c1.xlsx")
write_xlsx(as.data.frame(res.hcpc$desc.var$category$`2`),"c2.xlsx")
write_xlsx(as.data.frame(res.hcpc$desc.var$category$`3`),"c3.xlsx")
write_xlsx(as.data.frame(res.hcpc$desc.var$category$`4`),"c4.xlsx")
write_xlsx(as.data.frame(res.hcpc$desc.var$category$`5`),"c5.xlsx")

write_xlsx(base_acm,"base_ctg_pbe.xlsx")


