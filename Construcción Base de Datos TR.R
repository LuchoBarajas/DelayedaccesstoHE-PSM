##########################################################################
#       TRABAJO DE GRADO - TRÁNSITO REZAGADO A EDUCACIÓN SUPERIOR        #
##########################################################################


# ----------------------------------------------------------------------------------
# El presente código tiene como objetivo construir la base de datos de tránsito    -
# rezagado a Educación Superior de 2016. Se cruza la base de Primer Curso con      -
# la base del SIMAT y la base de SABER 11; A partir de esta información, se espera -
# contar con los datos necesarios para caracterizar a la población de Primer Curso -
# de 2016.                                                                         -      
#-----------------------------------------------------------------------------------


# Borrar todo

rm(list = ls())

# Librerías y Directorio de Trabajo

library(openxlsx)
library(haven)
library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(data.table)

setwd("/Users/lbarajas/Documents/Tesis/Bases de Datos/")

# Carga de Bases de Datos

SNIES = read_sav("/Users/lbarajas/Documents/Tesis/Bases de Datos/Primer_curso_2016.sav")
SIMAT = read.csv2("/Users/lbarajas/Documents/Tesis/PRIMER_CURSO_2016_MAT_H_2017.csv")
SIMAT_COLEGIOS = fread("/Users/lbarajas/Documents/Tesis/Bases de Datos/MATRICULA_ESTADISTICA_DEFINITIVA_2016_1.csv")
SABER_11 = read.csv2("/Users/lbarajas/Documents/Tesis/PRIMER_CURSO_2016_SABER11_2015.txt", sep = "æ")
GRADUADOS = fread("/Users/lbarajas/Documents/Tesis/Bases de Datos/reporte.txt")
MATRICULA_2016 = read_sav("/Users/lbarajas/Documents/Tesis/Matricula_2016_Semestres_1y2.sav")
MATRICULA_2017 = read_sav("/Users/lbarajas/Documents/Tesis/Matricula2017_sem1y2.sav")
MATRICULA_2018 = read_sav("/Users/lbarajas/Documents/Tesis/BASE_MATRICULA_2018_SEMESTRE.sav")

# SNIES----------------------------------------------------------------------------------------------------------------------------------------------------------------------

SNIES %<>% filter(NIVEL == "PREGRADO")
SNIES %<>% filter(ID_METODOLOGIA == "Presencial" & SEMESTRE == 1)
SNIES %<>% select(ID_PARTICIPANTE,NUMERO_IDENTIFICACION,PRIMER_NOMBRE, SEGUNDO_NOMBRE, PRIMER_APELLIDO, SEGUNDO_APELLIDO,FECHA_NACIMIENTO = FECHA_NACIMIENTO_A, SEXO_BIOLOGICO = GENERO, ID_IES, IES_NOMBRE, SECTOR_IES = ORIGEN,
                  CARACTER_ACADEMICO = CARACTER_ACADEMICO,IES_ACREDITADA = TIPO_ACREDITACION, CODIGO_PROGRAMA, PROGRAMA,NIVEL_FORMACION = MODALIDAD, AREA_CONOCIMINETO,
                  NBC= ID_NBC, PROGRAMA_ACREDITADO = TIPO_ACREDITACION_PROG, CODIGO_DEPARTAMENTO_PROGRAMA, DEPARTAMENTO_PROGRAMA, CODIGO_MUNICIPIO_PROGRAMA, MUNICIPIO_PROGRAMA,
                  E_CIVIL = DESC_ESTADO_CIVIL, TIPO_VINCULACION = DESC_TIPO_VINCULACION, EDAD = EDAD1)

# SIMAT----------------------------------------------------------------------------------------------------------------------------------------------------------------------

SIMAT %<>% select(NUMERO_IDENTIFICACION = NRO_DOCUMENTO, ESTRATO, CODIGO_DANE, CODIGO_DANE_SEDE, AÑO_BACHILLER = ANNO_INF, VICTIMA = POB_VICT_CONF,DISCAPACIDAD = TIPO_DISCAPACIDAD,
                  RAZA = ETNIA, ZON_ALU, SECTOR_COLEGIO = CTE_ID_SECTOR, CONTRATACION = SECTOR_CONPES)
SIMAT_COLEGIOS %<>% select(CODIGO_DANE, CODIGO_DANE_SEDE, DPTO_CARGA, Divipola_MUNICIPIO) %>% unique()
SIMAT_COLEGIOS$CODIGO_DANE %<>% as.character()
SIMAT_COLEGIOS$CODIGO_DANE_SEDE %<>% as.character()

SIMAT$CODIGO_DANE %<>% as.character()
SIMAT$CODIGO_DANE_SEDE %<>% as.character()

SIMAT %<>% left_join(SIMAT_COLEGIOS)

SIMAT = SIMAT[!duplicated(SIMAT$NUMERO_IDENTIFICACION),]


# SABER 11-------------------------------------------------------------------------------------------------------------------------------------------------------------------

SABER_11 %<>% filter(TIPO_CRITERIO != "TIPO 30")
SABER_11 %<>% filter(NRO_DOCUMENTO %in% SNIES$NUMERO_IDENTIFICACION)
SABER_11_A = SABER_11 %>% filter(!is.na(FUENTE_1))
SABER_11_A %<>% select(NUMERO_IDENTIFICACION = NRO_DOCUMENTO, 
                       FAMI_ING_FMILIAR_MENSUAL,MATEMATICAS_PUNT, AÑO_PRUEBA = FUENTE_1)

SABER_11_A %<>% mutate(SABER_POST_2014 = 0)

SABER_11_B = SABER_11 %>% filter(!is.na(FUENTE_2))
SABER_11_B %<>% select(NUMERO_IDENTIFICACION = NRO_DOCUMENTO,   
                       FAMI_ING_FMILIAR_MENSUAL = FINS_INGRESOMENSUALHOGAR, 
                       MATEMATICAS_PUNT = PUNT_MATEMATICAS,AÑO_PRUEBA = FUENTE_2)

SABER_11_B %<>% mutate(SABER_POST_2014 = ifelse(AÑO_PRUEBA == 201401, 0, 1))

SABER_11 =  rbind(SABER_11_A, SABER_11_B)

DUPLICADOS_SABER = SABER_11 %>% group_by(NUMERO_IDENTIFICACION)%>% summarise(TOTAL= n())
DUPLICADOS_SABER %<>% filter(TOTAL > 1)

SABER_11_UNICOS = SABER_11 %>% filter(!(NUMERO_IDENTIFICACION %in% DUPLICADOS_SABER$NUMERO_IDENTIFICACION))
SABER_11_DUPLICADOS = SABER_11 %>% filter(NUMERO_IDENTIFICACION %in% DUPLICADOS_SABER$NUMERO_IDENTIFICACION)
SABER_11_DUPLICADOS %<>% group_by(NUMERO_IDENTIFICACION) %>% filter(MATEMATICAS_PUNT == max(MATEMATICAS_PUNT))
SABER_11_DUPLICADOS %<>% group_by(NUMERO_IDENTIFICACION) %>% filter(AÑO_PRUEBA == max(AÑO_PRUEBA))
SABER_11_DUPLICADOS = SABER_11_DUPLICADOS[!duplicated(SABER_11_DUPLICADOS$NUMERO_IDENTIFICACION),]
SABER_11_DUPLICADOS %<>% as.data.frame()

SABER_11 = rbind(SABER_11_UNICOS, SABER_11_DUPLICADOS)
SABER_11 %<>% select(-AÑO_PRUEBA)


# Unión de Bases-----------------------------------------------------------------------------------------------------------------------------------------------------------


  ## SIMAT

ENCONTRADOS = SIMAT %>% select(NUMERO_IDENTIFICACION) %>% mutate(ENCONTRADO = "ENCONTRADO")

SNIES %<>% left_join(ENCONTRADOS)
SNIES$ENCONTRADO[is.na(SNIES$ENCONTRADO)]= "NO ENCONTRADO"
SNIES %<>% filter(ENCONTRADO == "ENCONTRADO") %>% select(-ENCONTRADO)
table(SNIES$TIPO_VINCULACION)

SNIES %<>% left_join(SIMAT)


 ##  SABER 11


SNIES %<>% left_join(SABER_11)

# Identificación de Graduados------

GRADUADOS %<>% filter(AÑO.GRADO < 2016)
GRADUADOS_MARCACION = GRADUADOS %>% select(NUMERO_IDENTIFICACION = NUMERO.IDENTIFICACION) %>% mutate(GRADUADO = "GRADUADO") %>% unique()
SNIES$NUMERO_IDENTIFICACION %<>% as.character()
SNIES %<>% left_join(GRADUADOS_MARCACION)
table(SNIES$GRADUADO)


Verificacion = SNIES %>% filter(GRADUADO == "GRADUADO")
Verificacion_2 = GRADUADOS %>% filter(NUMERO.IDENTIFICACION %in% Verificacion$NUMERO_IDENTIFICACION)

# LIMPIEZA BASE DE DATOS----------------------------------------------------------------------------------------------------------------------

SNIES = read.xlsx("SNIES_SIMAT.xlsx")
SNIES %<>% mutate(SEXO_BIOLOGICO = ifelse(SEXO_BIOLOGICO == "Hombre", 1, 0))
SNIES %<>% mutate(SECTOR_IES = ifelse(SECTOR_IES == "OFICIAL", 1, 0))
SNIES %<>% mutate(CARACTER_ACADEMICO = ifelse(CARACTER_ACADEMICO == "Universidad", 1,0))
SNIES %<>% mutate(IES_ACREDITADA = ifelse(IES_ACREDITADA == "Alta Calidad", 1, 0))
SNIES %<>% mutate(PROGRAMA_ACREDITADO = ifelse(PROGRAMA_ACREDITADO == "Acreditacion de Alta Calidad", 1, 
                                               ifelse(PROGRAMA_ACREDITADO == "Registro Calificado", 0, NA)))
SNIES %<>% mutate(E_CIVIL = ifelse(E_CIVIL == "Casado(a)" | E_CIVIL == "Unión libre", 1, 
                                   ifelse(E_CIVIL == "Soltero(a)" | E_CIVIL == "Viudo(a)" |
                                            E_CIVIL == "Divorciado(a)" | E_CIVIL == "Religioso(a)" |
                                            E_CIVIL == "Separado(a)", 0, NA)))
SNIES$ESTRATO %<>% str_replace("ESTRATO 0", "0")
SNIES$ESTRATO %<>% str_replace("ESTRATO 1", "1")
SNIES$ESTRATO %<>% str_replace("ESTRATO 2", "2")
SNIES$ESTRATO %<>% str_replace("ESTRATO 3", "3")
SNIES$ESTRATO %<>% str_replace("ESTRATO 4", "4")
SNIES$ESTRATO %<>% str_replace("ESTRATO 5", "5")
SNIES$ESTRATO %<>% str_replace("ESTRATO 6", "6")
SNIES$ESTRATO %<>% str_replace("9", " ")
SNIES$ESTRATO %<>% as.numeric()

SNIES$VICTIMA %<>% str_replace("NO APLICA", "9")
SNIES %<>% mutate(VICTIMA = ifelse(VICTIMA == "9", 0,
                                   ifelse(VICTIMA == " ", NA, 1)))

SNIES$DISCAPACIDAD %<>% str_replace("NO APLICA", "99")
SNIES %<>% mutate(DISCAPACIDAD = ifelse(DISCAPACIDAD == 99, 0,
                                        ifelse(DISCAPACIDAD == " ", NA,1)))

SNIES$RAZA %<>% str_replace("NO APLICA", "0")
SNIES %<>% mutate(RAZA = ifelse(RAZA == "0", 0,
                                ifelse(RAZA == " ", NA,1)))

SNIES$ZON_ALU %<>% str_replace("809", "URBANA")
SNIES$ZON_ALU %<>% str_replace("810", "RURAL")
SNIES$ZON_ALU %<>% str_replace("1", "URBANA")
SNIES$ZON_ALU %<>% str_replace("2", "RURAL")
SNIES$ZON_ALU %<>% str_replace("0", " ")

SNIES %<>% mutate(RURALIDAD = ifelse(ZON_ALU == "RURAL",1, ifelse(ZON_ALU == "URBANA", 0, NA)))

SNIES$SECTOR_COLEGIO %<>% str_replace("806", "OFICIAL")
SNIES$SECTOR_COLEGIO %<>% str_replace("807", "NO OFICIAL")
SNIES$SECTOR_COLEGIO %<>% str_replace("1001", "OFICIAL")
SNIES$SECTOR_COLEGIO %<>% str_replace("1002", "NO OFICIAL")
SNIES %<>% mutate(SECTOR_COLEGIO = ifelse(SECTOR_COLEGIO == "OFICIAL", 1,
                                          ifelse(SECTOR_COLEGIO == "NO OFICIAL", 0, NA)))

SNIES %<>% mutate(MIGRACION = ifelse(is.na(DPTO_CARGA),NA, 
                                     if_else(DPTO_CARGA != CODIGO_DEPARTAMENTO_PROGRAMA, 1, 0)))

# VARIABLE DE INTERÉS

SNIES %<>% mutate(TRANSITO = ifelse(AÑO_BACHILLER > 2014, 1,0))

SNIES %<>% mutate(TRANSITO_1 = ifelse(AÑO_BACHILLER > 2014, 0,1))

# VARIABLE DEPENDIENTE

DESERCION = SNIES %>% select(ID_PARTICIPANTE,CODIGO_PROGRAMA)
DESERCION %<>% mutate(LLAVE = paste0(ID_PARTICIPANTE,CODIGO_PROGRAMA))
DESERCION %<>% mutate(A_2016_1 = 1)

MATRICULA_2016 %<>% filter(SEMESTRE == 2)
MATRICULA_2016 %<>% select(ID_PARTICIPANTE, CODIGO_PROGRAMA) %>% unique()
MATRICULA_2016 %<>% mutate(LLAVE = paste0(ID_PARTICIPANTE,CODIGO_PROGRAMA))
MATRICULA_2016 %<>% mutate(A_2016_2 = 1)
MATRICULA_2016 %<>% select(LLAVE,A_2016_2)
MATRICULA_2016 = MATRICULA_2016[!duplicated(MATRICULA_2016$LLAVE),]
DESERCION %<>% left_join(MATRICULA_2016)

MATRICULA_2017_1 = MATRICULA_2017 %>% filter(SEMESTRE == 1)
MATRICULA_2017_1 %<>% select(ID_PARTICIPANTE, CODIGO_PROGRAMA) %>% unique()
MATRICULA_2017_1 %<>% mutate(LLAVE = paste0(ID_PARTICIPANTE,CODIGO_PROGRAMA))
MATRICULA_2017_1 %<>% mutate(A_2017_1 = 1)
MATRICULA_2017_1 %<>% select(LLAVE,A_2017_1)
MATRICULA_2017_1 = MATRICULA_2017_1[!duplicated(MATRICULA_2017_1$LLAVE),]
DESERCION %<>% left_join(MATRICULA_2017_1)

MATRICULA_2017_2 = MATRICULA_2017 %>% filter(SEMESTRE == 2)
MATRICULA_2017_2 %<>% select(ID_PARTICIPANTE, CODIGO_PROGRAMA) %>% unique()
MATRICULA_2017_2 %<>% mutate(LLAVE = paste0(ID_PARTICIPANTE,CODIGO_PROGRAMA))
MATRICULA_2017_2 %<>% mutate(A_2017_2 = 1)
MATRICULA_2017_2 %<>% select(LLAVE,A_2017_2)
MATRICULA_2017_2 = MATRICULA_2017_2[!duplicated(MATRICULA_2017_2$LLAVE),]
DESERCION %<>% left_join(MATRICULA_2017_2)

MATRICULA_2018_1 = MATRICULA_2018 %>% filter(SEMESTRE == 1)
MATRICULA_2018_1 %<>% select(ID_PARTICIPANTE, CODIGO_PROGRAMA) %>% unique()
MATRICULA_2018_1 %<>% mutate(LLAVE = paste0(ID_PARTICIPANTE,CODIGO_PROGRAMA))
MATRICULA_2018_1 %<>% mutate(A_2018_1 = 1)
MATRICULA_2018_1 %<>% select(LLAVE,A_2018_1)
DESERCION %<>% left_join(MATRICULA_2018_1)

MATRICULA_2018_2 = MATRICULA_2018 %>% filter(SEMESTRE == 2)
MATRICULA_2018_2 %<>% select(ID_PARTICIPANTE, CODIGO_PROGRAMA) %>% unique()
MATRICULA_2018_2 %<>% mutate(LLAVE = paste0(ID_PARTICIPANTE,CODIGO_PROGRAMA))
MATRICULA_2018_2 %<>% mutate(A_2018_2 = 1)
MATRICULA_2018_2 %<>% select(LLAVE,A_2018_2)
DESERCION %<>% left_join(MATRICULA_2018_2)

DESERCION$A_2016_2[is.na(DESERCION$A_2016_2)]= 0
DESERCION$A_2017_1[is.na(DESERCION$A_2017_1)]= 0
DESERCION$A_2017_2[is.na(DESERCION$A_2017_2)]= 0
DESERCION$A_2018_1[is.na(DESERCION$A_2018_1)]= 0
DESERCION$A_2018_2[is.na(DESERCION$A_2018_2)]= 0


DESERCION %<>% mutate(D1= ifelse(A_2016_2== 0 & A_2017_1 == 0 &
                                   A_2017_2 == 0 & A_2018_1 == 0 & A_2018_2 == 0, 1,0))

DESERCION %<>% mutate(D2= ifelse(A_2016_2== 1 & A_2017_1 == 0 &
                                   A_2017_2 == 0 & A_2018_1 == 0 & A_2018_2 == 0, 1,0))

DESERCION %<>% mutate(D3= ifelse(A_2016_2== 1 & A_2017_1 == 1 &
                                   A_2017_2 == 0 & A_2018_1 == 0 & A_2018_2 == 0, 1,0))

DESERCION %<>% mutate(D4= ifelse(A_2016_2== 1 & A_2017_1 == 1 &
                                   A_2017_2 == 1 & A_2018_1 == 0 & A_2018_2 == 0, 1,0))

DESERCION %<>% mutate(D5= ifelse(A_2016_2== 0 & A_2017_1 == 1 &
                                   A_2017_2 == 0 & A_2018_1 == 0 & A_2018_2 == 0, 1,0))

DESERCION %<>% mutate(D6= ifelse(A_2016_2== 1 & A_2017_1 == 0 &
                                   A_2017_2 == 1 & A_2018_1 == 0 & A_2018_2 == 0, 1,0))

DESERCION %<>% mutate(D7= ifelse(A_2016_2== 0 & A_2017_1 == 1 &
                                   A_2017_2 == 1 & A_2018_1 == 0 & A_2018_2 == 0, 1,0))

DESERCION %<>% mutate(D8= ifelse(A_2016_2== 0 & A_2017_1 == 0 &
                                   A_2017_2 == 1 & A_2018_1 == 0 & A_2018_2 == 0, 1,0))

DESERCION %<>% mutate(DESERTOR = ifelse(D1 == 1|D2 == 1| D3==1 | D4== 1|D5==1 | 
                                          D6== 1 | D7 == 1 |D8 == 1, 1, 0))

DESERCION %<>% select(LLAVE,DESERTOR)
DESERCION %<>% mutate(DUPLICADO = duplicated(LLAVE))

SNIES %<>% mutate(LLAVE = paste0(ID_PARTICIPANTE,CODIGO_PROGRAMA))

SNIES %<>% left_join(DESERCION)

lm = lm(SNIES$DESERTOR ~ SNIES$TRANSITO_1 + SNIES$SEXO_BIOLOGICO + SNIES$SECTOR_IES + SNIES$IES_ACREDITADA + SNIES$PROGRAMA_ACREDITADO + 
     SNIES$E_CIVIL + SNIES$EDAD + SNIES$RURALIDAD + SNIES$SECTOR_COLEGIO + SNIES$MATEMATICAS_PUNT + SNIES$SABER_POST_2014 + SNIES$MIGRACION)

# VARIABLE INSTRUMENTAL 

SNIES %<>% mutate(Z_1 = ifelse(CONTRATACION == 2 |CONTRATACION == 3, 1,
                               ifelse(CONTRATACION == 1 | CONTRATACION == 4, 0, NA)))

SNIES %<>% mutate(Z_2 = ifelse(CONTRATACION == 3, 1,
                               ifelse(CONTRATACION == 1 |CONTRATACION == 2| CONTRATACION == 4, 0, NA)))

SNIES %<>% select(-NUMERO_IDENTIFICACION,-ID_PARTICIPANTE, -PRIMER_NOMBRE,
                    -SEGUNDO_NOMBRE, -PRIMER_APELLIDO, - SEGUNDO_APELLIDO, 
                    -FECHA_NACIMIENTO, -TIPO_VINCULACION, - CODIGO_DANE, 
                    - CODIGO_DANE_SEDE, -ZON_ALU, - DUPLICADO)

SNIES %<>% mutate(GRADUADO = ifelse(GRADUADO == "GRADUADO", 1,0))

write.xlsx(SNIES, "BASE_TESIS_TRANSITO_REZAGADO.xlsx")
write.csv2(SNIES, "BASE_TESIS_TRANSITO_REZAGADO.csv")
write.xlsx(Verificacion_2, "Graduados_PC_2016.xlsx")
write.csv2(Verificacion_2, "Graduados_PC_2016.csv")




