/*******************************************************************************
							  Modificación SIRA
			  Ministerio de Desregulación y Transformación del Estado
					 Secretaria de Simplificación del Estado
								 Martín Rossi
				Autores: Abigail Riquelme y Facundo Gómez García
********************************************************************************/
clear all
set more off

else if  "`c(username)'" == "Usuario" {
	global main "C:\Users\Usuario\Desktop\Trabajo - UdeSA\Trabajo - Ministerio\Evaluación Textiles"
	}

*Crear carpetas de "input" (donde estará la base de datos) y "output" (para las tablas/gráficos):
global input "$main/input"
global output "$main/output"

cd "$main"

import excel "$input/base_ipc_clean_final_norm.xlsx", firstrow clear
*Aquí, la variable "value" representa el cociente entre la diferencia del IPC reportado para cada categoría y el IPC general, dividido por el IPC general. Luego, este cociente se multiplica por 100.

*Reordenamos la base de datos para las estimaciones:
replace ipc_type = subinstr(ipc_type, "Prendas de vestir", "ipc_vestir_calzado", .)
replace ipc_type = subinstr(ipc_type, "Seguros médicos", "ipc_seguros_med", .)
replace ipc_type = subinstr(ipc_type, "Servicios de recreación", "ipc_recreacion", .)
replace ipc_type = subinstr(ipc_type, "Seguros", "ipc_seguros", .)
sort fecha ipc_type
drop primer_valor value
ren value_norm value
reshape wide value, i(fecha NivelGeneral empalmar) j(ipc_type) string

*===========================*
*		ESTIMACIONES
*===========================*
*Se generaron dos variables clave para el análisis: una que captura la tendencia temporal durante el periodo estudiado, y otra que identifica los meses afectados por la modificación del SIRA. Los meses tratados son aquellos posteriores a la fecha de la modificación, por lo que se consideran como tratados las observaciones a partir de diciembre de 2023. Además, para minimizar el ruido en los datos, restringimos el análisis a un periodo cercano a la intervención, limitándolo a las variaciones observadas entre enero de 2022 y la actualidad.
sort fecha
drop if fecha<mdy(1,1,2022)
gen treat=0
replace treat=1 if fecha>=mdy(12,1,2023)
gen t=_n

*Agregamos un control más a la estimación que se define como la interacción entre la variable de tratamiento y la tendencia:
gen treat_t=treat*t

*A continuación, se presentan las estimaciones utilizando un modelo "Before and After" con tendencia temporal. En todas las especificaciones, la variable de interés es la interacción entre el tratamiento y la tendencia temporal, es decir, Tratamiento*Tendencia. El coeficiente asociado a esta variable capturará el cambio en la tendencia tras la modificación del SIRA, permitiendo determinar si la intervención generó en promedio una disminución o un aumento en el valor relativo del IPC a lo largo del tiempo.
*De acuerdo con los resultados, el coeficiente de la interacción resulta negativo y significativo, lo que implicaría que después de la intervención los precios de las prendas de vestir y calzado comenzaron a caer por debajo de la inflación general a través del tiempo.
 
label variable t "Tendencia"
label variable treat "Tratamiento"
label variable treat_ "Tratamiento*Tendencia"
label variable valueipc_vestir_calzado "Vestimenta y Calzado"

extrdate month mes = fecha 
extrdate month year = fecha

reg valueipc_vestir_calzado treat t treat_t
outreg2 using "$output/Textiles_tabla.doc", nocons dec(2) label replace nonotes addnote("Errores estandar entre parentesis", "*** p<0.01, ** p<0.05, * p<0.1")

reg valueipc_vestir_calzado treat t treat_t i.mes

reg valueipc_vestir_calzado treat t treat_t, robust

reg valueipc_vestir_calzado treat t treat_t i.mes, robust

*Permutaciones de inferencia (Heß, 2017): dado que el número de observaciones es relativamente pequeño, realizamos un análisis de inferencia aleatoria mediante permutaciones basadas en simulaciones de Monte Carlo a través del comando ritest.
cap ssc install ritest

ritest treat (_b[treat]/_se[treat]), reps(1000) seed(123) kdensityplot: reg valueipc_vestir_calzado treat t treat_t

ritest treat (_b[treat]/_se[treat]), reps(1000) seed(123) kdensityplot: reg valueipc_vestir_calzado treat t treat_t i.mes

*Además, reportaremos los errores estándar consistentes con heterocedasticidad y autocorrelación de Newey y West (1994).
*El número óptimo de rezagos es el numero entero inferior de floor[4(31/100)^{2/9}]=3:
tsset t
unique t

newey valueipc_vestir_calzado treat t treat_t, lag(3)
outreg2 using "$output/Textiles_newey_tabla.doc", ctitle("Vestimenta y Calzado") dec(2) label replace nonotes addnote("Errores estandar consistentes con heterocedasticidad y autocorrelacion de Newey y West entre parentesis", "*** p<0.01, ** p<0.05, * p<0.1")
newey valueipc_vestir_calzado treat t treat_t i.mes, lag(3)


*===========================*
* 	      Placebos
*===========================*
*En esta sección, replicamos los análisis previamente realizados para otras subcategorías del IPC General que no se vieron afectadas por la modificación del SIRA. El objetivo es evaluar si los resultados son estadísticamente significativos en estos casos "placebo" y, de ser así, determinar si el efecto del Tratamiento*Tendencia es negativo en todos los meses considerados.
*Particularmente, las categorías elegidas corresponden a "Seguros médicos", "Seguros" y "Servicios de recreación".

label variable valueipc_seguros_med "Seguros médicos"
label variable valueipc_seguros "Seguros" 
label variable valueipc_recreacion "Servicios de recreación"

reg valueipc_vestir_calzado treat t treat_t
outreg2 using "$output/Textiles_tabla_placebos.tex", nocons ctitle("Vestimenta y Calzado") dec(2) label replace nonotes addnote("Errores estandar entre parentesis", "*** p<0.01, ** p<0.05, * p<0.1")

foreach var of varlist valueipc_seguros_med valueipc_seguros valueipc_recreacion {
    reg `var' treat t treat_t
	outreg2 using "$output/Textiles_tabla_placebos.tex", nocons dec(2) label append    
}

// Conclusión sobre los placebos: La intervención relacionada con la modificación del SIRA tuvo un impacto significativo y negativo exclusivamente en la evolución del IPC de "Vestimenta y Calzado" en comparación con el IPC general. En otras palabras, mientras que las demás categorías mostraron una tendencia positiva en el aumento de precios por encima del IPC general tras la modificación del SIRA, las prendas de vestir y calzado exhibieron una tendencia negativa.


*===========================*
* 	   Otros chequeos
*===========================*
import excel "$input/base_ipc_clean_final_norm.xlsx", firstrow clear
replace ipc_type = subinstr(ipc_type, "Prendas de vestir", "ipc_vestir_calzado", .)
replace ipc_type = subinstr(ipc_type, "Seguros médicos", "ipc_seguros_med", .)
replace ipc_type = subinstr(ipc_type, "Servicios de recreación", "ipc_recreacion", .)
replace ipc_type = subinstr(ipc_type, "Seguros", "ipc_seguros", .)
sort fecha ipc_type
drop primer_valor value
ren value_norm value
reshape wide value, i(fecha NivelGeneral empalmar) j(ipc_type) string

*Como diciembre de 2023 puede considerarse un mes atípico para todos los mercados, se considera analizar los resultados eliminando de nuestra muestra este mes. Es importante mencionar que los efectos hallados son consistente con lo discutido anteriormente.
sort fecha
drop if fecha<mdy(1,1,2022)
gen treat=0
replace treat=1 if fecha>=mdy(12,1,2023)
drop if fecha==mdy(12,1,2023)
gen t=_n
gen treat_t=treat*t

label variable t "Tendencia"
label variable treat "Tratamiento"
label variable treat_ "Tratamiento*Tendencia"
extrdate month mes = fecha 
extrdate month year = fecha

reg valueipc_vestir_calzado treat t treat_t
reg valueipc_vestir_calzado treat t treat_t i.mes
reg valueipc_vestir_calzado treat t treat_t, robust

label variable valueipc_seguros_med "Seguros médicos"
label variable valueipc_seguros "Seguros" 
label variable valueipc_recreacion "Servicios de recreación"

reg valueipc_vestir_calzado treat t treat_t
outreg2 using "$output/Textiles_tabla_placebos2.tex", nocons ctitle("Vestimenta y Calzado") dec(2) label replace nonotes addnote("Errores estandar entre parentesis", "*** p<0.01, ** p<0.05, * p<0.1")

foreach var of varlist valueipc_seguros_med valueipc_seguros valueipc_recreacion {
    reg `var' treat t treat_t
	outreg2 using "$output/Textiles_tabla_placebos2.tex", nocons dec(2) label append    
}

