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
	global main "dirección"
	}

*Crear carpetas de "input" (donde estará la base de datos) y "output" (para las tablas/gráficos):
global input "$main/input"
global output "$main/output"

cd "$main"

import excel "$input/base_ipc_clean_final_norm.xlsx", firstrow clear
keep if ipc_type=="Prendas de vestir" | ipc_type=="Seguros médicos" |  ipc_type=="Servicios de recreación" | ipc_type=="Seguros" | ipc_type=="Artículos textiles para el hogar" | ipc_type=="Electrodomesticos" | ipc_type=="Productos de recreacion"
*Aquí, la variable "value" representa el cociente entre la diferencia del IPC reportado para cada categoría y el IPC general, dividido por el IPC general menos 1. Luego, este cociente se multiplica por 100.

*Reordenamos la base de datos para las estimaciones:
replace ipc_type = subinstr(ipc_type, "Prendas de vestir", "ipc_vestir_calzado", .)
replace ipc_type = subinstr(ipc_type, "Seguros médicos", "ipc_seguros_med", .)
replace ipc_type = subinstr(ipc_type, "Servicios de recreación", "ipc_recreacion", .)
replace ipc_type = subinstr(ipc_type, "Seguros", "ipc_seguros", .)
replace ipc_type = subinstr(ipc_type, "Artículos textiles para el hogar", "ipc_blanqueria", .)
replace ipc_type = subinstr(ipc_type, "Electrodomesticos", "ipc_electro", .)
replace ipc_type = subinstr(ipc_type, "Productos de recreacion", "ipc_prod_recreacion", .)

sort fecha ipc_type
drop primer_valor value
ren value_norm value
reshape wide value, i(fecha NivelGeneral empalmar) j(ipc_type) string

*===========================*
*		ESTIMACIONES
*===========================*
*Se generaron dos variables clave para el análisis: una que captura la tendencia temporal durante el periodo estudiado, y otra que identifica los meses afectados por la modificación del SIRA. Los meses tratados son aquellos posteriores a la fecha de la modificación/derogación, por lo que se consideran como tratados las observaciones a partir de diciembre de 2023. Además, para minimizar el ruido en los datos, restringimos el análisis a un periodo cercano a la intervención, limitándolo a las variaciones observadas entre enero de 2021 y la actualidad.
sort fecha
gen treat=0
replace treat=1 if fecha>=mdy(12,1,2023)
gen t=_n

*Agregamos un control más a la estimación que se define como la interacción entre la variable de tratamiento y la tendencia:
gen treat_t=treat*t

*A continuación, se presentan las estimaciones utilizando un modelo "Before and After" con tendencia temporal. En todas las especificaciones, la variable de interés es la interacción entre el tratamiento y la tendencia temporal, es decir, Tratamiento*Tendencia. El coeficiente asociado a esta variable capturará el cambio en la tendencia tras la modificación del SIRA, permitiendo determinar si la intervención generó en promedio una disminución o un aumento en el valor relativo del IPC a lo largo del tiempo.
*De acuerdo con los resultados, el coeficiente de la interacción es negativo y significativo, lo que sugiere que, tras la intervención, los precios de las prendas de vestir y calzado, artículos de blanquería, productos recreativos (como juguetes, equipos deportivos y artículos de jardinería) y electrodomésticos comenzaron a disminuir en términos relativos a la inflación general a lo largo del tiempo.
 
label variable t "Tendencia"
label variable treat "Tratamiento"
label variable treat_ "Tratamiento*Tendencia"
label variable valueipc_vestir_calzado "Vestimenta y Calzado"
label variable valueipc_blanqueria "Blanquería"
label variable valueipc_electro "Electrodomésticos"
label variable valueipc_prod_recreacion "Prod. de recreación"

extrdate month mes = fecha 
extrdate month year = fecha

reg valueipc_vestir_calzado treat t treat_t
outreg2 using "$output/Elim_SIRA_tabla.tex", nocons dec(2) label replace nonotes addnote("Errores estandar entre parentesis", "*** p<0.01, ** p<0.05, * p<0.1")

reg valueipc_blanqueria treat t treat_t
outreg2 using "$output/Elim_SIRA_tabla.tex", nocons dec(2) label append nonotes addnote("Errores estandar entre parentesis", "*** p<0.01, ** p<0.05, * p<0.1")

reg valueipc_electro treat t treat_t
outreg2 using "$output/Elim_SIRA_tabla.tex", nocons dec(2) label append nonotes addnote("Errores estandar entre parentesis", "*** p<0.01, ** p<0.05, * p<0.1")

reg valueipc_prod_recreacion treat t treat_t
outreg2 using "$output/Elim_SIRA_tabla.tex", nocons dec(2) label append nonotes addnote("Errores estandar entre parentesis", "*** p<0.01, ** p<0.05, * p<0.1")


*Realizmaos otras estimaciones para Vestimenta y Calzado para chequear que los resultados son consistentes:
reg valueipc_vestir_calzado treat t treat_t i.mes

reg valueipc_vestir_calzado treat t treat_t, robust

reg valueipc_vestir_calzado treat t treat_t i.mes, robust

*Permutaciones de inferencia (Heß, 2017): dado que el número de observaciones es relativamente pequeño, realizamos un análisis de inferencia aleatoria mediante permutaciones basadas en simulaciones de Monte Carlo a través del comando ritest.
cap ssc install ritest

ritest treat (_b[treat]/_se[treat]), reps(1000) seed(123) kdensityplot: reg valueipc_vestir_calzado treat t treat_t

ritest treat (_b[treat]/_se[treat]), reps(1000) seed(123) kdensityplot: reg valueipc_vestir_calzado treat t treat_t i.mes

*Además, reportaremos los errores estándar consistentes con heterocedasticidad y autocorrelación de Newey y West (1994).
*El número óptimo de rezagos es el numero entero inferior de floor[4(43/100)^{2/9}]=3:
tsset t
unique t

newey valueipc_vestir_calzado treat t treat_t, lag(3)
outreg2 using "$output/Elim_SIRA_newey_tabla.tex", ctitle("Vestimenta y Calzado") dec(2) label replace nonotes addnote("Errores estandar consistentes con heterocedasticidad y autocorrelacion de Newey y West entre parentesis", "*** p<0.01, ** p<0.05, * p<0.1")
newey valueipc_vestir_calzado treat t treat_t i.mes, lag(3)

newey valueipc_blanqueria treat t treat_t, lag(3)
outreg2 using "$output/Elim_SIRA_newey_tabla.tex", ctitle("Vestimenta y Calzado") dec(2) label append nonotes addnote("Errores estandar consistentes con heterocedasticidad y autocorrelacion de Newey y West entre parentesis", "*** p<0.01, ** p<0.05, * p<0.1")

newey valueipc_electro treat t treat_t, lag(3)
outreg2 using "$output/Elim_SIRA_newey_tabla.tex", ctitle("Vestimenta y Calzado") dec(2) label append nonotes addnote("Errores estandar consistentes con heterocedasticidad y autocorrelacion de Newey y West entre parentesis", "*** p<0.01, ** p<0.05, * p<0.1")

newey valueipc_prod_recreacion treat t treat_t, lag(3)
outreg2 using "$output/Elim_SIRA_newey_tabla.tex", ctitle("Vestimenta y Calzado") dec(2) label append nonotes addnote("Errores estandar consistentes con heterocedasticidad y autocorrelacion de Newey y West entre parentesis", "*** p<0.01, ** p<0.05, * p<0.1")

*===========================*
* 	      Placebos
*===========================*
*En esta sección, replicamos los análisis previamente realizados para otras subcategorías del IPC General que no se vieron afectadas por la modificación del SIRA. El objetivo es evaluar si los resultados son estadísticamente significativos en estos casos "placebo" y, de ser así, determinar si el efecto del Tratamiento*Tendencia es negativo en todos los meses considerados.
*Particularmente, las categorías elegidas corresponden a "Seguros médicos", "Seguros" y "Servicios de recreación".

label variable valueipc_seguros_med "Seguros médicos"
label variable valueipc_seguros "Seguros" 
label variable valueipc_recreacion "Servicios de recreación"

reg valueipc_vestir_calzado treat t treat_t
outreg2 using "$output/Elim_SIRA_tabla_placebos.tex", nocons ctitle("Vestimenta y Calzado") dec(2) label replace nonotes addnote("Errores estandar entre parentesis", "*** p<0.01, ** p<0.05, * p<0.1")

foreach var of varlist valueipc_blanqueria valueipc_electro valueipc_prod_recreacion valueipc_seguros_med valueipc_seguros valueipc_recreacion {
    reg `var' treat t treat_t
	outreg2 using "$output/Elim_SIRA_tabla_placebos.tex", nocons dec(2) label append    
}

// Conclusión sobre los placebos: La intervención relacionada con la modificación del SIRA tuvo un impacto significativo y negativo exclusivamente en la evolución del IPC de "Vestimenta y Calzado" en comparación con el IPC general. En otras palabras, mientras que las demás categorías mostraron una tendencia positiva en el aumento de precios por encima del IPC general tras la modificación del SIRA, las prendas de vestir y calzado exhibieron una tendencia negativa.


*===========================*
* 	   Otros chequeos
*===========================*
import excel "$input/base_ipc_clean_final_norm.xlsx", firstrow clear
keep if ipc_type=="Prendas de vestir" | ipc_type=="Seguros médicos" |  ipc_type=="Servicios de recreación" | ipc_type=="Seguros" | ipc_type=="Artículos textiles para el hogar" | ipc_type=="Electrodomesticos" | ipc_type=="Productos de recreacion"
replace ipc_type = subinstr(ipc_type, "Prendas de vestir", "ipc_vestir_calzado", .)
replace ipc_type = subinstr(ipc_type, "Seguros médicos", "ipc_seguros_med", .)
replace ipc_type = subinstr(ipc_type, "Servicios de recreación", "ipc_recreacion", .)
replace ipc_type = subinstr(ipc_type, "Seguros", "ipc_seguros", .)
replace ipc_type = subinstr(ipc_type, "Artículos textiles para el hogar", "ipc_blanqueria", .)
replace ipc_type = subinstr(ipc_type, "Electrodomesticos", "ipc_electro", .)
replace ipc_type = subinstr(ipc_type, "Productos de recreacion", "ipc_prod_recreacion", .)
sort fecha ipc_type
drop primer_valor value
ren value_norm value
reshape wide value, i(fecha NivelGeneral empalmar) j(ipc_type) string



*Como diciembre de 2023 puede considerarse un mes atípico para todos los mercados, se considera analizar los resultados eliminando de nuestra muestra este mes. Es importante mencionar que los efectos hallados son consistente con lo discutido anteriormente.
sort fecha
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

label variable valueipc_vestir_calzado "Vestimenta y Calzado"
label variable valueipc_blanqueria "Blanquería"
label variable valueipc_electro "Electrodomésticos"
label variable valueipc_prod_recreacion "Prod. de recreación"
label variable valueipc_seguros_med "Seguros médicos"
label variable valueipc_seguros "Seguros" 
label variable valueipc_recreacion "Servicios de recreación"

reg valueipc_vestir_calzado treat t treat_t
outreg2 using "$output/Elim_SIRA_tabla_placebos2.tex", nocons ctitle("Vestimenta y Calzado") dec(2) label replace nonotes addnote("Errores estandar entre parentesis", "*** p<0.01, ** p<0.05, * p<0.1")

foreach var of varlist valueipc_blanqueria valueipc_electro valueipc_prod_recreacion valueipc_seguros_med valueipc_seguros valueipc_recreacion {
    reg `var' treat t treat_t
	outreg2 using "$output/Elim_SIRA_tabla_placebos2.tex", nocons dec(2) label append    
}

