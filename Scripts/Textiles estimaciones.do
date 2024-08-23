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

import excel "$input/base_ipc_clean_2.xlsx", firstrow clear
*Aquí, la variable "ipc_categoría del ICP" corresponde al cociente entre el IPC reportado para cada categoría y el IPC a nivel general, donde cada cociente se multiplica por 100.

*Reordenamos la base de datos para las estimaciones:
replace ipc_type = subinstr(ipc_type, "ipc_alimentos_bebidas_no_alcoh", "ipc_alim_beb_no_al", .)
replace ipc_type = subinstr(ipc_type, "ipc_bebidas_alcoh_tabaco", "ipc_beb_alcoh_tab", .)
replace ipc_type = subinstr(ipc_type, "ipc_vivienda_agua_elect_ots_combus", "ipc_viv_agua_elect", .)

sort fecha ipc_type

reshape wide value, i(fecha ipc_general empalmar) j(ipc_type) string

*===========================*
*		ESTIMACIONES
*===========================*
*Generamos la variable que define la tendencia en el tiempo del periodo analizado y aquella que define los meses tratados, que corresponden a todos aquellos que se encuentran después de la fecha que corresponde a la modificación del SIRA, por lo que tomamos como tratados las observaciones de diciembre de 2023 en adelante:
sort fecha
gen treat=0
replace treat=1 if fecha>=mdy(12,1,2023)
gen t=_n


*Agregamos un control más a la estimación que se define como la interacción entre la variable de tratamiento y la tendencia:
gen treat_t=treat*t

*A continuación se presentan las estimaciones, siguiendo un modelo de "Before and After" con tendencia temporal. En todos las especificaciones presentadas, el efecto total del tratamiento es igual al coeficiente asociado al "treat" más el producto entre el coeficiente de "treat_t" por el valor de la tendencia para la observación tratada.
*De acuerdo con los resultados, el efecto total promedio del tratamiento sobre la categorías de vestimenta y calzado resulta negativo y significativo, lo que implicaría que después de la intervención los precios de las prendas de vestir y calzado cayeron por debajo de la inflación general.
 
label variable t "Tendencia"
label variable treat "Tratamiento"
label variable treat_ "Tratamiento*Tendencia"

extrdate month mes = fecha 
extrdate month year = fecha

reg valueipc_vestir_calzado treat t treat_t
display ((8*456.3303-3.308631*138-3.308631*139-3.308631*140-3.308631*141-3.308631*142-3.308631*143-3.308631*144-3.308631*145-3.308631*146)/8) // El efecto promedio del tratamiento entre los meses tratados es de -72.223502
outreg2 using "$output/Textiles_tabla.doc", nocons ctitle("Vestimenta y calzado") dec(2) label replace nonotes addnote("Errores estandar entre parentesis", "*** p<0.01, ** p<0.05, * p<0.1", "El efecto promedio del tratamiento entre los meses tratados para la Vestimenta y Calzado es de -72.22")

reg valueipc_vestir_calzado treat t treat_t i.mes

reg valueipc_vestir_calzado treat t treat_t, robust

reg valueipc_vestir_calzado treat t treat_t i.mes, robust

*Permutaciones de inferencia (Heß, 2017): dado que el número de observaciones es relativamente pequeño, realizamos un análisis de inferencia aleatoria mediante permutaciones basadas en simulaciones de Monte Carlo a través del comando ritest.
cap ssc install ritest

ritest treat (_b[treat]/_se[treat]), reps(1000) seed(123) kdensityplot: reg valueipc_vestir_calzado treat t treat_t

ritest treat (_b[treat]/_se[treat]), reps(1000) seed(123) kdensityplot: reg valueipc_vestir_calzado treat t treat_t i.mes

*Además, reportaremos los errores estándar consistentes con heterocedasticidad y autocorrelación de Newey y West (1994).
*El número óptimo de rezagos es el numero entero inferior de floor[4(145/100)^{2/9}]=4:
tsset t
unique t

newey valueipc_vestir_calzado treat t treat_t, lag(4)
outreg2 using "$output/Textiles_newey_tabla.doc", ctitle("Vestimenta y Calzado") dec(2) label replace nonotes addnote("Errores estandar consistentes con heterocedasticidad y autocorrelacion de Newey y West entre parentesis", "*** p<0.01, ** p<0.05, * p<0.1", "El efecto promedio del tratamiento entre los meses tratados para la Vestimenta y Calzado es de -72.22")
newey valueipc_vestir_calzado treat t treat_t i.mes, lag(4)
 


*===========================*
* 	      Placebos
*===========================*
*En esta sección, replicamos los análisis previamente realizados para otras categorías comparables con "Vestimenta y Calzado". El objetivo es evaluar si los resultados son estadísticamente significativos y, en caso afirmativo, determinar si el efecto promedio total del tratamiento es negativo en todos los meses considerados.
*Particularmente, las categorías elegidas corresponden a "Alimentos y Bebidas no Alcoh.", "Restaurante y Hoteles", "Salud", "Transporte".

ren valueipc_alim_beb_no_al alim_beb_no_al
ren valueipc_restaurantes_hoteles restaurantes_hoteles
ren valueipc_salud salud
ren valueipc_transporte transporte

reg valueipc_vestir_calzado treat t treat_t
outreg2 using "$output/Textiles_tabla_placebos.tex", nocons ctitle("Vestimenta y Calzado") dec(2) label replace nonotes addnote("Errores estandar entre parentesis", "*** p<0.01, ** p<0.05, * p<0.1", "El efecto promedio del tratamiento entre los meses tratados para la Vestimenta y Calzado es de -72.22")

foreach var of varlist alim_beb_no_al restaurantes_hoteles salud transporte {
    reg `var' treat t treat_t
	outreg2 using "$output/Textiles_tabla_placebos.tex", nocons ctitle(`var') dec(2) label append    
}

*Ahora, analizamos los efectos totales promedio para las categorías donde los resultados resultan estadísticamente significativo;

display(229.6422 -1.502798*138)
display(229.6422 -1.502798*145)
//Alimentos y bebidas no alcoholicas: el efecto total promedio del tratamiento para esta categoría es de 22.256076 para el 1° mes tratado y de 11.73649 para el último mes. Esto implica que el efecto total promedio siempre fue positivo sobre el IPC relativo, contrario a lo observado en vestimenta y calzado.

display ((-137.2034*8 +.9801827*138 +.9801827*139 +.9801827*140 +.9801827*141 +.9801827*142 +.9801827*143 +.9801827*144  +.9801827*145)/8)
// Salud: En este caso, podemos observar que el efecto total promedio del tratamiento entre los meses tratados para la categoría de Salud es positivo.

// Conclusión sobre los placebos: La intervención relacionada con la modificación del SIRA afectó de manera significativa y negativa únicamente la evolución del IPC de "Vestimenta y Calzado" en comparación con el IPC general. En otras palabras, mientras que las demás categorías registraron un aumento promedio de precios por encima del IPC general, las prendas de vestir y calzado mostraron variaciones de precios negativas, es decir, incrementos por debajo de la inflación general.


*===========================*
* 	   Otros chequeos
*===========================*
import excel "$input/base_ipc_clean_2.xlsx", firstrow clear
replace ipc_type = subinstr(ipc_type, "ipc_alimentos_bebidas_no_alcoh", "ipc_alim_beb_no_al", .)
replace ipc_type = subinstr(ipc_type, "ipc_bebidas_alcoh_tabaco", "ipc_beb_alcoh_tab", .)
replace ipc_type = subinstr(ipc_type, "ipc_vivienda_agua_elect_ots_combus", "ipc_viv_agua_elect", .)
sort fecha ipc_type
reshape wide value, i(fecha ipc_general empalmar) j(ipc_type) string

*Como diciembre de 2023 puede considerarse un mes atípico para todos los mercados, se considera analizar los resultados eliminando de nuestra muestra este mes. Es importante mencionar que los efectos totales promedios observados de la intervención son consistentes con los resultados hallados anteriormente.
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

ren valueipc_alim_beb_no_al alim_beb_no_al
ren valueipc_restaurantes_hoteles restaurantes_hoteles
ren valueipc_salud salud
ren valueipc_transporte transporte

reg valueipc_vestir_calzado treat t treat_t
outreg2 using "$output/Textiles_tabla_placebos2.tex", nocons ctitle("Vestimenta y Calzado") dec(2) label replace nonotes addnote("Errores estandar entre parentesis", "*** p<0.01, ** p<0.05, * p<0.1", "El efecto promedio del tratamiento entre los meses tratados para la Vestimenta y Calzado es de -14.1")

foreach var of varlist alim_beb_no_al restaurantes_hoteles salud transporte {
    reg `var' treat t treat_t
	outreg2 using "$output/Textiles_tabla_placebos2.tex", nocons ctitle(`var') dec(2) label append    
}

// Vestimenta y calzado:
display ((349.7073*7  -2.580196*138  -2.580196*139  -2.580196*140  -2.580196*141 + -2.580196*142  -2.580196*143  -2.580196*144)/7) //negativo

// Alimentos y bebidas no al.:
display(308.3059-2.066995*138) //positivo
display(308.3059-2.066995*144) //positivo

// Restaurantes y hoteles:
display(236.8785-1.589042*138) //positivo
display(236.8785-1.589042*144) //positivo

// Salud:
display(-16.96719+.1401236*138) //positivo
display(-16.96719+.1401236*144) //positivo

//Transporte:
display(153.2285*7-1.07993*138-1.07993*139-1.07993*140-1.07993*141-1.07993*142-1.07993*145-1.07993*144)/7 //positivo




