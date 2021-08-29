# Sit-conviven-jovenes-25-a-34
Población entre 25 y 34 años que vive en casa de sus padres. Su relación con el número de hijos por mujer y sentimiento de felicidad

Es conocida la dificultad de 
independizarse de los jóvenes entre 25-34 años, 
ya sea por el precio de la vivienda/alquiler,
el escaso poder adquisitivo debido a los salarios o a
la inestabilidad en el empleo o la asociación de todo ello.

### **¿Qué porcentaje de jóvenes viven con sus padres en España?**
En España en el año 2019, el 46% de los españoles entre 25 y 
34 años de edad aún viven en casa de sus padres. 
Preocupante cifra, si lo comparamos con los países nórdicos
que apenas superan el 5%. O países como Francia, Reino Unido o Alemania
que se sitúan alrededor del 15%

### **¿Cuánto dista con respecto a países de nuestro entorno?**
Según datos de Eurostat, España está 16 puntos por encima 
de la media del año 2019 de los países analizados.

## **Problemática**

Puesto que puede concebirse la etapa que comprende entre 25 a 34 años
como período donde el deseo de desapegarse del calor paternal
es máximo. Razones no faltan, entre otras cosas,
han completado su formación académica,
tienen suficiente madurez como para tomar decisiones de forma autónoma,
el instinto de formar una familia empieza a despertar o 
simplemente apetece tener espacio como cualquier persona adulta. 

### **Relación entre jóvenes de 25 y 34 años que viven en casa de sus padres y número de hijos por mujer**

En los países occidentales más desarrollados, a día de hoy y 
en términos generales, la  opción de tener hijos y en qué número, 
parte principalmente de si a éstos se les puede atender correctamente. 
Una no emancipación del domicilio familiar,
implicaría que existen dificultades para encajar los salarios con el precio de la
vivienda y la cantidad/calidad del contrato de trabajo.

Un buen indicador, puede ser la **tasa de fertilidad**. Del conjunto de países analizados,
observamos que existen una notable correlación negativa donde el descenso del número de
jóvenes que viven con sus padres, implica un aumento del número de hijos por mujer.

En este caso, la recta ajustada por mínimos cuadrados no ilustra del todo
la estructura que siguen los datos. Incorporando la regresión local (loess) para una mejor
detección del patrón que siguen los datos. Altamente influenciado por los países bálcanicos.

Mediante k-means visualizamos a los países agrupados que comparten observaciones similares.

![](https://github.com/aaant987/Sit-conviven-jovenes-25-a-34/blob/master/p123.png)

### **¿Son los jóvenes entre 25-34 años que vive con sus padres menos felices con respecto a aquellos que han salido del hogar paternal?**

A vista de los datos existe una diferencia significativa entre el sentimiento de felicidad y 
la situación de convivencia. Siendo de media 0.5 puntos menos felices quienes viven en el 
domicilio paternal.

### **¿Qué probabilidad existe de que un joven no sea feliz y viva con sus padres?** 

Es plausible pensar que el estado de felicidad pueda influir en la **probabilidad** de vivir
con los padres. En este caso, de forma conjunta, aunque la tendencia es que conforme disminuye el estado de felicidad
aumenta la probabilidad de que el entrevistado viva con sus padres, en ningún tramo de la curva
supera el 0.5

### **¿En qué paises es más acusada la diferencia de felicidad entre aquellos que viven con sus padres con respecto a los que no?**

Como era de esperar, en la mayoría de países quienes aún viven en casa de sus padres 
son menos felices. Siendo en Islandia donde la diferencia es más pronunciada (-3.8).

![](https://github.com/aaant987/Sit-conviven-jovenes-25-a-34/blob/master/p456.png)

En definitiva, a tenor de los resultados no puede afirmarse por completo que vivir con los padres 
implique una menor felicidad entre los jóvenes de 25 y 34 años. Aunque si hay indicios de que mediante un 
estudio ad hoc al respecto, puede arrojarse mayor luz sobre el tema y ser más consistente en los resultados.
Por ejemplo preguntando a lo largo de un cuestionario, en primera instancia:
"Con respecto a su situación de convivencia en el domicilio que actualmente vive. En una escala de
0 a 10, ¿cuál es su sentimiento de felicidad?"
Para preguntar a posteriori qué relación les une con las personas donde convive: padres, pareja/cónyuge, hijos, solo, con compañeros, otra situación...
Añadiendo las típicas preguntas de clasificación socio económica para cerrar el cuestionario.
