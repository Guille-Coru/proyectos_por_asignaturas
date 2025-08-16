#Script tarea 2 Programación Matemática

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##                                                                            ##
###~~~~~~~~~~~~~~~~~~~~~~~Inicialización común~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
##                                                                            ##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
{ #Clicando en la flecha a la dcha del nº de linea esa llave pliega toda la sección
library(pracma) #libreria con la  función Norm, con la que calcular cómodamente
#la norma de un vector (norm() solo acepta matrices). La N mayusc importa 

n_iteraciones=40 #guardo en el mismo sitio el punto de partida 
#y las posiciones de las 9 iteraciones

dimensiones=2 # Usado en algunos sitios
#por si en un futuro amplio el código a dimensión arbitraria

xini=c(5,-9)
#xini=c(56,19)#Punto inicial. el script no lo va a cambiar
#durante los algoritmos. puede ir cambiandose y probar distintos puntos de 
#arranque. Cuanto mas se aleje del centro peor parece comportarse todo

# fobj=function(x_1,x_2){
#   z=2*cos(sqrt(x_1^2+x_2^2))+(sqrt(x_1^2+x_2^2))/6-log(x_1^2+sin(x_1-x_2)+3)
# }#Función Objetivo
fobj=function(x,y){
  z=-log(sin(x-y)+x^2+3)+2*cos(sqrt(x^2+y^2))+sqrt(x^2+y^2)/6
}

valiter=matrix(numeric(length=(n_iteraciones+1)*dimensiones),ncol = dimensiones)
#Matriz donde guardaremos los distintos (x_1^i,x_2^i)
## colnames opcional opcional##
##colnames(valiter)=c("x_1^i","x_2^i") 

valiter[1,1:2]=xini #almacenamos punto de partida

fobjvec=numeric(n_iteraciones) #aquí almacenaremos el valor de
#la función objetivo en cada iteracion

fobjvec[1]=fobj(xini[1],xini[2]) #no le vale fobj(xini), caprichos de sintaxis

checkparada=numeric(n_iteraciones-1) #Valor a comparar con Epsilon. Guardado
#Para comprobar si el algoritmo escrito se comporta como sería esperado

dirdescenso=matrix(numeric(length = (n_iteraciones)*dimensiones)
                   ,ncol=dimensiones) #Aquí se almacenará x^k-x^(k-1)

dirnorm=matrix(numeric(length = (n_iteraciones)*dimensiones),ncol=dimensiones)
#almacenamos la dirección de descenso normalizada

tamaño_paso=numeric(length=n_iteraciones)#almacenaremos norma de (x^k-x^(k-1))

convergio=0 #pasa a 1 si el algoritmo converge en menos pasos que n_iteraciones

iteraciones_hasta_parar=10 #si converge durante el proceso el algoritmo
#actualizará el valor
}#esto hace plegable toda la sección. Clicar en la flecha a la dcha del nº de linea

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##                                                                            ##
###~~~~~~~~~~~~~~~~~~~~~~Fin inicialización común~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
##                                                                            ##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##                                                                            ##
###~~~~~~~~~~~~~~~~~Algoritmo de descenso por coordenadas~~~~~~~~~~~~~~~~~~~~###
##                                                                            ##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
{ #esto hace plegable (flecha a la dcha del número de linea) todo el algoritmo
yini=xini
ymat=matrix(numeric(length=n_iteraciones*dimensiones*2)-dimensiones,
            ncol = dimensiones) 
ymat[1,]=yini
fobjeny=numeric(dim(ymat)[1]-1)
fobjeny[1]=fobjvec[1]
#Para almacenar los "pasos parciales" para alguna representación. 
#Fijarse que en la figura 4.12 de los apuntes no se representan las x^k. Si no 
#que se representan los distintos valores de y^2, y^3 en cada repetición del
#Paso 1 en el algoritmo
#
epsi=0.0001 #modificable al gusto
metodo_parada=2 #1 implica criterio parada diferencia entre (x^k y x^(k-1))/Norm(x^(k-1))
#2 implica criterio de parada basado en |fobjetivo(x^k)-fobjetivo(x^(k-1))|

#Voy a utilizar distintas funciones para la búsqueda en linea, ya implementadas en 
#R
for (i in 2:n_iteraciones){
  # yaux1=optimize(f=function(lambda) fobj(yini[1]+lambda,yini[2])
  #                ,maximum = F,interval = c(-100,100)) #he preferido nlm()
  f_linea1=function(lambda){
    fobj(yini[1]+lambda,yini[2])
  }
  yaux1=nlm(f_linea1,p=0)
  #Optimiza en x_1
  yini[1]=yini[1]+yaux1$estimate 
  ymat[2*(i-1),]=yini #actualización parcial
  fobjeny[2*(i-1)]=fobj(yini[1],yini[2]) #evaluación paso parcial
  #la función nlm devuelve un vector con el minimo
  #y el valor de la función objetivo en dicho mínimo. Almacenamos aquí el minimo 
  f_linea2=function(lambda){
    fobj(yini[1],yini[2]+lambda)
  }
  yaux2=nlm(f_linea2,p=0)
  yini[2]=yini[2]+yaux2$estimate
  # yaux2=optimize(f=function(lambda) fobj(yini[1],yini[2]+lambda)
  #                ,maximum=F,interval=c(-100,100)) #Igual que con x_1
  # yini[2]=yini[2]+yaux2$minimum #Igual que con x_1
  ymat[2*i-1,]=yini #actialización parcial
  #####recogemos valores (seguimos dentro del bucle for aquí)######
  
  fobjvec[i]=fobj(yini[1],yini[2]) ##alternativamente yaux2$minimum
  fobjeny[2*i-1]=fobjvec[i]
  dirdescenso[i,]=c(yaux1$estimate,yaux2$estimate)
  tamaño_paso[i]=Norm(dirdescenso[i,])
  dirnorm[i,]=dirdescenso[i,]/Norm(dirdescenso[i,]) #normalizamos
  valiter[i,1:2]=yini#almacena el recorrido de las x^j a lo largo del algoritmo
  if(metodo_parada==1){
    checkparada[i-1]=(Norm(valiter[i,1:2]-valiter[i-1,1:2]))/(Norm(valiter[i-1,1:2])) 
    #Norm es la funcion de pracma, norm es para matrices. La mayuscula importa
    if(checkparada[i-1]<=epsi){ #Criterio parada y recortar tamaños
      convergio=1
      iteraciones_hasta_parar=i-1
      valiter=valiter[1:(iteraciones_hasta_parar+1),] #recortamos la matriz
      dirnorm=dirnorm[1:(iteraciones_hasta_parar+1),] #esta también lo cortamos
      #en la siguiente linea
      ymat=ymat[1:(2*(iteraciones_hasta_parar)+1),] #Y esta matriz
      tamaño_paso=tamaño_paso[1:(iteraciones_hasta_parar+1)] #este vector también
      fobjvec=fobjvec[1:(iteraciones_hasta_parar+1)]#y este
      fobjeny=fobjeny[1:(2*(iteraciones_hasta_parar)+1)] #y este también
      break
    }
  }
  if(metodo_parada==2){
    checkparada[i-1]=abs((fobjvec[i]-fobjvec[i-1])) 
    #Criterio de parada modificado
    if(checkparada[i-1]<=epsi){
      convergio=1
      iteraciones_hasta_parar=i-1
      valiter=valiter[1:(iteraciones_hasta_parar+1),] #recortamos la matriz
      dirnorm=dirnorm[1:(iteraciones_hasta_parar+1),] #esta también lo cortamos
      #en la siguiente linea
      ymat=ymat[1:(2*(iteraciones_hasta_parar)+1),] #Y esta matriz
      tamaño_paso=tamaño_paso[1:(iteraciones_hasta_parar+1)] #este vector también
      fobjvec=fobjvec[1:(iteraciones_hasta_parar+1)]#y este
      fobjeny=fobjeny[1:(2*(iteraciones_hasta_parar)+1)] #y este también
      break
    }
  }
  if(i==n_iteraciones){
    valiter=valiter[1:iteraciones_hasta_parar,]#esta linea arregla el error
    # cortando valiter la cosa es que valiter está hecha con una fila más que 
    #la necesaria  si converge en el último paso el criterio de parada busca 
    #valiter[1:(iteracioneshastaparar+1),]siendo iteraciones hasta parar=10
    # busca la fila 11 
  }
}#puede dar un error de limites con valiter si no converge. 
#Los datos se han recogido igualmente y el procedimiento sigue
ymat
valiter
fobjvec
###Funciona
  
##Observación de lo obtenido y posibles comentarios

convergio #1 implica que el método ha convergido en el nº de iteraciones
# prefijadas o menos, 0 significa que no

iteraciones_hasta_parar

# observaciones opcionales
#fijarse en la linea de abajo aparece a una flecha para plegar/desplegar
# {
# checkparada#valor de lo que comparamos con epsilon en cada paso 
# objetivo_en_final=fobj(valiter[iteraciones_hasta_parar+1,1]
#                        ,valiter[iteraciones_hasta_parar+1,2])
# objetivo_inicial=fobj(xini[1],xini[2])
# mejora=objetivo_inicial-objetivo_en_final
# mejora
# } 
{
  checkparada 
  if(convergio==1){
    objetivo_en_final=fobj(valiter[iteraciones_hasta_parar+1,1] 
                           ,valiter[iteraciones_hasta_parar+1,2])
    objetivo_inicial=fobj(xini[1],xini[2])
    mejora=objetivo_inicial-objetivo_en_final
    mejora}
  else{
    objetivo_en_final=fobj(valiter[iteraciones_hasta_parar,1] 
                           ,valiter[iteraciones_hasta_parar,2])
    objetivo_inicial=fobj(xini[1],xini[2])
    mejora=objetivo_inicial-objetivo_en_final
    mejora
  }
}

#Obtención de las tablas en código LaTeX sin tener que escribir cada valor
#install.packages("xtable") ##instalar si no se tiene instalada ya##

library(xtable)
tabla_descecords=cbind(valiter,fobjvec,dirnorm,tamaño_paso)
colnames(tabla_descecords)=c("x_1^t","x_2^t","fobj(x_1^t,x_2^t)"
                             ,"dirección normalizada 1","dirección normalizada 2","tamaño de paso")
latex_xtcoords=xtable(tabla_descecords,digits = 6)
titulo_tabla="Posiciones recorridas"
print(latex_xtcoords, type="latex")

##para crear un txt con los datos 
tablax=capture.output(print(latex_xtcoords, type="latex"))
write(tablax,file="tabla_con_las_x.txt")
#fin crear un txt

#Notar que la primera fila de algunos items es nula pues no tiene sentido 
#la dirección de descenso tomada al escoger el punto de partida. Se anulará
#en postproducción 
tabla_deygriegas=cbind(ymat,fobjeny)
colnames(tabla_deygriegas)=c("y_1","y_2","fobj(y_1,y_2)")
latex_ycoords=xtable(tabla_deygriegas,type="latex")

##Zona representación gráfica con funcion filled.contour

rejx1=seq(min(valiter[,1])-0.4,max(valiter[,1])+0.4,by=0.2) #rango con margen de x_1 
rejx2=seq(min(valiter[,2])-0.4,max(valiter[,2])+0.4,by=0.2) #rango con margen de x_2
evalz=outer(rejx1,rejx2,FUN="fobj")
source("Representando_fun_aux.r")#tener esto en el directorio de trabajo
filled.contour2(rejx1,rejx2,evalz, levels=seq(min(fobjvec)-1.9,max(fobjvec)+1.9,by=0.25))


#bucle para los puntos
ymat=ymat[1:19,] #da problema el recorte
for(i in 1:(dim(ymat)[1])){
  points(ymat[i,1],ymat[i,2],col="orange",pch=19,cex=1.2)
}

#bucle para lineas, se puede hacer todo en un único bucle
#pero si se va ejecutando linea a linea se ve la construcción

for(i in 1:(dim(ymat)[1])){
  lines(ymat[,1],ymat[,2],col="red",cex=1.5)
}
#notar que realmente los x^t son los elementos de valiter y no los de ymat.


for(i in 1:(dim(valiter)[1])){
  points(valiter[i,1],valiter[i,2],col="blue",pch=19,cex=1.2)
}

#bucle para lineas, se puede hacer todo en un único bucle
#pero si se va ejecutando linea a linea se ve la construcción

for(i in 1:(dim(ymat)[1])){
  lines(valiter[,1],valiter[,2],col="green",cex=1.5)

}
} #esto hace plegable toda la sección. Click en la flechita a la dcha del nº de línea
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##                                                                            ##
###~~~~~~~~~~~~~~~~~~~~~~Fin Descenso por coordenadas~~~~~~~~~~~~~~~~~~~~~~~~###
##                                                                            ## 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##                                                                            ##
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~Hooke y Jeeves~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
##                                                                            ## 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
{
#Este algoritmo es muy similar a descenso por coordenadas. Sólo requiere añadir
#una etapa de aceleración 

epsi=0.0001
yuno=xini
yini=xini
ymat=matrix(numeric(length=n_iteraciones*dimensiones*2)-dimensiones,ncol = dimensiones)
ymat[1,]=yini
fobjeny=numeric(dim(ymat)[1]-1)
fobjeny[1]=fobjvec[1]
metodo_parada=1 #1 implica criterio parada diferencia entre (x^k y x^(k-1))/Norm(x^(k-1))
#2 implica criterio de parada basado en |fobjetivo(x^k)-fobjetivo(x^(k-1))
for (i in 2:n_iteraciones){
  f_linea1=function(lambda){
    fobj(yini[1]+lambda,yini[2])
  }
  yaux1=nlm(f_linea1,p=0)
  yini[1]=yini[1]+yaux1$estimate 
  ymat[2*(i-1),]=yini #actualización parcial
  
  f_linea2=function(lambda){
    fobj(yini[1],yini[2]+lambda)
  }
  yaux2=nlm(f_linea2,p=0)
  yini[2]=yini[2]+yaux2$estimate #Igual que con x_1
  ymat[2*i-1,]=yini #actialización parcial
  #####ETAPA DE ACELERADO####
  dist=(yini-valiter[i-1,]) #el vector 
  f_linea3=function(lambda){
    fobj(yini[1]+lambda*dist[1],yini[2]+lambda*dist[1])
  }
  yaux3=nlm(f_linea3,p=0)
  valiter[i,1:2]=c(yini[1]+(yaux3$estimate)*dist[1],yini[2]+(yaux3$estimate)*dist[2])
  yini=valiter[i,1:2]
  ####Fin etapa  acelerado
  
  #####recogemos valores (seguimos dentro del bucle for aquí)######
  
  fobjvec[i]=yaux3$minimum #alternativamente fobj(yini[1],yini[2])
  #fobjeny[2*i-1]=fobjvec[i]
  dirdescenso[i,]=c((yaux3$estimate)*dist[1],(yaux3$estimate)*dist[2])
  tamaño_paso[i]=Norm(dirdescenso[i,])
  dirnorm[i,]=dirdescenso[i]/tamaño_paso[i] #normalizamos
  #valiter[i,1:2]=yini#almacena el recorrido de las x^j a lo largo del algoritmo
  if(metodo_parada==1){
    checkparada[i-1]=(Norm(valiter[i,1:2]-valiter[i-1,1:2]))/(Norm(valiter[i-1,1:2])) 
    #Norm es la funcion de pracma, norm es para matrices. La mayuscula importa
    if(checkparada[i-1]<=epsi){ #Criterio parada y recortar tamaños
      convergio=1
      iteraciones_hasta_parar=i-1
      valiter=valiter[1:(iteraciones_hasta_parar+1),] #recortamos la matriz
      dirnorm=dirnorm[1:(iteraciones_hasta_parar+1),] #esta también lo cortamos
      #en la siguiente linea
      ymat=ymat[1:(2*(iteraciones_hasta_parar)+1),] #Y esta matriz
      tamaño_paso=tamaño_paso[1:(iteraciones_hasta_parar+1)] #este vector también
      fobjvec=fobjvec[1:(iteraciones_hasta_parar+1)]#y este
      fobjeny=fobjeny[1:(2*(iteraciones_hasta_parar)+1)] #y este también
      break
    }
  }
  if(metodo_parada==2){
    checkparada[i-1]=abs((fobjvec[i]-fobjvec[i-1])) 
    #Criterio de parada modificado
    if(checkparada[i-1]<=epsi){
      convergio=1
      iteraciones_hasta_parar=i-1
      valiter=valiter[1:(iteraciones_hasta_parar+1),] #recortamos la matriz
      dirnorm=dirnorm[1:(iteraciones_hasta_parar+1),] #esta también lo cortamos
      #en la siguiente linea
      ymat=ymat[1:(2*(iteraciones_hasta_parar)+1),] #Y esta matriz
      tamaño_paso=tamaño_paso[1:(iteraciones_hasta_parar+1)] #este vector también
      fobjvec=fobjvec[1:(iteraciones_hasta_parar+1)]#y este
      fobjeny=fobjeny[1:(2*(iteraciones_hasta_parar)+1)] #y este también
      break
    }
  }
  if(i==n_iteraciones){
    valiter=valiter[1:iteraciones_hasta_parar,]
    }
}
}
{#Observaciones
valiter
fobjvec

 {
   checkparada 
   if(convergio==1){
   objetivo_en_final=fobj(valiter[iteraciones_hasta_parar+1,1] 
                          ,valiter[iteraciones_hasta_parar+1,2])
   objetivo_inicial=fobj(xini[1],xini[2])
   mejora=objetivo_inicial-objetivo_en_final
   mejora}
   else{
     objetivo_en_final=fobj(valiter[iteraciones_hasta_parar,1] 
                            ,valiter[iteraciones_hasta_parar,2])
     objetivo_inicial=fobj(xini[1],xini[2])
     mejora=objetivo_inicial-objetivo_en_final
     mejora
   }
 }

convergio
}
tabla_hookes=cbind(valiter,fobjvec,dirnorm,tamaño_paso)
colnames(tabla_hookes)=c("x_1^t","x_2^t","fobj(x_1^t,x_2^t)",
                         "dirección normalizada 1","dirección normalizada 2",
                         "tamaño de paso")
latex_xtcoords=xtable(tabla_hookes,digits = 6)
titulo_tabla="Posiciones recorridas"
print(latex_xtcoords, type="latex")
print(xtable(dirdescenso,digits = 6),type="latex")#por el fallo en la normalizacion

#para archivo txt
tablax=capture.output(print(latex_xtcoords, type="latex"))
write(tablax,file="tabla_con_las_x.txt")
#fin obtención txt


##Zona representación gráfica con funcion filled.contour
ymat=ymat[1:(dim(ymat)[1]-1),] #ymat da problemas en la ultima posicion

rejx1=seq(min(valiter[,1])-0.5,max(valiter[,1])+0.5,by=0.2) #rango con margen de x_1 
rejx2=seq(min(valiter[,2])-0.5,max(valiter[,2])+0.5,by=0.2) #rango con margen de x_2
evalz=outer(rejx1,rejx2,FUN="fobj")
source("Representando_fun_aux.r")#tener esto en el directorio de trabajo
filled.contour2(rejx1,rejx2,evalz, levels=seq(min(fobjvec)-1.9,max(fobjvec)+1.9,by=0.35))

#bucle para los puntos de pasos intermedios (coge las y^{n+1})

for(i in 1:(dim(ymat)[1])){
  points(ymat[i,1],ymat[i,2],col="orange",pch=19,cex=1.2)
}

#bucle para lineas de pasos intermedios (coge las y^{n+1})
#se puede hacer todo en un único bucle pero si se va ejecutando
#linea a linea se ve la construcción

for(i in 1:(dim(ymat)[1])){
  lines(ymat[,1],ymat[,2],col="red",cex=1.5)
} #notar que no son lineas perfectamente horizontales 
#como en descenso por coordenadas


#bucle para los x^t

for(i in 1:(dim(valiter)[1])){
  points(valiter[i,1],valiter[i,2],col="blue",pch=19,cex=1.2)
}

#bucle para lineas x^t
#se puede hacer todo en un único bucle pero si se va ejecutando
#linea a linea se ve la construcción

for(i in 1:(dim(valiter)[1])){
  lines(valiter[,1],valiter[,2],col="green",cex=1.5)
} #notar que no son lineas perfectamente horizontales 
#como en descenso por coordenadas

#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##                                                                            ##
###~~~~~~~~~~~~~~~~~~~~~~~~~~Fin Hooke y Jeeves~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
##                                                                            ## 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##                                                                            ##
###~~~~~~~~~~~~~~~~~~~~~~~~~~~Máximo Descenso~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
##                                                                            ## 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
epsi=0.01
epsilinea=1.0e-4
yuno=xini
yini=xini

parcial1=function(x_1,x_2){
  -(2 * x_1 * sin(sqrt(x_1^2 + x_2^2))) / sqrt(x_1^2 + x_2^2) -
    (cos(x_1 - x_2) + 2 * x_1) / (sin(x_1 - x_2) +
                                    x_1^2 + 3) + x_1 / (6 * sqrt(x_1^2 + x_2^2))
}

parcial2=function(x_1,x_2){
  -(2 * x_2 * sin(sqrt(x_2^2 + x_1^2))) / sqrt(x_2^2 + x_1^2) +
    cos(x_2 - x_1) / (-sin(x_2 - x_1) + x_1^2 + 3) +
    x_2 / (6 * sqrt(x_2^2 + x_1^2))
}

###################CON DEoptim (hace aparecer muchas lineas en la consola)######
library(DEoptim)
for (i in 1:(n_iteraciones-1)){ #Ojo empieza en 1 esta vez pues
  #el algoritmo empieza con el test de parada
  dirdescenso[i,]=-c(parcial1(yini[1],yini[2]),parcial2(yini[1],yini[2]))
  checkparada[i]=Norm(dirdescenso)
  dirnorm[i,]=dirdescenso[i,]/Norm(dirdescenso[i,])
  if (checkparada[i]<=epsi){
    valiter=valiter[1:i,]#recortamos valiter
    fobjvec=fobjvec[1:i]#recortamos el vector de objetivos en cada paso
    convergio=1
    iteraciones_hasta_parar=i
    tamaño_paso=tamaño_paso[1:(i+1),]
    break
  }else{
    #optimización unidimensional
    f_linea1=function(lambda){
      fobj(yini[1]+lambda*dirnorm[i,1],yini[2]+lambda*dirnorm[i,2])
    }
    
    lambdaux=DEoptim(f_linea1,lower=0,upper=100)
                                              
    #ojo ahora optimizamos con lambda siempre positivo
    #recogemos los valores
    tamaño_paso[i+1]=lambdaux$optim$bestmem #ahora está todo normalizado
    fobjvec[i+1]=lambdaux$optim$bestval #objetivo tras avanzar
    valiter[i+1,]=c(yini[1]+lambdaux$optim$bestmem*dirnorm[i,1],
    yini[2]+lambdaux$optim$bestmem*dirnorm[i,2]) #punto al que nos desplazamos
    yini=valiter[i+1,] #Actualizamos yini 
  }
  if(i==9){
    valiter=valiter[1:10,]#recortamos valiter
    fobjvec=fobjvec[1:10]#recortamos el vector de objetivos en cada paso
    convergio=0
    tamaño_paso=tamaño_paso[1:(10)]
  }
    
}
###
################Fin DEoptim########
###
####prueba Con nloptr# funciona mal
##install.package("nloptr") #instalar si necesario
library(nloptr)
for (i in 1:(n_iteraciones)){ #Ojo empieza en 1 esta vez pues el algoritmo
  #empieza con el test de parada
  dirdescenso[i,]=c(-parcial1(yini[1],yini[2]),-parcial2(yini[1],yini[2]))
  checkparada[i]=Norm(dirdescenso[i,])
  #dirnorm[i+1,]=dirdescenso[i,]/checkparada[i]
  if (checkparada[i]<=epsi){
    valiter=valiter[1:i,]#recortamos valiter
    fobjvec=fobjvec[1:i]#recortamos el vector de objetivos en cada paso
    convergio=1
    iteraciones_hasta_parar=i
    tamaño_paso=tamaño_paso[1:(i+1),]
    break
  }else{
    #optimización unidimensional
    f_linea1=function(lambda){
      fobj(yini[1]+lambda*dirdescenso[i,1],yini[2]+lambda*dirdescenso[i,2])
    }
    
    lambdaux=nloptr(x0=1,
                    eval_f = f_linea1,lb=0,ub=100,
                    opts=list(algorithm="NLOPT_GN_DIRECT_L",
                              xtol_rel=epsilinea))
    
    
    #ojo ahora optimizamos con lambda siempre positivo
    #recogemos los valores
    tamaño_paso[i+1]=Norm(dirdescenso)*lambdaux$solution #está todo normalizado
    fobjvec[i+1]=lambdaux$objective #objetivo tras avanzar
    valiter[i+1,]=c(yini[1]+lambdaux$solution*dirdescenso[i,1],
                    yini[2]+lambdaux$soltuion*dirdescenso[i,2]) #punto al que nos hemos desplazado
    yini=valiter[i+1,] #Actualizamos yini 
  }
}
valiter #falló recortando 
valiter=valiter[1:10,]#ejecutar si en el caso probado también falla recortando
fobjvec #tiene el mismo problema que cuando intenté optimize
#te da puntos a avanzar peores que el actual...

#para obtener las tablas

tabla_maxdes=cbind(valiter,fobjvec,dirnorm,tamaño_paso)
for (i in 1:length)
colnames(tabla_maxdes)=c("x_1^t","x_2^t","fobj(x_1^t,x_2^t)",
                         "dirección normalizada 1","dirección normalizada 2",
                         "tamaño de paso")
latex_xtcoords=xtable(tabla_maxdes,digits = 6)
titulo_tabla="Posiciones recorridas"
print(latex_xtcoords, type="latex")

##Zona representación gráfica con funcion filled.contour

rejx1=seq(min(valiter[,1])-0.41,max(valiter[,1])+0.41,by=0.2) #rango con margen de x_1 
rejx2=seq(min(valiter[,2])-0.41,max(valiter[,2])+0.41,by=0.2) #rango con margen de x_2
evalz=outer(rejx1,rejx2,FUN="fobj")
source("Representando_fun_aux.r")#tener esto en el directorio de trabajo
filled.contour2(rejx1,rejx2,evalz, levels=seq(min(fobjvec)-3.9,max(fobjvec)+3.9,by=0.35))

#bucle para los x^t

for(i in 1:(dim(valiter)[1])){
  points(valiter[i,1],valiter[i,2],col="blue",pch=19,cex=1.2)
}

#bucle para lineas x^t
#se puede hacer todo en un único bucle pero si se va ejecutando
#linea a linea se ve la construcción

for(i in 1:(dim(valiter)[1])){
  lines(valiter[,1],valiter[,2],col="green",cex=1.5)
} #se va desplazando casi en la misma recta salvo en los primeros pasos. Como 
#se puede ver en dirdescenso o dirnorm realmente no es la misma recta
#pero se tiene que alejar tanto el zoom que no se aprecia

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##                                                                            ##
###~~~~~~~~~~~~~~~~~~~~~~~~~~Fin Máximo Descenso~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
##                                                                            ## 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##                                                                            ##
###~~~~~~~~~~~~~~~~~~~~~~~~~~~Método de Newton~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
##                                                                            ## 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
epsi=0.01
epsilinea=1.0e-4
yini=xini
#necesitamos traer las parciales aquí también
parcial1=function(x_1,x_2){
  -(2 * x_1 * sin(sqrt(x_1^2 + x_2^2))) / sqrt(x_1^2 + x_2^2) - (cos(x_1 - x_2) + 2 * x_1) / (sin(x_1 - x_2) + x_1^2 + 3) + x_1 / (6 * sqrt(x_1^2 + x_2^2))
}

parcial2=function(x_1,x_2){
  -(2 * x_2 * sin(sqrt(x_2^2 + x_1^2))) / sqrt(x_2^2 + x_1^2) + cos(x_2 - x_1) / (-sin(x_2 - x_1) + x_1^2 + 3) + x_2 / (6 * sqrt(x_2^2 + x_1^2))
}

#Para este método necesitamos la hessiana
hess=matrix(numeric(dimensiones^2),ncol=dimensiones)#podría poner 2*2 pero bueno
hess11=function(x_1,x_2){
  z=-(2 * sin(sqrt(x_1^2 + x_2^2))) / sqrt(x_1^2 + x_2^2) +
    (2 * x_1^2 * sin(sqrt(x_1^2 + x_2^2))) / (x_1^2 + x_2^2)^(3 / 2) -
    (2 * x_1^2 * cos(sqrt(x_1^2 + x_2^2))) / (x_1^2 + x_2^2) -
    (2 - sin(x_1 - x_2)) / (sin(x_1 - x_2) + x_1^2 + 3) +
    (cos(x_1 - x_2) + 2 * x_1)^2 / (sin(x_1 - x_2) + x_1^2 + 3)^2 +
    1 / (6 * sqrt(x_1^2 + x_2^2)) - x_1^2 / (6 * (x_1^2 + x_2^2)^(3 / 2))
} 

hess12=function(x_1,x_2){
  z=(2 * x_1 * x_2 * sin(sqrt(x_2^2 + x_1^2))) / (x_2^2 + x_1^2)^(3 / 2) -
    (2 * x_1 * x_2 * cos(sqrt(x_2^2 + x_1^2))) / (x_2^2 + x_1^2) +
    sin(x_2 - x_1) / (-sin(x_2 - x_1) + x_1^2 + 3) -
    (cos(x_2 - x_1) * (cos(x_2 - x_1) + 2 * x_1)) / (-sin(x_2 - x_1) + x_1^2 + 3)^2 -
    (x_1 * x_2) / (6 * (x_2^2 + x_1^2)^(3 / 2))
}
hess21=function(x_1,x_2){
  z=-(2 * sin(sqrt(x_2^2 + x_1^2))) / sqrt(x_2^2 + x_1^2) +
    (2 * x_2^2 * sin(sqrt(x_2^2 + x_1^2))) / (x_2^2 + x_1^2)^(3 / 2) -
    (2 * x_2^2 * cos(sqrt(x_2^2 + x_1^2))) / (x_2^2 + x_1^2) -
    sin(x_2 - x_1) / (-sin(x_2 - x_1) + x_1^2 + 3) +
    cos(x_2 - x_1)^2 / (-sin(x_2 - x_1) + x_1^2 + 3)^2 +
    1 / (6 * sqrt(x_2^2 + x_1^2)) - x_2^2 / (6 * (x_2^2 + x_1^2)^(3 / 2))
}
hess22=function(x_1,x_2){
  z=-(2 * sin(sqrt(x_2^2 + x_1^2))) / sqrt(x_2^2 + x_1^2) +
    (2 * x_2^2 * sin(sqrt(x_2^2 + x_1^2))) / (x_2^2 + x_1^2)^(3 / 2) - 
    (2 * x_2^2 * cos(sqrt(x_2^2 + x_1^2))) / (x_2^2 + x_1^2) - 
    sin(x_2 - x_1) / (-sin(x_2 - x_1) + x_1^2 + 3) + 
    cos(x_2 - x_1)^2 / (-sin(x_2 - x_1) + x_1^2 + 3)^2 + 
    1 / (6 * sqrt(x_2^2 + x_1^2)) - x_2^2 / (6 * (x_2^2 + x_1^2)^(3 / 2))
}
###Vamos con el bucle 
for (i in 1:n_iteraciones){
  #Criterio de parada
  grad=c(parcial1(yini[1],yini[2]),parcial2(yini[1],yini[2]))
  checkparada[i]=Norm(grad)
  if(checkparada[i]<=epsi){
    n_iteraciones=i
    valiter=valiter[1:i,]#recortamos valiter
    fobjvec=fobjvec[1:i]#recortamos el vector de objetivos en cada paso
    convergio=1
    iteraciones_hasta_parar=i
    tamaño_paso=tamaño_paso[1:i]
    dirnorm=dirnorm[1:i,]
    break
  }else{#paso 2
    hess[1,1]=hess11(yini[1],yini[2])
    hess[1,2]=hess12(yini[1],yini[2])
    hess[2,1]=hess21(yini[1],yini[2])
    hess[2,2]=hess22(yini[1],yini[2])
    dirdescenso[i,]=(-solve(hess))%*%grad
    dirnorm[i,]=dirdescenso[i,]/(pracma::Norm(dirdescenso[i,]))
    yini=yini+dirdescenso[i,]
    valiter[i+1,]=yini
    tamaño_paso[i]=pracma::Norm(dirdescenso[i,])
    fobjvec[i+1]=fobj(yini[1],yini[2])
  }
}
valiter
fobjvec#No entiendo por qué la parte de recortarlas no ha funcionado
checkparada
n_iteraciones
tamaño_paso
dirnorm

for (i in 1:iteraciones_hasta_parar) {
  tamaño_paso[i]=pracma::Norm(dirdescenso[i,])
}

tabla_newton=cbind(valiter,fobjvec,dirnorm,tamaño_paso)
colnames(tabla_newton)=c("x_1^t","x_2^t","fobj(x_1^t,x_2^t)"
                             ,"dirección normalizada 1","dirección normalizada 2","tamaño de paso")
latex_xtcoords=xtable(tabla_newton,digits = 6)
titulo_tabla="Posiciones recorridas"
print(latex_xtcoords, type="latex")

##Zona representación gráfica con funcion filled.contour

rejx1=seq(min(valiter[,1])-0.5,max(valiter[,1])+0.5,by=0.2) #rango con margen de x_1 
rejx2=seq(min(valiter[,2])-0.5,max(valiter[,2])+0.5,by=0.2) #rango con margen de x_2
evalz=outer(rejx1,rejx2,FUN="fobj")
source("Representando_fun_aux.r")#tener esto en el directorio de trabajo
filled.contour2(rejx1,rejx2,evalz, levels=seq(min(fobjvec)-2.9,max(fobjvec)+2.9,by=0.25))

#bucle para los x^t

for(i in 1:(dim(valiter)[1])){
  points(valiter[i,1],valiter[i,2],col="brown",pch=19,cex=1.2)
}

#bucle para lineas x^t
#se puede hacer todo en un único bucle pero si se va ejecutando
#linea a linea se ve la construcción

for(i in 1:(dim(valiter)[1])){
  lines(valiter[,1],valiter[,2],col="green",cex=1.5)
} #se va desplazando casi en la misma recta salvo en los primeros pasos. Como 
#se puede ver en dirdescenso o dirnorm realmente no es la misma recta
#pero se tiene que alejar tanto el zoom que no se aprecia
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##                                                                            ##
###~~~~~~~~~~~~~~~~~~~~~~~~~~Fin Método de Newton~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
##                                                                            ## 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##                                                                            ##
###~~~~~~~~~~~~~~~~~~~~~~~~~~Gradiente Conjugado~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
##                                                                            ## 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
epsi=0.01
epsilinea=epsi/10
yini=xini
alfavec=numeric(length=n_iteraciones)
gradmat=dirdescenso #también guardo los distintos gradientes que van obteniendose
#no puedo tratar las direcciones como en los anteriores métodos
#necesitamos traer las parciales aquí también
parcial1=function(x_1,x_2){
  -(2 * x_1 * sin(sqrt(x_1^2 + x_2^2))) / sqrt(x_1^2 + x_2^2) - (cos(x_1 - x_2) + 2 * x_1) / (sin(x_1 - x_2) + x_1^2 + 3) + x_1 / (6 * sqrt(x_1^2 + x_2^2))
}

parcial2=function(x_1,x_2){
  -(2 * x_2 * sin(sqrt(x_2^2 + x_1^2))) / sqrt(x_2^2 + x_1^2) + cos(x_2 - x_1) / (-sin(x_2 - x_1) + x_1^2 + 3) + x_2 / (6 * sqrt(x_2^2 + x_1^2))
}
library(nloptr)
#Vamos con el bucle
for (i in 1:n_iteraciones){
  gradmat[i,]=c(parcial1(yini[1],yini[2]),parcial2(yini[1],yini[2]))
  if(Norm(as.vector(gradmat[i,]))<epsi){
    convergio=1
    iteraciones_hasta_parar=i
    valiter=valiter[1:i,]
    fobjvec=fobjvec[1:i]
    dirnorm=dirnorm[1:i,]
    dirdescenso=dirdescenso[1:i,]
    break
  }else{
  if(i==1){
    dirdescenso[i,]=-gradmat[i,]
  }
  if(i>=2){ #recordamos que alfa^1=0, no queremos actualizar ese valor
    alfavec[i]=(t(gradmat[i,])%*%(gradmat[i,]-gradmat[i-1,]))/(Norm(gradmat[i-1,])^2)
    #como i>=2 sabemos que existe gradmat[i-1,]
    dirdescenso[i,]=-gradmat[i,]+alfavec[i]*dirdescenso[i-1,] 
    #fijarse que en la expresión d^t usa d^{t-1} 
  }
  f_linea1=function(lambda){
    fobj(yini[1]+lambda*dirdescenso[i,1],yini[2]+lambda*dirdescenso[i,2])
  }
  lambdaux=nloptr(x0=1,
                  eval_f = f_linea1,lb=0,ub=100,
                  opts=list(algorithm="NLOPT_GN_DIRECT_L",
                            xtol_rel=epsilinea))
  tamaño_paso[i+1]=Norm(lambdaux$solution*dirdescenso[i,])
  yini=yini+lambdaux$solution*dirdescenso[i,]
  valiter[i+1,]=yini
  fobjvec[i+1]=fobj(yini[1],yini[2])
  dirnorm[i,]=dirdescenso[i,]/Norm(dirdescenso[i,])
  tamaño_paso[i]=Norm(dirdescenso[i,])
  }
}
valiter #falló el recorte valiter=valiter[1:10,]
fobjvec
convergio

##Zona representación gráfica con funcion filled.contour

#Como se han recorrido puntos muy distantes y la función ha tomado valores muy
#disparatados, hay que modificar los comandos para obtener las curvas de nivel
rejx1=seq(min(valiter[,1])-0.4,max(valiter[,1])+0.4,by=0.2) #rango con margen de x_1 
rejx2=seq(min(valiter[,2])-0.4,max(valiter[,2])+0.4,by=0.2) #rango con margen de x_2
#########CUIDADO NO EJECUTAR LA SIGUIENTE LINEA. GENERA UN VECTOR DE 7.4Gb
evalz=outer(rejx1,rejx2,FUN="fobj")
source("Representando_fun_aux.r")#tener esto en el directorio de trabajo
fobjvec
filled.contour2(rejx1,rejx2,evalz, levels=seq(min(fobjvec)-3.9,max(fobjvec)+3.9,by=0.35))


#bucle para los x^t

for(i in 1:(dim(valiter)[1])){
  points(valiter[i,1],valiter[i,2],col="brown",pch=19,cex=1.2)
}

#bucle para lineas x^t
#se puede hacer todo en un único bucle pero si se va ejecutando
#linea a linea se ve la construcción

for(i in 1:(dim(valiter)[1])){
  lines(valiter[,1],valiter[,2],col="green",cex=1.5)
} #Parece avanzar siempre en la misma dirección

#obtencion tabla
tabla_gradconj=cbind(valiter,fobjvec,dirnorm,tamaño_paso)
colnames(tabla_gradconj)=c("x_1^t","x_2^t","fobj(x_1^t,x_2^t)"
                         ,"dirección normalizada 1","dirección normalizada 2","tamaño de paso")
latex_xtcoords=xtable(tabla_gradconj,digits = 6)
titulo_tabla="Posiciones recorridas"
print(latex_xtcoords, type="latex")
