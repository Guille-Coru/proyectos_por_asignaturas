####No paramétricos script segunda parte###
library(latex2exp)
library(ks)
library(sm)
library(KernSmooth)
library(TDA)
datos= BostonHousing
estatus=datos$lstat
head(datos)
head(estatus)
breaks <- function(x,x0,h){
  b <- floor((min(x)-x0)/h) : ceiling((max(x)-x0)/h)
  b <- b*h+x0
  return(b)
}
x0 <- 20
h <- 1.5
hist(estatus,freq=FALSE,breaks=breaks(estatus,x0,h),
     main="",xlab="%poblacion bajo estatus", ylab="",
     col="skyblue",border="black",cex.lab=1.2)
##escogiendo ventanas (parámetro de suavizado)
nocheckCV=bw.ucv(estatus)
args(bw.ucv)
##intentando checkear
bw.ucv.mod <- function(x, nb = 1000L,
                       h.grid = diff(range(x)) * (seq(0.1, 1, l = 200))^2,
                       plot.cv = FALSE) {
  if ((n <- length(x)) < 2L)
    stop("need at least 2 data points")
  n <- as.integer(n)
  if (is.na(n))
    stop("invalid length(x)")
  if (!is.numeric(x))
    stop("invalid 'x'")
  nb <- as.integer(nb)
  if (is.na(nb) || nb <= 0L)
    stop("invalid 'nb'")
  storage.mode(x) <- "double"
  hmax <- 1.144 * sqrt(var(x)) * n^(-1/5)
  Z <- .Call(stats:::C_bw_den, nb, x)
  d <- Z[[1L]]
  cnt <- Z[[2L]]
  fucv <- function(h) .Call(stats:::C_bw_ucv, n, d, cnt, h)
  # h <- optimize(fucv, c(lower, upper), tol = tol)$minimum
  # if (h < lower + tol | h > upper - tol)
  #   warning("minimum occurred at one end of the range")
  obj <- sapply(h.grid, function(h) fucv(h))
  h <- h.grid[which.min(obj)]
  if (plot.cv) {
    plot(h.grid, obj, type = "o")
    rug(h.grid)
    abline(v = h, col = 2, lwd = 2)
  }
  h
}
sicheck=bw.ucv.mod(estatus, plot.cv = T)
sm.density(estatus,sicheck,add=T,lwd=3,col="tomato2")
regladedo=bw.nrd(estatus)
sm.density(estatus,regladedo,add=T,lwd=3,col="blue3")
pluginw=hpi(estatus)
sm.density(estatus,pluginw,add=T,lwd=3,col="yellow")
mean(estatus)
sop=seq(-1,50,0.001)
length(sop)
lines(sop,dnorm(sop,mean=mean(estatus),sd=sqrt(var(estatus))),lwd=3,col="black")
s
legend("topright",c(expression(h[CV]==1.3145),expression(h[ROT]==2.178906),
                    expression(h[ST]==1.522609),"Normal"),
       lty=1,lwd=2,col=c("tomato2","blue3","yellow","black"),cex=0.818,inset=0.025)

rm <- datos$rm
mediana <- median(rm)
C_1 =  rm[rm < mediana]
C_2 = rm[rm < mediana]
C_1 = datos[datos$rm<mediana,]
C_2 = datos[datos$rm>mediana,]
##Si hay problemas con datos faltantes (aparentó ser el caso)
##usar los siguientes comandos:
## mediana <- median(datos$rm, na.rm=TRUE)
## C_1v = datos[datos$rm<mediana & !is.na(datos$rm),]
rm1=C_1$rm
rm2=C_2$rm
lstatC1=C_1$lstat
lstatC2=C_2$lstat
sm.density.compare(c(lstatC1,lstatC2),rep(1:2, each=253),
                   model="equal",col=c("red","blue"),col.band="goldenrod2",
                   lwd=3,style="l")

estilos=c(1,2,1)##para que el estilo de linea en la leyenda
##vaya acorde con el de la gráfica
legend("topright",c("Densidad  C_1","Densidad C_2",
                    "banda"),
       lty=estilos,lwd=3,col=c("red","blue","goldenrod2"),cex=0.818,inset=0.025)
lstat=datos$lstat
rm=datos$rm
modl=lm(rm~lstat)
margenes=c(min(rm),max(rm),min(lstat),max(lstat));margenes
par(mfrow=c(1,1))
plot(lstat,rm,pch=20)
abline(modl,col="blue",lwd=2)
modl2=lm(rm~lstat+I(lstat^2))
summary(modl2)
beta=coefficients(modl2)
curve(beta[1]+beta[2]*x+beta[3]*x^2, add=T,lwd=2,col="red")
legend("topright",c("lineal","cuad"), 
       lty=1,lwd=2,col=c("blue","red"),cex=0.8,inset=0.03)
Hord1 <- dpill(lstat,rm)
sm.regression(lstat, rm, poly.index=1, h=Hord1,lwd=2,add=T,col="red")
sm.regression(lstat, rm, poly.index=3, method="cv", lwd=2,add=T,col="purple")
legend("topright",c("local lineal      ","local cúbico      "), 
       lty=1,lwd=2,col=c("goldenrod2","purple"),cex=0.8,inset=0.03)
asd=quantile(lstat,probs=0.5)
evalu=quantile(lstat,probs=c(0.25,0.5,0.75))
str(evalu)
evalu1=evalu[1]
evalu1
evalu2=evalu[2]
plot(lstat,rm)
estloes=loess(rm~lstat,span=0.2, data=BostonHousing)
suavizado=predict(estloes,lstat)
curve(x=lstat,y=suavizado)
head(suavizado)
lines(lstat,suavizado,col="blue")
prediccion<-predict(estloes,evalu,SE=T)
prediccion
abline(v=evalu,lty=1,col=c("blue","green","purple"),)
