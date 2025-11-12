# Complex plane transform between the reflection coefficient and the normalized impedance
# www.overfitting.net
# https://www.overfitting.net/2025/11/la-carta-del-astuto-smith-con-r.html


library(tiff)  # save 16-bit TIFF's

NewBitmap = function(dimx, dimy, val=0) {
    # Crea bitmap de dimensiones dimx y dimy
    return(array(val,c(dimx,dimy)))
}

# Por Carlos Gil Bellosta
indices.drawline = function(x0, y0, x1, y1) {
    x0=round(x0)
    x1=round(x1)
    y0=round(y0)
    y1=round(y1)
    
    if (y0 == y1) return(cbind(x0:x1, y0)) # Recta de m=0 o un punto
    if (abs(x1 - x0) >= abs(y1 - y0)) { # Recta de 0 < |m| <= 1
        m = (y1 - y0) / (x1 - x0)
        cbind(x0:x1, round(y0 + m * ((x0:x1) - x0)))
    } else indices.drawline(y0, x0, y1, x1)[, 2:1]  # Recta de |m| > 1
    # Llamada traspuesta recursiva y traspuesta
}

DrawLine = function(img, x0, y0, x1, y1, inc=TRUE, val=1) {
    # Dibuja recta desde (x0,y0)-(x1,y1)
    # Por defecto método no destructivo y con valor=1
    indices=indices.drawline(x0, y0, x1, y1)
    if (inc) img[indices]=img[indices]+val
    else img[indices]=val
    
    return(img)
}

DrawEllip = function(img, x0, y0, a, b, inc=TRUE, val=1, fill=FALSE, thick=1) {
    # Dibuja elipse de centro (x0,y0) y radios a y b
    # Por defecto método no destructivo, con valor=1 y sin relleno
    # Puede elegirse el grosor si no se rellena
    # Aquí no redondeamos para tener más precisión en la división
    if (fill) {
        indices=which( ((row(img)-x0)/a)^2 + ((col(img)-y0)/b)^2 <= 1 )
    } else {
        indices=which( ((row(img)-x0)/(a+thick/2))^2 + ((col(img)-y0)/(b+thick/2))^2 <=  1 &
                           ((row(img)-x0)/(a-thick/2))^2 + ((col(img)-y0)/(b-thick/2))^2 >= 1 )
    }
    if (inc) img[indices]=img[indices]+val
    else img[indices]=val
    
    return(img)
}

DrawCircle = function(img, x0, y0, r, inc=TRUE, val=1, fill=FALSE, thick=1) {
    # Dibuja círculo de centro (x0,y0) y radio r
    # Por defecto método no destructivo, con valor=1 y sin relleno
    # Puede elegirse el grosor si no se rellena
    img=DrawEllip(img, x0, y0, r, r, inc, val, fill, thick)
    
    return(img)
}

SaveBitmap = function(img, name, trunc=TRUE, gamma=1) {
    # Guarda bitmap en formato PNG
    # Solo si trunc=FALSE y la imagen excede de 1 se reescala a 1
    require(tiff)
    img[img<0]=0
    if (trunc) img[img>1]=1
    if (tolower(substr(name, nchar(name)-3, nchar(name))) != ".tif") name=paste0(name,".tif")
    # writePNG(t(img[,ncol(img):1] / max(max(img),1))^(1/gamma), name)
    writeTIFF(t(img[,ncol(img):1] / max(max(img),1))^(1/gamma), name,
              bits.per.sample=16)
}


##################################

# Hires transformation: z -> Gamma
set.seed(10)
N=80000
EXTEND=10
z=runif(N)*EXTEND + 1i * (runif(N)*EXTEND-EXTEND/2)
Gamma=(z-1)/(z+1)

x0=Re(z)
y0=Im(z)
x1=Re(Gamma)
y1=Im(Gamma)

DIMY=1080
DIMX=round(DIMY*(EXTEND+1)/EXTEND)
img=NewBitmap(DIMX, DIMY)

X0=round( (DIMX-1)/(EXTEND+1)*(x0+1)+1 )
Y0=round( (DIMY-1)/ EXTEND   *(y0+EXTEND/2)+1 )
X1=round( (DIMX-1)/(EXTEND+1)*(x1+1)+1 )
Y1=round( (DIMY-1)/ EXTEND   *(y1+EXTEND/2)+1 )

for (i in 1:N) img=DrawLine(img, X0[i], Y0[i], X1[i], Y1[i])
SaveBitmap(img, "smithtransform.tif", trunc=FALSE, gamma=2.2)


# Generate separate axes
OFFX=round( (DIMX-1)/(EXTEND+1)+1 )+1
OFFY=round(DIMY/2)
img=NewBitmap(DIMX, DIMY)
img=DrawLine(img, 1, OFFY, DIMX, OFFY)
img=DrawLine(img, OFFX, 1, OFFX, DIMY)
img=DrawCircle(img, OFFX, OFFY, OFFX-1)
SaveBitmap(img, "smithaxes.tif")


##################################

# Hires inverse transformation: Gamma -> z
set.seed(10)
N=80000
EXTEND=10
Gamma=runif(N)*2-1 + 1i * (runif(N)*2-1)
Gamma=Gamma[Mod(Gamma)<=1 & Gamma!=1]
N=length(Gamma)
plot(Re(Gamma), Im(Gamma), asp=1, col=rgb(1,0,0,0.2))

z=(1+Gamma)/(1-Gamma)
plot(Re(z), Im(z), asp=1)

x0=Re(z)
y0=Im(z)
x1=Re(Gamma)
y1=Im(Gamma)

DIMY=1080
DIMX=round(DIMY*(EXTEND+1)/EXTEND)
img=NewBitmap(DIMX, DIMY)

X0=round( (DIMX-1)/(EXTEND+1)*(x0+1)+1 )
Y0=round( (DIMY-1)/ EXTEND   *(y0+EXTEND/2)+1 )
X1=round( (DIMX-1)/(EXTEND+1)*(x1+1)+1 )
Y1=round( (DIMY-1)/ EXTEND   *(y1+EXTEND/2)+1 )

# HIRES plot
for (i in 1:N) {
    if (X0[i]>=1 & X0[i]<=DIMX & Y0[i]>=1 & Y0[i]<=DIMY) img=DrawLine(img, X0[i], Y0[i], X1[i], Y1[i])
}
SaveBitmap(img, "smithtransforminv.tif", trunc=FALSE, gamma=2.2)
