# MinihidroPower
# Calculation of MHP potential
# Gerardo Alcal'a
# Universidad Veracruzana
# Updated 12 April 2019


#1. Procesos Iniciales
{
  graphics.off() 
  remove(list=ls())

  setwd("..")
  dir <- getwd()
  setwd(dir)
  getwd()
  
  #setwd("C:/Users/galca/Desktop/MiniHidro")

  library(sp)
  library(raster)
  library(rgeos)
  library(rgdal)
  library(tools)
  library(gdistance)
  library(imputeTS) #para la interpolacion

  flagGraficas <- 0
}

#2. Rasters y Vectores
{
  file1 <- "VectorFiles/GastoRio/Datos_de_arcgisbueno.shp"
  puntos <- shapefile(file1)
  
  file2 <- "RasterFiles/A-RasterRio.TIF"; 
  file3 <- "RasterFiles/B-RasterDEM.TIF"
  RasRio <- raster(file2); 
  RasDEM <- raster(file3)
  
  # Ponemos la misma resolucion basados en RasRio
  RasDEM=projectRaster(RasDEM,RasRio)
  RasDEM <- crop(RasDEM,RasRio)
  names(RasDEM) <- "Altura"; names(RasRio) <- "Gasto"
  
  RasMHP <- RasDEM
  RasMHP$PotenciakW <- NA; RasMHP$GastoMax <- NA; RasMHP$HMax <- NA
  RasMHP$xTurbina <- NA; RasMHP$yTurbina <- NA; RasMHP$zTurbina <- NA
  RasMHP$xIntake <- NA; RasMHP$yIntake <- NA; RasMHP$zIntake <- NA
  RasMHP$DistTurbina <- NA; RasMHP$DistIntake <- NA
  RasMHP <- stack(RasMHP,RasRio)
  RasMHP <- brick(RasMHP)
  
  # Raster MHP to Point Grid
  pgrid <- as(RasMHP,"SpatialPointsDataFrame")
  pgrid$gasto <- NULL
  
  # Extract Polygon from raster
  polrio <- as(RasRio,"SpatialPointsDataFrame")
  polrio <- extract(RasDEM,polrio,sp=TRUE)

}

#3. Asignamos valores de puntos al poly-rio
{
  # Vectores a  DataFrames
  
  dfpuntos <- data.frame(puntos)
  dfpuntos2 <- dfpuntos
  
  ### Cambio el orden de los puntos (Ojo, este es muy particular al arreglo que se tiene. Tener cuidado)
  for(i in 1:nrow(dfpuntos)){
    dfpuntos2$PROMEDIO[i] <- dfpuntos$PROMEDIO[26-i]
  }
  
  dfpuntos$PROMEDIO <- dfpuntos2$PROMEDIO
  puntos$PROMEDIO <- dfpuntos$PROMEDIO
  
  dfrio <- data.frame(polrio)
  
  puntos$xcor <- dfpuntos$coords.x1; puntos$ycor <- dfpuntos$coords.x2
  polrio$xcord <- dfrio$x; polrio$ycord <- dfrio$y
  
  polrio$gasto<- NA
  polrio$ENE<- NA; polrio$FEB<- NA; polrio$MAR<- NA
  polrio$APR<- NA; polrio$MAY<- NA; polrio$JUN<- NA
  polrio$JUL<- NA; polrio$AUG<- NA; polrio$SEP<- NA
  polrio$OCT<- NA; polrio$NOV<- NA; polrio$DIC<- NA
  
  #k=1
  
  # Buscar sitios mas cercanos de puntos espaciales al rio
  #h <- 0
  for(k in 1:length(puntos)){
  
    polrio$diferenciax <- polrio$xcord
    polrio$diferenciax <- polrio$diferenciax-puntos$xcor[k]
    polrio$diferenciax <- polrio$diferenciax*polrio$diferenciax
    polrio$diferenciax <- sqrt(polrio$diferenciax)
    
    polrio$diferenciay <- polrio$ycord
    polrio$diferenciay <- polrio$diferenciay-puntos$ycor[k]
    polrio$diferenciay <- polrio$diferenciay*polrio$diferenciay
    polrio$diferenciay <- sqrt(polrio$diferenciay)
    
    polrio$diferenciat <- polrio$diferenciax + polrio$diferenciay
    
    j <- which.min(polrio$diferenciat)
    
    polrio$gasto[j] <- puntos$PROMEDIO[k]
    polrio$ENE[j]<- puntos$ENE[k]; polrio$FEB[j]<- puntos$FEB[k]; polrio$MAR[j]<- puntos$MAR[k]
    polrio$APR[j]<- puntos$APR[k]; polrio$MAY[j]<- puntos$MAY[k]; polrio$JUN[j]<- puntos$JUN[k]
    polrio$JUL[j]<- puntos$JUL[k]; polrio$AUG[j]<- puntos$AUG[k]; polrio$SEP[j]<- puntos$SEP[k]
    polrio$OCT[j]<- puntos$OCT[k]; polrio$NOV[j]<- puntos$NOV[k]; polrio$DIC[j]<- puntos$DIC[k]
    
    #h[k] <- j
    
  }
  
  # Obtenemos el dataframe del polrio, con los gastos actualizados
  dfrio <- data.frame(polrio)
  
  dfrio$gasto <- na.interpolation(dfrio$gasto)
  dfrio$ENE <- na.interpolation(dfrio$ENE); dfrio$FEB <- na.interpolation(dfrio$FEB); dfrio$MAR <- na.interpolation(dfrio$MAR)
  dfrio$APR <- na.interpolation(dfrio$APR); dfrio$MAY <- na.interpolation(dfrio$MAY); dfrio$JUN <- na.interpolation(dfrio$JUN)
  dfrio$JUL <- na.interpolation(dfrio$JUL); dfrio$AUG <- na.interpolation(dfrio$AUG); dfrio$SEP <- na.interpolation(dfrio$SEP)
  dfrio$OCT <- na.interpolation(dfrio$OCT); dfrio$NOV <- na.interpolation(dfrio$NOV); dfrio$DIC <- na.interpolation(dfrio$DIC)
  
  polrio$gasto <- dfrio$gasto
  polrio$ENE <- dfrio$ENE; polrio$FEB <- dfrio$FEB; polrio$MAR <- dfrio$MAR
  polrio$APR <- dfrio$APR; polrio$MAY <- dfrio$MAY; polrio$JUN <- dfrio$JUN
  polrio$JUL <- dfrio$JUL; polrio$AUG <- dfrio$AUG; polrio$SEP <- dfrio$SEP
  polrio$OCT <- dfrio$OCT; polrio$NOV <- dfrio$NOV; polrio$DIC <- dfrio$DIC
  
  
  
}

#4. Funcion de NumCruces. Generamos polilinea y la intersectamos con InterRadio
fNumCruces <- function(InterRadiox,InterRadioy,puntoix,puntoiy) {
  
  #xc <- c(InterRadiox[247], puntoix); yc <- c(InterRadioy[247], puntoiy)
  xc <- c(InterRadiox, puntoix); yc <- c(InterRadioy, puntoiy)
  xyc <- cbind(xc,yc)
  Linea <- spLines(xyc,crs=crs(RasMHP))
  LineaRecta <- buffer(Linea, width=res(RasMHP)[1]*0.75)
  InterCruces <- intersect(InterRadio,LineaRecta)
  NumCruces <- dim(InterCruces)[1]
}

### INICIA CICLO POR TODO EL GRID ### 
#for(i in 1:length(pgrid)){

for(i in c(1,50000,100000,150000,200000,250000,300000,350000,400000,450000,500000,550000,600000,650000,700000)){

    #5. Inicio y contador
    {
      #i=400000
      if((i/1000)%%1==0){cat("\n Punto", i ,"de",length(pgrid),"\n")}
      
      puntoi <- pgrid[i,]
      hi <- puntoi$Altura
      }
    
    #6. Interseccion con el rio dentro de un radio especificado
    {
      Radio <- buffer(puntoi, width=2000)
      InterRadio <- intersect(polrio,Radio)
    }
    
    #7. Verificamos si al menos se intersecta el rio 1 vez
    if(length(InterRadio)==0){
      pgrid$PotenciakW[i] <- 0; pgrid$GastoMax[i] <- 0; pgrid$HMax[i] <- 0
    }else{
      
      #8.Puntos de la interseccion
      {
        InterRadiox <- InterRadio@coords[,1]; InterRadioy <- InterRadio@coords[,2] # Son todos los puntos de la interseccion
        puntoix <- puntoi@coords[1]; puntoiy <- puntoi@coords[2]   # Es el punto actual sobre el DEM
      }
      
      #9. Obtner NumCruces con el rio
      {
      InterRadio$NumCruces <- mapply(fNumCruces,InterRadiox,InterRadioy,puntoix,puntoiy)
      }
      #10. Puntos con un sólo cruce con el río
      {
        k <- which(InterRadio$NumCruces ==1)
      }
      
      #11. Aseguramos que haya al menos un cruce con el rio
      if(length(k) < 1){
        pgrid$PotenciakW[i] <- 0; pgrid$GastoMax[i] <- 0; pgrid$HMax[i] <- 0
      } else{ 
        
        #12. Ubicacion TURBINA (Altura minima)
        {  
          InterUnCruce <- InterRadio[k,]
          j <- which.min(InterUnCruce$Altura) 
          
          pTurb <- InterUnCruce[j[length(j)],] # para que tome el ultimo en caso que hubieran varios valores
          
          hTurb <- InterUnCruce$Altura[j] 
          HMax <- hi-hTurb 
          
          if(HMax <0){HMax <- 0}
          
          # Guardamos Altura maxima
          pgrid$HMax[i] <- HMax
        
          # Guardamos la coordenada de la turbina
          pgrid$xTurbina[i] <- pTurb@coords[1]
          pgrid$yTurbina[i] <- pTurb@coords[2]
          pgrid$zTurbina[i] <- hTurb
          pgrid$DistTurbina[i] <-((pTurb@coords[1]-puntoix)^2 + (pTurb@coords[2]-puntoiy)^2 +HMax^2 )^(0.5)
          
        }
        
        #13. Ubicacion DESVIACION
        {
          j <- which(hi+2<InterUnCruce$Altura )
          
          if(length(j)==0){
            # Guardamos Gasto Maximo
            qmax <- 0 
            pgrid$GastoMax[i] <- qmax
          }else{
            qmax<- max(InterUnCruce$gasto[j])
            k <- which(InterUnCruce$gasto==qmax)
            
            pIntake <- InterUnCruce[k[length(k)],]  # Tomo ultimo punto con gasto maximo
            #pIntake2 <- InterUnCruce[k[1],]  # Tomo primer punto con gasto maximo
            #InterQMax <- InterUnCruce[k,]  # Todos los puntos con gasto maximo
            
            # Guardamos Gasto Maximo
            pgrid$GastoMax[i] <- qmax
            
            # Guardamos puntos el Intake
            pgrid$xIntake[i] <- pIntake@coords[1]
            pgrid$yIntake[i] <- pIntake@coords[2]
            pgrid$zIntake[i] <- pIntake$Altura
            pgrid$DistIntake[i] <- ((puntoix-pIntake@coords[1])^2 + (puntoiy-pIntake@coords[2])^2 + (puntoi$Altura -pIntake$Altura )^2)^(0.5)
          }
          
          #13. Asignacion PotenciakW
          {
            # Guardamos PotenciakW
            pgrid$PotenciakW[i] <- 1000*9.81*qmax*HMax/1000
          }
          
          #14. Graficas
          if(flagGraficas==1){
            #Linea a Turbina
            {
              xc <- c(pTurb@coords[1], puntoi@coords[1]); yc <- c(pTurb@coords[2], puntoi@coords[2])
              xyc <- cbind(xc,yc)
              lntur <- spLines(xyc,crs=crs(RasMHP))
            }
            
            # Linea a Intake
            {
              xc <- c(pIntake@coords[1], puntoi@coords[1]); yc <- c(pIntake@coords[2], puntoi@coords[2])
              xyc <- cbind(xc,yc)
              lnIntake <- spLines(xyc,crs=crs(RasMHP))
            }
            
            # Grafica 
             {
              #graphics.off()
              #x11()
              #plot(RasMHP$Altura)
              #plot(polrio, col='blue', add=TRUE)
              #plot(puntoi, col='red', pch=20, cex=3,add=TRUE)
              #plot(Radio,lwd=2,add=TRUE)
              #plot(InterRadio, col='green', add=TRUE)  # Intreseccion
              #plot(pTurb, col='red', pch=20, cex=3,add=TRUE)
              #plot(pIntake, col='pink', pch=20, cex=3,add=TRUE)
              #plot(lnIntake, col='red',lwd=3, add=TRUE)
              #plot(lntur, col='red',lwd=3, add=TRUE)
              
              #plot(InterUnCruce, col='purple',cex=1, pch=20, add=TRUE) # Interseccion con 1 cruce
              ##plot(InterQMax, col='pink', cex=3, pch=20, add=TRUE) # Candidatos para Intake
              ##plot(pIntake2, col='black', pch=20, cex=1.5,add=TRUE)
            }
          
            }
          
          
        }
      }
    }

}
### FIN CICLO POR TODO EL GRID ###

#16. Guardar Raster
{
  
  RasFinal <- raster(pgrid,ncol=ncol(RasMHP),nrow=nrow(RasMHP),ext=extent(RasMHP),res=res(RasMHP),crs=crs(RasMHP))
  
  RasFinal$PotenciakW <-  pgrid$PotenciakW                 # 1
  RasFinal$Altura <- pgrid$Altura                          # 2
  RasFinal$GastoMax <-  pgrid$GastoMax                     # 3
  RasFinal$HMax <-  pgrid$HMax                             # 4
  RasFinal$xTurbina <-  pgrid$xTurbina                     # 5
  RasFinal$yTurbina <-  pgrid$yTurbina                     # 6
  RasFinal$zTurbina <- pgrid$zTurbina                      # 7
  RasFinal$xIntake <- pgrid$xIntake                        # 8
  RasFinal$yIntake <- pgrid$yIntake                        # 9
  RasFinal$zIntake <- pgrid$zIntake                        # 10
  RasFinal$DistTurbina <- pgrid$DistTurbina                # 11
  RasFinal$DistIntake <-   pgrid$DistIntake                # 12
  
  RasFinal <- brick(RasFinal)
  
  x <- writeRaster(RasFinal, 'RasterFiles/1-RasterMHP.tif', overwrite=TRUE) # Con todas las columnas de Atributos
  y <- writeRaster(RasFinal$DistTurbina, 'RasterFiles/2-RasterDistanciaTurbina.tif', overwrite=TRUE)
  z <- writeRaster(RasFinal$DistIntake, 'RasterFiles/3-RasterDistanciaIntake.tif', overwrite=TRUE)
  #x2 <- writeRaster(RasFinal, 'bpotenciaminihidro.tif', band=3,overwrite=TRUE)
  
  dffinal <- data.frame(pgrid)
  archivof1 <- paste("TextFiles/DatosMHP.csv",sep="")
  write.csv(dffinal, file = archivof1,row.names=FALSE)
  
  #x11()
  #plot(RasFinal$PotenciakW)
  #plot(polrio, col='blue', add=TRUE)
  
}
