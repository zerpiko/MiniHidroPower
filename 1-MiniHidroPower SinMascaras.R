# MinihidroPower
# Calculation of MHP potential
# Coded by: Gerardo Alcal'a
# Universidad Veracruzana
# First Version December 19 2018

# Updated December 09 2019

####I. Procesos Iniciales
{
    graphics.off() 
    remove(list=ls())
    
    ##setwd("C:/Users/Gerardo Alcalá/Desktop/MiniHidro")
    setwd("/home/c.c1045890/git/MiniHidroPower")

    library(sp)
    library(raster)
    library(rgeos)
    library(rgdal)
    library(tools)
    library(gdistance)

    radIntake <- 2200
    radTurbina <- 2000
}
###II. Raster y Vectoriales
{
    ## Direccones Archivos
    CharFile1 <- "A-RasterRio.tif"
    CharFile2 <- "B-RasterDEM.tif"
    ## Leer Rasters y Vectoriales
    RasRio <- brick(CharFile1)
    RasDEM <- raster(CharFile2)
    ## Homologar Proyección
    RasRio <- projectRaster(RasRio,RasDEM)
    ## Homologar Extension
    RasRio  <- crop(RasRio,RasDEM)
    ## Nombres Atributo
    names(RasRio) <- c("ID","Gasto","Base","Entrada","Salida")
    names(RasDEM) <- "Altura"
}
###III. Malla de puntos
{
    ## a. Puntos Rio (Points from Raster)
    SPPointsRio <- as(RasRio,"SpatialPointsDataFrame")
    SPPointsRio <- extract(RasDEM,SPPointsRio,sp=TRUE)
    ## b. Raster MHP (Variables Calculadas)
    RasMHP <- RasDEM          #1
    RasMHP$PotenciakW <- NA
    RasMHP$GastoMax <- NA
    RasMHP$HMax     <- NA     #4
    RasMHP$xTurbina   <- NA
    RasMHP$yTurbina <- NA
    RasMHP$zTurbina <- NA
    RasMHP$BaseTurbina <- NA  #8
    RasMHP$xIntake    <- NA
    RasMHP$yIntake  <- NA
    RasMHP$zIntake  <- NA
    RasMHP$BaseIntake  <- NA  #12
    ## c. Raster MHP a Malla de Puntos (OJO! Celdas con puro NA's no generan punto)
    SPPointsGrid <- as(RasMHP,"SpatialPointsDataFrame")
    
    if(length(SPPointsGrid)!=ncell(RasMHP))
    {
        cat("Error, hay celdas del Raster con puro NA's")  
        stop("FIN")
    }
    else
    {
        cat("No hay NA's en el Raster: El programa puede seguir") 
    }
}
###IV. Funcion de NumCruces. Intersecta Linea Recta con Rio (Sitios Radio)
fNumCruces <- function(NumCoordsRioRadiox,NumCoordsRioRadioy,NumCoordPiletaix,NumCoordPiletaiy)
{
    ##Numxc <- c(NumCoordsRioRadiox[10], NumCoordPiletaix); Numyc <- c(NumCoordsRioRadioy[10], NumCoordPiletaiy)
    Numxc <- c(NumCoordsRioRadiox, NumCoordPiletaix)
    Numyc <- c(NumCoordsRioRadioy, NumCoordPiletaiy)
    Matxyc <- cbind(Numxc,Numyc)
    SLLineaRecta <- spLines(Matxyc,crs=crs(RasMHP))
    SPolLineaRecta <- buffer(SLLineaRecta, width=res(RasMHP)[1]*0.75)
    InterCruces <- intersect(SPPointsRioRadio,SPolLineaRecta)
    NumCruces <- dim(InterCruces)[1]
}

#x11()
#plot(RasDEM)             
#plot(SPPointsRio,col='blue',add=TRUE,pch=19,cex=0.1)

mult <- 5000
##############################################
###VII  INICIA CICLO POR EL GRID
library(doParallel)
cores=detectCores()
cl <- makeCluster(cores[1])
registerDoParallel(cl)
print(paste0("Running program with : ",cores[1]," cores."))
print(paste0("Grid size            : ",length(SPPointsGrid)))

#for(i in 1:length(SPPointsGrid)){
#for(i in seq(1+150,length(SPPointsGrid),1000)){
#for(i in c(1,50000,100000,250000,350000,450000,550000,600000,700000,850000,900000,1050000,1100000,1250000,1300000,1400000,1500000,1600000,1700000,1800000,1900000,2000000,2100000,2200000,2300000)){
ptime <- system.time({
    ##foreach(i=1:length(SPPointsGrid)) %dopar% {
    foreach(i=1:230000) %dopar%
        {
            library(sp)
            library(raster)
### PILETA ###
###1. Inicio: Ubicar la pileta
            ##if((i/mult)%%1==0){cat("\n Punto", i ,"de",length(SPPointsGrid),"\n")}
            SPPointPiletai <- SPPointsGrid[i,]
            hi <- SPPointPiletai$Altura
            ##plot(SPPointPiletai,pch=19,col='red',add=TRUE,cex=0.7)
###2. Si rio esta fuera del radio de la Pileta (NEXT)
            ## a. Crear buffer
            SPolRadio <- buffer(SPPointPiletai, width=max(radIntake,radTurbina))
            SPPointsRioRadio <- intersect(SPPointsRio,SPolRadio)
            
            ## b. Verificar si se intersecta el rio, sino terminar Iteracion
            if(length(SPPointsRioRadio)==0)
            {
                SPPointsGrid$PotenciakW[i] <- 0
                SPPointsGrid$GastoMax[i] <- 0
                SPPointsGrid$HMax[i] <- 0
                ##plot(SPPointPiletai,pch=4,col='black',add=TRUE,cex=0.6)
                ##cat("\n Paso2. Rio fuera del alcance: Potencia, Gasto Hmax son cero",i)
                ##next
            }
            else
            {
###3. Trayectorias Rectas (Pileta-Rio) que solamente toquen al rio 1 vez (NEXT)
                ## a. Puntos de la interseccion (Para aplicar NumCruces con el rio)
                ## Son todos los puntos de la interseccion
                NumCoordsRioRadiox <- SPPointsRioRadio@coords[,1]
                NumCoordsRioRadioy <- SPPointsRioRadio@coords[,2]
                ## Es el punto actual sobre el DEM
                NumCoordPiletaix <- SPPointPiletai@coords[1]
                NumCoordPiletaiy <- SPPointPiletai@coords[2]
                ## b. Se obtienen numero de cruces con el rio
                SPPointsRioRadio$NumCruces <- mapply(fNumCruces,NumCoordsRioRadiox,NumCoordsRioRadioy,NumCoordPiletaix,NumCoordPiletaiy)
                ## c.  Sitios que solamente atraviesan 1 vez el rio
                k <- which(SPPointsRioRadio$NumCruces ==1)
                ## d. Sitios que crucen al rio solamente 1 vez (else NEXT)
                if(length(k) < 1)
                {
                    SPPointsGrid$PotenciakW[i] <- 0;
                    SPPointsGrid$GastoMax[i] <- 0;
                    SPPointsGrid$HMax[i] <- 0
                    ##plot(SPPointPiletai,pch=4,col='black',add=TRUE,cex=0.6)
                    ##cat("\n Paso3. No hay ruta que toque 1 sola vez al rio: Potencia, Gasto Hmax son cero")
                    ##next
                }
                else
                {
### TURBINA ###
###4. Sitios con menor altura a la Pileta (else NEXT)
                    ## a. Sitios Un Cruce en el Radio de la Turbina
                    SPPointsUnCruce <- SPPointsRioRadio[k,]
                    SPolRadioTurb <- buffer(SPPointPiletai, width=radTurbina)
                    SPPointsUnCruceTurb <- intersect(SPPointsUnCruce,SPolRadioTurb)
                    ## b. Nos fiajmos en los sitios a lo largo del rio con una altura menor a la pileta 
                    k1 <- which(SPPointsUnCruceTurb$Altura+4 < SPPointPiletai$Altura)
                    ## c. En caso que no haya sitios mas bajo que la pileta
                    if(length(k1)<1)
                    {
                        SPPointsGrid$PotenciakW[i] <- 0
                        SPPointsGrid$GastoMax[i] <- 0
                        SPPointsGrid$HMax[i] <- 0
                        ##plot(SPPointPiletai,pch=4,col='black',add=TRUE,cex=0.6)
                        ##cat("\n Paso4. No hay sitios mas bajos que la pileta: Potencia, Gasto Hmax son cero")
                        ##next
                    }
                    else
                    {
###5. Localizacion de la Turbina
                        ## a. Localizacion Turbina
                        SPPointsTurbina <- SPPointsUnCruceTurb[k1,]
                        k3 <- which.min(SPPointsTurbina$Altura)
                        ## Posicion con menor altura sin obstaculos
                        SPPointTurb <- SPPointsTurbina[k3[1],] 
                        ## b. Gradiente de altura maximo
                        HTurb <- SPPointTurb$Altura
                        HMax <- hi-HTurb 
                        ## c. Guardamos Gradiente Altura maxima
                        SPPointsGrid$HMax[i] <- HMax
                        ## d. Guardamos la coordenada de la turbina
                        SPPointsGrid$xTurbina[i] <- SPPointTurb@coords[1]
                        SPPointsGrid$yTurbina[i] <- SPPointTurb@coords[2]
                        SPPointsGrid$zTurbina[i] <- HTurb
                        SPPointsGrid$BaseTurbina[i] <- SPPointTurb$Base
###6. Ruta Recta de la Turbina
                        if((i/(10*mult))%%1==0)
                        {
                            Numxc <- c(SPPointTurb@coords[1], SPPointPiletai@coords[1])
                            Numyc <- c(SPPointTurb@coords[2], SPPointPiletai@coords[2])
                            Matxyc <- cbind(Numxc,Numyc)
                            
                            SLTurbinai <- spLines(Matxyc,crs=crs(RasDEM)) # SpatialLinea
                        }
###7. Graficas Turbina
                        ##plot(SPolRadioTurb,border='orange',pch=19,add=TRUE)
                        ##plot(SPPointTurb,col='purple',pch=19,add=TRUE)
                        ##lines(SLTurbinai,col='orange',lwd=2)
### DESVIACION ###
###8. El Intake pertenece a la misma rama del rio (else NEXT)
                        j <- which(SPPointsUnCruce$Base==SPPointTurb$Base |SPPointsUnCruce$Salida==SPPointTurb$Entrada) 
                        SPPointsMismaRama <- SPPointsUnCruce[j,]
                        
                        SPolRadioIntake <- buffer(SPPointPiletai, width=radIntake)
                        SPPointsMismaRamaIntake <- intersect(SPPointsMismaRama,SPolRadioIntake)
###9. Sitios con mayor altura que la pileta (else NEXT) 
                        j <- which(hi+3<SPPointsMismaRamaIntake$Altura)
                        if(length(j)==0)
                        {
                            ## Guardamos Gasto Maximo
                            QMax <- 0
                            SPPointsGrid$GastoMax[i] <- QMax; SPPointsGrid$PotenciakW[i] <- 0
                            ##plot(SPPointPiletai,pch=4,col='black',add=TRUE,cex=0.6)
                            ##cat("\n Paso9. El intake no tiene altura suficiente: Potencia y Gasto son cero")
                            ##next 
                        }
                        else
                        {
###10. Localizacion de Intake
                            SPPointsIntake <- SPPointsMismaRamaIntake[j,]
                            ## a. Localizacion Turbina
                            k5 <- which.max(SPPointsIntake$Gasto)
                            SPPointIntakei <- SPPointsIntake[k5[1],] # Posicion con mayor gasto sin obstaculos
                            ## b. Guardamos Gasto Maximo
                            QMax <- SPPointIntakei$Gasto
                            SPPointsGrid$GastoMax[i] <- QMax    
                            ## c. Guardamos puntos el Intake
                            SPPointsGrid$xIntake[i] <- SPPointIntakei@coords[1]
                            SPPointsGrid$yIntake[i] <- SPPointIntakei@coords[2]
                            SPPointsGrid$zIntake[i] <- SPPointIntakei$Altura
                            SPPointsGrid$BaseIntake[i] <- SPPointIntakei$Base
###11. Linea Intake Pilet
                            if((i/(10*mult))%%1==0)
                            {
                                Numxc <- c(SPPointIntakei@coords[1], SPPointPiletai@coords[1])
                                Numyc <- c(SPPointIntakei@coords[2], SPPointPiletai@coords[2])
                                Matxyc <- cbind(Numxc,Numyc)
                                
                                SLIntakei <- spLines(Matxyc,crs=crs(RasDEM)) # SpatialLinea
                            }
###12. Graficas
                            ##plot(SPolRadioIntake,add=TRUE,border='black')
                            ##plot(SPPointIntakei,add=TRUE,pch=19,col='yellow')
                            ##lines(SLIntakei,col='black',lwd=2)
### POTENCIA ###
###13. Asignacion PotenciakW
                            SPPointsGrid$PotenciakW[i] <- 1000*9.81*QMax*HMax/1000
                        }
                    }
                }
            }
        }
})[3]
stopCluster(cl)
ptime

#### Termina Ciclo for del grid
##################################


### FIN CICLO POR TODO EL GRID ###

#16. Guardar Raster
{
    RasFinal <- raster(SPPointsGrid,ncol=ncol(RasMHP),nrow=nrow(RasMHP),ext=extent(RasMHP),res=res(RasMHP),crs=crs(RasMHP))
    
    RasFinal$PotenciakW <-  SPPointsGrid$PotenciakW                       # 1
    RasFinal$Altura <- SPPointsGrid$Altura                                # 2
    RasFinal$GastoMax <-  SPPointsGrid$GastoMax                           # 3
    RasFinal$HMax <-  SPPointsGrid$HMax                                   # 4
    RasFinal$xTurbina <-  SPPointsGrid$xTurbina                           # 5
    RasFinal$yTurbina <-  SPPointsGrid$yTurbina                           # 6
    RasFinal$zTurbina <- SPPointsGrid$zTurbina                            # 7
    RasFinal$BaseTurbina <- SPPointsGrid$BaseTurbina                      # 8
    
    RasFinal$xIntake <- SPPointsGrid$xIntake                              # 9
    RasFinal$yIntake <- SPPointsGrid$yIntake                              # 10
    RasFinal$zIntake <- SPPointsGrid$zIntake                              # 11
    RasFinal$BaseIntake <- SPPointsGrid$BaseIntake                        # 12
    
    RasFinal$RadioTurbina <- radTurbina                                   # 13
    RasFinal$RadioIntake <- radIntake                                     # 14
    
    RasFinal <- brick(RasFinal)
    ## Con todas las columnas de Atributos
    x <- writeRaster(RasFinal, '1-RasterMHP.tif', overwrite=TRUE)
    
    dffinal <- data.frame(SPPointsGrid)
    archivof1 <- paste("DatosMHP.csv",sep="")
    write.csv(dffinal, file = archivof1,row.names=FALSE)
    
    ##View(dffinal)
    ##x11()
    ##plot(RasFinal$PotenciakW)
    ##plot(SPPointsRio, col='blue', add=TRUE)  
}
