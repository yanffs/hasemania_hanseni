#************************************************#
#### Extraindo Variaveis ambientais Hasemania ####
#************************************************#

#### instala e carrega pacotes ####
#install.packages(c("shapefiles",
#                   "raster",
#                   "maps"), type = "source")

#Carrega bibliotecas
library(shapefiles) #para carregar arquivos .shp
library(maps) #plota mapas com dados carregado
library(raster) #para trabalhar com dados espaciais

#### Carrega dados de ocorrencia ####
d <- read.csv("ocorrencia_hasemania.csv",
              header = TRUE,
              sep = ";")

#cria conjunto de dados so c/ coord
dcoor <- d[,c("decimalLongitude","decimalLatitude")]
names(dcoor) <- c("Long", "Lat")

#### Plota mapas c/ dados de ocorrencia####
windows(8,8)
map_br <- map("world", "brazil") #package "maps" - camada c/ mapa do Brazil
points(dcoor$Long, dcoor$Lat, col = "red", pch = 3, cex = 2.5)#plota dados de ocorrencia

#### Baixando dados climaticos - worldclim 2.5 res####
pred <- getData("worldclim", var = "bio", res = 2.5) #package "raster"
windows(8,8)
plot(pred, 1) #modifique o '1' para valores entre 1 e 19

# restringindo WorldClim ao brasil
p <- data.frame(x = map_br$x, y = map_br$y)
p <- p[!is.na(p$x), ]
coordinates(p) <- ~x + y
pred <- crop(pred, extent(p))
rm(p)
windows(8,8)
plot(pred, 1)
map("world", "brazil", add = T)
points(dcoor, col = "red", pch = 3, cex = 2.5)


pontos <- SpatialPoints(dcoor, proj4string = pred@crs)#tranformado dados para extrair

values <- extract(pred,pontos)#extraindo dados para as coordenadas

valores_wc <- cbind.data.frame(coordinates(pontos),values)#convertendo, como tabela
#names(data) <- c("x", "y", "name", "value")

#### Dados SRTM 30m - Elevacao ####
#Drenagens do Brasil ANA
##https://dadosabertos.ana.gov.br/datasets/0e1dd8d2169e49d189ed04184f1a1e3b_0

#SRTM - 30m resolução: https://earthexplorer.usgs.gov/
##Importa dados de SRTM 30 - fonte: "earthdata.nasa.gov"

srtm <- raster("ASTGTMV003_S16W047_dem.tif")
srtm1 <- raster("ASTGTMV003_S16W048_dem.tif")
srtm2 <- raster("ASTGTMV003_S16W049_dem.tif")
srtm3 <- raster("ASTGTMV003_S17W047_dem.tif")
srtm4 <- raster("ASTGTMV003_S17W048_dem.tif")
srtm5 <- raster("ASTGTMV003_S17W049_dem.tif")

#Faz merge - ou seja - junta as camadas de SRTM carregadas
srtmmosaic <- mosaic(srtm,
                     srtm1,
                     srtm2,
                     srtm3,
                     srtm4,
                     srtm5,
                     fun = mean)
srtmmosaic ##Verifica arquivo criado nas linhas anteriores

#plota elevacao
windows(8,8)
plot(srtmmosaic)##Vamos visualizar - cria plotagem

#Podemos utilizar o arquivo DEM para calcular os seus derivados - "commands: raster::terrain()" 

area_slope <- terrain(srtmmosaic, opt = 'slope', unit = 'degrees') #calculate slope

area_aspect <- terrain(srtmmosaic, opt = 'aspect', unit = 'degrees') #calculate aspect

area_slope #podemos verificar o arquivo criado aqui
area_aspect

#extraindo dados - SRTM (elevacao)
values_srtm <- extract(srtmmosaic,pontos)#extraindo dados para as coordenadas

valores_srtm <- cbind.data.frame(coordinates(pontos),values_srtm)#convertendo, como tabela

#extraindo dados - SRTM (inclinacao)
values_slope <- extract(area_slope,pontos)#extraindo dados para as coordenadas

valores_slope <- cbind.data.frame(coordinates(pontos),values_slope)#convertendo, como tabela

#Prepara data frame
dados_env <- cbind.data.frame(valores_wc, values_slope, values_srtm)
dados_env<- cbind.data.frame(d$catalogNumber, d$higherGeographyID, dados_env)
#exporta data frame
write.csv(dados_env,"env_date.csv", row.names = FALSE)



library(rgdal)
sp1 <- readOGR("C:/Users/Yan Felipe/Desktop/Dados_esp_amb/Hydroshed/RiverATLAS_v10_shp/RiverATLAS_v10_sa_south.shp")
sp2 <- readOGR("C:/Users/Yan Felipe/Desktop/Dados_esp_amb/Hydroshed/BasinATLAS_v10_shp/BasinATLAS_v10_lev12.shp")

#Carrega aruivo SHP - package shapefile
#sp1 <- read.shp("C:/Users/Yan Felipe/Desktop/Dados_esp_amb/Hydroshed/RiverATLAS_v10_shp/RiverATLAS_v10_sa_south.shp")
#sp2 <- read.shp("C:/Users/Yan Felipe/Desktop/Dados_esp_amb/Hydroshed/BasinATLAS_v10_shp/BasinATLAS_v10_lev12.shp")

