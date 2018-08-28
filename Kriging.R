library(ggmap)
library(viridis)
options(jupyter.plot_mimetypes = 'image/png') # reduces file size

#Faz a leitura
df<-read.csv("Dengue\\dados_finais.csv")
df$Dengue<-1
dados<-df[,c("Dengue","latitude","longitude")]
colnames(dados)<-c("Dengue","lat","lon")
# remove any rows with missing data
dados <- dados[complete.cases(dados), ]


# load a basemap
### Set a range
lat <- c(min(dados$lat), max(dados$lat))                
lon <- c(min(dados$lon), max(dados$lon))   

### Get a map
basemap <- get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 6,
               maptype = "satellite", source = "google")


ggmap(basemap) + 
  geom_point(data = dados, col = "red", size = .1)

dados1<-subset(dados,lon> -53)
dados2<-subset(dados,lon< -53)


#Região 1
pdf("Mapa1.pdf", paper="a4r",width=0,height=0)
lat <- c(min(dados1$lat), max(dados1$lat))                
lon <- c(min(dados1$lon), max(dados1$lon))  
basemap1 <- get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 14,
                   maptype = "satellite", source = "google")
ggmap(basemap1) + 
  geom_point(data = dados1, col = "red", size = 3)
dev.off()

pdf("Mapa2.pdf", paper="a4r",width=0,height=0)
ggmap(basemap1) + 
  stat_density2d(aes(fill = ..level..), alpha = .2,
                 geom = "polygon", data = dados1) + 
  scale_fill_viridis() + 
  theme(legend.position = 'none')+  geom_point(data = dados1, col = "red", size = 3)
dev.off()

pdf("Mapa3.pdf", paper="a4r",width=0,height=0)
ggmap(basemap1) + 
  stat_density2d(aes(fill = ..level..), alpha = .2,
                 geom = "polygon", data = dados1) + 
  scale_fill_viridis() + 
  theme(legend.position = 'none')
dev.off()


#You can pass arguments for kde2d through the call to stat_density2d. In this case, we alter the argument h, which is a bandwidth parameter related to the spatial range or smoothness of the density estimate.
ggmap(basemap) + 
ggmap(basemap1) + 
  stat_density2d(aes(fill = ..level..), alpha = .5, 
                 h = .02, n = 300,
                 geom = "polygon", data = dados1) + 
  scale_fill_viridis() + 
  theme(legend.position = 'none')






dengue<-ggmap(map, extent="device", legend="topleft")
dengue<- dengue + geom_point(data=dados, aes(x=longitude, y=latitude), size=Dengue)
dengue+geom_density2d(mapping=aes(x = longitude, y = latitude),data = dados)