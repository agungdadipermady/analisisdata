library(ggplot2)
library(sp)
library(rgdal)
library(rgeos)
library(ggrepel)
library(reshape2)
library(leaflet)
library(htmltools)
library(cluster)
library(factoextra)
library(htmlwidgets)

df = read.csv("Soal-01.csv",stringsAsFactors = F)
names(df)[names(df) == "kode_kota_kabupaten"] <- "id"
df2 = dcast(df, provinsi+id+nama_kota_kabupaten~tahun,value.var = "prevalensi_stunting", mean)
jabar <- readOGR(dsn = "kota_kabupaten.geojson")
jabar$y_2015 = df2[match(as.character(jabar$bps_kode),df2$id),4]
jabar$y_2016 = df2[match(as.character(jabar$bps_kode),df2$id),5]
jabar$y_2017 = df2[match(as.character(jabar$bps_kode),df2$id),6]
jabar$coor=coordinates(jabar)
jabar$kemendagri_nama = as.character(jabar$kemendagri_nama)
d.jabar=data.frame(jabar)
#label
d.jabar$coor=coordinates(jabar)

jabar_df <- fortify(jabar, region = "bps_kode")
jabar_df = merge(x = df, y = jabar_df, by = "id", all = TRUE)
class(jabar_df$tahun)


for(i in 1:length(unique(jabar_df$tahun))){
  f_name = paste("Prevalensi Stunting Jabar ",unique(jabar_df$tahun)[i],".png",sep = "")
  png(f_name, units="in", width=16, height=8, res=300)
  p=ggplot() +
    geom_polygon(data = jabar_df[jabar_df$tahun %in% unique(jabar_df$tahun)[i],], 
                 aes(x=long, y=lat, group = group,fill = prevalensi_stunting),color='white')  +
    geom_label_repel(data = d.jabar, aes(x = d.jabar$coor[,1], 
                                         y = d.jabar$coor[,2], 
                                         label = d.jabar$kemendagri_nama),col="black",size=3)+
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line.y = element_blank(),
          axis.text.x = element_text(),text = element_text(size=12),
          plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = 'white', colour = 'white'),
          legend.position="right")+
    labs(x="Longitude",y="Latitude",
         title = paste("Data Prevalensi Stunting di Jawa Barat Tahun",
                       unique(jabar_df$tahun)[i],sep = " "))+
    scale_fill_gradient2(low = "white", high = "blue")
  print(p)
  dev.off()
}


pal <- colorNumeric(palette = "Blues",domain = jabar$y_2015)
pal2 <- colorNumeric(palette = "Blues",domain = jabar$y_2016)
pal3 <- colorNumeric(palette = "Blues",domain = jabar$y_2017)
labels1 <- paste(jabar$kemendagri_nama, jabar$y_2015,"2015",sep="<br/>")%>% lapply(htmltools::HTML)
labels2 <- paste(jabar$kemendagri_nama, jabar$y_2016,"2016",sep="<br/>")%>% lapply(htmltools::HTML)
labels3 <- paste(jabar$kemendagri_nama, jabar$y_2017,"2017",sep="<br/>")%>% lapply(htmltools::HTML)

m <- leaflet(jabar) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
  addPolygons(color = ~pal(y_2015),
                  smoothFactor = 0.2,
                  stroke = FALSE,
                  fillOpacity = 1,
                  highlight = highlightOptions(color = "green",
                                               weight = 2,
                                               fillOpacity = 0.5,
                                               bringToFront = TRUE),
                  label = labels1,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
                  group = "2015") %>%
  addPolygons(color = ~pal2(y_2016),
              smoothFactor = 0.2,
              stroke = FALSE,
              fillOpacity = 1,
              highlight = highlightOptions(color = "green",
                                           weight = 2,
                                           fillOpacity = 0.5,
                                           bringToFront = TRUE),
              label = labels2,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "2016") %>%
  addPolygons(color = ~pal3(y_2017),
              smoothFactor = 0.2,
              stroke = FALSE,
              fillOpacity = 1,
              highlight = highlightOptions(color = "green",
                                           weight = 2,
                                           fillOpacity = 0.5,
                                           bringToFront = TRUE),
              label = labels3,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "2017") %>%
  addCircleMarkers(radius = 5,~coor[,1], ~coor[,2], 
                   label = ~htmlEscape(kemendagri_nama),color="red",group = "label") %>%
  addLayersControl(baseGroups = c("2015", "2016","2017"),
                   overlayGroups = c("label"),
                   options = layersControlOptions(collapsed = FALSE))


saveWidget(m, file="stunting_jabar.html")


#clustering

mydf = df2[,c("nama_kota_kabupaten","2015","2016","2017")]
rownames(mydf) = mydf[,c("nama_kota_kabupaten")]
mydf = mydf[,-1]

#menghitung k optimal
set.seed(060294)
gap_stat <- clusGap(mydf, FUN = kmeans, nstart = 5,
                    K.max = 8, B = 150)

fviz_gap_stat(gap_stat)

set.seed(060294)
final <- kmeans(mydf, 4, nstart = 3)
print(final)
fviz_cluster(final, data = mydf)

#perbedaan median 2 kelompok bebas
df$type = ifelse(grepl("kota",df$nama_kota_kabupaten,ignore.case = T),"Kota","Kabupaten")
wilcox.test(prevalensi_stunting~type,data=df)

b = boxplot(prevalensi_stunting~type,data=df, main="Jawa Barat", 
        ylab="Prevalensi Stunting")

print(b)
