library(ggplot2)
library(maps)
library(mapdata)

#variable que setea el top
filtro <- 20

# Read CSV into R
products <- read.csv(file="C:/Users/Diego/Downloads/BFPD_csv_07132018/Products.csv", header=TRUE, sep=",")
nutrients <- read.csv(file="C:/Users/Diego/Downloads/BFPD_csv_07132018/Nutrients.csv", header=TRUE, sep=",")

#ejemplo de un plot nativo
plot(products$manufacturer)


#para unir ambos csv
merged_data <- merge(products, nutrients, by = "NDB_Number")



#Crea una tabla con todas las entradas de proteinas
protein_obs <- merged_data$Nutrient_name == "Protein"
proteins <- merged_data[protein_obs, ]
#ordena las proteinas descendentemente
sorted_proteins2 <- proteins[order(-proteins$Output_value),]
#crea un top20
top20_proteins <- sorted_proteins[1:filtro,]


#merge de toda la informacion de los 20 top elementos con proteina (para ver los demas nutrientes)
merged_protein_data <- merge(top20_proteins, nutrients, by = "NDB_Number")
#Crea una tabla con todas las entradas de energia
energy_obs <- merged_protein_data$Nutrient_name.y == "Energy"
energies <- merged_protein_data[energy_obs, ]
#ordena las energias descendentemente
sorted_energies <- energies[order(-energies$Output_value.y),]
sorted_energies <- sorted_energies[1:filtro,]

##################################################################################################################################

#plot nativo que no funciona
#plot(top20_proteins$long_name, top20_proteins$Output_value)

#grafico que muestra el top20 de proteinas)
ggplot(data=top20_proteins, aes(x = top20_proteins$Output_value, 
                            y = top20_proteins$long_name)) + geom_point(size=3) + scale_y_discrete(limits=top20_proteins$long_name)


##################################################################################################################################
#Variables creadas para crear el grafico final
Type = sorted_energies$long_name
Value1 = c(sorted_energies$Output_value.x)
Value2 = c(sorted_energies$Output_value.y)
FOOD_NAME <-rep(Type,2)
QUANTITY <-c(Value1,Value2)
Groups<-c(rep("Protein",filtro),rep("Energy",filtro))

df<-data.frame(FOOD_NAME,QUANTITY,Groups)

#grafico final
ggplot(df, aes(x=FOOD_NAME,y=QUANTITY, fill=Groups)) + geom_bar(stat='identity', position = "fill")  + coord_flip() + theme_classic() + 
  labs(x = "Type of Food", y = "Quantity", title = "Top 20 Types of Food with Protein (+ Energy)") + scale_fill_brewer(palette = "Blues")

#+ theme(axis.text.x = element_text(angle = 90)) #crea el texto del eje X vertical
               
##################################################################################################################################

#cargar un mapa
usa_map <- map_data("usa") # we already did this, but we can do it again
ggplot() + geom_polygon(data = usa_map, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
