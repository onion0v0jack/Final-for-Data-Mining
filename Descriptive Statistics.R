#1. Training的樹種統計圖
barplot(table(df$Cover_Type)[c(7,5,6,2,1,3,4)],main="Training的樹種統計圖",col=brewer.pal(9, "Blues")[3:9],xlab="Tree Cover Type")

#2. Training的土壤統計圖
dfSoil=table(df$Soil)[c(1,11,22,33,35,36,37,38,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21,23,24,25,26,27,28,29,30,31,32,34)]
dfSoilN=c(a[c(1:6)],0,a[c(7:13)],0,a[c(14:38)]); names(dfSoilN)=c(1:40)
barplot(dfSoilN,main="Training的樹種栽種土壤統計圖",col=brewer.pal(12,"Set3"),xlab="Soil Type")

#3. Training的地區統計圖
barplot(table(df$Region)[4:1],main="Training的地區統計圖",col=brewer.pal(9,"Reds")[c(8,6,4,2)],xlab="Region Type")

#4. 從df繪製樹種與土壤的柱狀圖
ggplot(data=df) +
  geom_bar(aes(x=Cover_Type,fill=Cover_Type),color="black") + 
  facet_wrap(~Soil,scale="free") +
  theme_bw() +
  xlab("Count") + 
  ylab("Tree Cover Type") + 
  ggtitle("樹種種植土壤統計圖")+
  theme(axis.text=element_blank()) + 
  theme(legend.position= "bottom")

#5. 從df繪製樹種與區域的柱狀圖
ggplot(data=df) +
  geom_bar(aes(x=Cover_Type),fill="pink",color="black") + #"#66CC99"display.brewer.pal(5, "Dark2")
  facet_wrap(~Region) +
  coord_flip() +
  theme_bw() +
  xlab("Count") + 
  ylab("Tree Cover Type") + 
  ggtitle("Coverage Type vs Region")


