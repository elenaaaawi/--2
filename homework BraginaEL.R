#Считываем данные
library(readxl)
D<-read_excel("problems_1.xlsx")
#Выбыраем необходимые данные и преобразуем их
Data<-data.frame(D$AdmArea[2:length(D$AdmArea)], D$Month[2:length(D$Month)],
                 as.numeric(D$Year[2:length(D$Year)]),
                 as.numeric(D$TotalAmount[2:length(D$TotalAmount)]))
colnames(Data)<-c("AdmArea","Month","Year","TotalAmount")
View(Data)
#расчитать среднюю сумму начисленных средств(TotalAmount) 
#в разрезе административных округов с детализацией по 2016, 2017, 2018, 2019 гг.
Data2<-subset(Data,Data$Year<=2019)
library(rpivotTable)
rpivotTable::rpivotTable(Data2,rows="AdmArea",cols="Year", 
                         vals="TotalAmount",aggregatorName = "Average")
#рассчитать суммарные помесячные начисления за 2021 год.
Data3<-subset(Data,Data$Year==2021)
rpivotTable::rpivotTable(Data3,rows="Month", 
                         vals="TotalAmount",aggregatorName = "Sum")
#построить график средних ежегодных начислений по всем административным округам
Data3<-rpivotTable::rpivotTable(Data2,rows="Year", 
                                vals="TotalAmount",
                                aggregatorName = "Average")
View(Data3)
#Из исходных данных отобрать только центральный административный округ, создав из него новый датафрейм.
Dcenter<-subset(Data,Data$AdmArea=="Центральный административный округ")
View(Dcenter)
#В созданном датафрейме отобрать только те месяца, начисления в которых 
#оказались выше среднего уровня начислений в течение соответствующего года
Current<-data.frame()
for (i in c(2016:2021)) Current<-rbind(Current,mean(Dcenter$TotalAmount[Dcenter$Year==i]))
Current<-data.frame(Current,c(2016:2021))
colnames(Current)<-c("Total","Year")
View(Current)
DAvg<-rpivotTable::rpivotTable(Dcenter,rows="Year",vals="TotalAmount", aggregatorName = "Average")
View(DAvg)
result<-data.frame()
for (i in Current$Year)
{
  j<-as.vector(Dcenter$Month[(Dcenter$Year==i) & (Dcenter$TotalAmount>=Current$Total[Current$Year==i])])
  print(c(i,j))
  for (k in j) {result<-rbind(result, c(i,k), stringsAsFactors=FALSE)}
}
colnames(result)<-c("Year","Month")
View(result)
result<-data.frame(as.numeric(result$Year),as.character(result$Month),stringsAsFactors = FALSE)
colnames(result)<-c("Year","Month")
View(result)
save(Dcenter,result,file="res.dat")
load("res.dat")
View(Dcenter)
Cсылка на github: https://github.com/elenaaaawi/Project_My_BraginaEL
