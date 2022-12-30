#### setting ####
library(ggplot2)
library(gridExtra)
library(dplyr)
library(ggmap)
library(raster)
library(knitr)
library(rmarkdown)
library(dplyr)
library(rgeos)
library(maps)
library(knitr)
library(rgdal)
library(tidyr)
library(stringr)
library(lubridate)
library(TTR)
library(rvest)
library(R6)
####금리조정 ####f
data<-read.csv("코로나_한국금리_코스피1.csv",header=T)
data$한국금리<-ifelse(data$일자<="2007-07-12",4.75,
                  ifelse(data$일자<=" 2007-08-09",5,
                         ifelse(data$일자<=" 2008-08-07",5.25,
                                ifelse(data$일자<=" 2008-10-09",5.00,
                                       ifelse(data$일자<=" 2008-10-27",4.25,
                                              ifelse(data$일자<="2008-11-07",4.00,
                                                     ifelse(data$일자<="2008-12-11",4.00,
                                                            ifelse(data$일자<="2009-01-09",2.50,
                                                                   ifelse(data$일자<="2009-02-12",2.00,
                                                                          ifelse(data$일자<=" 2010-07-09",2.25,
                                                                                 ifelse(data$일자<=" 2010-11-16 ",2.50,
                                                                                        ifelse(data$일자<=" 2010-11-16 ",2.50,
                                                                                               ifelse(data$일자<=" 2011-01-13 ",2.75,
                                                                                                      ifelse(data$일자<=" 2011-03-10 ",3.00,
                                                                                                             ifelse(data$일자<=" 2011-06-10 ",3.25,
                                                                                                                    ifelse(data$일자<="2012-07-12 ",3.00,
                                                                                                                           ifelse(data$일자<="2012-10-11",2.75,
                                                                                                                                  ifelse(data$일자<="2013-05-09",2.50,
                                                                                                                                         ifelse(data$일자<="2014-08-14",2.25, 
                                                                                                                                                ifelse(data$일자<="2014-10-15",2.00, 
                                                                                                                                                       ifelse(data$일자<="2015-03-12", 1.75,
                                                                                                                                                              ifelse(data$일자<="2015-06-11",1.50,
                                                                                                                                                                     ifelse(data$일자<=" 2016-06-09",1.25,
                                                                                                                                                                            ifelse(data$일자<="2017-11-30",1.50,
                                                                                                                                                                                   ifelse(data$일자<="2018-11-30", 1.75,
                                                                                                                                                                                          ifelse(data$일자<="2019-07-18",1.50,
                                                                                                                                                                                                 ifelse(data$일자<="2020-03-17",1.50,
                                                                                                                                                                                                        ifelse(data$일자<="2020-05-28",0.50,0.75))))))))))))))))))))))))))))


#### EDA ####
graph<-function(x,k,t){
  data<-x
  
  if (k==""){
    data1<-subset(data,select=c("일자",t))
    print(data1)
    colnames(data1)<-c("일자","asd")
    
    a<-ggplot(data1,aes(x=일자,y=asd))+
      geom_line()
      
  }else {
    data1<-subset(data,select=c("일자",k,t))
    
    colnames(data1)<-c("일자","asd","qwe")
    
    a<-ggplot(data1,aes(x=일자,y=asd),col="blue")+
      geom_line()+
      geom_line(aes(x=일자,y=qwe),col="red")
  }
  
  return(a)
  
}

x11();
a<-graph(total,"","종가")+labs(title="종가에 따른 추세파악",y="종가")
b<-graph(total,"","시가")+labs(title="시가에 따른 추세파악",y="시가")
c<-graph(total,"","고가")+labs(title="고가에 따른 추세파악",y="고가")
d<-graph(total,"","저가")+labs(title="저가에 따른 추세파악",y="저가")
graph(total,"","거래량")+labs(title="거래량에 따른 추세파악",y="거래량")
x11()
grid.arrange(a,b,c,d,nrow=2)
####덧붙여서 코로나 확진자 추세도 파악

####연도와 거래량의 상관관계 및 상관분석 파악 ####
str(total)
total$연도<-year(total$일자)
total$월<-month(total$일자)

#변동시점 파악해서 상관성파악
#이때 금리 변동 후 어느정도까지 영향성을 미치는지 파악
cor1_covid_graph<-function(x,y){
  data<-x
  
  #상관분석 및 상관관계 파악
  
  if (y=="종가") {
    result<-cor.test(data$한국금리,data$종가)
    sm=data %>% group_by(연도) %>% summarise(cor(한국금리,종가))
    print(sm)
  
  }else if(y=="시가"){
    result<-cor.test(data$한국금리,data$시가)
    sm=data %>% group_by(연도) %>% summarise(cor(한국금리,시가))
    print(sm)
    
  }else if(y=="고가"){
    result<-cor.test(data$한국금리,data$고가)
    sm=data %>% group_by(연도) %>% summarise(cor(한국금리,고가))
    print(sm)
    
  }else if(y=="저가"){
    result<-cor.test(data$한국금리,data$저가)
    sm=data %>% group_by(연도) %>% summarise(cor(한국금리,저가))
    print(sm)
  
  }else if(y=="거래량"){
    result<-cor.test(data$한국금리,data$거래량)
    sm=data %>% group_by(연도) %>% summarise(cor(한국금리,거래량))
    print(sm)
  }
}
   #상관분석 한 값을 생성하여 파악

#### 함수 파악 ####
cor1(total,"종가")
cor1(total,"시가")
cor1(total,"고가")
cor1(total,"저가")
cor1(total,"거래량") #19년 21년 왜 NA가 나오는지 파악

#### ####
x11();

####금리와 종가 그래프 대비 파악 ####
#단위성 파악
#=>할때 그냥 scale을 하자!!!!@!!!
##함수는 scale임.

#### 이중 축 그래프 ####
vvs<-function(x,y){
  data<-x
  if (y=="종가") {
    a=ggplot(data,aes(x=일자, y=종가,col="종가"))+
      geom_line()+
      geom_line(aes(x=일자, y=한금금리변형,col="한국금리"))+
      labs(title="역대 한국 금리 추세")+
      scale_y_continuous(sec.axis = sec_axis(~./1000, name="한국금리"))
    
  }else if(y=="시가"){
    a=ggplot(data,aes(x=일자, y=시가,col="시가"))+
      geom_line()+
      geom_line(aes(x=일자, y=한금금리변형,col="한국금리"))+
      labs(title="역대 한국 금리 추세")+
      scale_y_continuous(sec.axis = sec_axis(~./1000, name="한국금리"))
    
  }else if(y=="고가"){
    a=ggplot(data,aes(x=일자, y=고가,col="고가"))+
      geom_line()+
      geom_line(aes(x=일자, y=한금금리변형,col="한국금리"))+
      labs(title="역대 한국 금리 추세")+
      scale_y_continuous(sec.axis = sec_axis(~./1000, name="한국금리"))
    
  }else if(y=="저가"){
    a=ggplot(data,aes(x=일자, y=저가,col="저가"))+
      geom_line()+
      geom_line(aes(x=일자, y=한금금리변형,col="한국금리"))+
      labs(title="역대 한국 금리 추세")+
      scale_y_continuous(sec.axis = sec_axis(~./1000, name="한국금리"))
    
  }else if(y=="거래량"){
    
   a=ggplot(data,aes(x=일자, y=거래량, col="거래량"))+
      geom_line()+
      geom_line(aes(x=일자, y=한금금리변형1,col="한국금리"))+
      labs(title="역대 한국 금리 추세")+
      scale_y_continuous(sec.axis = sec_axis(~./1000000, name="한국금리"))
  }
  return(a) 
  }

x11();
a<-vvs(total,"종가")
b<-vvs(total,"고가")
c<-vvs(total,"시가")
d<-vvs(total,"저가")
grid.arrange(a,b,c,d,nrow=2)
x11();
vvs(total,"거래량")


data1<-read.csv("제출용.csv",header=T)

data1$연<-year(data1$일자)
data1$월<-month(data1$일자)
data1$일<-day(data1$일자)
data1_2<-aggregate(종가~연+월, data1, mean)
data1_2$일자<-as.Date(paste0(data1_2$연,"-",data1_2$월,"-01"))
data1_3=data1_2 %>% arrange(일자)
data1_4=data1_3[,3]
#### 계절성 ####
df<-ts(data1_4,start=c(2007,1),end=c(2021,11),frequency=12)
asd<-decompose(df)
x11();
plot(asd)
plot(df)
x11();
par(mfrow=c(2,1))
acf(diff(df))
pacf(diff(df))
x11();
plot(df,main="코스피지수 예측, 실제값 비교 그래프")



##### ####
df_1=diff(df,1)
auto.arima(df) #ARIMA(0,0,1)(2,0,0)
a
a.arima <- arima(df, order=c(0,1,1),seasonal=list(order=c(2,0,0),period=12))

forcast1<-forecast(a.arima) #
lines(forcast1$fitted,col="red")
plot(forcast1,main="2021~2023년 11월 까지 예측 그래프")
#비정상성을 상징하기 때문에 이때 단위근 검정을 실시한다.
adf.test(df,k=0) #그냥 실시하는 경우_0.5785 차분의 의미를 파악한다.
adf.test(diff(df),k=0)  #0.01

x11();
Box.test(a.arima$residuals,type='Ljung-Box')
tsdiag(a.arima)
