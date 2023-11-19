library(tidyverse)
library(readxl)
library(writexl)
library(zoo)
library(patchwork)
theme_set(theme_classic())

## Lakukan setwd('') jika diperlukan

setwd("C:/github/prospera/excise/docs") ## Ubah dengan wd anda

## Loading 2 different datasets.

trd<-read_excel("cukai_use2.xlsx") ## Pastikan format filenya sama

prices<-read_excel("trad_prices.xlsx") ## Pastikan format filenya sama

## Treating trd

trd$mo<-"mo"

trd<-trd |> mutate(mo=case_when(BULAN=="Jan"~"01",
                            BULAN=="Feb"~"02",
                            BULAN=="Mar"~"03",
                            BULAN=="Apr"~"04",
                            BULAN=="May"~"05",
                            BULAN=="Jun"~"06",
                            BULAN=="JUL"~"07",
                            BULAN=="Aug"~"08",
                            BULAN=="Sep"~"09",
                            BULAN=="Oct"~"10",
                            BULAN=="Nov"~"11",
                            BULAN=="Dec"~"12"))

trd$dates<-as.yearmon(paste(trd$Tahun, " ", trd$mo), "%Y %m")

trd$quarter<-case_when(trd$BULAN=="Jan"|trd$BULAN=="Feb"|trd$BULAN=="Mar"~1,
                       trd$BULAN=="Apr"|trd$BULAN=="May"|trd$BULAN=="Jun"~2,
                       trd$BULAN=="Jul"|trd$BULAN=="Aug"|trd$BULAN=="Sep"~3,
                       trd$BULAN=="Oct"|trd$BULAN=="Nov"|trd$BULAN=="Dec"~4)
trd$date<-as.yearqtr(paste(trd$Tahun, " ", trd$quarter), "%Y %q")

trd2<-trd |>
  group_by(date,kind) |> 
  summarise(RT=sum(RT),QM=sum(QM))

## Treating prices: making quarters
  
prices$date<-as.yearqtr(paste(prices$tahun, " ", prices$quarter), "%Y %q")

## Combine 2 datasets

trd3<-left_join(trd2,prices,by=join_by(date,kind))

trd3$CB<-trd3$RT/trd3$QM*1000
trd3$CR<-trd3$CB/trd3$htp

trd3 |> write_xlsx("final_data.xlsx")

## Plots

trd3 |>
  ggplot(aes(x=date,y=QM,color=kind,linetype=kind))+geom_line(linewidth=1.1)+
  labs(y="Kuantitas (miliar batang)",
       x="Kuartal",
       title = "Kuantitas produksi 3 jenis rokok",
       caption="sumber: Bea Cukai")+
  ggsave("produksi.png")
trd3 |>
  ggplot(aes(x=date,y=htp,color=kind,linetype=kind))+geom_line(linewidth=1.1)+
  labs(y="Harga (rupiah/batang)",
       x="Kuartal",
       title = "Harga Transaksi Pasar (HTP) 3 jenis rokok",
       caption="sumber: Bea Cukai")+
  ggsave("htp.png")
trd3 |>
  ggplot(aes(x=date,y=RT,color=kind,linetype=kind))+geom_line(linewidth=1.1)+
  labs(y="Revenue (Triliun rupiah)",
       x="Kuartal",
       title = "Total tariff revenue 3 jenis rokok",
       caption="sumber: Bea Cukai")+
  ggsave("revenue.png")
trd3 |>
  ggplot(aes(x=date,y=CR,color=kind,linetype=kind))+geom_line(linewidth=1.1)+
  labs(y="Cukai (% equivalent)",
       x="Kuartal",
       title = "Advalorem equivalent cukai 3 jenis rokok",
       caption="sumber: Bea Cukai")+
  ggsave("advalorem.png")

## Regression

