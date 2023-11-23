library(tidyverse) ## for data wrangling
library(readxl) ## Reading excel files
library(writexl) ## Writing excel files
library(zoo) ## Time series manipulation
library(sjPlot) ## Saving regression into html
theme_set(theme_classic()) ## Theming ggplot

## Lakukan setwd('') jika diperlukan

setwd("C:/github/prospera/excise/docs") ## Ubah dengan wd anda

## Loading 2 different datasets.

trd<-read_excel("cukai_use2.xlsx") ## Pastikan format filenya sama

prices<-read_excel("trad_prices.xlsx") ## Pastikan format filenya sama

pdb<-read_excel("ekon.xlsx") ## Pastikan format filenya sama

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

## Quarterly for other two dataset
  
prices$date<-as.yearqtr(paste(prices$tahun, " ", prices$quarter), "%Y %q")
pdb$date<-as.yearqtr(paste(pdb$tahun, " ", pdb$quarter), "%Y %q")

## Combine 2 datasets

trd3<-left_join(trd2,prices,by=join_by(date,kind))
trd3<-left_join(trd3,pdb)

trd3$CB<-trd3$RT/trd3$QM*1000
trd3$CR<-trd3$CB/trd3$htp

trd3 |> write_xlsx("final_data.xlsx")

## Total all traditional cigarettes

trdsum<-trd3 |>
  group_by(date) |> 
  summarise(htp=weighted.mean(htp,QM),RT=sum(RT),QM=sum(QM))
trdsum<-left_join(trdsum,pdb)
trdsum$CB<-trdsum$RT/trdsum$QM*1000
trdsum$CR<-trdsum$CB/trdsum$htp

## Plots

trd3 |>
  ggplot(aes(x=date,y=QM,color=kind,linetype=kind))+geom_line(linewidth=1.1)+
  labs(y="Kuantitas (miliar batang)",
       x="Kuartal",
       title = "Kuantitas produksi 3 jenis rokok",
       caption="sumber: Bea Cukai")
ggsave("fig/produksi.png")
trd3 |>
  ggplot(aes(x=date,y=htp,color=kind,linetype=kind))+geom_line(linewidth=1.1)+
  labs(y="Harga (rupiah/batang)",
       x="Kuartal",
       title = "Harga Transaksi Pasar (HTP) 3 jenis rokok",
       caption="sumber: Bea Cukai")
ggsave("fig/htp.png")
trd3 |>
  ggplot(aes(x=date,y=RT,color=kind,linetype=kind))+geom_line(linewidth=1.1)+
  labs(y="Revenue (Triliun rupiah)",
       x="Kuartal",
       title = "Total tariff revenue 3 jenis rokok",
       caption="sumber: Bea Cukai")
ggsave("fig/revenue.png")
trd3 |>
  ggplot(aes(x=date,y=CR,color=kind,linetype=kind))+geom_line(linewidth=1.1)+
  labs(y="Cukai (% equivalent)",
       x="Kuartal",
       title = "Advalorem equivalent cukai 3 jenis rokok",
       caption="sumber: Bea Cukai")
ggsave("fig/advalorem.png")

## Logging everything

ltrd3<-trd3 |>
  mutate(RT=log(RT),
         QM=log(QM),
         htp=log(htp),
         hje=log(hje),
         CB=log(CB),
         quarter=as.factor(quarter),
         y=log(y))

ltrdsum<-trdsum |>
  mutate(htp=log(htp),
         RT=log(RT),
         QM=log(QM),
         CB=log(CB),
         quarter=as.factor(quarter),
         y=log(y))
  
## Final data note: 
### Gunakan ltrd3 untuk regresi biasa/masing2 jenis rokok
### Gunakan ltrdsum untuk regresi total (SKM+SPM+SKT)

## Regression

### Quantity vs Price
#### Tabel ini sama dengan tabel 5.1 dan 5.2 di file report.pdf

ggplot(data=ltrd3,aes(y=QM,x=htp,color=kind,shape=kind))+geom_point(size=2)+
  stat_smooth(method = "lm")+labs(x="log price",y="log quantity")
ggsave("fig/cs_elasticity.png")

reg1sum<-lm(QM~htp+y+quarter,data=ltrdsum)
reg1all<-lm(QM~htp+y+kind+quarter,data=ltrd3)
reg1skm<-lm(QM~htp+y+quarter,data=subset(ltrd3,kind=="SKM"))
reg1spm<-lm(QM~htp+y+quarter,data=subset(ltrd3,kind=="SPM"))
reg1skt<-lm(QM~htp+y+quarter,data=subset(ltrd3,kind=="SKT"))

tab_model(reg1sum,reg1all,reg1skm,reg1spm,reg1skt,
          p.style="stars",collapse.se=T,show.ci = F,
          dv.labels = c("Total","All","SKM","SPM","SKT"),
          file="reg/elasticity.html")

### Tax-price passthrough
#### Tabel ini sama dengan tabel 5.4 dan 5.5 di report.pdf

ggplot(data=ltrd3,aes(y=htp,x=CB,color=kind,shape=kind))+geom_point(size=2)+
  stat_smooth(method = "lm")+labs(x="log excise",y="log price")
ggsave("fig/cs_taxprice.png")

reg2sum<-lm(htp~CB+y+quarter,data=ltrdsum)
reg2all<-lm(htp~CB+y+kind+quarter,data=ltrd3)
reg2skm<-lm(htp~CB+y+quarter,data=subset(ltrd3,kind=="SKM"))
reg2spm<-lm(htp~CB+y+quarter,data=subset(ltrd3,kind=="SPM"))
reg2skt<-lm(htp~CB+y+quarter,data=subset(ltrd3,kind=="SKT"))

tab_model(reg2sum,reg2all,reg2skm,reg2spm,reg2skt,
          p.style="stars",collapse.se=T,show.ci = F,
          dv.labels = c("Total","All","SKM","SPM","SKT"),
          file="reg/taxprice.html")

### Tax-revenue

ggplot(data=ltrd3,aes(y=RT,x=CB,color=kind,shape=kind))+geom_point(size=2)+
  stat_smooth(method = "lm")+labs(x="log excise",y="log total revenue")
ggsave("fig/cs_taxrev.png")

ltrdsum$CB2<-ltrdsum$CB*ltrdsum$CB
ltrd3$CB2<-ltrd3$CB*ltrd3$CB

### Monotonic curve
#### Tabel ini sama dengan tabel 5.6 dan 5.7 di report.pdf

reg3sum<-lm(RT~CB+y+quarter,data=ltrdsum)
reg3all<-lm(RT~CB+y+kind+quarter,data=ltrd3)
reg3skm<-lm(RT~CB+y+quarter,data=subset(ltrd3,kind=="SKM"))
reg3spm<-lm(RT~CB+y+quarter,data=subset(ltrd3,kind=="SPM"))
reg3skt<-lm(RT~CB+y+quarter,data=subset(ltrd3,kind=="SKT"))

tab_model(reg3sum,reg3all,reg3skm,reg3spm,reg3skt,
          p.style="stars",collapse.se=T,show.ci = F,
          dv.labels = c("Total","All","SKM","SPM","SKT"),
          file="reg/taxrev.html")

### Laffer curve
#### Yang ini menggunakan cukai kuadrat untuk memeriksa hubungan parabola dari
#### cukai vs revenue.

reg3sum<-lm(RT~CB2+CB+y+quarter,data=ltrdsum)
reg3all<-lm(RT~CB2+CB+y+kind+quarter,data=ltrd3)
reg3skm<-lm(RT~CB2+CB+y+quarter,data=subset(ltrd3,kind=="SKM"))
reg3spm<-lm(RT~CB2+CB+y+quarter,data=subset(ltrd3,kind=="SPM"))
reg3skt<-lm(RT~CB2+CB+y+quarter,data=subset(ltrd3,kind=="SKT"))

tab_model(reg3sum,reg3all,reg3skm,reg3spm,reg3skt,
          p.style="stars",collapse.se=T,show.ci = F,
          dv.labels = c("Total","All","SKM","SPM","SKT"),
          file="reg/taxrev2.html")
