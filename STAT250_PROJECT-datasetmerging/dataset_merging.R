

Sys.setlocale("LC_ALL", "tr_TR.UTF-8") #setting this bc we will see some turkish city names in console


library(rvest)                          #installing required packages
library(httr)
library(readxl)
library(dplyr)

mean_iq_url <- "https://www.zekatestimerkezi.com/istatistikler.php"     #scrapping iq and long-lat data

res <- GET(mean_iq_url)
html_con <- content(res, "text")
html_mean_iq <- read_html(html_con)

mean_iq <- html_mean_iq |> 
  html_nodes("table") |> 
  html_table()
mean_iq <- as.data.frame(mean_iq)

long_lat_url <- "https://www.odevde.com/illerin-koordinatlari/"

res <- GET(long_lat_url)
html_con <- content(res, "text")
html_long_lat <- read_html(html_con)

long_lat <- html_long_lat |> 
  html_nodes("table") |> 
  html_table()
long_lat <- as.data.frame(long_lat[[1]])
colnames(long_lat) <- long_lat[1, ]
long_lat <- long_lat[2:82, ]
head(long_lat)
class(long_lat)

colnames(mean_iq) <- c("order", "city", "mean_iq", "number_of_candidates")
colnames(long_lat) <- c("city", "long", "lat")


iq_data <- merge(mean_iq, long_lat, by = "city", all.x = TRUE)        #merging them
head(iq_data)



illere_gore_egitim <- read_excel("egitim_durumu_2020_fixed.xlsx")
head(illere_gore_egitim)


tr_data <- merge(iq_data, illere_gore_egitim, by = "city", all = TRUE)
head(tr_data)

tr_data <- tr_data[-4]


akraba_evliligi_orani <- read_excel("akraba_evliligi_orani.xlsx")
tr_data <- merge(tr_data, akraba_evliligi_orani, by = "city", all = TRUE)
tr_data
tr_data <- tr_data[-2]
head(tr_data)
str(tr_data)

nufus_url <- "https://tr.wikipedia.org/wiki/2020_T%C3%BCrkiye_adrese_dayal%C4%B1_n%C3%BCfus_kay%C4%B1t_sistemi_sonu%C3%A7lar%C4%B1"

res <- GET(nufus_url)
html_con <- content(res, "text")
nufus_html <- read_html(html_con)

nufus <- nufus_html |> 
  html_nodes("table") |> 
  html_table()
nufus_table <- nufus[2]
nufus_table <- data.frame(nufus_table)
nufus_table <- nufus_table[2:3]
str(nufus_table)
colnames(nufus_table) <- c("city", "population")
pos <- which(nufus_table$city == "Hakkari")            #there is a problem with 'hakkari', i just fixed it
nufus_table$city[pos] <- "Hakkari"
head(nufus_table)
str(nufus_table)
nufus_table$population <- as.character(nufus_table$population)
nufus_table$population <- as.integer(gsub("\\.", "", nufus_table$population)) 

tr_data_pop <- merge(tr_data, nufus_table, by = "city", all.x = TRUE)
                                                                        #we can't use the population directly, we are looking for proportions
tr_data_pop$okumamis_orani <- (tr_data_pop$okuma_yazma_bilmeyen_toplam + tr_data_pop$okuma_yazma_evet_okul_hayir_toplam) / tr_data_pop$population
str(tr_data_pop)
tr_data_pop$okumus_orani <- (tr_data_pop$lisans_toplam + tr_data_pop$yl_toplam + tr_data_pop$doktora_toplam) / tr_data_pop$population

ortalama_ogrenci_sayisi <- read_excel("ogretmen_basina_ogrenci.xlsx")
ortalama_ogrenci_sayisi <- ortalama_ogrenci_sayisi[-2:-4]
colnames(ortalama_ogrenci_sayisi) <- c("city", "ogretmen_basina_dC<sen_ortalama_ogrenci_sayisi(ilkokul, ortaokul, lise")
head(ortalama_ogrenci_sayisi)
ortalama_ogrenci_sayisi$city <- sub("-.*", "", ortalama_ogrenci_sayisi$city)      #fixing the problem with city names
ortalama_ogrenci_sayisi$`ogretmen_basina_dC<sen_ortalama_ogrenci_sayisi(ilkokul, ortaokul, lise` <- round(ortalama_ogrenci_sayisi$`ogretmen_basina_dC<sen_ortalama_ogrenci_sayisi(ilkokul, ortaokul, lise`, digits = 2)
str(ortalama_ogrenci_sayisi)
head(ortalama_ogrenci_sayisi)
ortalama_ogrenci_sayisi$`ogretmen_basina_dC<sen_ortalama_ogrenci_sayisi(ilkokul, ortaokul, lise` <- as.numeric(ortalama_ogrenci_sayisi$`ogretmen_basina_dC<sen_ortalama_ogrenci_sayisi(ilkokul, ortaokul, lise`)
?round

tr_data_pop <- merge(tr_data_pop, ortalama_ogrenci_sayisi, by = "city")
str(tr_data_pop)
kutup_cocuk <- read_excel("kutup_kayitli_cocuk.xlsx")
kutup_cocuk$city <- sub("-.*", "", kutup_cocuk$city)
tr_data_pop_cult <- merge(tr_data_pop, kutup_cocuk, by = "city")

cocuk_nufus <- read_excel("il_basina_cocuk.xlsx")
trdata <- merge(tr_data_pop_cult, cocuk_nufus, by = "city")
trdata$kutuphaneye_giden_cocuk_orani <- trdata$halk_kutuphanesine_kayitli_cocuk_sayisi / trdata$cocuk_nufus



trdata_sum <- trdata %>%          #these are variables that we can use in our analysis. just took a summarized dataset for analysis.
  select(il_kodu,city, long, lat, mean_iq, kutuphaneye_giden_cocuk_orani, `ogretmen_basina_dC<sen_ortalama_ogrenci_sayisi(ilkokul, ortaokul, lise`, akraba_evliligi_orani, okumus_orani, okumamis_orani)

trdata_sum$long <- as.numeric(gsub(",", ".", trdata_sum$long))      #long and lat are character with commas, fixing it
trdata_sum$lat <- as.numeric(gsub(",", ".", trdata_sum$lat))

?write.csv
write.table(trdata_sum, "tr_data.csv", sep = ",", fileEncoding = "UTF-8")
data_read <- read.csv("tr_data.csv", fileEncoding = "UTF-8")
