library(tidyverse)
library(kit)
library(rvest)


setwd("C:/Users/gilbe/Desktop/Arbeit Lehrstuhl Statistik/Landtagswahlen")

drop_identical_columns <-function(table){
  table_new <- table[,!duplicated(colnames(table))]
  return(table_new)
}

###IMPORT FILE FOR UPDATE
dat_SU <- read_csv("dat_W 26.09.2021_new.csv")
dat_SU_compare <- dat_SU

###ADD LINKS FOR STATES TO UPDATE
html_BW <- "https://www.wahlrecht.de/umfragen/landtage/baden-wuerttemberg.htm"
html_BY <- "https://www.wahlrecht.de/umfragen/landtage/bayern.htm"
html_BE <- "https://www.wahlrecht.de/umfragen/landtage/berlin.htm"
html_BB <- "https://www.wahlrecht.de/umfragen/landtage/brandenburg.htm"
html_HB <- "https://www.wahlrecht.de/umfragen/landtage/bremen.htm"
html_HH <- "https://www.wahlrecht.de/umfragen/landtage/hamburg.htm"
html_HE <- "https://www.wahlrecht.de/umfragen/landtage/hessen.htm"
html_MV <- "https://www.wahlrecht.de/umfragen/landtage/mecklenburg-vorpommern.htm"
html_NI <- "https://www.wahlrecht.de/umfragen/landtage/niedersachsen.htm"
html_NW <- "https://www.wahlrecht.de/umfragen/landtage/nrw.htm"
html_RP <- "https://www.wahlrecht.de/umfragen/landtage/rheinland-pfalz.htm"
html_SL <- "https://www.wahlrecht.de/umfragen/landtage/saarland.htm"
html_SN <- "https://www.wahlrecht.de/umfragen/landtage/sachsen.htm"
html_ST <- "https://www.wahlrecht.de/umfragen/landtage/sachsen-anhalt.htm"
html_SH <- "https://www.wahlrecht.de/umfragen/landtage/schleswig-holstein.htm"
html_TH <- "https://www.wahlrecht.de/umfragen/landtage/thueringen.htm"

###SCRAPE DATA
content_BW <- read_html(html_BW)
content_BY <- read_html(html_BY)
content_BE <- read_html(html_BE)
content_BB <- read_html(html_BB)
content_HB <- read_html(html_HB)
content_HH <- read_html(html_HH)
content_HE <- read_html(html_HE)
content_MV <- read_html(html_MV)
content_NI <- read_html(html_NI)
content_NW <- read_html(html_NW)
content_RP <- read_html(html_RP)
content_SL <- read_html(html_SL)
content_SN <- read_html(html_SN)
content_ST <- read_html(html_ST)
content_SH <- read_html(html_SH)
content_TH <- read_html(html_TH)

###TRANSFORM HTML DATA
tables_BW <- content_BW %>% html_table(fill= TRUE)
tables_BY <- content_BY %>% html_table(fill= TRUE)
tables_BE <- content_BE %>% html_table(fill= TRUE)
tables_BB <- content_BB %>% html_table(fill= TRUE)
tables_HB <- content_HB %>% html_table(fill= TRUE)
tables_HH <- content_HH %>% html_table(fill= TRUE)
tables_HE <- content_HE %>% html_table(fill= TRUE)
tables_MV <- content_MV %>% html_table(fill= TRUE)
tables_NI <- content_NI %>% html_table(fill= TRUE)
tables_NW <- content_NW %>% html_table(fill= TRUE)
tables_RP <- content_RP %>% html_table(fill= TRUE)
tables_SL <- content_SL %>% html_table(fill= TRUE)
tables_SN <- content_SN %>% html_table(fill= TRUE)
tables_ST <- content_ST %>% html_table(fill= TRUE)
tables_SH <- content_SH %>% html_table(fill= TRUE)
tables_TH <- content_TH %>% html_table(fill= TRUE)

table_BW <- tables_BW[[2]]
table_BY <- tables_BY[[2]]
table_BE <- tables_BE[[2]]
table_BB <- tables_BB[[2]]
table_HB <- tables_HB[[2]]
table_HH <- tables_HH[[2]]
table_HE <- tables_HE[[2]]
table_MV <- tables_MV[[2]]
table_NI <- tables_NI[[2]]
table_NW <- tables_NW[[2]]
table_RP <- tables_RP[[2]]
table_SL <- tables_SL[[2]]
table_SN <- tables_SN[[2]]
table_ST <- tables_ST[[2]]
table_SH <- tables_SH[[2]]
table_TH <- tables_TH[[2]]

###ASSIGN LABEL FOR EACH STATE
table_BW$BL <- "BW"
table_BY$BL <- "BY"
table_BE$BL <- "BE"
table_BB$BL <- "BB"
table_HB$BL <- "HB"
table_HH$BL <- "HH"
table_HE$BL <- "HE"
table_MV$BL <- "MV"
table_NI$BL <- "NI"
table_NW$BL <- "NW"
table_RP$BL <- "RP"
table_SL$BL <- "SL"
table_SN$BL <- "SN"
table_ST$BL <- "ST"
table_SH$BL <- "SH"
table_TH$BL <- "TH"

###DROP IDENTICAL COLUMNS
table_BW <- drop_identical_columns(table_BW)
table_BY <- drop_identical_columns(table_BY)
table_BE <- drop_identical_columns(table_BE)
table_BB <- drop_identical_columns(table_BB)
table_HB <- drop_identical_columns(table_HB)
table_HH <- drop_identical_columns(table_HH)
table_HE <- drop_identical_columns(table_HE)
table_MV <- drop_identical_columns(table_MV)
table_NI <- drop_identical_columns(table_NI)
table_NW <- drop_identical_columns(table_NW)
table_RP <- drop_identical_columns(table_RP)
table_SL <- drop_identical_columns(table_SL)
table_SN <- drop_identical_columns(table_SN)
table_ST <- drop_identical_columns(table_ST)
table_SH <- drop_identical_columns(table_SH)
table_TH <- drop_identical_columns(table_TH)

###BIND TABLES AND CREATE ONE DATASET
dat_new <- bind_rows(table_BW,
          table_BY,
          table_BE,
          table_BB,
          table_HB,
          table_HH,
          table_HE,
          table_MV,
          table_NI,
          table_NW,
          table_RP,
          table_SL,
          table_SN,
          table_ST,
          table_SH,
          table_TH)

### CREATE DATAFRAME
dat_new <- as.data.frame(dat_new)
dat_new <- dat_new[3:nrow(dat_new),]
colnames(dat_new)[5] <- "empty" 
dat_new <- dat_new %>% select(-one_of("empty"))

dat_new %>% select(everything())%>%filter(BL =="HB") %>% pull(Institut)
dat_SU %>% select(everything()) %>% filter(BL == "HB") %>% pull(end_date)

### CLEAN DATA REGARDING STOPWORDS
stopwords_Institut<-"LINKE|GRÜNE|Telefon|CDU|SPD|Umfragewert|Landtagswahl|Abgeordnetenhauswahl|Befragte|Bürgerschaftswahl"
dat_new_wo_LW <- dat_new %>% select(everything()) %>% filter(!str_detect(Institut,stopwords_Institut))
dat_new_wo_LW <- dat_new_wo_LW %>% select(-Sonstige,,-NPD,-Auftraggeber)

### TRANSFORM DATA FOR EACH PARTY TO NUMERIC FEATURE SPACE
dat_new_wo_LW$CDU<-as.numeric(gsub(x=str_trim(gsub("%","",x=dat_new_wo_LW$CDU)),",","."),na.rm=TRUE)/100
dat_new_wo_LW$CSU<-as.numeric(gsub(x=str_trim(gsub("%","",x=dat_new_wo_LW$CSU)),",","."),na.rm=TRUE)/100
dat_new_wo_LW$SPD<-as.numeric(gsub(x=str_trim(gsub("%","",x=dat_new_wo_LW$SPD)),",","."),na.rm=TRUE)/100
dat_new_wo_LW$GRÜNE<-as.numeric(gsub(x=str_trim(gsub("%","",x=dat_new_wo_LW$GRÜNE)),",","."),na.rm=TRUE)/100

dat_new_wo_LW$LINKE<-as.numeric(gsub(x=str_trim(gsub("%","",x=dat_new_wo_LW$LINKE)),",","."),na.rm=TRUE)/100

dat_new_wo_LW$AfD<-as.numeric(gsub(x=str_trim(gsub("%","",x=dat_new_wo_LW$AfD)),",","."),na.rm=TRUE)/100
dat_new_wo_LW$FW<-as.numeric(gsub(x=str_trim(gsub("%","",x=dat_new_wo_LW$FW)),",","."),na.rm=TRUE)/100
dat_new_wo_LW$PIRATEN<-as.numeric(gsub(x=str_trim(gsub("%","",x=dat_new_wo_LW$PIRATEN)),",","."),na.rm=TRUE)/100
dat_new_wo_LW$SSW<-as.numeric(gsub(x=str_trim(gsub("%","",x=dat_new_wo_LW$SSW)),",","."),na.rm=TRUE)/100

glimpse(dat_new_wo_LW)

### CLEAN COLUMN - BEFRAGTE
dat_new_wo_LW$Befragte<-gsub("•","",dat_new_wo_LW$Befragte)
dat_new_wo_LW$Befragte<-gsub("T","",dat_new_wo_LW$Befragte)
dat_new_wo_LW$Befragte<-gsub("O","",dat_new_wo_LW$Befragte)
dat_new_wo_LW$Befragte<-gsub("M","",dat_new_wo_LW$Befragte)
dat_new_wo_LW$Befragte<-gsub("F","",dat_new_wo_LW$Befragte)
dat_new_wo_LW$Befragte<-str_trim(dat_new_wo_LW$Befragte)
dat_new_wo_LW$Befragte<-substr(dat_new_wo_LW$Befragte,1,nchar(dat_new_wo_LW$Befragte)-13)
dat_new_wo_LW$Befragte<-gsub("\\.","",dat_new_wo_LW$Befragte)
dat_new_wo_LW$Befragte<-gsub("\\?","",dat_new_wo_LW$Befragte)
dat_new_wo_LW$Befragte <- as.numeric(str_trim(dat_new_wo_LW$Befragte),na.rm=TRUE)

head(dat_new_wo_LW)

colnames(dat_SU)
colnames(dat_new_wo_LW)

### RENAME COLUMNS 
dat_new_wo_LW <- dat_new_wo_LW%>%rename(end_date=Datum,
                       PB_SPD = SPD,
                       PB_Gruene = GRÜNE,
                       PB_FDP = FDP,
                       PB_DIE.LINKE = LINKE,
                       PB_AfD = AfD,
                       PB_Piraten = PIRATEN,
                       PB_FreieW = FW,
                       PB_SSW = SSW,)%>% mutate(PB_OeDP = NA,
                                   PB_Rechte = NA,
                                   PB_Graue = NA,
                                   PB_Schill = NA,
                                   ZANummer = NA,
                                   PB_kA = NA,
                                   PB_WBet = NA,
                                   PB_wirt.Lage_gut = NA,
                                   PB_wirt.Lage_schlecht = NA,
                                   PB_wirt.Lage_teils = NA,
                                   PB_wirt.Lage = NA)

### COMBINE CDU AND CSU COLUMN
dat_new_wo_LW <- transform(dat_new_wo_LW, PB_CDU_CSU=psum(CDU, CSU, na.rm=TRUE))
dat_new_wo_LW <- dat_new_wo_LW %>% select(-CDU,-CSU)
dat_new_wo_LW$end_date <- gsub("\\.","/",dat_new_wo_LW$end_date)

df_Parteien <- dat_new_wo_LW %>% select(PB_CDU_CSU,
                                        PB_SPD,
                                        PB_Gruene,
                                        PB_FDP,
                                        PB_DIE.LINKE,
                                        PB_AfD,
                                        PB_Piraten,
                                        PB_FreieW,
                                        PB_SSW)

### CALCULATE COLUMN - Sonstige
dat_new_wo_LW$PB_Sonstige <- 1 - rowSums(df_Parteien, na.rm = TRUE)
col_order <- colnames(dat_SU)
dat_new_wo_LW <- dat_new_wo_LW[,col_order]

### TRANSFORM DATE COLUMN
dat_new_wo_LW$end_date<-as.Date(dat_new_wo_LW$end_date,format="%d/%m/%Y")
dat_SU$end_date<-as.Date(dat_SU$end_date,format="%d/%m/%Y")

### CREATE TWO DATASET WHICH CONTAIN THE MAX DATE OF OLD DATASET AND MAX DATE OF SCRAPED DATASET
date_max_SU<- dat_SU %>% select(everything()) %>% group_by(BL) %>% summarize(max_date=max(end_date,na.rm = TRUE))
date_max_new <- dat_new_wo_LW %>% select(everything()) %>% group_by(BL) %>% summarise(max_date= max(end_date,na.rm=TRUE))

### IF MAX DATE OF SCRAPED DATASET IS LARGER THAN MAX DATE OF OLD DATASET THAN UPDATE OLD DATASET
### DO THIS FOR EACH
for (i in 1:nrow(date_max_SU)){
  if(date_max_new$max_date[i] > date_max_SU$max_date[i])
  {
    column <- date_max_new$BL[i]
    df_append <- subset(dat_new_wo_LW,BL == column)
    df_append <- df_append %>% filter(end_date > date_max_SU$max_date[i])
    
    dat_SU <- rbind(dat_SU,df_append)
    }
  else{print(paste(date_max_new$BL[i],FALSE))
  }
}

### CHECK UPDATE 1
update <- dat_SU %>% select(everything()) %>% group_by(BL) %>% summarize(min_date=min(end_date,na.rm = TRUE))
old<-dat_new_wo_LW %>% select(everything()) %>% group_by(BL) %>% summarize(min_date=min(end_date,na.rm = TRUE))
update == old

### CHECK UPDATE 2
update_max <- dat_SU %>% select(everything()) %>% group_by(BL) %>% summarize(max_date=max(end_date,na.rm = TRUE))
old_max <-dat_new_wo_LW %>% select(everything()) %>% group_by(BL) %>% summarize(max_date=max(end_date,na.rm = TRUE))
update_max == old_max

### RETRANSFORM DATASET TO THE RESPECTIVE FORMAT
dat_SU <- dat_SU %>% select(everything()) %>% arrange(BL,desc(end_date))
dat_SU$end_date <- as.character(dat_SU$end_date)
dat_SU$end_date <- format(as.Date(dat_SU$end_date, '%Y-%m-%d'), "%d/%m/%Y")

### EXPORT UPDATED DATASET
path_to_export <- "/Users/gilbe/Desktop/Arbeit Lehrstuhl Statistik/Landtagswahlen"
file_name <- paste("//dat_W_",Sys.Date(),".csv",sep="")

write.csv(dat_SU,
          paste(path_to_export,file_name,sep=""),
          row.names = FALSE)
