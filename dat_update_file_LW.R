library(tidyverse)
library(rvest)
library(janitor)
library(gsubfn)
library(tm)
library(matchmaker)
library(kit)

dat_LW <- read.delim("LTW_DATENSATZ3.txt")
dat_LW_compare <- dat_LW

html_BW <- "https://www.wahlrecht.de/ergebnisse/baden-wuerttemberg.htm"
html_BY <- "https://www.wahlrecht.de/ergebnisse/bayern.htm"
html_BE <- "https://www.wahlrecht.de/ergebnisse/berlin.htm"
html_BB <- "https://www.wahlrecht.de/ergebnisse/brandenburg.htm"
html_HB <- "https://www.wahlrecht.de/ergebnisse/bremen.htm"
html_HH <- "https://www.wahlrecht.de/ergebnisse/hamburg.htm"
html_HE <- "https://www.wahlrecht.de/ergebnisse/hessen.htm"
html_MV <- "https://www.wahlrecht.de/ergebnisse/mecklenburg.htm"
html_NI <- "https://www.wahlrecht.de/ergebnisse/niedersachsen.htm"
html_NW <- "https://www.wahlrecht.de/ergebnisse/nordrhein-westfalen.htm"
html_RP <- "https://www.wahlrecht.de/ergebnisse/rheinland-pfalz.htm"
html_SL <- "https://www.wahlrecht.de/ergebnisse/saarland.htm"
html_SN <- "https://www.wahlrecht.de/ergebnisse/sachsen.htm"
html_ST <- "https://www.wahlrecht.de/ergebnisse/sachsen-anhalt.htm"
html_SH <- "https://www.wahlrecht.de/ergebnisse/schleswig-holstein.htm"
html_TH <- "https://www.wahlrecht.de/ergebnisse/thueringen.htm"

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

transform_table_type_1 <- function(table){
  table <- table %>% row_to_names(row_number=1)
  table_t <- t(table)
  rownames_table_t <- rownames(table_t)
  df <- as.data.frame(cbind(table_t, rownames_table_t),row.names = NA)
  df <- df %>% row_to_names(row_number=1)
  
  colnames_df_t <- colnames(df)
  colnames_df_t <- tolower(colnames_df_t)
  colnames_df_t <- removeNumbers(colnames_df_t)
  colnames_df_t <- gsubfn(pattern="[[:punct:]]", engine = "R", 
                          replacement=function(x) 
                            ifelse(x=="/","/",""),colnames_df_t)
  colnames(df) <- colnames_df_t
  #delete last row since only text
  df <- df[1:nrow(df)-1,]
  return(df)
}

transform_table_type_2 <- function(table){
  rownames_table_t <- rownames(table)
  df <- as.data.frame(cbind(table, rownames_table_t),row.names = NA)
  df <- df %>% row_to_names(row_number=1)
  
  colnames_df_t <- colnames(df)
  colnames_df_t <- tolower(colnames_df_t)
  colnames_df_t <- removeNumbers(colnames_df_t)
  colnames_df_t <- gsubfn(pattern="[[:punct:]]", engine = "R", 
                          replacement=function(x) 
                            ifelse(x=="/","/",""),colnames_df_t)
  colnames(df) <- colnames_df_t
  return(df)
}

table_BW <- tables_BW[[2]]
table_BW[nrow(table_BW)+1,] <- "BW"

table_HB <- tables_HB[[2]]
table_HB[nrow(table_HB)+1,] <- "HB"

table_RP <- tables_RP[[2]]
table_RP[nrow(table_RP)+1,] <- "RP"

table_SL <- tables_SL[[2]]
table_SL[nrow(table_SL)+1,] <- "SL"

list_tables_type1 <- list(table_BW,table_HB,table_RP,table_SL)
list_tables_type1 <- lapply(list_tables_type1,transform_table_type_1)

table_BY_t <- t(tables_BY[[2]])
table_BY_t <- cbind(table_BY_t,"BY")

table_BE_t <- t(tables_BE[[2]])
table_BE_t <- cbind(table_BE_t,"BE")

table_BB_t <- t(tables_BB[[2]])
table_BB_t <- cbind(table_BB_t,"BB")

table_HH_t <- t(tables_HH[[2]])
table_HH_t <- cbind(table_HH_t,"HH")

table_HE_t <- t(tables_HE[[2]])
table_HE_t <- cbind(table_HE_t,"HE")

table_MV_t <- t(tables_MV[[2]])
table_MV_t <- cbind(table_MV_t,"MV")

table_NI_t <- t(tables_NI[[2]])
table_NI_t <- cbind(table_NI_t,"NI")

table_NW_t <- t(tables_NW[[2]])
table_NW_t <- cbind(table_NW_t,"NW")

table_SN_t <- t(tables_SN[[2]])
table_SN_t <- cbind(table_SN_t,"SN")

table_ST_t <- t(tables_ST[[2]])
table_ST_t <- cbind(table_ST_t,"ST")

table_SH_t <- t(tables_SH[[2]])
table_SH_t <- cbind(table_SH_t,"SH")

table_TH_t <- t(tables_TH[[2]])
table_TH_t <- cbind(table_TH_t,"TH")

list_tables_type2 <- list(table_BY_t,table_BE_t,table_BB_t,table_HH_t,table_HE_t,
                          table_MV_t,table_NI_t,table_NW_t,table_SN_t,table_ST_t,
                          table_SH_t,table_TH_t)
list_tables_type2 <- lapply(list_tables_type2,transform_table_type_2)

for(i in 1:length(list_tables_type1)){
  colnames(list_tables_type1[[i]])[1] <- "key"
  length_coln <- length(colnames(list_tables_type1[[i]]))
  colnames(list_tables_type1[[i]])[length_coln] <- "t"
  colnames(list_tables_type1[[i]])[length_coln-1] <- "BL"
}

for(i in 1:length(list_tables_type2)){
  colnames(list_tables_type2[[i]])[1] <- "key"
  length_coln <- length(colnames(list_tables_type2[[i]]))
  colnames(list_tables_type2[[i]])[length_coln] <- "t"
  colnames(list_tables_type2[[i]])[length_coln-1] <- "BL"
}

list_LW <- c(list_tables_type1,list_tables_type2)

dat_new <- bind_rows(list_LW[[1]],
                     list_LW[[2]],
                     list_LW[[3]],
                     list_LW[[4]],
                     list_LW[[5]],
                     list_LW[[6]],
                     list_LW[[7]],
                     list_LW[[8]],
                     list_LW[[9]],
                     list_LW[[10]],
                     list_LW[[11]],
                     list_LW[[12]],
                     list_LW[[13]],
                     list_LW[[14]],
                     list_LW[[15]],
                     list_LW[[16]])

dat_new <- dat_new %>% select(where(~!all(is.na(.)))) %>% clean_names()
glimpse(dat_new)
#check if R detects columns as unique
dat_new <- dat_new%>%select(bl,
                            key,
                            t,
                            wahlbeteiligung,
                            cdu,
                            spd,
                            fdp_dvp,
                            grune,
                            afd,
                            pds_die_linke,
                            fdp,
                            die_linke,
                            csu,
                            bundnis,
                            b_grune,
                            grune_gal,
                            pds_die_linke_2)

transform_df <- function(df){
  for(i in 1:ncol(df)){
    col_clean <- df[,i]
    df[,i] <- gsub("–",NA,col_clean)
    df[,i] <- gsub(",",".",col_clean)
  }
  df$t <- gsub("[[:punct:]]","",df$t)
  df$t <- gsub("I","",df$t)
  
  df_new <- df%>%select(-bl,-key,-t)%>%mutate(across(.cols = everything(),.fns = as.numeric,na.rm=TRUE))
  df_new$BL <- df$bl
  df_new$t <- df$t
  return(df_new)
}

dat_new_Sitze <- dat_new%>%select(everything())%>%filter(key=="Sitze")%>%mutate(Sitze= wahlbeteiligung)
dat_new_Sitze <- transform_df(dat_new_Sitze)
dat_new_Sitze <- transform(dat_new_Sitze, CDU_CSU=psum(cdu, csu, na.rm=TRUE))
dat_new_Sitze <- transform(dat_new_Sitze, Gruene=psum(grune, bundnis,b_grune,grune_gal, na.rm=TRUE))
dat_new_Sitze <- transform(dat_new_Sitze, DIE_LINKE=psum(pds_die_linke,die_linke,pds_die_linke_2, na.rm=TRUE))
dat_new_Sitze <- transform(dat_new_Sitze, FDP=psum(fdp_dvp,fdp, na.rm=TRUE))

dat_new_WB <- dat_new%>%select(everything())%>%filter(key=="%")%>%mutate(WB = wahlbeteiligung)
dat_new_WB <- transform_df(dat_new_WB)
dat_new_WB <- transform(dat_new_WB, CDU_CSU.Z=psum(cdu, csu, na.rm=TRUE))
dat_new_WB <- transform(dat_new_WB, Gruene.Z=psum(grune, bundnis,b_grune,grune_gal, na.rm=TRUE))
dat_new_WB <- transform(dat_new_WB, DIE_LINKE.Z=psum(pds_die_linke,die_linke,pds_die_linke_2, na.rm=TRUE))
dat_new_WB <- transform(dat_new_WB, FDP.Z=psum(fdp_dvp,fdp, na.rm=TRUE))

bind_cols(dat_new_Sitze,dat_new_WB)

glimpse(transform_df(dat_new_Sitze))
glimpse(transform_df(dat_new_WB))

dat_new_wo_LW <- transform(dat_transform, CDU=psum(CDU, CSU, na.rm=TRUE))


