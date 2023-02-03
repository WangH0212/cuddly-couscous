#' Quantitative Stratigraphy Data Conversion function
#'
#' This function allows you to convert your data between data formats required by several quantitative stratigraphy software.
#' The function needs "tidyverse", "xlsx" and "stringr" package as front.
#' 
#' @param input Enter the format of your data as follow: "STANDARD","SINOCOR","CONOP","RASC"
#' @param output Enter the format the data you want to get as follow: "STANDARD","SINOCOR","CONOP","RASC"
#' @param file1,file2,file3 Enter the path to the input data file, file1=.xlsx/.dic/.sct, file2=.dep/.evt,file3=.dat
#' @keywords Quantitative Stratigraphy
#' @export
#' @examples 
#' date2day_fun("STANDARD", "RASC", "C:/Users/Unknown/example.xlsx")
#' 
qs.convert <- function(input,output,file1,file2,file3)
{options(useFancyQuotes = FALSE)
  switch(input,
         STANDARD={data1 <- read.xlsx(file1,1)
         colnames(data1) <- c("Taxa","Section","thickness_FO","thickness_LO","level_FO","level_LO","weight_FO","weight_LO")},
         SINOCOR={data3 <- read.xlsx(file1,1)  
         data3 <- data3[,-1]
         data4 <- colnames(data3)
         for (l in seq(2,length(data4),2)) {data4[l+1] <- paste(data4[l],"LO.thickness",sep="_");data4[l] <- paste(data4[l],"FO.thickness",sep="_")}
         colnames(data3) <- data4
         data1 <- pivot_longer(data3,!Taxa, names_to = c( "Section",".value"),  names_sep = "_",  values_drop_na = TRUE) %>% arrange(Section, Taxa) %>% cbind(FO.level="",LO.level="",FO.weight="",LO.weight="")
         colnames(data1) <- c("Taxa","Section","thickness_FO","thickness_LO","level_FO","level_LO","weight_FO","weight_LO")},
         CONOP={CONOP_back1 <- read.table(file1,sep="'",fill=T)
         CONOP_back2 <- read.table(file2,sep="'",fill=T)
         CONOP_back3 <- read.table(file3)
         data1 <- mutate(CONOP_back3,Taxa=CONOP_back2$V4[match(V1,CONOP_back2$V1)],data_type=case_when(V2 == "1" ~ "FO",V2 == "2" ~ "LO"),Section=CONOP_back1$V4[match(V3,CONOP_back1$V1)],thickness=as.numeric(V4),level=as.numeric(V5),weight=as.numeric(V7),V1=NULL,V2=NULL,V3=NULL,V4=NULL,V5=NULL,V6=NULL,V7=NULL,V8=NULL) %>% pivot_wider(names_from=data_type,values_from=c(thickness,level,weight),names_glue="{.value}_{data_type}") %>% as.data.frame()
         colnames(data1) <- c("Taxa","Section","thickness_FO","thickness_LO","level_FO","level_LO","weight_FO","weight_LO")},
         RASC={RASC_back1 <- read.table(file1,sep="\n",fill=T) %>% apply(2, function(x){gsub(pattern = "                                     ", replacement = ",",x)}) %>% as.data.frame() %>% separate(V1,c("Taxa_name","Taxa_number"),"[,]")
         RASC_back5 <- read.table(file2,sep="\n",fill=T)
         RASC_back6 <- separate(RASC_back5,V1,c("V1","V2","V3","V4","V5","V6","V7","V8","V9"),"[,]")
         RASC_back7 <- as.data.frame(lapply(RASC_back6,as.numeric))
         RASC_back8 <- data.frame(RASC_back6[2,1],RASC_back6[5,1])
         i=1
         j=5
         k=1
         l=3
         for(i in 1:(nrow(RASC_back6)*ncol(RASC_back6)))
         {
           if(k>9){k=1;j=j+1;l=l+1;}
           if(is.na(RASC_back7[j,k])&k>1){k=1;j=j+4;l=3}
           else if (is.na(RASC_back7[j,k])&k==1){j=j+3;l=3}
           RASC_back8[i,1] <- RASC_back6[j-l,1];
           RASC_back8[i,2] <- RASC_back6[j,k];
           k=k+1}
         RASC_back8 <- na.omit(RASC_back8)
         RASC_back9 <- read.fwf(file3,widths = rep(4,20))
         RASC_back10 <- as.data.frame(lapply(RASC_back9,as.numeric))
         RASC_back11 <- data.frame(V1=str_trim(paste0(na.omit(RASC_back9[1,1]),na.omit(RASC_back9[1,2]),na.omit(RASC_back9[1,3]),na.omit(RASC_back9[1,4]),na.omit(RASC_back9[1,5]),na.omit(RASC_back9[1,6]),na.omit(RASC_back9[1,7]),na.omit(RASC_back9[1,8]),na.omit(RASC_back9[1,9]),na.omit(RASC_back9[1,10]),na.omit(RASC_back9[1,11]),na.omit(RASC_back9[1,12]),na.omit(RASC_back9[1,13]),na.omit(RASC_back9[1,14]),na.omit(RASC_back9[1,15]),na.omit(RASC_back9[1,16]),na.omit(RASC_back9[1,17]),na.omit(RASC_back9[1,18]),na.omit(RASC_back9[1,19]),na.omit(RASC_back9[1,20])),"right"),V2=RASC_back6[5,1])
         i=1
         j=2
         k=1
         l=1
         for(i in 1:(nrow(RASC_back9)*ncol(RASC_back9)))
         {
           if(k>20){k=1;j=j+1;l=l+1;}
           
           if(is.na(RASC_back10[j,k])&k>1){k=1;j=j+2;l=1}
           else if (is.na(RASC_back10[j,k])&k==1){j=j+1;l=1}
           if(grepl("LAST",RASC_back9[j,k])|grepl("LAST",RASC_back9[j-1,k])){break;}
           RASC_back11[i,1] <- str_trim(paste0(na.omit(RASC_back9[j-l,1]),na.omit(RASC_back9[j-l,2]),na.omit(RASC_back9[j-l,3]),na.omit(RASC_back9[j-l,4]),na.omit(RASC_back9[j-l,5]),na.omit(RASC_back9[j-l,6]),na.omit(RASC_back9[j-l,7]),na.omit(RASC_back9[j-l,8]),na.omit(RASC_back9[j-l,9]),na.omit(RASC_back9[j-l,10]),na.omit(RASC_back9[j-l,11]),na.omit(RASC_back9[j-l,12]),na.omit(RASC_back9[j-l,13]),na.omit(RASC_back9[j-l,14]),na.omit(RASC_back9[j-l,15]),na.omit(RASC_back9[j-l,16]),na.omit(RASC_back9[j-l,17]),na.omit(RASC_back9[j-l,18]),na.omit(RASC_back9[j-l,19]),na.omit(RASC_back9[j-l,20])),"right");
           RASC_back11[i,2] <- RASC_back9[j,k];k=k+1}
         RASC_back11 <- na.omit(RASC_back11)
         RASC_back12 <- data.frame(Section=RASC_back11[,1],Taxa=as.numeric(RASC_back11[,2]))
         i=1
         j=1
         RASC_back12[i,3] <- RASC_back8[j,2]
         for(i in 2:(nrow(RASC_back12)))
         {if(RASC_back12[i,2]<0){RASC_back12[i,3] <- RASC_back8[j,2]}
           else{j=j+1;RASC_back12[i,3] <- RASC_back8[j,2]}}
         RASC_back14 <- na.omit(data.frame(Section=RASC_back12[,1],Taxa=RASC_back1$Taxa_name[match(abs(RASC_back12$Taxa),RASC_back1$Taxa_number)],RASC_back12[,3])) %>% separate(Taxa,c("Taxa","Type"),sep="_")
         colnames(RASC_back14) <- c("Section","Taxa","Data_type","thickness")
         data1<- pivot_wider(RASC_back14,names_from=Data_type,values_from=thickness,names_glue = "{.value}_{Data_type}") %>% mutate(thickness_FO=as.numeric(thickness_FO),thickness_LO=as.numeric(thickness_LO),level_FO="",level_LO="",weight_FO="",weight_LO="") %>% subset(select=c(2,1,4,3,5,6,7,8)) %>% as.data.frame()
         colnames(data1) <- c("Taxa","Section","thickness_FO","thickness_LO","level_FO","level_LO","weight_FO","weight_LO")})
  switch(output,
         STANDARD={write.xlsx(data1,"STANDARD.xlsx",sheetName="1",row.names=FALSE,showNA = FALSE)},
         SINOCOR={data2 <- pivot_wider(data1,id_cols=Taxa,names_from=Section,values_from=c(thickness_FO,thickness_LO),names_glue="{Section}_{.value}", names_vary="slowest",names_repair = "universal")
         write.xlsx(data2,"SinoCor.xlsx",sheetName="Fossil ranges",showNA = FALSE)},
         CONOP={data1[data1==""] <- NA
         CONOP_sct1<- data1 %>% group_by(Section) %>% summarise(thickness_FO=sum(thickness_FO),thickness_LO=sum(thickness_LO))
         CONOP_sct2 <- mutate(Section_name=sQuote(Section),Section=NULL,thickness_FO=NULL,thickness_LO=NULL,data_type=ifelse(is.na(CONOP_sct1[,2])&is.na(CONOP_sct1[,3]),0,1),section_abbreviation=sQuote(str_c(rownames(CONOP_sct1),substr(CONOP_sct1$Section,1,3),sep="_")),section_number=rownames(CONOP_sct1),order_of_sections_in_fence_diagrams=rownames(CONOP_sct1),CONOP_sct1) %>% subset(select=c(4,3,5,1,2))
         write.table(CONOP_sct2,file="CONOP.sct",sep="    ",row.names = FALSE,col.names = FALSE,quote = FALSE)
         CONOP_evt1 <- distinct(data1,Taxa)
         CONOP_evt2 <- mutate(Event_name=sQuote(Taxa),Taxa=NULL,Event_abbreviation=sQuote(str_c("EVT",rownames(CONOP_evt1),sep="_")),Event_number=rownames(CONOP_evt1),CONOP_evt1) %>% subset(select=c(3,2,1))
         write.table(CONOP_evt2,file="CONOP.evt",sep="    ",row.names = FALSE,col.names = FALSE,quote = FALSE)
         CONOP_dat1 <- pivot_longer(data1,cols = thickness_FO:last_col(),names_to = c(".value", "Event_type"), names_sep = "_",              values_drop_na = TRUE)%>% group_by(Section) %>% mutate(level= dense_rank(thickness))%>%  arrange(Section, level) %>% mutate(Taxa=sQuote(Taxa),Section=sQuote(Section))%>%  mutate(Taxa=as.numeric(CONOP_evt2$Event_number[match(Taxa,CONOP_evt2$Event_name)]),Section=as.numeric(CONOP_sct2$section_number[match(Section,CONOP_sct2$Section_name)]),Event_type=as.numeric(case_when(Event_type=="FO"~"1",Event_type=="LO"~"2")),Extension_direction=Event_type,weight=replace_na(as.numeric(weight),1),weight2=weight,thickness=sprintf("%0.2f",thickness)) %>% subset(select=c(1,3,2,4,5,7,6,8)) %>% arrange(Taxa, Event_type)
         write.table(CONOP_dat1,file="CONOP.dat",sep="    ",row.names = FALSE,col.names = FALSE,quote = FALSE)
         CONOP_cfg1 <- str_c("  SECTIONS=",max(as.integer(CONOP_sct2$section_number)))
         CONOP_cfg2 <- str_c("  TAXA=",max(as.integer(CONOP_evt2$Event_number)))
         CONOP_cfg3 <- str_c("  MAX_LEVELS=",max(as.integer(CONOP_dat1$level)))
         CONOP_cfg4 <- c("  ","  &getinn","  PROJECT='Notitle'",CONOP_cfg1,CONOP_cfg2,"  EVENTS=0",CONOP_cfg3,"  MAX_LABELS=15","  LOADFILE='CONOP.dat'","  PREPFILE='OFF'","  SECTFILE='CONOP.sct'","  SECTTAGFILE='OFF.tag'","  SECTTAGS='OFF.dat'","  LABELFILE='OFF.lbl'","  EVENTFILE='CONOP.evt'","  EVENTTAGFILE='OFF.tag'","  EVENTTAGS='OFF.dat'","  BESTKNOWN=0.000","  /","   ","  &getans","  PENALTY='INTERVAL'","  LETCONTRACT='OFF'","  WEIGHTING='ON'","  USENEGATIVE='OFF'","  NEARENOUGH=5.000","  EXCLUSIVES='NO'","  FORCECOEX='SS'","  FORCEFb4L='ON'","  HOMERANGE='SL'","  SMOOTHER=0.000","  SQUEEZER=0.000","  SHRINKER=0.000","  TEASER=0.010","  STACKER='COEX'","  /","  ","  &getrun","  SOLVER='anneal'","  STEPS=500","  TRIALS=500","  STARTEMP=200","  RATIO=0.980000019","  HOODSIZE='BIG'","  STARTYPE='RAND'","  STARTSECT=1","  STARTEVENT=0","  SHOWMOVIES='CHT'","  TRAJECTORY='ALL'","  VIDEOMODE='SVGA'","  PAUSES='OFF'","  CURVFILE='grid.grd'","  CRV2FILE='grd2.gr2'","  /","  ","  &getout","  COLUMNS=7","  UNLOADMAIN='outmain.txt'","  FITS_OUT='ON'","  CNFG_OUT='ON'","  SEQN_OUT='ON'","  INCR_OUT='ON'","  LOC_OUT='ON'","  OBS_OUT='ON'","  COMP_OUT='ON'","  UNLOADSECT='outsect.txt'","  SECT_OUT='MIN'","  UNLOADEVNT='outevnt.txt'","  EVNT_OUT='ON'","  COEX_OUT='ON'","  RUNLOGFILE='runlog.txt'","  CULLFILE='cull.txt'","  SOLNLIST='OFFsolution.sln'","  STARTFILE='soln.dat'","  STEPFILE='OFFstepsoln.dat'","  BESTARTFILE='bestsoln.dat'","  COMPOSFILE='cmpst.dat'","  COMPOSNMBR=1","  COMPOSTYPE='ZST'","  OBSDFILE='ab.dat'","  PLCDFILE='albet.dat'","  EXTNFILE='delta.dat'","  COEXISTFILE='coex.dat'","  FAD_LADFILE='fb4l.dat'","  ORDERFILE='ordr.dat'","  /")
         cat(CONOP_cfg4, file = "conop9.cfg", sep = "\n")},
         RASC={data1[data1==""] <- NA
         RASC1 <- data1[,1:4] %>% pivot_longer(cols = thickness_FO:last_col(),names_to = c(".value", "Event_type"), names_sep = "_") %>% mutate(Taxa=str_c(Taxa,Event_type,sep = "_"),Event_type=NULL)
         RASC_dic1 <- distinct(RASC1,Taxa)
         RASC_dic2 <- mutate(RASC_dic1,Taxa_number=rownames(RASC_dic1))
         LAST <- c("LAST",rep("",ncol(RASC_dic2)-1))
         RASC_dic3 <- rbind(RASC_dic2,LAST)
         write.table(RASC_dic3,file="RASC.dic",sep="                                     ",row.names = FALSE,col.names = FALSE,quote = FALSE)
         RASC_dep1 <- RASC1 %>% na.omit() %>% group_by(Section) %>%distinct(thickness) %>% arrange(Section, -thickness)
         RASC_dep1$thickness <- str_pad(sprintf("%0.2f",RASC_dep1$thickness),7,side="left"," ")
         RASC_dep2 <- data.frame(c("DECIMAL DEPTH FILE",paste(RASC_dep1[1,1],"                           "),"m  0.0    0.0","AUTHOR none",as.character(RASC_dep1[1,2])))
         i=5
         j=2
         for(j in 2:length(RASC_dep1$thickness))
         {if (RASC_dep1[j,1] != RASC_dep1[j-1,1]&str_length(RASC_dep2[i,1])<71){RASC_dep2[i,1] <-  paste0(RASC_dep2[i,1],",");RASC_dep2[i+1,1] <- paste(RASC_dep1[j,1],"                           ");RASC_dep2[i+2,1] <- "m  0.0    0.0";RASC_dep2[i+3,1] <- "AUTHOR none";i=i+4}
           else if (RASC_dep1[j,1] != RASC_dep1[j-1,1]&str_length(RASC_dep2[i,1])>=71){RASC_dep2[i+1,1] <- paste(RASC_dep1[j,1],"                           ");RASC_dep2[i+2,1] <- "m  0.0    0.0";RASC_dep2[i+3,1] <- "AUTHOR none";i=i+4}
           if (is.na(RASC_dep2[i,1])){RASC_dep2[i,1] <- as.character(RASC_dep1[j,2])}
           else if (str_length(RASC_dep2[i,1])<71){RASC_dep2[i,1] <-               paste(RASC_dep2[i,1],RASC_dep1[j,2],sep=",")}
           else{i=i+1;RASC_dep2[i,1] <- as.character(RASC_dep1[j,2])}}
         if (str_length(RASC_dep2[i,1])<71){RASC_dep2[i,1] <-  paste0(RASC_dep2[i,1],",")}
         RASC_dep2[i+1,1] <- "LAST"
         write.table(RASC_dep2,file="RASC.dep",row.names = FALSE,col.names = FALSE,quote = FALSE)
         RASC_dat1 <- mutate(RASC1,Taxa=as.numeric(RASC_dic3$Taxa_number[match(Taxa,RASC_dic3$Taxa)])) %>% arrange(Section, -thickness) %>% na.omit()
         l=2
         for(l in 2:length(RASC_dat1$thickness)){if(RASC_dat1[l,3]==RASC_dat1[l-1,3]&RASC_dat1[l,2]==RASC_dat1[l-1,2]){RASC_dat1[l,1] <- -RASC_dat1[l,1]}}
         RASC_dat2 <- data.frame(c(str_pad(RASC_dat1[1,2],80,side="right"," "),as.character(str_pad(RASC_dat1[1,1],4,side="left"," "))))
         i=2
         j=2
         for(j in 2:length(RASC_dat1$thickness))
         {if (str_length(RASC_dat2[i,1])<80&RASC_dat1[j,2] != RASC_dat1[j-1,2]){RASC_dat2[i,1] <-  str_pad(paste0(RASC_dat2[i,1],"-999"),80,side="right"," ");RASC_dat2[i+1,1] <- str_pad(RASC_dat1[j,2],80,side="right"," ");i=i+2}
           else if (str_length(RASC_dat2[i,1])>=80&RASC_dat1[j,2] != RASC_dat1[j-1,2]){i=i+1;RASC_dat2[i,1] <-  str_pad("-999",80,side="right"," ");RASC_dat2[i+1,1] <- str_pad(RASC_dat1[j,2],80,side="right"," ");i=i+2}
           if (is.na(RASC_dat2[i,1])){RASC_dat2[i,1] <- as.character(str_pad(RASC_dat1[j,1],4,side="left"," "))}
           else if (str_length(RASC_dat2[i,1])<80){RASC_dat2[i,1] <-               paste0(RASC_dat2[i,1],str_pad(RASC_dat1[j,1],4,side="left"," "))}
           else{i=i+1;RASC_dat2[i,1] <- as.character(str_pad(RASC_dat1[j,1],4,side="left"," "))}}
         if (str_length(RASC_dat2[i,1])<80){RASC_dat2[i,1] <-  str_pad(paste0(RASC_dat2[i,1],"-999"),80,side="right"," ");RASC_dat2[i+1,1] <-"LAST"}else{i=i+1;RASC_dat2[i,1] <-  str_pad("-999",80,side="right"," ");RASC_dat2[i+1,1] <- "LAST"}
         write.table(RASC_dat2,file="RASC.dat",row.names = FALSE,col.names = FALSE,quote = FALSE)
         RASC_inp1 <- c(length(unique(RASC1$Section)),"6","0","1","1","3","1")
         cat(RASC_inp1, file = "RASC.inp", sep = " ")})}