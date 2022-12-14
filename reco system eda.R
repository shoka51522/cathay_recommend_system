library(dplyr)
library(ggplot2)
library(plotly)
library(data.table)

test = fread("test data.csv", stringsAsFactors = F); #test$FUNCTION_CATEGORY = NULL

test$time = substr(test$LOG_TIME, 11, 18)
Sys.setlocale("LC_TIME", "C")
test$date = as.Date(substr(test$LOG_TIME, 1, 9), format = "%d%b%Y")
test$datetime = as.POSIXct(paste(test$date, test$time), format = "%Y-%m-%d %H:%M:%S")
test$time = NULL; #test$date = NULL

test$t_1 = lag(test$datetime, k = 1)
#test$diff = test$datetime - test$t_1

test$ag_1 = lag(test$AGIDNO); test$date_1 = lag(test$date)

test[,c(2:4)][test[,2:4] == ""] = NA
test$cs_1 = lag(test$CSIDNO); test$sl_1 = lag(test$SELECT_ID)

test$Y1[test$AGIDNO != test$ag_1 | test$date != test$date_1] = 1; test$Y1[is.na(test$Y1)] = 0
test$Y2[test$CSIDNO != test$cs_1 | test$SELECT_ID != test$sl_1] = 1; test$Y2[is.na(test$Y2)] = 0
#######################################################
t1 = test[,c(2,6,9,10,15,16)]


t1$diff = t1$datetime - t1$t_1
t1$diff[t1$Y1 == 1 | t1$Y2 == 1] = NA
t1$diff = as.numeric(t1$diff)

t2 = t1[(t1$diff <= 492.6| is.na(t1$diff)),]
summary(t2$diff)
t2$to = t2$FUNCTION_NAME
t2$from = lag(t2$FUNCTION_NAME)

# remove fisrt from
t2$from[is.na(t2$diff)] = NA

aa = is.na(t2$from) %>% which
bb = is.na(t2$from) %>% which; bb = bb[2:length(bb)]; aa = aa[1:length(aa)-1]
cc = bb-aa

#######################################################
#time dif
time_dif = matrix(nrow = 26857, ncol = 1)%>%as.data.frame()
time_dif$V1[1] = 107+6

i = 2
for(i in 2:26858){
  time_dif[i,1] = sum(t2$diff[bb[i-1]:bb[i]], na.rm = T)
}

#######################################################
DD$FUNCTION_NAME = gsub("加入收藏", "分享", DD$FUNCTION_NAME)
dd = DD$FUNCTION_NAME

test$X4[test$AGIDNO != test$X1] = 1
test$X5[test$DATE != test$T2] = 1
test$X6[test$X3 == 1 | test$X4 == 1 | test$X5 == 1] = 1

i = 1; k = 1

for(i in 1:nrow(test)){
  if(is.na(test$X3[i]) & is.na(test$X3[i])){test$X5[i] = k}
  if(!is.na(test$X3[i])|!is.na(test$X4[i])){test$X5[i] = k+1}
  if(!is.na(test$X3[i])|!is.na(test$X4[i])){k = k+1}
}

#######################################################
#eda for clicking: seperate clicks by log_no
test2 = test[,c(1,4,13)]

test2 = left_join(test2, DD[,1:3], by = "FUNCTION_NAME")

test2 = subset(test2, !is.na(test2$X6) | !is.na(test2$no))

test2$from = lag(test2$TOPIC_SER_NO)
test2$to = test2$TOPIC_SER_NO
test2$to[!is.na(test2$from) & is.na(test2$to)] = "out"

test2$from[is.na(test2$X6) & is.na(test2$from)] = "home"
test2$from[test2$X6 ==1 & !is.na(test2$to)] = "home"
#test2$from[test2$X6 == 1] = NA
#test2$from[is.na(test2$from) & !is.na(test2$to)] = "home"

test2 = subset(test2, test2$X6 == 1 | test2$from != test2$to)
test2 = subset(test2, !is.na(test2$from) & !is.na(test2$to))
test2 = subset(test2, !(test2$from == "home" & test2$to == "out"))

home = which(test2$from == "home")

test2$log_no = NA #create log_no
i = 1; k = 1
for(i in 1:nrow(test2)){
  if(i>=home[k] & i<home[k+1]){test2$log_no[i] = home[k]}
  if(i+1>=home[k+1]){k = k+1}
}
test2$log_no[home[8162]] = home[8162]
test2$log_no[is.na(test2$from) & is.na(test2$to)] = NA

#count clicks of each log_no, then bind into [test2]
test3 = test2 %>% filter(to != "out") %>%
  group_by(log_no) %>%
  summarise(cc = list(unique(to))) #2.34 clicks in avg.; med = 1
test3$total_clicks = lengths(test3$cc) 

test2 = left_join(test2,test3[,c(1,3)], by = "log_no")

test4 = table(test2$to, test2$log_no) %>% as.data.frame(stringsAsFactors = F)
test4 = test4[test4$Freq>0,]; test4$Freq = NULL; 
test4$key = paste(test4$Var1, "_", test4$Var2, sep = "")
test2$key = paste(test2$to, "_", test2$log_no, sep = "")
test4 = left_join(test4, test2[,9:10], by = "key")
test4 = unique(test4); test2$key = NULL
test4 = table(test4$Var1, test4$total_clicks) %>% as.data.frame(stringsAsFactors = F)
names(test4) = c("no", "clicks", "Freq")
test4$clicks = as.numeric(test4$clicks); #test4$no = as.character(test4$no)
#calculate % of 1 clicks
ttt = test4 %>% filter(clicks > 1) %>% group_by(no) %>%
  summarise(Freq2 = sum(Freq)); ttt$no = as.character(ttt$no)
test4 = left_join(test4[test4$clicks == 1,c(1,3)], ttt, by = "no");rm(ttt)
test4$multiper = test4$Freq2/(test4$Freq+test4$Freq2)
test4$totalclicks = test4$Freq + test4$Freq2

test4$multiper_group[test4$multiper >= 0.8743] = 1;test4$multiper_group[test4$multiper < 0.8743] = 0
test4$clicks_group[test4$totalclicks >=44] = 1;test4$clicks_group[test4$totalclicks <44] = 0
test4$group[test4$multiper_group == 1 & test4$clicks_group == 1] = 1
test4$group[test4$multiper_group == 0 & test4$clicks_group == 1] = 2
test4$group[test4$multiper_group == 1 & test4$clicks_group == 0] = 3
test4$group[test4$multiper_group == 0 & test4$clicks_group == 0] = 4

group1 = test4$no[test4$group == 2]
#
test3_1 = test3[test3$total_clicks == 1,]
test3_1$cc = as.numeric(test3_1$cc)
test3_2 = test3[test3$total_clicks == 2,]
ttt = matrix(unlist(test3_2$cc), ncol = 2, byrow = T) %>% as.data.frame()
test3_2 = cbind(test3_2,ttt); rm(ttt); names(test3_2)[4:5] = c("c1", "c2")
#######################################################
#sankey diag.
library(networkD3)
DD = fread("lognames.csv", stringsAsFactors = F)
DD$TOPIC_SER_NO = as.character(DD$TOPIC_SER_NO)

i = 1
for(i in 1:nrow(DD)){
  if(DD$no[i] < 10){DD$TOPIC_SER_NO[i] = paste("00", DD$no[i], sep = "")}
  if(DD$no[i] >=10 & DD$no[i] <= 99){DD$TOPIC_SER_NO[i] = paste("0", DD$no[i], sep = "")}
}


#filter by group, find log_no with group1s
test2$y = NA
test2$y[test2$TOPIC_SER_NO %in% group1] = 1; test2$y[test2$from %in% group1] = 1
y = test2$log_no[test2$y == 1] %>% unique
test2$y[test2$log_no %in% y] = 1

# stories
a = table(test2[test2$log_no %in% cc,6:7]) %>% as.data.frame(stringsAsFactors = F) #%>% filter(Freq > 29)
a = a[a$from != "home",]; a = a[a$to != "out",]
#a = a %>% filter(Freq>15)
d1 = DD[,c(3,4)] %>% unique; names(d1)[1] = c("from"); d1$TOPIC_TITLE[is.na(d1$TOPIC_TITLE)] = "NA"
a = left_join(a, d1, by = "from"); names(a)[4] = "from1"
a$from1[is.na(a$from1)] = "home" ####
d1 = DD[,c(3,4)] %>% unique; names(d1)[1] = c("to"); d1$TOPIC_TITLE[is.na(d1$TOPIC_TITLE)] = "NA"
a = left_join(a, d1, by = "to"); names(a)[5] = "to1"
a$to1[is.na(a$to1)] = "out" ###
a = a[,c(4,5,3)]; names(a) = c("from", "to", "Freq")
rm(d1)

a$from = substr(a$from, 1, 10); a$to = substr(a$to, 1, 10)

nodes = data.frame(
  name=c(as.character(a$from), 
         as.character(a$to)) %>% unique()
)
nodes = nodes[order(nodes$name),] %>% as.data.frame()
names(nodes) = "name"

a$IDfrom <- match(a$from, levels(nodes$name))-1
a$IDto <- match(a$to, levels(nodes$name))-1

sankeyNetwork(Links = a, Nodes = nodes,
              Source = "IDfrom", Target = "IDto",
              Value = "Freq", NodeID = "name", 
              sinksRight=FALSE)
.#saveNetwork(b, file = "b.html", selfcontained = TRUE)

# tags
a = table(test2[,6:7]) %>% as.data.frame(stringsAsFactors = F) %>% filter(Freq >= 20)
d1 = DD[,c(3,5)] %>% unique; names(d1)[1] = c("from"); d1$TAG[is.na(d1$TAG)] = "NA"
a = left_join(a, d1, by = "from"); names(a)[4] = "from1"
d1 = DD[,c(3,5)] %>% unique; names(d1)[1] = c("to"); d1$TAG[is.na(d1$TAG)] = "NA"
a = left_join(a, d1, by = "to"); names(a)[5] = "to1"
a = a[,c(4,5,3)]; names(a) = c("from", "to", "Freq")
rm(d1)
a = a %>% group_by(from, to) %>%
  summarise(Freq = sum(Freq))

nodes = data.frame(
  name=c(as.character(a$from), 
         as.character(a$to)) %>% unique()
)
nodes = nodes[order(nodes$name),] %>% as.data.frame()
names(nodes) = "name"

a$IDfrom <- match(a$from, levels(nodes$name))-1
a$IDto <- match(a$to, levels(nodes$name))-1

sankeyNetwork(Links = a, Nodes = nodes,
                             Source = "IDfrom", Target = "IDto",
                             Value = "Freq", NodeID = "name", 
                             sinksRight=FALSE)

#######################################################
ag = fread("D:/box/推薦系統/EACH_AG.csv", stringsAsFactors = F)
names(ag)[1] = "AGIDNO"
test_ag = left_join(test2, ag, by = "AGIDNO")

test_ag = test_ag[,c(1,8:13)] %>% unique()



tt = left_join(test2, test_ag[,c(2,5)], by = "log_no")
tt = tt[,c(7,8,10)] %>% unique
#tt = table(tt$UNIT_NO, tt$to) %>% as.data.frame(stringsAsFactors = F) %>% filter(Freq >0)
#names(tt) = c("UNIT_NO", "to", "Freq")
#tt = tt[tt$UNIT_NO %in% cc,]
#ttt = table(test_ag$UNIT_NO) %>% sort(decreasing = T) %>% head(11) %>% as.data.frame(stringsAsFactors = F)
#names(ttt)  =c("UNIT_NO", "total")
#tt = left_join(tt, ttt, by = "UNIT_NO"); rm(ttt)

tt1  = table(tt$to) %>% as.data.frame(stringsAsFactors = F)
names(tt1) = c("to", "all")
tt2 = table(tt$to[tt$UNIT_NO == "NF4"]) %>% as.data.frame(stringsAsFactors = F)
names(tt2) = c("to", "unit")
tt1 = left_join(tt1,tt2, by = "to")
tt3 = table(tt$to[tt$UNIT_NO == "NM4"]) %>% as.data.frame(stringsAsFactors = F)
names(tt3) = c("to", "unit2")
tt1 = left_join(tt1,tt3, by = "to")
tt1$unit[is.na(tt1$unit)] = 0; tt1$unit2[is.na(tt1$unit2)] = 0


chisq.test(tt1$all, tt1$unit)


chisq.test(tt1$unit, tt1$unit2)
