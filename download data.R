library(dplyr)
library(XML)

combine = NULL
for(year in 2002:2014){
  url = paste("http://nflcombineresults.com/nflcombinedata.php?year=",
              year, "&pos=RB&college=", sep="")
  
  temp = readHTMLTable(url)
  temp = temp[["NULL"]]
  
  combine = rbind(combine, temp)
  
}


combine_data = select(combine, -College, -POS, -Wonderlic)

names(combine_data) = c('Year','Name','height','weight','forty','bench','vert','broad','shuttle','cone')
combine_data$Name = as.character(combine_data$Name)
combine_data$Name[129] = "Adrian Peterson2"
combine_data[,3:10] = apply(combine_data[,3:10],2, function(x) as.numeric(gsub("[*]","",x) ))

# lesean mccoy
temp = c(2009, "Lesean McCoy", 70, 204, 4.45, 17, 29, 107, 4.18, 6.82)

combine_data = rbind(combine_data, temp)



### Extract all names from combine data ###

temp = combine_data
temp$Year = as.numeric(as.character(temp$Year))
temp = filter(temp, Year<=2011)

temp$Name = gsub("[-']", "", temp$Name)  

rb_names = strsplit(as.character(temp$Name), " ")
rb_names = data.frame(do.call(rbind, rb_names))

names(rb_names) = c("fname", "lname")

blah=rb_names


rb_names$l_first = substr(rb_names$lname, 1, 1)
rb_names$url_name = paste(substr(rb_names$lname, 1, 4), substr(rb_names$fname, 1, 2), sep="")

rb_names = rb_names[order(rb_names$lname),]


missing = matrix(0, 0, 2) # Players for whom data was not retrieved
data_names  = matrix(0, 0, 2)
data_stat = matrix(0, 0, 5)

for (i in 1:nrow(rb_names)){
  j=0
  k=0
  
  while(j!=10){
  
    url = paste("http://www.pro-football-reference.com/players/", rb_names$l_first[i], "/", 
                rb_names$url_name[i], "0", j,".htm", sep="")  
    temp = readHTMLTable(url)
    
    if(length(temp)==0) {
      missing = rbind(missing, rb_names[i, 1:2])
      j=10 # no data - terminate while loop
      k=1 
    } else{
      temp = temp$rushing_and_receiving
      temp = temp[temp$Year!="",]
      
      # Check if right time frame and pos == RB
      temp2 = as.character(temp$Year)
      temp2 = as.numeric(gsub("[^0-9]" , "", temp2))
      rb = length(intersect(c("RB","rb"), unique(temp$Pos)))
      
      if(min(temp2)<2002 | rb==0) {j=j+1 # wrong player, keep looping
      } else j=10 # successfully found data - terminate while loop
    }
  
  }
  
  if(k==1) {next  #while loop terminated because no data
  } else{

    # stats
    num_rows = min(nrow(temp), 4)
    temp = temp[1:num_rows,]
    
    temp = temp[,c(9,10,17,19,25)]
    
    temp = apply(temp, 2, function(x) sum(as.numeric(x)))
    
    temp[is.na(temp)] = 0
    
    data_names = rbind(data_names, rb_names[i, 1:2])
    data_stat = rbind(data_stat, temp)        
  }
}

stat = data.frame(data_names, data_stat)
names(stat)[3:7] = c('ry','tdr','recy','tdre','fuml')
stat$fname = as.character(stat$fname)
stat$lname = as.character(stat$lname)

# Adrian "Purple Jesus" Peterson
temp = readHTMLTable("http://www.pro-football-reference.com/players/P/PeteAd01.htm")
temp = temp$rushing_and_receiving
num_rows = min(nrow(temp), 4)
temp = temp[1:num_rows,]
temp = temp[,c(9,10,17,19,25)]
temp = apply(temp, 2, function(x) sum(as.numeric(x)))
stat[73, 3:7] = temp
stat$lname[73] = 'Peterson2'

# Fantasy Points, rush+pass yards
stat = mutate(stat, fpoints = ry*.1 + tdr*6 + recy*.1 + tdre*6 - fuml*2,
              scrim = ry+recy)


# Top Players
stat[order(-stat$ry),][1:10,]
stat[order(-stat$scrim),][1:10,]
stat[order(-stat$fpoints),][1:10,]


##### Merge combine data and stats

stat$Name = paste(stat$fname, stat$lname)
merge_data = merge(stat, combine_data, by="Name")
save(merge_data, file="data.rdata")
