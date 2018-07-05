library(jsonlite)
url = 'https://www.instagram.com/<username>/media'
jsonData <- fromJSON(url);

#get the latest 20 posts
ids = jsonData$items$id
link = jsonData$items$link
postType = jsonData$items$type
createdtime = jsonData$items$created_time
likes = jsonData$items$likes$count
caption = jsonData$items$caption$text

moreAvailable = jsonData$more_available;

maxid = ids[length(ids)];

#depending on more availability do the request again and again
while(moreAvailable) {
	url2 = 'https://www.instagram.com/<username>/media/?max_id='
	url2 = paste0(url2, maxid);
	jsonDataTemp <- fromJSON(url2);
	
	names(jsonDataTemp)
	idstemp = jsonDataTemp$items$id
	linktemp = jsonDataTemp$items$link
	postTypetemp = jsonDataTemp$items$type
	createdtimetemp = jsonDataTemp$items$created_time
	likestemp = jsonDataTemp$items$likes$count
	captiontemp = jsonDataTemp$items$caption$text
  
	maxid = idstemp[length(idstemp)];
	moreAvailable = jsonDataTemp$more_available;
      	min = length(ids) + 1;
	max = length(ids) + length(idstemp);
	ids[min:max] = idstemp;
	link[min:max] = linktemp;
	postType[min:max] = postTypetemp 
	createdtime[min:max] = createdtimetemp 
	likes[min:max] = likestemp 
	caption[min:max] = captiontemp 
	length(ids)
}

#convert date into readable format and extract day of posting and time of posting
createdtime = as.POSIXct(as.numeric(as.character(createdtime)), origin='1970-01-01')
createdDay = format(createdtime, '%a')
createdHour = format(createdtime, '%H')

instaData = data.frame(ids, link, postType, createdtime, createdDay, createdHour, caption, likes);


captions = instaData$caption
captionsSave <- c()

dict <- c()

#cleaning the caption to store only the hashtags
for (i in 1:length(captions)){
  print(i)
  temp = gsub("[^A-Za-z0-9#]", "", captions[i])
  temp = gsub('+[\n]+', '', temp)
  temp = gsub("[#]", ' #', temp)
  temp = tolower(temp)  
  dictTemp <- strsplit(temp, " ")[[1]]
  dictTemp <- dictTemp[grepl("[#]", dictTemp)]
  
  if(length(dictTemp)>0) {
    min = length(dict) + 1
    max = length(dict) + length(dictTemp)
    dict[min:max] = dictTemp  
  }
  captionsSave[i] = paste(dictTemp, collapse = ' ');
}

#save into a table based on frequency of occurrence of each hashtag
freqtable = table(dict)
freqtableSorted<-sort(freqtable, decreasing=TRUE)

#save into a csv if required
instaDataSaving = data.frame(postType, createdDay, createdHour, captionsSave, likes);

write.table(instaDataSaving, file = "instaData.csv",row.names=FALSE,col.names=TRUE, sep=",")
write.table(freqtableSorted, file = "wordsDict.csv",row.names=FALSE,col.names=FALSE, sep=",")
