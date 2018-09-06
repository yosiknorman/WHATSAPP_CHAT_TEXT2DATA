library(stringr)

rm(list = ls())
pilih_bulan = "04"
pilih_tahun = "2018"
text = read.delim(paste0("Chat_Whatsapp/input_",pilih_tahun, "08", ".txt" ), header = F)

text = as.vector(unlist(text))
links = substr(text, 1,14 )
first2 = substr(links, 1,2)
first2 = as.numeric(first2)
first2[is.na(first2)] = 0
f2 = c()
for(i in 1:length(first2)){
  if(  nchar(first2[i]) == 1  ){
    f2[i] = paste0("0", first2[i])
  }else if( nchar(first2[i]) == 2 ){
    f2[i] = paste0( first2[i])
  }
}

link = substr(links, 2, nchar(links))
# grep(link[1], pattern = "[[:alnum:]]")
Digit  = gsub("\\D+", "", link)
ncDigit = nchar(Digit)

# text[ncDigit == 9]
ifs = which(ncDigit == 9)

list_ifs = list()
ok = c()
for( i in 1:(length(ifs)-1)){
  list_ifs[[i]] = ifs[i]:ifs[i+1]
  ok[i] = paste(text[ifs[i]:((ifs[i+1])-1)], collapse = " ")
}

ok = tolower(ok)
# substr(ok, 1,30)
# jj = grep(ok, pattern = "hingga")
# kk = grep(ok[jj], pattern = "berpotensi")
# ll = grep(ok[jj[kk]], pattern = "lebat")
# mm = grep(ok[jj[kk[ll]]], pattern = "pkl")



x = c("peljas","peringatan", "dini cuaca", "hingga", "berpotensi", "pkl")
prakicu = list()
for(i in 1:length(x)){
  prakicu[[i]] = grep(ok, pattern =  x[i])
}

ih = 1:length(ok)
 
grab_text = function(xpattern, x){
  res = 0 
  if(any(prakicu[[xpattern]] == ih[x])){
    res = 1
  }
  return(res)
}


fhasil = matrix(0, nrow = length(ih), ncol = length(x))
for(i in 1:length(x)){
  for(j in 1:length(ih)){
    fhasil[j,i] = grab_text(xpattern = i, x = j)
  }
}

sum_fhasil = c()
for(i in 1:dim(fhasil)[1]){
  sum_fhasil[i]= sum(fhasil[i,])
}

# write.table(ok,"ok.txt")

simpan = ok[which(sum_fhasil == (length(x)))]
bulan = substr(simpan, 4, 5)

# pilih_bulan = "08"
if(!any(bulan == pilih_bulan)){
  write.table(data.frame(Whatsapp_Date = NA, Message = NA), paste0("hasil/simpan_",pilih_tahun,pilih_bulan,".txt"), col.names = T, row.names = F)
}else{
  simpan = simpan[bulan == pilih_bulan]
  
  td = strsplit(simpan, split = " - peljas: ")
  tgl = c()
  message = c()
  for(i in 1:length(td)){
    tgl[i] = td[[i]][1]
    message[i] = td[[i]][2]
  }
  
  write.table(data.frame(Whatsapp_Date = tgl, Message = message), paste0("hasil/simpan_",pilih_tahun,pilih_bulan,".txt"), col.names = T, row.names = F)
  
}


