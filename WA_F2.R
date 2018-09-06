#!/usr/bin/Rscript


rm(list = ls())
pilih_bulan = "08"
pilih_tahun = "2018"
df = read.table(paste0("hasil/simpan_",pilih_tahun,pilih_bulan,".txt"), header = T)

is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

is.character0 <- function(x)
{
  is.character(x) && length(x) == 0L
}

dfM = as.character(as.vector(unlist(df$Message)))
if(any(is.na(dfM))){
  print("nothing")
}else{
  tglx = strsplit(dfM, split = "tgl.")
  tgl = c()
  for(i in 1:length(tglx)){
    tgl[i] = tglx[[i]][2]
  }
  jamx = strsplit(dfM, split = "wita")
  
  ntgl = c()
  for(i in 1:length(tglx)){
    ntgl[i] = length(tglx[[i]])
  }
  
  njam = c()
  for(i in 1:length(jamx)){
    njam[i] = length(jamx[[i]])
  }
  
  untgl = as.data.frame(table(ntgl))
  nsama_hari =  as.numeric(as.vector(unlist(untgl$ntgl[which( as.numeric(untgl$Freq) == max(as.numeric(untgl$Freq)) )])))
  nganti_hari = nsama_hari+1
  sama_hari = dfM[ ntgl == nsama_hari ]
  ganti_hari = dfM[ ntgl == nganti_hari ]
  ngH = which( ntgl == nganti_hari )
  
  jam_only = function(x){
    jam_only = c()
    for(i in 1:length(jamx[[x]]) ){
      jam_only[i] = strsplit(jamx[[x]][i], split = "pkl")[[1]][2]
    }
    jam_only  = gsub("\\D+", "", jam_only)
    jam_only = jam_only[!is.na(jam_only)]
    return(jam_only)
  }
  
  tgl_only = function(x){
    tgl_only = c()
    for(i in 1:length(jamx[[x]]) ){
      tgl_only[i] = strsplit(tglx[[x]][i], split = "pkl")[[1]][1]
    }
    tgl_only  = gsub("\\D+", "", tgl_only)
    
    tgl_only = tgl_only[!is.na(tgl_only)]
    tgl_only = tgl_only[nchar(tgl_only) == 6]
    return(tgl_only)
  }
  
  tanggal = list()
  jam  = list()
  for(i in 1:length(jamx)){
    tanggal[[i]] = tgl_only(x = i)
    for(j in 1:length(tanggal[[i]])){
      tanggal[[i]][j] = paste0( substr( tanggal[[i]][j], 3,6), "-",pilih_bulan, "-", 
                                substr( tanggal[[i]][j], 1,2))
    }
    jam[[i]] = jam_only(x = i)
    for(j in 1:length(jam[[i]])){
      jam[[i]][j] = paste0( substr( jam[[i]][j], 1,2), ":", substr( jam[[i]][j], 3,4))
    }
  }
  
  # x = 27
  system(paste0("echo ",ngH, "> ngH.txt"))
  as_urut = function(x){
    x1 = paste0( tanggal[[x]][1]," ", jam[[x]][2]  )
    x2 = paste0( tanggal[[x]][length(tanggal[[x]])]," ", jam[[x]][length(jam[[x]])]  )
    
    if(as.POSIXct(x2) - as.POSIXct(x1) < 0){
      x2 = as.character(as.POSIXct(x2) + (3600*24))
      ngH = c(ngH, x)
      system(paste0("echo ",x, ">> ngH.txt"))
    }
    lt = seq(as.POSIXct(x1), as.POSIXct(x2), by= "hours")
    jika30 = as.numeric(substr(as.character(lt[length(lt)]),15, 16 ))
    jika30[is.na(jika30)] = 0
    if( jika30 > 30  ){
      lt = seq(as.POSIXct(x1), (as.POSIXct(x2)+ 3600), by= "hours")
    }
    return(lt)
  }
  
  Date_time = list()
  for(i in 1:length(jam)){
    Date_time[[i]] = as_urut(i)
  }
  
  sampai_jam = list()
  for(i in 1:length(Date_time)){
    jam[[i]] = substr(Date_time[[i]], 1, 13)
  }
  Sulsel = read.table("Sulsel_DataFrame.txt", header = T)
  
  # Yang Kurang Kotanya
  # Sulsels =Sulsel
  # Sulsels = rbind(as.matrix(Sulsels), c("TANALILI","KAB. LUWU UTARA", "SULAWESI SLEATAN", "SULAWESI"))
  # Sulsels  = data.frame(Sulsels)
  # colnames(Sulsels) = colnames(Sulsel)
  # Sulsel = Sulsels
  # write.table(file = "Sulsel_DataFrame.txt", Sulsel, col.names =  T, row.names = F)
  # Sulsel[dim(Sulsel)[1],]
  
  
  
  
  
  
  
  Kecamatan = tolower(as.character(Sulsel$Kecamatan))
  Kabupaten = tolower(as.character(Sulsel$Kabupaten))
  Kabupaten = gsub(pattern = "kota ", replacement = "", Kabupaten)
  Kabupaten = gsub(pattern = "kab. ", replacement = "", Kabupaten)
  Kabupaten = gsub(pattern = "-pare", replacement = "", Kabupaten)
  dfM = gsub(pattern = "bmkg masamba", replacement = "", dfM) 
  # Kecamatan = gsub(pattern = "tanalili", replacement = "tanralili", Kecamatan)
  
  # x = 1
  # y = 1
  catch_kecamatan = function(x,y){
    sm = strsplit(dfM[x], split = " ")[[1]]
    bara = grep(sm, pattern = Kecamatan[y])
    # bara = grep(sm, pattern = "jozz")
    barat = grep(sm, pattern = "barat")
    # barat = grep(sm, pattern = "jozz")
    if(is.integer0(bara)){
      hs = 0
    }else if(!is.integer0(bara) & !is.integer0(barat)){
      if(all(bara == barat)){
        hs = 0
      }else{
        hs = 1
        # print(paste0("ada kec. ", Kecamatan[y]))
      }
    }else{
      hs = 1
      # print(paste0("ada kec. ", Kecamatan[y]))
    }
    
    return(hs)
  }
  
  id_catch = matrix(0, nrow = length(Kecamatan), ncol = length(dfM))
  for(i in 1:length(Kecamatan)){
    for(j in 1:length(dfM)){
      id_catch[i,j] = catch_kecamatan(x = j, y = i)
    }
  }
  id_catch[,3]
  
  # which(id_catch == 1)
  
  mat_kec = matrix(0, nrow = length(Kecamatan), ncol = length(dfM))
  for(i in 1:length(Kecamatan)){
    mat_kec[i,] = rep(Kecamatan[i], length(dfM))
  }
  
  apa_saja = function(x){
    vec_kec = as.character(mat_kec)
    vec_kec[id_catch[,x] != 0]
    hhhh = which(id_catch[,x] != 0)
    return(vec_kec[hhhh])
  }
  
  UKab = unique(Kabupaten)
  kecamatan_terpilih = list()
  parah = c()
  for(i in 1:length(dfM)){
    kecamatan_terpilih[[i]] = apa_saja(i)
    
    if(is.character0(kecamatan_terpilih[[i]])){
      # print(paste0("ada di ", i))
      kecamatan_terpilih[[i]] = "jozz"
      parah[i] = 1
    }else{
      parah[i] = 0
    }
  }
  
  
  kab_terpilih = function(x){
    jag = c()
    for(i in 1:length(UKab)){
      su = grep(pattern = UKab[i], dfM[[x]])
      if(!is.integer0(su)){
        jag[i] = grep(pattern = UKab[i], dfM[[x]])
      }else{
        jag[i] = 0
      }
    }
    kepilih = UKab[jag != 0]
    Kecamatan[which(Kabupaten %in% kepilih)]
    Kabupaten[which(Kabupaten %in% kepilih)]
    # x = 34
    ixx = id_catch[,x]
    ixx[which(Kabupaten %in% kepilih)] = 1
    id_catch[,x] = ixx
    
    return(list(id_catch_add = ixx, Uid = jag))
  }
  
  kec_pseudo = list()
  if(any(parah == 1)){
    ip = which(parah == 1)
    for(i in 1:length(ip)){
      kec_pseudo[[i]] = kab_terpilih(x = ip[i])
      add_it = kec_pseudo[[i]]$id_catch
      id_catch[,i] = add_it
    }
  }
  
  sip = list()
  for(i in 1:length(jam)){
    sip[[i]] = matrix(rep(id_catch[,i],length(jam[[i]]) ), nrow = dim(id_catch)[1])
  }
  
  sipbgt = sip[[1]]
  for(i in 1:(length(sip)-1)){
    sipbgt = cbind(sipbgt, sip[[i+1]])
  }
  
  
  unli_jam = unlist(jam)
  table_jam = table(unlist(jam))
  table_jam = data.frame(table_jam)
  
  mats = matrix(0,nrow = length(Kabupaten), ncol = dim(table_jam)[1])
  for(i in 1:dim(table_jam)[1]){
    if(as.numeric(table_jam$Freq[i]) > 1){
      jf = which(unli_jam == as.character(table_jam$Var1[i]))
      mats[,i] = apply(sipbgt[,jf], c(1), FUN = max)
    }
  }
  
  
  ngH = as.numeric( as.vector( unlist( read.table(file = "ngH.txt", header = F, col.names = F) ) ) ) #lewat hari
  
  setahun = seq( as.POSIXct( paste0(pilih_tahun,"-", 01, "-", "01", " 00:00:00")),
                 as.POSIXct(paste0(pilih_tahun,"-", 12, "-", "31", " 23:00:00")) , 
                 by = "hour")
  
  setahun_C = substr(setahun, 1, 13)
  setahun_selected = which(setahun_C %in% unlist(jam))
  index = rep(0, length(setahun_C)) 
  index[setahun_selected]  = 1
  
  index_matrix = matrix(0, ncol = length(index),nrow = length(Kabupaten) )
  index_matrix[,index == 1] = mats
  index_matrix[index_matrix == 1] = 2
  # index_matrix[index_matrix == 2]
  
  row_jam = which(substr(setahun_C,6,7 ) == pilih_bulan )
  final = index_matrix[,row_jam]
  
  dayys = unique(substr(setahun_C[row_jam], 9, 10))
  times = unique(substr(setahun_C[row_jam], 12, 13))
  yup = list()
  # system(paste0("mkdir digitize/", pilih_tahun,pilih_bulan))
  system(paste0("mkdir digitize/se_Kec/", pilih_tahun,pilih_bulan))
  for(i in 1:length(dayys)){
    iday = which(substr(setahun_C[row_jam], 9, 10) == dayys[i])
    yup[[i]] = data.frame(Kabupaten, Kecamatan,final[,iday])
    colnames(yup[[i]]) = c("Kabupaten", "Kecamatan", times)
    # write.table(file = paste0("digitize/", pilih_tahun,pilih_bulan, "/warning_whatsapp_", pilih_tahun, pilih_bulan, dayys[i], ".txt") , yup[[i]], col.names = T, row.names = F)
    write.table(file = paste0("digitize/se_Kec/", pilih_tahun,pilih_bulan, "/warning_whatsapp_", pilih_tahun, pilih_bulan, dayys[i], ".txt") , yup[[i]], col.names = T, row.names = F)
  }
  
}



