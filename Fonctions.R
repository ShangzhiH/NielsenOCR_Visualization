library(rCharts)
library(shiny)
library(RODBC)

# Statistique de long terme(la somme des Days_Delivery)
GET_DATA_FROM_BBD <- function(DataSource = 'Shangzhi',UidValue = 'ShHuang',PwdValue = 'a19910707B',
                              TableName = 'Shangzhi.REPORT_PLACEMENT_EXPOSURE_WITH_SPLITED_TARGET',
                              LineColumns = c('ADVERTISER_NAME','SITE_NAME'),
                              PivotColumn = NULL, ResponseColumn = "Days_Delivery", CampaignType = "ALL", SiteType = "ALL", Target = "ALL", GENRE = NULL) {
  
  library(RODBC)
  library(rCharts)
  
  
  get_data <-function(table) { 
    #cette fonction donne les noms de variables et leur contenu 
    # d'un ficher importer.
    name = colnames(table)
    get_data = list(name = name, data = table[[1]])
  }
  
  
  # Collecte des valeurs distinct de la colonne Pivot 
  CONNEXTION <- odbcConnect(dsn = "Shangzhi",uid = UidValue, pwd = PwdValue)
  on.exit(odbcClose(CONNEXTION))
  
  
  #Ecrire un table tmpo pour calcumer la population pour le campaign
  SQuery = ''
  
  
  #Ecrire  de la requete  SQL  d'aggregation 
  
  SQuery  = paste(SQuery, "  SELECT ",LineColumns[1])
  GroupeBy =  paste("GROUP BY  ",LineColumns[1])
  OrderBy =  paste("ORDER BY  ",LineColumns[1]) 
  
  if(length(LineColumns)>1) {
    for(i in 2:length(LineColumns)) {
      SQuery = paste(SQuery,",",LineColumns[i])
      GroupeBy = paste(GroupeBy,",",LineColumns[i])
      OrderBy = paste(OrderBy,",",LineColumns[i])
    }
  } 
  
  # s'il y a la variable de pivot
  if(!is.null(PivotColumn)) {
    PivotQuery = ''
    PivotQuery = paste("SELECT DISTINCT  ",PivotColumn, " FROM  ",TableName)
    PivotDistinct = sqlQuery(CONNEXTION, PivotQuery)
    PivotDistinct = get_data(PivotDistinct)$data
    # pas de ponderation pour les resultats avec pivotcolumn
    if(length(PivotDistinct)>0) {
      for(k  in  1:length(PivotDistinct)) {
        if(Target == "NONE") {
          SQuery = paste(SQuery, ",SUM(CASE WHEN", PivotColumn,"=")
          Value = paste("'", PivotDistinct[k], "'",sep='')              
          SQuery = paste(SQuery, Value, "THEN ", ResponseColumn, " ELSE 0 END) AS ", Value)
        }
        else if(Target == "ALL") {
          SQuery = paste(SQuery, ",SUM(CASE WHEN", PivotColumn,"=")
          Value = paste("'", PivotDistinct[k], "'",sep='')              
          SQuery = paste(SQuery, Value, "THEN (CASE WHEN CAMPAIGN_ON_TARGET = 'True' THEN", ResponseColumn, " ELSE 0 END)", "ELSE 0 END) AS ", Value)
        }
        else if(Target == "GENRE") {
          SQuery = paste(SQuery, ",SUM(CASE WHEN", PivotColumn,"=")
          Value = paste("'", PivotDistinct[k], "'",sep='')              
          SQuery = paste(SQuery, Value, "THEN (CASE WHEN CAMPAIGN_ON_TARGET_GENRE = 'True' THEN", ResponseColumn, "ELSE 0 END)", " ELSE 0 END) AS ", Value)
        }
        else {
          SQuery = paste(SQuery, ",SUM(CASE WHEN", PivotColumn,"=")
          Value = paste("'", PivotDistinct[k], "'",sep='')              
          SQuery = paste(SQuery, Value, " THEN (CASE WHEN CAMPAIGN_ON_TARGET = 'True' AND CAMPAIGN_TARGET = '",Target,"' THEN ", ResponseColumn, " ELSE 0 END)", " ELSE 0 END) AS ", Value,sep = '')
        }
        
      }
    }
  }
  else {
    SQuery = paste(SQuery, ",SUM(",ResponseColumn)
    SQuery = paste(SQuery,") AS Delivery_Sum")
    if(Target == "ALL") {
      SQuery = paste(SQuery, ",SUM(CASE WHEN CAMPAIGN_ON_TARGET = 'True' THEN", ResponseColumn, "/(5*CIBLAGE_UNIVERSE_ESTIMATE) ELSE 0 END) AS Delivery_Sum_On_Target")
    }
    else if(Target == "GENRE") {
      if(GENRE == "M") {
        # pas de ponderation pour le ciblage de genre
        SQuery = paste(SQuery, ",SUM(CASE WHEN CAMPAIGN_ON_TARGET_GENRE = 'True' AND CAMPAIGN_TARGET_GENRE = 'M' THEN", ResponseColumn, " ELSE 0 END) AS Delivery_Sum_On_Target_GENRE")
      }
      else if(GENRE == "F") {
        # pas de ponderation pour le ciblage de genre
        SQuery = paste(SQuery, ",SUM(CASE WHEN CAMPAIGN_ON_TARGET_GENRE = 'True' AND CAMPAIGN_TARGET_GENRE = 'F' THEN", ResponseColumn, "ELSE 0 END) AS Delivery_Sum_On_Target_GENRE")
      }
    }
    else {       
      SQuery = paste(SQuery, ",SUM(CASE WHEN CAMPAIGN_ON_TARGET = 'True' AND CAMPAIGN_TARGET = '",Target,"' THEN ", ResponseColumn, " ELSE 0 END) AS Delivery_Sum_On_Target_SPECIAL", sep = '')
    }
  }
  
  
  
  SQuery = paste(SQuery,"FROM")
  if(Target == "ALL") {
    Table_Ratio = "[Shangzhi].[REPORT_PLACEMENT_EXPOSURE_TABLE_RATIO],"
    
  }
  else if(Target == "GENRE") {
    if(GENRE == "M") {
      Table_Ratio = "Shangzhi.REPORT_PLACEMENT_EXPOSURE_CAMPAIGN_HOMME,"
    }
    else if(GENRE == "F") {
      Table_Ratio = "Shangzhi.REPORT_PLACEMENT_EXPOSURE_CAMPAIGN_FEMME,"
    }
    }
  else {
    Table_Ratio = paste("(SELECT DISTINCT CAMPAIGN_NAME AS CAMPAIGN_NAME_CIBLAGE
                        FROM [DM_1259_GroupMFrance].[Shangzhi].[REPORT_PLACEMENT_EXPOSURE_WITH_SPLITED_TARGET] 
                        WHERE CAMPAIGN_TARGET ='", Target,"')a,", sep = '')
  }
  
  Table_Ratio = gsub("[\r\n]", "", Table_Ratio)
  
  
  
  SQuery = paste(SQuery, Table_Ratio, TableName)
  SQuery = paste(SQuery, "WHERE CAMPAIGN_NAME = CAMPAIGN_NAME_CIBLAGE")
  
  if(CampaignType == "VIDEO") {
    SQuery = paste(SQuery, " AND PLACEMENT_TYPE = 'VIDEO'")
  }
  else if(CampaignType == "STANDARD") {
    SQuery = paste(SQuery, " AND PLACEMENT_TYPE = 'STANDARD'")
  }
  
  
  if(SiteType == "RESEAUX") {
    SQuery = paste(SQuery, " AND GROUPE = 'RESEAUX'")
  }
  else if(SiteType == "EDITEURS") {
    SQuery = paste(SQuery, " AND GROUPE = 'EDITEURS'")
  }
  
  
  SQuery = paste(SQuery,GroupeBy)
  SQuery = paste(SQuery,OrderBy)
  
  
  
  
  DataSet =  sqlQuery(CONNEXTION, paste(SQuery))
  
  
  print("get_data")
  return(DataSet)
}

GET_CAMPAIGN_NUMBER <- function(DataSource = 'Shangzhi',UidValue = 'ShHuang',PwdValue = 'a19910707B',
                                TableName = 'Shangzhi.REPORT_PLACEMENT_EXPOSURE_WITH_SPLITED_TARGET', Target = 'B13-20') {
  
  CONNEXTION <- odbcConnect(dsn = "Shangzhi",uid = UidValue, pwd = PwdValue)
  on.exit(odbcClose(CONNEXTION))
  SQuery = paste("SELECT COUNT(DISTINCT CAMPAIGN_NAME) AS CAMPAIGN_NUMBER
                 FROM [DM_1259_GroupMFrance].[Shangzhi].[REPORT_PLACEMENT_EXPOSURE_WITH_SPLITED_TARGET] 
                 WHERE CAMPAIGN_TARGET = '",Target,"'", sep = "")
  
  NUMBER = sqlQuery(CONNEXTION, SQuery)
  return(NUMBER)
}

# proportion de ciblage de differents sites
# fusionner les donnees
FUSION <- function(DataSet) {
  
  
  list1 = NULL
  for(i in c('AD-DSP FR','Assemblee de Dieu de Saint Pierre','GroupM')) {
    list1 = c(list1,which(DataSet$SITE_NAME == i))
  }
  
  list2 = NULL
  for(i in c('Hi Media','HiMedia')) {
    list2 = c(list2,which(DataSet$SITE_NAME == i))
  }
  
  list3 = NULL
  for(i in c('Amaury Medias','AmauryMedias')) {
    list3 = c(list3,which(DataSet$SITE_NAME == i))
  }
  
  list4 = NULL
  for(i in c('20minutes.fr','20minutes.fr Homepage')) {
    list4 = c(list4,which(DataSet$SITE_NAME == i))
  }
  
  list5 = NULL
  for(i in c('France Televisions','France Televisions Homepage')) {
    list5 = c(list5,which(DataSet$SITE_NAME == i))
  }
  
  list6 = NULL
  for(i in c('Orange','Orange Homepage')) {
    list6 = c(list6,which(DataSet$SITE_NAME == i))
  }
  
  list7 = NULL
  for(i in c('Microsoft Homepages - US', 'MSN Homepage')) {
    list7 = c(list7,which(DataSet$SITE_NAME == i))
  } 
  
  list8 = NULL
  for(i in c('Figaro Medias', 'Le Figaro')) {
    list8 = c(list8,which(DataSet$SITE_NAME == i))
  } 
  
  list9 = NULL
  for(i in c('Dailymotion', 'Dailymotion Homepage')) {
    list9 = c(list9,which(DataSet$SITE_NAME == i))
  } 
  
  list10 = NULL
  for(i in c('MYTF1 TV', 'TF1 TV')) {
    list10 = c(list10,which(DataSet$SITE_NAME == i))
  }
  
  
  index = list(list1 = list1, list2 = list2, list3 = list3, list4 = list4, list5 = list5, list6 = list6, list7 = list7, list8 = list8, list9 = list9, list10 = list10)
  
  delete = NULL
  for( list in index) {
    if(length(list) >= 2) {
      DataSet[list[1],-1] = apply(DataSet[list,-1], MARGIN = 2, FUN = sum) 
      delete = c(delete, -list[-1])  
    }
  }
  if(!is.null(delete)) {
    DataSet = DataSet[delete,]
  }
  
  # changer les noms trop longs
  
  if(sum(levels(DataSet$SITE_NAME)=="AlloCine Homepage") != 0)
    levels(DataSet$SITE_NAME)[levels(DataSet$SITE_NAME)=="AlloCine Homepage"] = "AlloCine"
  if(sum(levels(DataSet$SITE_NAME)=="Cosmopolitan France") != 0)
    levels(DataSet$SITE_NAME)[levels(DataSet$SITE_NAME)=="Cosmopolitan France"] = "Cosmopolitan"
  if(sum(levels(DataSet$SITE_NAME)=="L Equipe Homepage") != 0)
    levels(DataSet$SITE_NAME)[levels(DataSet$SITE_NAME)=="L Equipe Homepage"] = "L Equipe"
  if(sum(levels(DataSet$SITE_NAME)=="Leboncoin.fr Homepage") != 0)
    levels(DataSet$SITE_NAME)[levels(DataSet$SITE_NAME)=="Leboncoin.fr Homepage"] = "Leboncoin.fr"
  if(sum(levels(DataSet$SITE_NAME)=="LeParisien.fr / Aujourdhui.fr") != 0)
    levels(DataSet$SITE_NAME)[levels(DataSet$SITE_NAME)=="LeParisien.fr / Aujourdhui.fr"] = "LeParisien.fr"
  if(sum(levels(DataSet$SITE_NAME)=="Meteo France Homepage") != 0)
    levels(DataSet$SITE_NAME)[levels(DataSet$SITE_NAME)=="Meteo France Homepage"] = "Meteo France"
  if(sum(levels(DataSet$SITE_NAME)=="Microsoft Homepages - US") != 0)
    levels(DataSet$SITE_NAME)[levels(DataSet$SITE_NAME)=="Microsoft Homepages - US"] = "MSN"
  if(sum(levels(DataSet$SITE_NAME)=="Skyrock Homepage") != 0)
    levels(DataSet$SITE_NAME)[levels(DataSet$SITE_NAME)=="Skyrock Homepage"] = "Skyrock"
  if(sum(levels(DataSet$SITE_NAME)=="Tele Loisirs Homepage") != 0)
    levels(DataSet$SITE_NAME)[levels(DataSet$SITE_NAME)=="Tele Loisirs Homepage"] = "Tele Loisirs"
  if(sum(levels(DataSet$SITE_NAME)=="Yahoo! Homepage") != 0)
    levels(DataSet$SITE_NAME)[levels(DataSet$SITE_NAME)=="Yahoo! Homepage"] = "Yahoo!"
  if(sum(levels(DataSet$SITE_NAME)=="YouTube Homepage") != 0)
    levels(DataSet$SITE_NAME)[levels(DataSet$SITE_NAME)=="YouTube Homepage"] = "YouTube"
  
  
  
  
  
  
  
  
  
  
  
  return(DataSet)
}



########### supprimer quelques sites manuellemnt
SUPPRIMER <- function(DataSet, SITE) {

  DELETE = which(DataSet == SITE)
  if(sum(DELETE) != 0) {
    DataSet = DataSet[-DELETE,]
  }
  
  return(DataSet)
}


AFFICHAGE <- function(Data, seuil) {
  
  
  
  
  Data = Data[order(Data$Delivery_Sum_On_Target/Data$Delivery_Sum),]
  
  h <- Highcharts$new()
  h$chart(type = 'area')
  h$title(text = "Pourcentage des impressions respectantes la cible definie de la campagne")
  
  h$subtitle(text = paste("Source: GroupMFrance NielsenOCR  <br/>Date:", date(),"<br/>Seuil:", seuil))
  
  h$set(width=1200, height=800)
  
  
  Proportion = Data$Delivery_Sum_On_Target/Data$Delivery_Sum
  categories = c(as.character(Data$SITE_NAME))
  color = c('#FF0000', '#00FF00', '#0000FF', '#FFFF00',	'#00FFFF', '#FF00FF', '#C0C0C0')
  CLUSTER = floor(sqrt(length(Proportion)))
  if(CLUSTER > 5)
    CLUSTER = 5
  if(CLUSTER > 1) {
    class = kmeans(Proportion, centers = CLUSTER, iter.max = 1000, nstart = 20)
    point = 0.5
    index = 1
    SCRIPT = "plotLines = list("
    for(i in 2:length(class$cluster)) {
      
      if(class$cluster[i] != class$cluster[i-1]) {
        SCRIPT = paste(SCRIPT, "list(zIndex = -1, from = ", point-0.5, ",to = ",i - 1.5,",color = '", color[(index-1)%%7+1], "'),")
        point = i - 1
        index = index + 1
      }
      if(i == length(class$cluster)) {
        SCRIPT = paste(SCRIPT, "list(zIndex = -1, from = ", point-0.5, ",to = ", length(class$cluster) - 1, ",color = '", color[(index-1)%%7+1], "'))")
      }
    }
    
    
    eval(parse(text = SCRIPT))
    
    h$xAxis(gridLineWidth = 1, categories = categories, plotLines = plotLines, labels=list(rotation = -60))
  }
  else {
    h$xAxis(gridLineWidth = 1, categories = categories, labels=list(rotation = -60))
  }
  
  h$yAxis(gridLineWidth = 1, title = list(text ='Proportion Ciblee'))
  
  
  h$series(name = 'Impression Non Ciblee',  data = Data$Delivery_Sum - Data$Delivery_Sum_On_Target)
  h$series(name = 'Impression Ciblee',  data = Data$Delivery_Sum_On_Target)
  
  h$tooltip(pointFormat = "{series.name}: <b>{point.percentage:.1f}%</b> ({point.y})<br/>", shared = TRUE)
  
  
  h$plotOptions(area = list(stacking = 'percent',lineColor = '#ffffff', lineWidth = 2, marker = list(lineWidth = 1, lineColor = '#ffffff')))
  

  
  return(h)
}