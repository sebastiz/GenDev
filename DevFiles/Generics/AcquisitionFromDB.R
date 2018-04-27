#Data acquisition scripts for database acquisitions:

#Ideally create a function here that integrates two tables and other functions based on the compexity of the table merges- for now do them as datasetgnostic


library(RODBC)
.libPaths() 
.libPaths("S:\\Gastroenterology\\Seb\\R\\R-3.3.1\\library")
.libPaths()

channel <- odbcConnectAccess("S:\\Gastroenterology\\Seb\\JavaPortableLauncher\\PhysiPopDONOTTOUCH\\Physiology6.mdb")


Impedance<-function(x){
  data <- sqlQuery( channel , "SELECT  Impedance2.* FROM Impedance2")
  return(data) 
}


BRAVO<-function(x){
  data <- sqlQuery( channel , "SELECT  BravoDay1And2.* FROM BravoDay1And2")
  return(data)
}

HRMAndSwallows<-function(x){
  

  data <- sqlQuery( channel , "SELECT PatientData.*, HRMImportSwallows.*, HRMImportMain.* 
                         FROM PatientData INNER JOIN (HRMImportMain INNER JOIN HRMImportSwallows ON HRMImportMain.HRM_Id = HRMImportSwallows.HRM_Id) ON PatientData.HospNum_Id = HRMImportMain.HospNum_Id
                         WHERE HRMImportMain.HRM_Id=HRMImportSwallows.HRM_Id")
  return(data)
}

HRMAndDiag<-function(x){

data <- sqlQuery( channel , "SELECT DISTINCT HRMImportMain.*, Diag.IndicANDHx, Diag.*, PatientData.*
FROM (PatientData INNER JOIN Diag ON PatientData.HospNum_Id = Diag.HospNum_Id) INNER JOIN HRMImportMain ON PatientData.HospNum_Id = HRMImportMain.HospNum_Id
WHERE HRMImportMain.VisitDate=Diag.VisitDate")
return(data)
}
