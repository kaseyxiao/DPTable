file_init <- function(CV){
  if(CV == 0.2){
    files <- list()
    files[["Data1"]][[as.character(0.09)]] = "Data1_CV_0.2_20141230_191709"
    files[["Data1"]][[as.character(0.05)]] = "Data1_CV_0.2_20141224_155954"
    files[["Data1"]][[as.character(0.1)]] = "Data1_CV_0.2_20141224_153627"
    files[["Data1"]][[as.character(0.2)]] = "Data1_CV_0.2_20141224_162926"
    files[["Data1"]][[as.character(0.4)]] = "Data1_CV_0.2_20141228_222052"
    files[["Data1"]][[as.character(0.5)]] = "Data1_CV_0.2_20141224_172102"
    
    files[["Data2"]][[as.character(0.09)]] = "Data2_CV_0.2_20141230_195732"
    files[["Data2"]][[as.character(0.05)]] = "Data2_CV_0.2_20141224_174932"
    files[["Data2"]][[as.character(0.1)]] = "Data2_CV_0.2_20141224_181638"
    files[["Data2"]][[as.character(0.2)]] = "Data2_CV_0.2_20141224_184303"
    files[["Data2"]][[as.character(0.4)]] = "Data2_CV_0.2_20141224_190905"
    files[["Data2"]][[as.character(0.5)]] = "Data2_CV_0.2_20141224_193426"
    
    files[["Data3"]][[as.character(0.05)]] = "Data3_CV_0.2_20141224_205459"
#     files[["Data3"]][[as.character(0.1)]] = "Data3_CV_0.2_20141224_215117"
    files[["Data3"]][[as.character(0.1)]] = "Data3_CV_0.2_20141229_223757"
    files[["Data3"]][[as.character(0.2)]] = "Data3_CV_0.2_20141225_085533"
    files[["Data3"]][[as.character(0.4)]] = "Data3_CV_0.2_20141225_094939"
    files[["Data3"]][[as.character(0.5)]] = "Data3_CV_0.2_20141225_104211"
    
    files[["Data4"]][[as.character(0.05)]] = "Data4_CV_0.2_20141224_214117"
    files[["Data4"]][[as.character(0.1)]] = "Data4_CV_0.2_20141224_215518"
    files[["Data4"]][[as.character(0.2)]] = "Data4_CV_0.2_20141224_220827"
    files[["Data4"]][[as.character(0.4)]] = "Data4_CV_0.2_20141224_222117"
    files[["Data4"]][[as.character(0.5)]] = "Data4_CV_0.2_20141224_223256"

    files[["Train2"]][[as.character(0.05)]] = "Train2_CV_0.2_20141226_154850"
    files[["Train2"]][[as.character(0.1)]] = "Train2_CV_0.2_20141226_161555"
    files[["Train2"]][[as.character(0.2)]] = "Train2_CV_0.2_20141226_164227"
    files[["Train2"]][[as.character(0.4)]] = "Train2_CV_0.2_20141226_170915"
    files[["Train2"]][[as.character(0.5)]] = "Train2_CV_0.2_20141226_173433"
    
    files[["Train4"]][[as.character(0.05)]] = "Train4_CV_0.2_20141227_104942"
    files[["Train4"]][[as.character(0.1)]] = "Train4_CV_0.2_20141227_110244"
    files[["Train4"]][[as.character(0.2)]] = "Train4_CV_0.2_20141227_111630"
    files[["Train4"]][[as.character(0.4)]] = "Train4_CV_0.2_20141227_112922"
    files[["Train4"]][[as.character(0.5)]] = "Train4_CV_0.2_20141227_114111"

    files[["Train3"]][[as.character(0.1)]] = "Train3_CV_0.2_20141231_213914"

    
    
    
    
  }else if(CV == 0.3){
    files[["Data1"]][[as.character(0.05)]] = ""
    files[["Data1"]][[as.character(0.1)]] = ""
    files[["Data1"]][[as.character(0.2)]] = ""
    files[["Data1"]][[as.character(0.4)]] = ""
    files[["Data1"]][[as.character(0.5)]] = ""
    
    files[["Data2"]][[as.character(0.05)]] = ""
    files[["Data2"]][[as.character(0.1)]] = ""
    files[["Data2"]][[as.character(0.2)]] = ""
    files[["Data2"]][[as.character(0.4)]] = ""
    files[["Data2"]][[as.character(0.5)]] = ""
    
    files[["Data3"]][[as.character(0.05)]] = ""
    files[["Data3"]][[as.character(0.1)]] = ""
    files[["Data3"]][[as.character(0.2)]] = ""
    files[["Data3"]][[as.character(0.4)]] = ""
    files[["Data3"]][[as.character(0.5)]] = ""
    
    files[["Data4"]][[as.character(0.05)]] = ""
    files[["Data4"]][[as.character(0.1)]] = ""
    files[["Data4"]][[as.character(0.2)]] = ""
    files[["Data4"]][[as.character(0.4)]] = ""
    files[["Data4"]][[as.character(0.5)]] = ""
    
  }
  return(files)
  
}