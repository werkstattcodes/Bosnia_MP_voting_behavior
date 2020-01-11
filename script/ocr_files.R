# setup -------------------------------------------------------------------
library(rvest)
library(tidyverse)
library(paletteer)
library(glue)
library(furrr)
plan(multiprocess)
library(pdftools)
library(tesseract)

wdr <- getwd() 

# define tesseract engine -------------------------------------------------

path_bos <- paste0(wdr, "/tesseract_lang/")
tesseract_download("bos")#, datapath=paste0(wdr, "/tesseract_lang/"))
myengine<- tesseract(language = "bos",
                     datapath =paste0(wdr, "/tesseract_lang/") )
tesseract_info()

file_list  <- list.files(path=paste0(wdr,"/data/voting_records/"), 
                         pattern=".pdf$",
                         all.files=T,
                         full.names = T) 


# ATTEMPT: pdf_ocr_text  ------------------------------------------------------------
#PDF error: No display font for 'ArialUnicode'


fn_extract_text <- function(x) {
  
  x %>% 
    pdf_ocr_text(., language="bos") %>% 
    data.frame(text_raw=.)
}

x<- file_list %>% 
  set_names() %>% 
  future_map_dfr(., fn_extract_text, .progress=TRUE, .id="link") 


df_ocr_results <- x %>% 
  group_by(link) %>% 
  summarise(raw_text=paste(text_raw, collapse=", "),
            pages=n()) %>% 
  mutate(raw_text=iconv(raw_text, from="UTF-8", to="windows-1253")) 
  

write_csv2(df_ocr_results, path=paste0(wdr, "/data/df_ocr_results.csv"))
df_ocr_results<- readr::read_csv2(file = paste0(wdr, "/data/df_ocr_results.csv"))

df_missing_ocr_results<- df_ocr_results %>% 
  filter(is.na(text_raw)) #17 documents
df_missing_ocr_results


#results in from function => 1 row per page; has to be grouped by document;
#resutls with is.na(text_raw) are empty pages, e.g. at end of document; 
#every ocr-ed document has content => all good.


 
#there is one pdf which is badly scanned and leads to inconsistencies with the names
bad_pdf <- "C:/Users/Roland/Google Drive/Events - Projects/R-Projects/Bosnia_MP_voting_behavior/data/voting_records/law_id_50082-record_id_121403.pdf"	

bad_pdf_ocr <- pdftools::pdf_ocr_text("C:/Users/Roland/Google Drive/Events - Projects/R-Projects/Bosnia_MP_voting_behavior/data/voting_records/law_id_50082-record_id_121403.pdf",
                                      language="bos", dpi=1200)


bad_pdf_tesseract <- pdf_render_page(bad_pdf, dpi=1600)  
png::writePNG(bad_pdf_tesseract, "page.png")

