# 1_key_text.R
# bits of text that are key
# May 2022

## things to replace in titles and abstracts before counting pronouns
# remove subscript and superscript, bold, italic, etc
special.words = c('sub','sup','super','i','b','exp','fraction')
open = paste('\\<', special.words, '\\>', sep='') 
close = paste('\\<\\/', special.words, '\\>', sep='')
to.replace = c(open, close)
to.replace = to.replace[order(-nchar(to.replace))] # longest to shortest
to.replace = paste(to.replace, collapse='|')

# remove these words/phrases from the title (first word)
bogus.acronyms.title = c('WITHDRAWN','CORRIGENDUM','EDITORIAL','MEDICAL','TRANSACTIONS','CASE RECORDS','SYMPOSIUM','MASSACHUSETTS GENERAL HOSPITAL',"INF POS=\"STACK\"") # plus one bit of html code
bogus.acronyms.title = bogus.acronyms.title[order(-nchar(bogus.acronyms.title))] # longest to shortest
bogus.acronyms.title = paste(bogus.acronyms.title, collapse='|')
# remove punctuation, can't use [:punct:] because it includes & and we want to keep that, e.g. for "AT&T"
# decided to remove + because of things like 28517515[pmid] and 28516485[pmid]; added narrow hyphen 
narrow.hyphen = Unicode::u_char_inspect(Unicode::as.u_char('2011'))$Char # e.g, 31524255[pmid]
punctuation = unique(c("!","‴","'",'"',"#","%","(",")","*",",","-",".","\\","/",":",";","<","=","═",">","?","@","[","/","]","^","_","{","|","}","~","′","$","‰","¬","÷","†","‡","“","”","�","（","）","＋","│","£","¢","➔","⁃","æ","ᇞ","⫽","⁎","＊","`","ـ","+","②","③","④","⑤","•","Â","?","?","°","±","×","²","¿","«","＞","＜","⩽","⩾","″","¼","½","¾","–","-","…", narrow.hyphen))    
punctuation = paste(paste('\\', punctuation, sep=''), collapse='|')

