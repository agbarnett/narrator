# 99_key_characters_words_phrases.R
# key characters, words and phrases
# September 2022

# remove these upper case words as acronyms from the abstract (also removed `Graphical Abtract`)
# also removed sub-headings in 0_read_pubmed_api.R, but some remain
# some short words added with colons as they may also be acronyms
# some found in random checking, e.g., 18688099[pmid] 
bogus.acronyms.abstract = c('ABSTRACT','KEYWORDS','KEY WORDS','PRéCIS',
                            "PHYSICIAN'S QUALIFICATIONS","FACILITY",'PREOPERATIVE COUNSELING AND INFORMED CONSENT','ANESTHESIA','POSTOPERATIVE CARE', # from 18688102[pmid]
                            'WITHDRAWN','CORRIGENDUM','BACKGROUND','INTRODUCTION','OBJECTIVE',
                            'METHODS','METHOD','MATERIAL AND METHODS','MATERIALS AND METHODS','MATERIALS \\& METHODS',
                            'PRACTITIONER POINTS','RANDOMISATION','AIM:','AIMS:','DESIGN:',
                            "HYPOTHETICAL PATIENTS",'PHYSICIAN SUBJECTS','DATA SOURCES',
                            'MEASUREMENTS','EXPERIMENTAL PREPARATIONS','NATURALSIZEFLAG',
                            'SETTING:','SUBJECTS:','PRIMARY OUTCOME','SECONDARY OUTCOMES',
                            "PHYSICIANS' QUALIFICATIONS", 'INDICATIONS FOR BOTULINUM TOXIN', # 18688099[pmid] 
                            'STATISTICAL POWER','DEVELOPMENT WORK','INTERVENTION',
                            'PURPOSE','HYPOTHESIS','PARTICIPANTS','SUMMARY','DATA AVAILABILITY',
                            'SEARCH METHODS','SELECTION CRITERIA','DATA COLLECTION AND ANALYSIS','MAIN RESULTS',"AUTHORS' CONCLUSIONS",
                            'EXPERIMENTAL DESIGH','EXPERIMENTAL DESIGN', # with typo for 12114404
                            'RESULTS','DISCUSSION','CONCLUSION','CONCLUSIONs','IMPORTANCE','UNASSIGNED',
                            'DATA:','IMPLICATIONS FOR HEALTH CARE PROVISION AND USE','AIMS OF THE STUDY',
                            'IMPACT STATEMENT','SIGNIFICANCE','SIGNIFICANCE STATEMENT',
                            'SIGNIFICANCE OF RESULTS','SAMPLE, DESIGN AND MEASUREMENTS',
                            'RECOMMENDATIONS','PRACTICAL APPLICATION','Graphical Abstract',
                            'FORTHCOMING DEVELOPMENTS','MATRIX DEVELOPMENT',
                            'NEW \\& NOTEWORTHY','NEW &amp; NOTEWORTHY','RECOMMENDATION',
                            'PERSPECTIVE','FUNDING:','LAY ABSTRACT','LIMITATIONS',
                            'IMPLICATIONS FOR REHABILITATION','REHABILITATION',
                            'LEVEL OF EVIDENCE','Wiley Periodicals',
                            'This corrects the article DOI', 'This corrects the article',
                            'TRIAL REGISTRATION','ClinicalTrials.gov','CLINICALTRIALS','CLINICALTRIALSGOV',
                            '\\(?ABSTRACT TRUNCATED AT \\d+ WORDS\\)?','CopyrightInformation','Copyright Information')
if(length(bogus.acronyms.abstract) != length(unique(bogus.acronyms.abstract))){cat('Warning, duplicated sub-heading\n')}
bogus.acronyms.abstract = bogus.acronyms.abstract[order(-nchar(bogus.acronyms.abstract))] # longest to shortest
bogus.acronyms.abstract = paste(bogus.acronyms.abstract, collapse = '|')

# replace numbers because they can get linked to words and become wrongly classed as acronyms
numbers.to.replace = c("-\\d+\\.\\d+"," \\d+\\.\\d+", " \\d+", "-\\d+") # replace, e.g., 1.11, 22, -22
numbers.to.replace = paste(numbers.to.replace, collapse = '|')

### hedging ###
# list of hedging words and phrases
words_phrases_hedging = c("Largely",
"Possible",
"Potential",
"Potentially",
"May",
"Likely",
"Could",
"Perhaps",
"Further studies",
"Further study",
"Further investigations?",
"future studies",
"future study",
"future investigations?",
"I believe",
"I believed",
"I think",
"I thought",
"We believed",
"We believe",
"We think",
"We thought",
"Less sure",
"More sure",
"Seems?",
"Appears?",
"We attempt",
"We attempted",
"I attempt",
"I attempted",
"Plausible",
"Suggests?", # ? for optional s
"Suggested",
"Indicates?",
"Apparently",
"Generally",
"In general",
"in theory",
"theoretically",
"Possibly",
"Might",
"Uncertain",
"Maybe")
words_phrases_hedging = tolower(words_phrases_hedging) # can safely switch to lower text
words_phrases_hedging = words_phrases_hedging[order(-nchar(words_phrases_hedging))] # long to short
# look for whole words", "phrase
to_search_hedging = paste(paste0('\\b', words_phrases_hedging, '\\b'), collapse = '|')

### signposting ###
# list of signposting words and phrases
words_phrases_signposting = c("Firstly",
"Secondly",
"Thirdly",
"Fourthly",
"Fifthly",
"Sixthly",
"Seventhly",
"Eighthly",
"Ninthly",
"Tenthly",
"Furthermore", 
"Moreover", 
"Nevertheless", 
"However",
"Finally",
"Already",
"Before",
"Soon",
"Afterward",
"Formerly",
"Presently",
"Recently",
"Immediately",
"Instantly",
"Indeed",
"in turn",
"After",
"Beforehand",
"Subsequently",
"notwithstanding",
"Next")
words_phrases_signposting = tolower(words_phrases_signposting) # can safely switch to lower text
words_phrases_signposting = words_phrases_signposting[order(-nchar(words_phrases_signposting))] # long to short
# look for whole words, phrase
to_search_signposting = paste(paste0('\\b', words_phrases_signposting, '\\b'), collapse = '|')

### narrator ###
# list of narrator words and phrases
words_phrases_narrator = c("We", 'we', 'Us', 'us') # case-sensitive, otherwise gets confused with US as a country
words_phrases_narrator = words_phrases_narrator[order(-nchar(words_phrases_narrator))] # long to short
# look for whole words, phrase
to_search_narrator = paste(paste0('\\b', words_phrases_narrator, '\\b'), collapse = '|')

## copyright ##
# used to check that I haven't wrongly included copyright
words_phrases_copyright = c("copyright","rights reserved",'cc by') # 
words_phrases_copyright = tolower(words_phrases_copyright) # can safely switch to lower text
words_phrases_copyright = words_phrases_copyright[order(-nchar(words_phrases_copyright))] # long to short
# look for whole words, phrase
to_search_copyright = paste(paste0('\\b', words_phrases_copyright, '\\b'), collapse = '|')

## male gender ; ? for plural - assuming some bad grammar for `mans` ##
# add uncle, aunt, niece, nephew?
male_words = c('he', 'his', 'mens?', 'mans?', 'males?', 'boys?', 'fathers?', 'husbands?', 'brothers?', 'sons?') 
female_words = c('she', 'her', 'womens?', 'womans?', 'females?', 'girls?', 'mothers?', 'wife', 'wives', 'sisters?', 'daughters?') 
male_words = tolower(male_words)
female_words = tolower(female_words)
male_words = male_words[order(-nchar(male_words))] # long to short
female_words = female_words[order(-nchar(female_words))] # long to short
# look for whole words
to_search_male = paste(paste0('\\b', male_words, '\\b'), collapse = '|')
to_search_female = paste(paste0('\\b', female_words, '\\b'), collapse = '|')

## further test of male/female ##
# version 1
male_words1 = c('males?') 
female_words1 = c('females?') 
male_words1 = tolower(male_words1)
female_words1 = tolower(female_words1)
male_words1 = male_words1[order(-nchar(male_words1))] # long to short
female_words1 = female_words1[order(-nchar(female_words1))] # long to short
# version 2
male_words2 = c('mens?', 'mans?', 'boys?') 
female_words2 = c('womens?', 'womans?', 'girls?') 
male_words2 = tolower(male_words2)
female_words2 = tolower(female_words2)
male_words2 = male_words2[order(-nchar(male_words2))] # long to short
female_words2 = female_words2[order(-nchar(female_words2))] # long to short
# look for whole words
to_search_male1 = paste(paste0('\\b', male_words1, '\\b'), collapse = '|')
to_search_female1 = paste(paste0('\\b', female_words1, '\\b'), collapse = '|')
to_search_male2 = paste(paste0('\\b', male_words2, '\\b'), collapse = '|')
to_search_female2 = paste(paste0('\\b', female_words2, '\\b'), collapse = '|')

## negative control ##
negative_control = 'the'
to_search_negative_control = paste(paste0('\\b', negative_control, '\\b'), collapse = '|')

## misspellings ##
words_phrases_spelling = c(
"acident",
"acidently",
"accellerate",
"aquit",
"amature",
"calender",
"changeble",
"colectable",
"colum", 
"collumn",
"critisize",
"dessicate", 
"dessiccate",
"dieing",
"equiptment",
"excelent",
"explination",
"firey",
"genious",
"heigth",
"heirarchy",
"immitate",
"ignorence",
"imediate",
"liesure",
"miniture",
"personnell", 
"personel", 
"personell",
"reknowned",
"sieze",
"sucess", 
"succes",
"serprise",
"threshhold",
"twelth",
"wether",
"absense",
"absentse",
"abcense",
"absance ",
"acceptible",
"acheive",
"acknowlege",
"aknowledge",
"aquires?",
"accross",
"adress",
"agressive",
"agression",
"allegainces?",
"allegiences?",
"algeiances?",
"appearences?",
"appropraite",
"inappropraite",
"arguements?",
"basicly",
"beatiful",
"beginings?",
"beleives?",
"belives?",
"becuase",
"bizzare",
"buisness",
"camoflages?",
"camoflagues?",
"Carribean",
"cemetary",
"cematery",
"changable",
"comming",
"commitees?",
"commited",
"comitted",
"completly",
"conceeds?",
"concious",
"consious",
"curiousity",
"decieves?",
"dilemas?",
"dissapears?",
"dissapoints?",
"enviroments?",
"excedes?",
"exilerates?",
"existances?",
"fasinating",
"Farenheit",
"familar",
"finaly",
"flourescent",
"foriegn",
"forseeable",
"fourty",
"fowards?",
"freinds?",
"fullfils?",
"futhers?",
"gratefull",
"greatful",
"garantees?",
"garentees?",
"garanty",
"gaurds?",
"guages?",
"giudance",
"guideance",
"jist",
"happend",
"harrass",
"harrassment",
"idiosyncracy",
"immediatly",
"incidently",
"independant",
"immitates?",
"inteligence",
"intelligance",
"interupts?",
"irresistable",
"jewelery",
"kernals?",
"knowleges?",
"nife",
"liases?",
"liasons?",
"millenium",
"millenia",
"noticable",
"obitury",
"obituries",
"ocassions?",
"occured",
"occuring",
"occurances?",
"occurences?",
"pavillions?",
"persistant",
"perseverence",
"peices?",
"politicans?",
"Portugese",
"posessions?",
"prefered",
"prefering",
"propoganda",
"realy",
"recieves?",
"refering",
"referances?",
"rember",
"remeber",
"resistences?",
"sence",
"seige",
"supercedes?",
"suprises?",
"tatoos?",
"tendancy",
"threshholds?",
"tommorow",
"tommorrow",
"tounge",
"truely",
"unforseen",
"unfortunatly",
"wierd",
"whereever",
"wholey",
  "Assissts?",
  "Assissted",
  "Publically",
  "Ceasarean",
  "Definately",
  "Definate",
  "Goverment",
  "seperated?",
  "Occured",
  "Untill",
  "Recieved?",
  "Wich",
  "acommodate",
  "Accomodate",
  "aquires?",
  "apparant",
  "aparent",
  "apparrent",
  "aparrent",
  "calenders?",
  "collaegues?",
  "collegues?",
  "coleagues?",
  "consciencious",
  "concensus",
  "entrepeneurs?",
  "entreprenurs?",
  "entreperneurs?",
  "experiance",
  "indispensible",
  "liasion",
  "lisence",
  "maintainance",
  "maintnance",
  "neccessary",
  "necessery",
  "occassion",
  "pasttime",
  "privelege",
  "priviledge",
  "recomend",
  "reccommend",
  "refered",
  "relevent",
  "revelant",
"sqeezed",
  "succesful",
  "successfull",
  "sucessful",
  "underate",
  "withold")
words_phrases_spelling = unique(words_phrases_spelling) # in case of duplicates
words_phrases_spelling = tolower(words_phrases_spelling) # can safely switch to lower case
spelling_words = words_phrases_spelling[order(-nchar(words_phrases_spelling))] # long to short
to_search_spelling = paste(paste0('\\b', spelling_words, '\\b'), collapse = '|') # look for whole words

## hype, from JAMA 10.1001/jamanetworkopen.2022.28676
# Importance, removed major, significant
hype_words_1 = c("compelling","critical","crucial","essential","foundational","fundamental","imperative","important","indispensable","invaluable","key","paramount","pivotal","strategic","timely","ultimate","urgent","vital")
# Novelty, removed first       
hype_words_2 = c("creative","emerging","groundbreaking","innovative","latest","novel","revolutionary","unique","unparalleled","unprecedented")
# Rigor, removed systematic
hype_words_3 = c("accurate","advanced","careful","cohesive","detailed","nuanced","powerful","quality","reproducible","rigorous","robust","scientific","sophisticated","strong")
# Scale, removed "top"    
hype_words_4 = c("ample","biggest","broad","comprehensive","considerable","deeper","diverse","enormous","expansive","extensive","fastest","greatest","huge","immediate","immense","inter.?disciplinary","international","inter.?professional","largest","massive","multi.?disciplinary","myriad","overwhelming","substantial","trans.?disciplinary","tremendous","vast")
#  Utility
hype_words_5 = c("accessible","actionable","deployable","durable","easy","effective","efficacious","efficient","generalizable","generalisable","ideal","impactful","intuitive","meaningful","productive","ready","relevant","rich","safer","scalable","seamless","sustainable","synergistic","tailored","tangible","transformative","user.?friendly")
# Quality, removed experienced, qualified, senior
hype_words_6 = c("ambitious","collegial","dedicated","exceptional","intellectual","long.?standing","motivated","premier","prestigious","promising","renowned","skilled","stellar","successful","talented","vibrant")
# Attitude           
hype_words_7 = c("attractive","confident","exciting","incredible","interesting","intriguing","notable","outstanding","remarkable","surprising")
# Problem
hype_words_8 = c("alarming","daunting","desperate","devastating","dire","dismal","elusive","stark","unanswered","unmet")
# Mollie's list, removed must
hype_words_9 = c("astonishing","backbone","critically","decisive","dramatic","drastic","exceptional","extreme","extremely","frightening","fundamental","heavily","hot ?spots?","imperative","necessary","persistent","profound","radical","radically","rapid","rapidly","serious","severe","severely","urgently")
#
hype_words_1 = hype_words_1[order(-nchar(hype_words_1))] # long to short
to_search_hype_words_1 = paste(paste0('\\b', hype_words_1, '\\b'), collapse = '|') # look for whole words
#
hype_words_2 = hype_words_2[order(-nchar(hype_words_2))] # long to short
to_search_hype_words_2 = paste(paste0('\\b', hype_words_2, '\\b'), collapse = '|') # look for whole words
#
hype_words_3 = hype_words_3[order(-nchar(hype_words_3))] # long to short
to_search_hype_words_3 = paste(paste0('\\b', hype_words_3, '\\b'), collapse = '|') # look for whole words
#
hype_words_4 = hype_words_4[order(-nchar(hype_words_4))] # long to short
to_search_hype_words_4 = paste(paste0('\\b', hype_words_4, '\\b'), collapse = '|') # look for whole words
#
hype_words_5 = hype_words_5[order(-nchar(hype_words_5))] # long to short
to_search_hype_words_5 = paste(paste0('\\b', hype_words_5, '\\b'), collapse = '|') # look for whole words
#
hype_words_6 = hype_words_6[order(-nchar(hype_words_6))] # long to short
to_search_hype_words_6 = paste(paste0('\\b', hype_words_6, '\\b'), collapse = '|') # look for whole words
#
hype_words_7 = hype_words_7[order(-nchar(hype_words_7))] # long to short
to_search_hype_words_7 = paste(paste0('\\b', hype_words_7, '\\b'), collapse = '|') # look for whole words
#
hype_words_8 = hype_words_8[order(-nchar(hype_words_8))] # long to short
to_search_hype_words_8 = paste(paste0('\\b', hype_words_8, '\\b'), collapse = '|') # look for whole words
#
hype_words_9 = hype_words_9[order(-nchar(hype_words_9))] # long to short
to_search_hype_words_9 = paste(paste0('\\b', hype_words_9, '\\b'), collapse = '|') # look for whole words
