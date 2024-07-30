###################################################################
#Overview
#This script takes as input an ELAN annotation file (extension .eaf) containing a transcript of an interaction between two speakers and returns an .xml file with the tiers in interlinear format suitable for displaying on a web page. It assumes the ELAN file contains the following tiers:
# - a primary tier for Speaker A and Speaker B containing full sentences/utterances/intonation units
# - a tier with each word (a dependent of the intonation unit tier)
# - a tier with each morpheme (a dependent of the word tier)
# - a tier with a gloss for each morpheme (a dependent of the morpheme tier)
# - a tier with translation of each intonation unit (not dependent on any other tier)
#
#Notes
# - The script assumes that all tiers are completely filled in--e.g. there should not be any words that don't have morpheme or gloss annotations associated with them. 
# - The script was written for a transcript with two speakers and the tier structure outlined above, but it could be adapted to run on transcripts with more speakers, different numbers of tiers, etc.
###################################################################

rm(list=ls(all=TRUE))
library(XML)

##Load the file
current.file <- scan(
  file="Conversation1_transcript.eaf",
  what=character(),
  sep="\n")

##Read file as xml
current.file.tree <- xmlInternalTreeParse(current.file)
current.file.root <- xmlRoot(current.file.tree)

#set up meaningful names for the relevant child nodes
spkrB.ius <- current.file.root[[3]] 
spkrA.trans <- current.file.root[[4]]
spkrA.ius <- current.file.root[[5]]
spkrB.trans <- current.file.root[[6]]
spkrA.words <- current.file.root[[7]]
spkrB.words <- current.file.root[[8]]
spkrA.morphs <- current.file.root[[9]]
spkrB.morphs <- current.file.root[[10]]
spkrA.glosses <- current.file.root[[11]]
spkrB.glosses <- current.file.root[[12]]



##################################################################################
#collecting the IU tiers
##################################################################################

#create a new xml node for speaker A
speakerA <- newXMLNode("speakerA")

#create a new xml node for speaker B
speakerB <- newXMLNode("speakerB")

IUtoNode <- function(node, parent_node, speaker){
  #extract the attributes that we want to preseve
  id <- xmlGetAttr(node[[1]], "ANNOTATION_ID")
  ts1 <- xmlGetAttr(node[[1]], "TIME_SLOT_REF1")
  ts2 <- xmlGetAttr(node[[1]], "TIME_SLOT_REF2")
  #and get the actual text of the IU
  annotation_value <- xmlValue(node[[1]][[1]])
  
  #then write a new node with all that information
  newXMLNode("annotation", 
             #the attributes
             attrs = c(annotation_id = id, 
                       time_slot1 = ts1, 
                       time_slot2 = ts2, 
                       who = speaker), 
             #and add it as a child of the specified parent node
             parent = parent_node,
             #add a child node where the IU text will go
             newXMLNode("iu", annotation_value), 
             #and add a place-holder below it for the words
             #(since you can't easily add children to a text node) 
             newXMLNode("words")) 
}


#we need to do the speakers separately for this first stage so we can add an attribute to each IU saying which speaker it came from
#this creates all the child nodes containing the Speaker A annotations
xmlSApply(spkrA.ius, function(node) IUtoNode(node, speakerA, "Speaker A")) 

#this creates all the child nodes containing the Speaker B annotations
xmlSApply(spkrB.ius, function(node) IUtoNode(node, speakerB, "Speaker B"))

#put the speakers together
conversation1 <- newXMLNode("conversation1")

#add a copy of all the children of speakerA
copyA <- xmlClone(speakerA)
addChildren(conversation1, kids = xmlChildren(copyA))

#add a copy of all the children of speakerB
copyB <- xmlClone(speakerB)
addChildren(conversation1, kids = xmlChildren(copyB))


#now that we've got the speaker labeled on each IU in our new xml structure, let's make another structure that combines spkrA.ius and spkrB.ius from the .eaf
all_ius <- newXMLNode("all_ius")

#add a copy of all the children of spkrA.ius
A.ius_copy <- xmlClone(spkrA.ius)
addChildren(all_ius, kids = xmlChildren(A.ius_copy))

#add a copy of all the children of spkrB.ius
B.ius_copy <- xmlClone(spkrB.ius)
addChildren(all_ius, kids = xmlChildren(B.ius_copy))


########################################################################
#add the translation tier
########################################################################

#In my ELAN transcript structure, the translation tiers are not dependents of the IU tiers. So I need to add the translation before re-ordering the IUs according to timestamp.

#make a structure with the translations for both speakers
all_translations <- newXMLNode("all_translations")

#add all the children of spkrA.trans
A.trans_copy <- xmlClone(spkrA.trans)
addChildren(all_translations, kids = xmlChildren(A.trans_copy))

#add all the children of spkrB.trans
B.trans_copy <- xmlClone(spkrB.trans)
addChildren(all_translations, kids = xmlChildren(B.trans_copy))


#now add the translations as nodes under the IUs in conversation1
#loop over the IUs
for (i in 1:length(xmlChildren(conversation1))){
  
  #define the current IU
  curr_iu <- conversation1[[i]]
  
  #get the text of the English translation of the current IU
  curr_trans_text <- xmlValue(all_translations[[i]][[1]][[1]])
  
  #and write it to a node at the bottom of the current IU
  newXMLNode("eng", curr_trans_text, parent = curr_iu)
  
}

#spot check a random place to make sure it looks like we expect
conversation1[[4]]


##################################################################################
#adding the word tier 
##################################################################################

#make a new structure with all the words from Speaker A and Speaker B in it
all_words <- newXMLNode("all_words")

#add all the children of spkrA.words
A.words_copy <- xmlClone(spkrA.words)
addChildren(all_words, kids = xmlChildren(A.words_copy))

#add all the children of spkrB.words
B.words_copy <- xmlClone(spkrB.words)
addChildren(all_words, kids = xmlChildren(B.words_copy))


#This word_to_node() function takes a single node with a word annotation, extracts the attributes and the text node, and writes a new XML node containing the information under the specified parent node

word_to_node <- function(curr_word_node, parent_node) {
  #extract the attributes that we want to preserve
  word_id <- xmlGetAttr(curr_word_node[[1]], "ANNOTATION_ID")
  word_ref_id <- xmlGetAttr(curr_word_node[[1]], "ANNOTATION_REF")
  word_prev_id <- xmlGetAttr(curr_word_node[[1]], "PREVIOUS_ANNOTATION")
  #and get the actual text of the word
  word_value <- xmlValue(curr_word_node[[1]][[1]])
  
  #then write a new node with all that information
  newXMLNode("annotation", 
             #the attributes
             attrs = c(annotation_id = word_id, 
                       word_ref_id = word_ref_id, 
                       word_prev_id = word_prev_id), 
             #add it as a child of the specified parent node
             parent = parent_node, 
             #and then a child node for the actual text of the word 
             newXMLNode("word", word_value), 
             #and add a place-holder below it for the morphemes
             #(since you can't easily add children to a text node)
             newXMLNode("morphs"))
  
}

#Next, get the ANNOTATION_REF ids for all of the words
word_ref_ids = xmlSApply(all_words,
                         function(node)
                           xmlGetAttr(node[[1]], "ANNOTATION_REF"))

#Finally, loop over the IUs, match the words, and write them as children to the IU node:

for (i in 1:length(xmlChildren(conversation1))){
  #get the annotation id for the current IU  
  iu_id <- xmlGetAttr(all_ius[[i]][[1]], "ANNOTATION_ID") 
  
  #subset words for the ones that have iu.id as their ANNOTATION_REF attribute
  curr_words_subset <- all_words[which(word_ref_ids == iu_id)]
  
  #then apply the word_to_node function to each of the words in the subset
  lapply(curr_words_subset, word_to_node, conversation1[[i]][[2]])
}

#spot check a random place to make sure it looks like we expect
conversation1[[1]] 


##################################################################################
#adding the morph tier 
##################################################################################

#make a new structure with all the morphs from Speaker A and Speaker B in it
all_morphs <- newXMLNode("all_morphs")

#add all the children of spkrA.morphs
A.morphs_copy <- xmlClone(spkrA.morphs)
addChildren(all_morphs, kids = xmlChildren(A.morphs_copy))

#add all the children of spkrB.morphs
B.morphs_copy <- xmlClone(spkrB.morphs)
addChildren(all_morphs, kids = xmlChildren(B.morphs_copy))


#This morph_to_node() function takes a single node with a morph annotation, extracts the attributes and the text node, and writes a new XML node containing the information under the specified parent node
morph_to_node <- function(curr_morph_node, parent_node) {
  #extract the attributes that we want to preserve
  morph_id <- xmlGetAttr(curr_morph_node[[1]], "ANNOTATION_ID")
  morph_ref_id <- xmlGetAttr(curr_morph_node[[1]], "ANNOTATION_REF")
  morph_prev_id <- xmlGetAttr(curr_morph_node[[1]], "PREVIOUS_ANNOTATION")
  #and get the actual text of the morph
  morph_value <- xmlValue(curr_morph_node[[1]][[1]])
  
  #then write a new node with all that information
  newXMLNode("annotation", 
             #the attributes
             attrs = c(annotation_id = morph_id, 
                       morph_ref_id = morph_ref_id, 
                       morph_prev_id = morph_prev_id), 
             #add it as a child of the specified parent node
             parent = parent_node, 
             #and then a child node for the actual text of the morph 
             newXMLNode("morph", morph_value))
}


#Next, get the ANNOTATION_REF ids for all the morphemes
morph_ids = xmlSApply(all_morphs,
                      function(node)
                        xmlGetAttr(node[[1]], "ANNOTATION_REF"))


#and get the ANNOTATION_ID for all of Speaker B's IUs
iu_ids = xmlSApply(all_ius,
                   function(node)
                     xmlGetAttr(node[[1]], "ANNOTATION_ID"))


#Finally, loop over the the words, match the morphs, and write them as children to the word node:

for (i in 1:length(xmlChildren(all_words))){
  #get the annotation id for the current word  
  word_id <- xmlGetAttr(all_words[[i]][[1]], "ANNOTATION_ID") 
  
  #find the appropriate IU node in the xml structure we're creating
  ref_id <- xmlGetAttr(all_words[[i]][[1]], "ANNOTATION_REF")
  iu <- conversation1[[which(iu_ids == ref_id)]]
  
  #and find the appropriate word node within that IU node
  # - get the annotation ids for each word in the IU
  curr_word_ids = xmlSApply(iu[[2]],
                            function(node)
                              xmlGetAttr(node, "annotation_id"))
  # - then see which one matches the current word
  j <- which(curr_word_ids == word_id)
  
  #now subset for the morphs that have word_id as their ANNOTATION_REF attribute
  curr_morphs_subset <- all_morphs[which(morph_ids == word_id)]
  
  #then apply the morph_to_node function to each morph in the subset
  #(choosing the correct <morphs> parent node based on the query above)
  lapply(curr_morphs_subset, morph_to_node, iu[[2]][[j]][[2]])
  
}

#spot check a random place to make sure it looks like we expect
conversation1[[2]]


##################################################################################
#adding the gloss tier 
##################################################################################

#make a new structure with all the glosses from Speaker A and Speaker B in it
all_glosses <- newXMLNode("all_glosses")

#add all the children of spkrA.glosses
A.glosses_copy <- xmlClone(spkrA.glosses)
addChildren(all_glosses, kids = xmlChildren(A.glosses_copy))

#add all the children of spkrB.glosses
B.glosses_copy <- xmlClone(spkrB.glosses)
addChildren(all_glosses, kids = xmlChildren(B.glosses_copy))


#define gloss_to_node function
gloss_to_node <- function(curr_gloss_node, parent_node) {
  #extract the attributes that we want to preserve
  gloss_id <- xmlGetAttr(curr_gloss_node[[1]], "ANNOTATION_ID")
  gloss_ref_id <- xmlGetAttr(curr_gloss_node[[1]], "ANNOTATION_REF")
  #and get the actual text of the gloss
  gloss_value <- xmlValue(curr_gloss_node[[1]][[1]])
  
  #then write a new node with all that information
  newXMLNode("annotation", 
             #the attributes
             attrs = c(annotation_id = gloss_id, 
                       gloss_ref_id = gloss_ref_id), 
             #add it as a child of the specified parent node
             parent = parent_node, 
             #and then a child node for the actual text of the gloss 
             newXMLNode("gloss", gloss_value))
}

#Next, get the ANNOTATION_ID for all the morphs
morph_ids = xmlSApply(all_morphs,
                      function(node)
                        xmlGetAttr(node[[1]], "ANNOTATION_ID"))

#and same for all the words
word_ids = xmlSApply(all_words,
                     function(node)
                       xmlGetAttr(node[[1]], "ANNOTATION_ID"))

#and for all the IUs
iu_ids = xmlSApply(all_ius,
                   function(node)
                     xmlGetAttr(node[[1]], "ANNOTATION_ID"))


#loop over all the glosses, match each gloss to the morph it goes with, and write it below the corresponding morph in the new xml structure:

for (i in 1:length(xmlChildren(all_glosses))){
  #get the annotation ref for the current gloss  
  curr_gloss_ref <- xmlGetAttr(all_glosses[[i]][[1]], "ANNOTATION_REF") 
  
  #find the corresponding morph
  curr_morph <- all_morphs[[which(morph_ids == curr_gloss_ref)]]
  
  #get its annotation_ref
  curr_morph_ref <- xmlGetAttr(curr_morph[[1]], "ANNOTATION_REF")
  
  #find the corresponding word
  curr_word <- all_words[[which(word_ids == curr_morph_ref)]]
  
  #get its annotation ref
  curr_word_ref <- xmlGetAttr(curr_word[[1]], "ANNOTATION_REF")
  
  #and find which IU in the new xml structure that word is in
  curr_iu <- conversation1[[which(iu_ids == curr_word_ref)]]
  
  #now find the appropriate word node within that IU node
  # - get the annotation ids for each word in the IU
  curr_word_ids = xmlSApply(curr_iu[[2]],
                            function(node)
                              xmlGetAttr(node, "annotation_id"))
  
  # - then see which one matches the current word
  j <- which(curr_word_ids == curr_morph_ref)
  curr_word <- curr_iu[[2]][[j]]
  
  #and the appropriate morph node within that word node
  # - get the annotation ids for each morph in that word
  curr_morph_ids = xmlSApply(curr_word[[2]],
                             function(node)
                               xmlGetAttr(node, "annotation_id"))
  
  # - then see which one matches the current gloss
  k <- which(curr_morph_ids == curr_gloss_ref)
  curr_morph <- curr_word[[2]][[k]]
  
  #finally, now that we have found the correct parent node (curr_morph)
  #we can apply the gloss_to_node function
  gloss_to_node(all_glosses[[i]], curr_morph)
  
}

#spot check a random place to make sure it looks like we expect
conversation1[[2]]



##################################################################################
#collapsing the morphs for each word
##################################################################################

#loop over all the IUs in the new xml structure
for (i in 1:length(xmlChildren(conversation1))){
  
  #define the current IU
  curr_iu <- conversation1[[i]]
  
  #make a collector vector to put the words in
  word_collector <- c()
  
  #loop over the words in the current IU
  # (the words are hosted in the second element of curr_iu)
  for (j in 1:length(xmlChildren(curr_iu[[2]]))){
    
    #define the current word
    curr_word <- curr_iu[[2]][[j]]
    
    #identify the node that contains the morphs
    curr_morphs <- curr_word[[2]]
    
    #make a collector vector to put the morphs in
    morph_collector <- c()
    
    #loop over the morphs in the current word and collect them
    for (k in 1:length(xmlChildren(curr_morphs))) {
      
      #identify the current morpheme node
      curr_morph <- curr_morphs[[k]]
      
      #extract the actual text of the morpheme
      curr_morph_text <- xmlValue(curr_morph[[1]])
      
      #add it to the collector vector
      morph_collector <- c(morph_collector, curr_morph_text)
      
    } #close loop over the morphs in the current word
    
    #now collapse the collector vector into a single string
    #representing the word with morpheme breaks
    curr_word_text <- paste(morph_collector, collapse = "") 
    
    #and add that to a vector that collects the words for the current IU
    word_collector <- c(word_collector, curr_word_text)
    
  }  #close loop over words
  
  #now collapse the word collector vector into a single string
  # (adding a space between the words)
  curr_iu_text <- paste(word_collector, collapse = " ") 
  
  #and write that to a new node in the xml structure
  newXMLNode("morph", curr_iu_text, parent = curr_iu)
  
} #close loop over IUs

#spot check a random place to make sure it looks like we expect
conversation1[[10]]


#Excursus 1##################################
#The above code block fails if there is a word that doesn't have anything on the morph tier, because then length(xmlChildren(curr_morphs) == 0, and you can't subset by 0 in the next line curr_morph <- curr_morphs[[k]]. To fix this, I went back over the transcript and made sure all tiers were filled in for every word. But I may have missed one or two...that's what this code checks for:

#figuring out where the problem is 
iu_index <- c()
overall_morph_num_coll <- c()
for (i in 1:length(xmlChildren(conversation1))){
  
  #define the current IU
  curr_iu <- conversation1[[i]]
  
  #get the number of words in the current IU
  curr_num_words <- length(xmlChildren(curr_iu[[2]]))
  
  #make a collector for the number of morphs
  morph_num_collector <- c()
  
  #loop over the words in the current IU
  # (the words are hosted in the second element of curr_iu)
  for (j in 1:length(xmlChildren(curr_iu[[2]]))){
    
    #define the current word
    curr_word <- curr_iu[[2]][[j]]
    
    #identify the node that contains the morphs
    curr_morphs <- curr_word[[2]]
    
    #find out how many morphs there are
    curr_num_morphs <- length(xmlChildren(curr_morphs))
    
    #add that number to the morph index
    morph_num_collector <- c(morph_num_collector, curr_num_morphs)
    
  } #close loop over words
  
  #add the vector of morph numbers from that IU to the overall collector
  overall_morph_num_coll <- c(overall_morph_num_coll, morph_num_collector)
  
  #and add to the IU index so I know which IU each morph num came from
  iu_index <- c(iu_index, rep(i, length(morph_num_collector)))
  
} #close loop over IUs

length(iu_index)
length(overall_morph_num_coll) #good, those two are the same

x <- data.frame(iu_index, overall_morph_num_coll)
head(x)

#figure out which word doesn't have anything on the morph tier
x_prob <- x[x$overall_morph_num_coll==0, ] 

x_prob 

#For example, if one of the items in x_prob has 863 for its iu_index, find this line in the transcript; there's probably a tier with a missing annotation
conversation1[[863]][[1]] 

#once any problems are fixed, go back to the top of the script and run it again down to this excursus

#End Excursus 1##################################


#Excursus 2###############################
#here's another version to check if there's an entire IU with no annotations (which will also throw the same error with XMLChildren())

#figuring out where the problem is 
iu_index <- c()
word_num_collector <- c()
for (i in 1:length(xmlChildren(conversation1))){
  
  #define the current IU
  curr_iu <- conversation2[[i]]
  
  #get the number of words in the current IU
  curr_num_words <- length(xmlChildren(curr_iu[[2]]))
  
  #add that number to the word index
  word_num_collector <- c(word_num_collector, curr_num_words)
  
  #and add one to the IU index so I know which IU each morph num came from
  iu_index <- c(iu_index, i)
  
} #close loop over IUs

length(iu_index)
length(word_num_collector) #good, those two are the same

x <- data.frame(iu_index, word_num_collector)
head(x)

#figure out which IU doesn't have any annotations
x_prob <- x[x$word_num_collector==0, ] 

x_prob 

#For example, if one of the items in x_prob has 190 for its iu_index, find this line in the transcript; there's probably a tier with a missing annotation
conversation1[[190]][[1]] 

#end Excursus 2##################################



##################################################################################
#collapsing the glosses for each word
##################################################################################

#loop over all the IUs in the new xml structure
for (i in 1:length(xmlChildren(conversation1))){
  
  #define the current IU
  curr_iu <- conversation1[[i]]
  
  #make a collector vector to put the words in
  word_collector <- c()
  
  #loop over the words in the current IU
  # (the words are hosted in the second element of curr_iu)
  for (j in 1:length(xmlChildren(curr_iu[[2]]))){
    
    #define the current word
    curr_word <- curr_iu[[2]][[j]]
    
    #identify the node that contains the morphs
    curr_morphs <- curr_word[[2]]
    
    #make a collector vector to put the glosses in
    gloss_collector <- c()
    
    #loop over the morphs in the current word and collect them
    for (k in 1:length(xmlChildren(curr_morphs))) {
      
      #identify the current morpheme node
      curr_morph <- curr_morphs[[k]]
      
      #extract the text of the gloss for that morpheme
      curr_gloss_text <- xmlValue(curr_morph[[2]][[1]])
      
      #add it to the collector vector
      gloss_collector <- c(gloss_collector, curr_gloss_text)
      
    } #close loop over the morphs in the current word
    
    #now collapse the collector vector into a single string
    #representing the word, with hyphens added for morpheme breaks
    curr_word_gloss <- paste(gloss_collector, collapse = "-") 
    
    #and add that to a vector that collects the words for the current IU
    word_collector <- c(word_collector, curr_word_gloss)
    
  }  #close loop over words
  
  #now collapse the word collector vector into a single string
  # (adding a space between the words)
  curr_iu_gloss <- paste(word_collector, collapse = " ") 
  
  #and write that to a new node in the xml structure
  newXMLNode("gloss", curr_iu_gloss, parent = curr_iu)
  
} #close loop over IUs

#spot check
conversation1[[10]]



########################################################################
#delete all the subordinate nodes with the morphs and glosses
########################################################################

#super easy
xmlSApply(conversation1, function(node) removeNodes(node[[2]]))

#spot check a random place to make sure it looks like we expect
conversation1[[2]]


################################################################
#move the translation tier to the last position, below morph and gloss
################################################################

xmlSApply(conversation1, function(node) addSibling(node[["gloss"]], node[["eng"]])
)

#spot check
conversation1[[5]]



#########################################################################
#order all IUs for all speakers according to timestamp
#########################################################################

#collect all the timestamps 
timestamp_collector <- c()

for (i in 1:length(xmlChildren(conversation1))){
  #define the current iu
  curr_iu <- conversation1[[i]]
  
  #get its starting time_id
  curr_start_time <- xmlGetAttr(curr_iu, "time_slot1")
  
  #convert it to a number
  curr_start_time_num <- as.numeric(gsub("ts", "", curr_start_time, perl = TRUE))
  
  #and add it to the collector
  timestamp_collector <- c(timestamp_collector, curr_start_time_num)
  
}


#make a new version of the xml
c1_final <- newXMLNode("c1_final")

#here's the order the timestamps need to be in 
iu_order <- order(timestamp_collector)

#now loop over the numbers in the order, and add the corresponding node from conversation1 to c1_final
for (i in 1:length(iu_order)){
  
  #define the current index number
  current_iu_number <- iu_order[[i]] 
  
  #find the corresponding iu in conversation1
  current_iu <- xmlClone(conversation1[[current_iu_number]])
  
  #and add it to the final version
  addChildren(c1_final, kids = list(current_iu))
  
}

#spot check
c1_final[[4]]



################################################################
#convert the timestamps in conversation1 to HH:MM:SS.000 format
################################################################

#extract the timestamps into a nice dataframe with columns for the time slot ids and the actual times

#define the node with the timestamp info
timesNode <- current.file.root[[2]]

#extract the time slot ids
TIME_SLOTS = xmlSApply(timesNode,
                       function(node)
                         xmlGetAttr(node, "TIME_SLOT_ID"))

#unlist them
TIME_SLOTS2 <- unlist(TIME_SLOTS)

#now get the actual time values
TIME_VALUES = as.numeric(xmlSApply(timesNode,
                                   function(node)
                                     xmlGetAttr(node, "TIME_VALUE")))

#make another copy with the time values converted to HH:MM:SS.000 format
TIME_VALUES2 <- sapply(TIME_VALUES, function(x) format(as.POSIXct(as.numeric(x) / 1000, "UTC", origin = "1970-01-01"), "%H:%M:%OS3"))

#and put them into a dataframe
timetable <- data.frame(TIME_SLOTS, TIME_VALUES, TIME_VALUES2)


#now loop over the ius in c1_final and add the timestamps

for (i in 1:length(xmlChildren(c1_final))){
  
  #define current IU
  curr_iu <- c1_final[[i]]
  
  #get start time id and look up the time value in the timetable
  start_time_id <- xmlGetAttr(curr_iu, "time_slot1")
  start_time <- timetable$TIME_VALUES2[timetable$TIME_SLOTS == start_time_id]
  start_time
  
  #get start time id and look up the time value in the timetable
  end_time_id <- xmlGetAttr(curr_iu, "time_slot2")
  end_time <- timetable$TIME_VALUES2[timetable$TIME_SLOTS == end_time_id]
  
  #put them together into a time annotation
  timestamp <- paste(c(start_time, end_time), collapse = "-")
  
  #and add the timestamp to the current IU node
  addSibling(curr_iu[["iu"]], newXMLNode("timestamp", timestamp), after = FALSE)
  
}

#spot check
c1_final[[1]]


#remove the attribute with the timeslots, since we don't need that anymore

xmlSApply(c1_final, function(node) removeAttributes(node, attrs = "time_slot1"))
xmlSApply(c1_final, function(node) removeAttributes(node, attrs = "time_slot2"))


#and replace the annotation_id attribute with a simple index called "ref"
for (i in 1:length(xmlChildren(c1_final))){
  
  #define current IU
  curr_iu <- c1_final[[i]]
  
  #get rid of the "annotation_id"
  removeAttributes(curr_iu, attrs = "annotation_id")
  
  #make a simple index
  index <- i
  
  #and add that as a new attribute "ref"
  xmlAttrs(curr_iu) <- c(ref = index)
  
}

#spot check
c1_final[[3]]


#########################################################
#create a header with metadata
#########################################################

#create header node
header <- newXMLNode("header")

#create metadata node under header
metadata <- newXMLNode("metadata", parent = header)

#add metadata items
#language
newXMLNode("language", "Uyghur, Modern Standard", attrs = c(iso = "uig", glottocode = "uigh1240"), parent = metadata)

#generic line of code for adding other metadata items
newXMLNode("item", "value", parent = metadata)


#info about participants
participants <- newXMLNode("participants", parent = metadata)

#Speaker A
speaker_A <- newXMLNode("speaker_A", parent = participants)

#generic line of code for adding info about Speaker A
newXMLNode("item", "value", parent = speaker_A)
#repeat as needed


#Speaker B
speaker_B <- newXMLNode("speaker_B", parent = participants)

#generic line of code for adding info about Speaker B
newXMLNode("item", "value", parent = speaker_B)
#repeat as needed

#check to see it looks right
header



#########################################################
#Write the results to file
#########################################################

#create a new root node
c1root = newXMLNode("c1root", newXMLNode("header"))

#make an XML document with it
c1 = newXMLDoc(node = c1root)

#add the header (created above)
xmlParent(header) <- c1root

#add a node for the actual transcript annotations
transcript_body <- newXMLNode("transcript_body", parent = c1root)

#and assign c1_final to that node
xmlParent(c1_final) <- transcript_body

#and write the file
saveXML(
  c1, 
  file = "conversation_1.xml",
  encoding="UTF-8")
