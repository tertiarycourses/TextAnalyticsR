#########################################################################
#                             paste()
##########################################################################

a="dwight"; b="was" ; c="here"
paste(a,b,c)   # default seperator is space
paste(a,b,c,sep="")
paste(a,b,c,sep=",")

#genrating sequence Ram1, Ram2 and so on
paste("Ram", seq(1:20), sep="")# we put nothing as seperator


##########################################################################
#                           strsplit()
##########################################################################

a="Today is a good day"

strsplit(a," ") # "Today" "is" "a" "good" "day"
strsplit(a,"") # "T" "o" "d" "a" "y" .... individual letters, split by blank

############################################################################
#                       substr() and sub()
############################################################################

x="Dwight Nuwan Fonseka"
substr(x,1,3)  # get 1st letter to 3rd letter
substr(x,1,6)="Dwayne"   #"Dwi" replaced with "Dwa"
x

substr(x,1,6)="Dwa" # will not replace all 6 words

substr(x,1,6)   # "Dwight"
substr(x,8,12)  #  "Nuwan"
substr(x,14,20) # "Fonseka"


################################################################################
#                           nchar()
################################################################################

nchar(x)  # use above example
#returns number of characters


###############################################################################
#				gsub() and grep()
###############################################################################

x = "dwight is a hero"
gsub("dwight", "dwayne", x)
gsub("dwight", "dwa", x)


y=c("dwight", "is", "a", "heroine")


grep("hero",x, value=F)   #>1       (we get the location if we put value=F)
grep("hero",y, value=F)   #>4

grep("hero",x, value=T)   #>"hero"  (we get the word itself containing "hero")
grep("hero",y, value=T)   #> heroine


#################################################################################
#                           working with strings
#################################################################################

mystring="NORTH KOREA fired a missile early on Tuesday that flew over Japan and landed in the Pacific waters off the northern region of Hokkaido, South Korea and Japan said, in a sharp escalation of tensions on the Korean peninsula. The United States, Japan and South Korea considered that launch to have been a ballistic missile test while NORTH KOREA said it was a rocket carrying a communications satellite into orbit. South Korea's military said the latest missile was launched from the Sunan region near the NORTH KOREA capital Pyongyang."

numstring="My password is '$%^&*%$@007'. I have 150 R scripts, 110 Python scripts and 20 Java scripts."


tolower(mystring)
toupper(mystring)
strsplit(mystring, " ") # split string after the space

tt=table(strsplit(mystring, " "))

barplot(tt)

############################### working with gsub ################################

gsub("Korea", "KOREA", mystring)
# gsub substitutes all the occurances of the "Korea" into "KOREA"
# take not that only "Korea" and not "korea" is substituted
# it is case sensitive

sub("Korea", "KOREA", mystring)
# sub substitutes only the first occurance of "Korea" into "KOREA"

gsub("Korea", "KOREA", mystring, ignore.case=T)
# all occurances of "Korea" and "korea" are substituted
# case sensitvity ignored

################### Advanced Methods ##################################


######## need to check the regular expression syntax for gsub
# to find the code  eg . "\\D" refers to digits and so on

#             \\d   >> digit
#             \\D   >> not digit
#             \\s   >> space
#             \\S   >> not space
#             \\w   >> word
#             \\W   >> not word
#             \\t   >> tab

gsub("\\d", "", numstring)
# replaces all digits with "" (blank)

gsub("\\D", "", numstring)
# replaces anything not with a digit with "" (blank)

gsub("\\s", "", numstring)
# replaces all the spaces with "" (blank)


gsub("[st]", "@", numstring)
# replaces alphabets "s" and "t" with "@"

gsub("ss", "@", numstring)
# replaces alphabets "ss" with "@"

gsub("sc", "@", numstring)
# replaces alphabets "sc" with "@"

gsub("[[:punct:]]","", numstring)
# removing punctuation

gsub("[^[:graph:]]","",numstring)
# removing anything but graphical characters > remove spaces

################# package stringr for more text manipulation

library(stringr)

str_c(c(mystring,numstring))
# adding two strings together

str_count(numstring, "s")
# count the alphabet "s" occurances

str_locate(numstring, "script")
# locate specific numbers or alphabet (first occurance)

str_locate_all(numstring, "script")
# locate specific numbers or alphabet

str_replace(numstring, "script", "999")
# replace similiar to sub function

str_replace_all(numstring, "script", "999")
# replace similiar to gsub function

str_replace(numstring, "\\d", "")
# replace first digit occurance > similiar to sub

str_replace_all(numstring, "\\d", "")
# replaces all the numbers with blank >> like gsub


####################################################
#                   ROOT word
####################################################

# looking for all forms of a root word in a text
# eg work > working, worked, works etc...

library(stringr)
docs=readLines(file.choose())   # choose hitler
dd2=strsplit(docs," ")

dd3=c()
ww3="German"  # you can replace this with your word

for(i in 1:length(dd2)){
  d3=unlist(dd2[i])
  dd4=str_count(d3,ww3)
  if(length(dd4==0)==0){next}
  d33=grep(ww3,d3, value=T)
  d11=gsub("[[:punct:]]","", d33)
  d11=gsub("[^[:graph:]]","",d11)
  dd3=append(dd3,d11)
}

dd3=unique(dd3)
dd3


##################################################################
#                    Search words in documents
###################################################################


words=c(" hate "," war "," suffer ") # please put space before and after
documents=list.files(choose.dir())
path=choose.dir()

l1=length(words)
l2=length(documents)

mydf=matrix(nrow=l1,ncol=l2,NA)
row.names(mydf)=words
colnames(mydf)=documents

for(i in 1:l1){
  for(j in 1:l2){
library(stringr)  
myfile=paste0(path,"\\",documents[j])   
x=str_count(readLines(myfile), words[i])
x2=sum(x)
mydf[i,j]=x2
  }
}

mydf
