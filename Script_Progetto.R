#######################################
### DRUG CONSUMPTION CLASSIFICATION ###
#######################################

## CONTEXT: 
# Database contains records for 1885 respondents. For each respondent 12 attributes are known: Personality measurements which include NEO-FFI-R (neuroticism, extraversion, openness to experience, agreeableness, and conscientiousness), BIS-11 (impulsivity), and ImpSS (sensation seeking), level of education, age, gender, country of residence and ethnicity.
# All input attributes are originally categorical and are quantified. After quantification values of all input features can be considered as real-valued. In addition, participants were questioned concerning their use of 18 legal and illegal drugs (alcohol, amphetamines, amyl nitrite, benzodiazepine, cannabis, chocolate, cocaine, caffeine, crack, ecstasy, heroin, 
# ketamine, legal highs, LSD, methadone, mushrooms, nicotine and volatile substance abuse and one fictitious drug (Semeron) which was introduced to identify over-claimers. For each drug they have to select one of the answers: never used the drug, used it over a decade ago, or in the last decade, year, month, week, or day. Database contains 18 classification problems.
# Each of independent label variables contains seven classes: "Never Used", "Used over a Decade Ago", "Used in Last Decade", "Used in Last Year", "Used in Last Month", "Used in Last Week", and "Used in Last Day".

## ATTRIBUTES MEANING:

# Age: Age is the age of participant and has one of the values:
# -0.95197: "18-24",
# -0.07854: "25-34",
# 0.49788: "35-44",
# 1.09449: "45-54",
# 1.82213: "55-64",
# 2.59171: "65+"

# Gender: Gender is gender of participant: 0 for males and 1 for females

# Education: Education is level of education of participant and has one of the values:
# -2.43591:	Left School Before 16 years
# -1.73790:	Left School at 16 years
# -1.43719:	Left School at 17 years
# -1.22751:	Left School at 18 years
# -0.61113:	Some College,No Certificate Or Degree
# -0.05921:	Professional Certificate/ Diploma
# 0.45468:  University Degree
# 1.16365:  Masters Degree	
# 1.98437:  Doctorate Degree

# Country: Country is country of current residence of participant

# Ethnicity: Ethnicity is ethnicity of participant

# Nscore: Nscore is NEO-FFI-R Neuroticism. Neuroticism is one of the Big Five higher-order personality traits in the study of psychology.
# Individuals who score high on neuroticism are more likely than average to be moody and to experience such feelings as anxiety, worry, fear, anger, frustration, envy, jealousy, guilt, depressed mood, and loneliness.

# EScore: Escore is NEO-FFI-R Extraversion. Extraversion is one of the five personality traits of the Big Five personality theory.
# It indicates how outgoing and social a person is. A person who scores high in extraversion on a personality test is the life of the party. They enjoy being with people, participating in social gatherings, and are full of energy.

# Oscore: Oscore is NEO-FFI-R Openness to experience. Openness is one of the five personality traits of the Big Five personality theory. It indicates how open-minded a person is.
# A person with a high level of openness to experience in a personality test enjoys trying new things. They are imaginative, curious, and open-minded. Individuals who are low in openness to experience would rather not try new things.
# They are close-minded, literal and enjoy having a routine.

# Ascore: Ascore is NEO-FFI-R Agreeableness. Agreeableness is one of the five personality traits of the Big Five personality theory. A person with a high level of agreeableness in a personality test is usually warm, friendly, and tactful. 
# They generally have an optimistic view of human nature and get along well with others.

# Cscore: Cscore is NEO-FFI-R Conscientiousness. Conscientiousness is one of the five personality traits of the Big Five personality theory. A person scoring high in conscientiousness usually has a high level of self-discipline. 
# These individuals prefer to follow a plan, rather than act spontaneously. Their methodic planning and perseverance usually makes them highly successful in their chosen occupation.

# Impulsive: Impulsive is impulsiveness measured by BIS-11. In psychology, impulsivity (or impulsiveness) is a tendency to act on a whim, displaying behavior characterized by little or no forethought, reflection, or consideration 
# of the consequences. If you describe someone as impulsive, you mean that they do things suddenly without thinking about them carefully first.

# Sensation: SS is sensation seeing measured by ImpSS. Sensation is input about the physical world obtained by our sensory receptors, and perception is the process by which the brain selects, organizes, and interprets these sensations. 
# In other words, senses are the physiological basis of perception.

# The remaining columns are divided into 7 classes:
# CL0	Never Used (drugs)
# CL1	Used over a Decade Ago
# CL2	Used in Last Decade
# CL3	Used in Last Year
# CL4	Used in Last Month
# CL5	Used in Last Week
# CL6	Used in Last Day


# importiamo i dati:
library(readr)
drug_consumption <- read_csv("Lab Rstudio Inferenza/Progetto/drug_consumption.csv")
DC <- drug_consumption

# applichiamo i seguenti cambiamenti al dataset: 
# - rimuoviamo la prima colonna "ID";
# - trasformiamo i valori della colonna Gender in variabili binarie: 1 <- female, 0 <- male;
# - riportiamo i valori di "Country" ed "Ethnicity" a variabili categoriche, poichè, apparentemente, non risulta esserci
#   nessuna assunzione di ordinalità nella quantificazione.

DC = DC[, -1]

DC$Gender <- ifelse(DC$Gender > 0, 1, 0)  # 1 <- Female, 0 <- Male

DC$Country <- factor(DC$Country, 
                         levels = c(-0.57009, 0.96082, 0.24923, -0.09765, 
                                    0.21128, -0.46841, -0.28519),
                         labels = c("USA", "UK", "Canada", "Australia", 
                                    "Ireland", "New Zealand", "Other"))

DC$Ethnicity <- factor(DC$Ethnicity,
                           levels = c(-0.31685, -0.50212, -1.10702, 0.12600,
                                      -0.22166, 1.90725, 0.11440),
                           labels = c("White", "Asian", "Black", "Mixed-White/Asian",
                                      "Mixed-White/Black", "Mixed-Black/Asian", "Other"))
View(DC)
head(DC)
dim(DC)
summary(DC)
str(DC)

# Osserviamo che non ci sono valori mancanti
sum(is.na(DC))
print(sapply(DC,function(x) any(is.na(x))))


