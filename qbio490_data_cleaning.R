#install.packages('readr')
library(readr)
library(ggplot2)
library(grid)
library(gridExtra)
library (dplyr) 

postSem <- read_tsv('/Users/kaylaxu/Desktop/QBIO490_paper/post_semester_survey.tsv')
postSem_encoded <- postSem[, c("ID", "Year", "Semester", 
                               "BEFORE taking this class, which of the following did you feel comfortable with?",
                               "AFTER finishing this course, which of the following skills do you feel comfortable with?",
                               "AFTER finishing this course, which of the following skills do you feel least comfortable with?",
                               "BEFORE taking this class, were you in a lab?",
                               "If you answered No above, how prepared did you feel about joining a lab BEFORE this class?",
                               "AFTER completing this course, are you in a lab now?",
                               "How prepared do you feel about joining a lab AFTER this class?")]

postSem_text <- postSem[, !(colnames(postSem) %in% colnames(postSem_encoded))]
postSem_text$ID <- postSem$ID
postSem_text$Year <- postSem$Year
postSem_text$Semester <- postSem$Semester


# encode values for postSem_encoded
skills <- c('Command Line / GitHub', 
            'Basic R (i.e. syntax, documentation, etc.)',
            'Scientific R (i.e. SummarizedExperiments, MAF files, plotting, maftools, DESeq2, etc.)',
            'Basic Python (i.e. syntax, documentation, etc.)',
            'Scientific Python (i.e. numpy, pandas, matplotlib, seaborn, etc.)',
            'Machine Learning (sklearn, classification, dimensionality reduction, etc.)',
            'Presentation Skills',
            'Reading and Comprehending Scientific Literature')

# BEFORE class skills
for (i in 1:length(skills)) {
  newCol <- ifelse(grepl(skills[i], postSem_encoded$`BEFORE taking this class, which of the following did you feel comfortable with?`, fixed = TRUE), 1,0 )
  postSem_encoded <- cbind(postSem_encoded, newCol)
  colnames(postSem_encoded)[ncol(postSem_encoded)] <- paste0('before_', i)
}



# AFTER class skills
for (i in 1:length(skills)) {
  newCol <- ifelse(grepl(skills[i], postSem_encoded$`AFTER finishing this course, which of the following skills do you feel comfortable with?`, fixed = TRUE), 1,0 )
  postSem_encoded <- cbind(postSem_encoded, newCol)
  colnames(postSem_encoded)[ncol(postSem_encoded)] <- paste0('after_', i)
}

# AFTER class least comfortable
for (i in 1:length(skills)) {
  newCol <- ifelse(grepl(skills[i], postSem_encoded$`AFTER finishing this course, which of the following skills do you feel least comfortable with?`, fixed = TRUE), 1,0 )
  postSem_encoded <- cbind(postSem_encoded, newCol)
  colnames(postSem_encoded)[ncol(postSem_encoded)] <- paste0('after_least_', i)
}

write.table(postSem_encoded, file = '/Users/kaylaxu/Desktop/QBIO490_paper/postSem_encoded.tsv', sep = '\t', row.names = FALSE)


#### want out of class ########
specifics <- c("General Knowledge", "Learn R", "Learn Python", "Machine learning skills", "Learn Github", '',
               "General Experience", "Application of skills to real biological data", "Find Research Opportunities", "Collaborative Work", '',
            "Reading and Writing Scientific Papers", "Presentating research", '',
            "Explore bioinformatics field", "Get to know other students in degree program")
cat <- c("Computational Skills", '', '', '', '', '',
         "Research Experience", '','', '','',
         "Scientific Communication", '', '',
         "Community Building", '')
counts <- signif(c(35/83, 23/83, 19/83, 4/83, 3/83,  0, 43/83,  21/83, 9/83,  3/83, 0, 5/83, 1/83, 0, 11/83, 9/83) * 100, digits=3)
  

table_data <- as.data.frame(cbind(cat, specifics, counts))
grid.table(table_data , rows=NULL, cols = c('Category', 'Specific Outcomes', '% Student Responses'))




#### Alumni: Impact of PTP Learning ########
specifics <- c("Made content more approchable", "Improved student-instructor communication", "Liked the instructors",  "Greater sense of student community", "Instructor are more understanding", "Improved confidence with material",
               '', "Lack of instructor experience", "Student put less effort in class")
cat <- c("Positive", "", "", "", "", "", '',
         "Negative", "")
perc <- signif(c(6/13, 3/13, 2/13, 2/13, 1/13, 1/13, 0,
        2/13, 1/13) *100, digits = 3)

table_data <- as.data.frame(cbind(cat, specifics, perc))
grid.table(table_data , rows=NULL, cols = c('Impact of PTP', 'Alumni Response', '% Alumni Responses'))



### Students: Valuable aspects of class ######
specifics <- c('Bioinformatic Coding Skills',
               'Research Experience w/ Real World Data', '',
               
               'Understanding Scientific Writing',
               'Presentation Skills', '',
               
               'Building Student Community',
               'PTP Learning',
               'Supportive Environment')

cat <- c('Computational Skills and Research Experience', '','',
         'Scientific Communication', '','',
         'Community Building', '', '')


counts <- c(41.5,
            32.1, '',
            
            9.43,
            5.66, '',
            
            16.9,
            7.55,
            5.66)


table_data <- as.data.frame(cbind(cat, specifics, counts))

grid.table(table_data, rows=NULL, cols=c('Category','Most Valuable Aspect of QBIO490', "% Student Responses"))



#### Alumni: Impact of QBIO490 on Bioinformatics Career Outlook ####

outcome <- c('Interest in pursing bioinformatics',
             'Improved perception of bioinformatics',
             'Exposure to careers related to bioinformatics',
             'Interest in data analysis',
             'Improved ability to find research',
             'Pursuing a computational biology PhD', '',
             'Prefer medicine over research',
             'Prefered CS to biology',
             'No interest in data analysis')

impression <- c("Positive", "",'','','','','',
               "Negative", "",'')

count <- signif(c(6/13, 4/13,  4/13, 3/13, 2/13, 1/13, 0,5/13, 1/13, 1/13) *100, digits = 3)


table_data <- as.data.frame(cbind(impression, outcome, count))
grid.table(table_data, rows=NULL, cols=c("Impact of QBIO490", "Outcome", "% Alumni Responses"))

















