# Notes about coding for results analysis



## Introduction

This project is developed to generate visualization graphs displaying answers from participants in studies. The studies will have different total number of HITs generated, but will follow the same structure. 

The answers of the studies are first generated as csv files from Qualtrics, which is the platform on which we run our experiments.



## How to use

The first thing to do is to remove the header lines from the csv file produced by Qualtrics. The line to keep has the question indexed with a Q preceding their index, e.g. Q15.

- Make sure that inside the dataTransform.js file the csvFilePath links to the data you wish to use in the graphs.
- Run in the cmd the command: *node dataTransform.js*
- The resulting csv file in the *data/transformed* folder is written accordingly to  the date, with *survey_precise-study_* followed by a number.
- Inside the *Confidence_Interval.r* code, update the line that sets the working directory accordingly (e.g. *setwd("C:/Users/Kevin/Dropbox/Courses/PhD documents/R_studyResultsAnalysis")*).
- Inside *Confidence_Interval.r*, make sure that the variable d is updated accordingly to the file you generated (e.g. *d <- read.table(file="data/transformed/survey_precise-study_1620402561191.csv", TRUE, ",")*)
- Run the entire file to display the error bars (more graphs in progress)



## Technologies used

The technologies that have to be installed are: Node.js and R. We run the code on RStudio, but that choice is up to the user.



## Columns name and their meaning

| ResponseId          | The unique answer id                                         |
| ------------------- | ------------------------------------------------------------ |
| Progress            | How far in the study that question was recorded              |
| RecordedDate        | Date and time when the data is recorded                      |
| StartDate           | Date and time for the beginning of the question              |
| EndDate             | Date and time for the ending of the question                 |
| Finished            | Binary value to indicate if study finished                   |
| Duration_in_seconds | Difference between EndDate and StartDate                     |
| filename            | Name of the file of the stimuli                              |
| idc                 | id for the data combination                                  |
| drawnQn             | Quantitative attribute drawn in the stimuli                  |
| drawnQl             | Qualitative attribute drawn in the stimuli                   |
| queryString         | The query from which results the mask applied over the visualization |
| flips               | Indication whether the stimuli of the map is flipped         |
| nMasks              | Number of changes of status of mask being on or off          |
| dMask               | Difficulty categorization for the mask                       |
| dComplex_Qn         | Difficulty categorization for the quantitative attribute     |
| dComplex_Ql         | Difficulty categorization for the qualitative attribute      |
| dComplex_Where      | Difficulty categorization for the trajectory                 |
| focus               | Focus of the question: whether the                           |
| bslnA1              | Numerical correct value for question A1                      |
| bslnA2              | Numerical correct value for question A2                      |
| bslnA3              | Numerical correct value for question A3                      |
| bslnB               | Categorical correct value for question B                     |
| t                   | Time in seconds to click on the submit on this one page (the 4 questions and their trust) |
| cntrQ               | Question counter                                             |
| dComplex_focus      | Difficulty of the data which is focused on and necessary to answer the question |
| answerA1            | Participant answer for question A1                           |
| answerA2            | Participant answer for question A2                           |
| answerA3            | Participant answer for question A3                           |
| trustA1             | Participant self-assessed confidence in their answer for question A1 |
| trustA2             | Participant self-assessed confidence in their answer for question A2 |
| trustA3             | Participant self-assessed confidence in their answer for question A3 |
| answerB             | Participant answer for question B                            |
| trustB              | Participant self-assessed confidence in their answer for question B |
| diffA1              | Difference between the answer given for question A1 and the baseline |
| diffA2              | Difference between the answer given for question A2 and the baseline |
| diffA3              | Difference between the answer given for question A3 and the baseline |
| correctB            | Difference between the answer given for question B and the baseline |



## Inspirations concerning graphs to display the results 

Our main inspirations considering the questions asked and how to analyze them are Heer et al [1] and Pena-Araya et al. [2].  

Both Heer et al. and Pena-Araya et al. display error bars to analyze and display numerical values.

Additionally, Pena-Araya et al. use stacked bar charts to analyze and display categorical values.



## Categories and drafts of graphs to produce

The categories we aim to compare are the same no matter the question. The following graphs indicate them. The difficulty is aggregated accordingly to the focus and mask difficulties result in more categories. We should have them aligned to assess if the changes are significant. 

![image-20210716225344637](C:\Users\Kevin\AppData\Roaming\Typora\typora-user-images\image-20210716225344637.png)



We also aim to look at the stacked bar charts colored according to self-reported confidence in their answers for the same categories.

![image-20210717005216096](C:\Users\Kevin\AppData\Roaming\Typora\typora-user-images\image-20210717005216096.png)


## References



[1] Heer, J. and Bostock, M. (2010). Crowdsourcing graphical perception: using mechanical turk to assessvisualization design.  InProceedings of the SIGCHI conference on human factors in computing systems,pages 203–212

[2] Peña-Araya, V., Pietriga, E., and Bezerianos, A. (2019). A comparison of visualizations for identifyingcorrelation over space and time.IEEE transactions on visualization and computer graphics, 26(1):375–385
