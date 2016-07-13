#### 4. Understanding the Results

##### LDAvis

Find the documentation to LDAvis <a href="https://github.com/cpsievert/LDAvis" target="_blank">here</a>.

##### LDA Tables

The phi-table is the score of each word token for each topic. If you combine it with n-gram searches, you can use it, for instance, to detect multi-word compounds and then re-run the topic-modelling with the multi-word compounds as single tokens. Alternatively you could train other search&link based supervised topic detection methods with the scores.

The theta-table is the score of each document for each topic. Use it for text-reuse or, for instance, to find similar passages for students when <a href="https://thomask81.shinyapps.io/sightreading_app/" target="_blank">teaching Ancient Greek</a>.