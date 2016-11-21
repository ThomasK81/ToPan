#### 2. Morphological Normalisation

Morphological normalisation is mainly necessary for morphologically complex languages. If you have an English corpus you can skip this step.

ToPān lets you morphologically normalise your data. For this it uses Perseid's Morphological Service API (a Morpheus implementation for Latin and Greek and newer more experimental parser for Arabic and Persian). Perseid's Morphological Service API only allows you to query one word per request (which is a lot of requests). This crawls unfortunately. You can also use a local Stemming Dictionary which is a CSV file with the word in the first column and the lemmata seperated by a semicolon in the second column.

*NB: ToPān will create a Stemming Dictionary if you select Morpheus API as your parser, so you do not have to go to lunch break for a second time...*

**TreeBanking Files:** If you have selected TreeBank XML to ingest the corpus data. A parsed version of the corpus has already been created during the import and you can go straight to topic modelling.