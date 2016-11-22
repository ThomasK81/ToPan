##### (Meletē)ToPān

The name (Meletē)ToPān is based on the Greek principle μελέτη τὸ πᾶν, which roughly translates as "take everything into consideration". Topic modelling helps select the interesting bits from a large corpus of texts while technically having processed every document.

ToPān is topic modelling for everyone: from people without programming knowledge to people who want to build teaching and text-reuse tools and apps based on topic modelling data without having to develop their own tool or having to majorly restructure their textual data. ToPān is made to be shared and used. This is why I have modularised ToPān in such a way that in each step you can ingest your own data. It works best however, if you work your way from left to right: from "Data Input" to "LDA Tables" (please find more details under "Instructions"). ToPān works best with files that are structured according to the <a href="http://cite-architecture.github.io" target="_blank">CTS/CITE architecture</a>.

ToPān is still under active development: this is an alpha release. More features will be added and you are encouraged to roadtest the app. Please send me feedback or report bugs.

*Technical Note: ToPān is written in RShiny and uses the following R-libraries: shiny, RCurl (also I mainly switched to httr, because of RCurl's mysterious errors and performance issues when sending lots and lots of requests), XML (but also install xml2), httr (and curl), lda, LDAvis, servr. If you want to install a local version of ToPān, simply clone the  <a href="https://github.com/ThomasK81/ToPan" target="_blank">repo</a>, install R, RStudio, and the mentioned libraries.*

##### About Me

I'm a traditional Classicist turned Digital Philologist: after completing a traditional PhD in Classics at the University of Otago, New Zealand, I am now Assistant Professor at the Alexander-von-Humboldt Chair for Digital Humanities in the Institute for Computer Science at the University of Leipzig. Please find more information <a href="http://www.dh.uni-leipzig.de/wo/team/thomas-koentges/" target="_blank">here</a>.
