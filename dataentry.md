#### 1. Data Entry

In ToPān you have different methods to read in data. Although you will need an internet connection to pull-in all references to available texts at the Perseids Project's repository and for the DNZ API query, the speediest text inputs are local methods: either you install CAPITainS, or you prepare a CSV / TSV text file that contains at least two columns mapping identifiers to logical chunks of text of your corpus (currently ToPan works best with the CTS architecture), or third. A fourth accepted local file format is the new (and experimental) <a href="http://neelsmith.github.io/2016/07/11/textformats/" target="_blank">OHCO2 Exchange Format (.82X)</a>.

##### CTS API Based Data Input

*CTS AP...What?!?* CTS stands for Canonical Text Services and is a way to divide your way into logical, citable, and ordered chunks of text (one could say the CTS strategy is to find the logical structure of a text and use it to make this text citable). You can find some more information about it 
<a href="http://cite-architecture.github.io" target="_blank">here</a>.

The <a href="http://www.perseids.org" target="_blank">Perseids Project</a> at Tufts University and the <a href="http://www.dh.uni-leipzig.de" target="_blank">Open Philology Project</a> at Leipzig University pulled some resources together to develop a service that one can use to retrieve CTS (and Epidoc XML) compliant text documents from their server. Pretty much everything we develop at OPP or Perseids is open source and you can access our stuff through our github repositories. Our CTS API is called CAPITainS. Two capitalised groups of letters (CTS and API) have been merged by the humor of the main developer of this API (Thibault Clerice).

When ToPān starts up, it sends a "GetCapabilities"-request to the Perseids server. So you are always up to date with the CTS data the team at Leipzig and Tufts add to the repository. You then can directly select the text using the dropdown-menu and the CTS URN of the text you want to retrieve and ToPān will fetch the text for you and convert to an R-binary (which you can later export in a format more meaningful to you).

![Image of Perseids CTS API Data Input](/ScreenShots_ToPan/CTSAPIDataInput.png)

However, CAPITainS has been made available offline to. You can download and install a <a href="https://www.docker.com/products/docker-toolbox" target="_blank">Docker</a> container and follow this <a href="https://www.youtube.com/watch?v=_Vmwz_761GM" target="_blank">instruction video</a> to set it up. Under "CAPITainS Data Input" you just have to type in your local address for the API and the URN of the text you'd like to retrieve. While this may sound technical, it will be much more comfortable in the long run (also for Perseid's server.)

![Image of Local CTS API Data Input](/ScreenShots_ToPan/CAPITainSDataInput.png)

##### DNZ API Based Data Input

This one is the odd one out and it is only a matter of time until I fork it out to its only project. You can query and topicmodel cultural heritage description meta-data of the preponderance of New Zealand's cultural heritage institutions using the <a href="http://www.digitalnz.org/developers/api-docs-v3" target="_blank">Digital NZ API</a>. This work is based on previous work on the Alexander Turnbull Library's cartoon metadata and Index NZ's (INNZ) abstracts of medium to large articles published in New Zealand. You can read more about it <a href="http://ala.sagepub.com/content/early/2016/05/10/0955749016647821.full.pdf+html" target="_blank">here</a>.

*NB: A collection of abstracts and descriptions of DNZ metadata is technically not a corpus compliant with OHCO2. However, Topic-Modelling works also with non-CTS-compliant and non-OHCO2-compliant texts (mainly because it treats each abstracts as its own document and because the word order does not matter in simple LDA topic-modelling).*

*NB2: I have currently disabled the DNZ API method. But it will be back online soon. In the meantime you can experiment with the old version from my <a href="https://github.com/ThomasK81/TopicModellingR" target="_blank">Github account</a>.*

![Image of DNZ API Based Data Input](/ScreenShots_ToPan/DNZAPIDataInput.png)

##### CSV Based Data Input

**To upload data using CSV is probably the easiest way for topic modelling text not in any of the repositories.** All you need is a simple text table where the different columns are either separated by a ",", a ";", or a "	" (Tab). The file has to have at least two columns: the first columns are the identifiers of the data and the second column are the passages. All other columns are up to you. You can use a header or not and you may use quotation marks to mark-up cell content (but this is optional). Technically, you are free to choose your identifiers as you please. **However**, you are strongly encouraged to use CTS compliant identifiers: the R-binaries that are created and all files that are exported will be named assuming that the identifier in the first cell of your data is a CTS identifier: you do not want to end up with the filename: NA.rds. ;)
Everything else is easy. Just select the format of your text table, select the file, and press submit to upload to ToPān. My colleague, Maxim Romanov, attempted directly (and successfully) to break the code by uploading a large CSV file (2MB). ToPān will be alright now with uploads up to 30MB (Thanks, Maxim for finding this bug!). 

![Image of CSV Data Input](/ScreenShots_ToPan/CSVDataInput.png)

##### TreeBankXML Based Data Input

A very handy way to upload data are TreeBank XML files (ideally produced with Arethusa at the <a href="http://sites.tufts.edu/perseids/instructions/treebanking-instructions/" target="_blank">Perseid's Collaborative Editing Environment</a>). The big advantage is that instead of having to make our best guess about a lemma of a word, we can use verified data. The disadvantage is that the preponderance of Classical text is not yet treebanked (although it is more and more used in teaching). Hand-treebanked sentences provide usually very reliable information. Because of this, no extra morphological normalisation is needed. When you upload your Treebank XML file a text binary and a parsed text binary are created at the same time. Get treebanking, folks! 

![Image of CSV Data Input](/ScreenShots_ToPan/TreeBankDataInput.png)

##### OHCO2 Exchange Format

Neel Smith and I came up with a proposal about what we think to be the simplest way to share OHCO2 compliant texts with each other. Find here some more information about the <a href="http://neelsmith.github.io/2016/07/11/textformats/" target="_blank">OHCO2 Exchange Format (.82X)</a>. 
82X is basically just a table consisting of 5-columns: 1. Identfier, 2. PrevIndex, 3. Index, 4. NextIndex, 5. Text. While you can choose whatever format you want to express this .82X is text table with no header with the delimiter being a "#". Currently ToPān only understands "plain" text in 5, but it will be an additional feature that ToPān will also be able to handle Markdown and XML.

![Image of CSV Data Input](/ScreenShots_ToPan/82XDataInput.png)

##### Success

Once you have successfully ingested a text, you will see a table like this. What you do not see is that it also created an R binary of the data (.rds). If you have provided your own data CTS/CITE architecture

![Image of Imported Text](/ScreenShots_ToPan/CSVDataInput2.png)