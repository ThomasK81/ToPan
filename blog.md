## ToPān in Action

_ _ _ _ _

### May 2017

_ _ _ _ _

#### ToPān Workshop at the University of Iowa

I am looking forward to talking about ToPān and Topic Modelling at this workshop at the University of Iowa. I'm thrilled to know that it was fully booked within two days of advertisement:

![Iowa ToPān Workshop](http://drive.google.com/uc?export=view&id=0BzNW0LZy0RUOZndoZDh4RjVURzg)

_ _ _ _ _

### April 2017

_ _ _ _ _

#### Original Undergraduate Research

The Classics department of the College of the Holy Cross has become a veritable Fullbright factory! I am happy that those talented students have decided to use ToPān as one tool and topic modelling as one method in their research:

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">Charlie and Melody are able to model topics in scholia using to pan from <a href="https://twitter.com/ThomasKoentges">@ThomasKoentges</a> <a href="https://t.co/a5t1nkfHmc">pic.twitter.com/a5t1nkfHmc</a></p>&mdash; Neel Smith (@neelsmithhc) <a href="https://twitter.com/neelsmithhc/status/857668553692106752">April 27, 2017</a></blockquote> <script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr"><a href="https://twitter.com/HCMID">@hcmid</a> <a href="https://t.co/0S7ltC7YM3">https://t.co/0S7ltC7YM3</a> m Wauke using topic modelling from <a href="https://twitter.com/ThomasKoentges">@ThomasKoentges</a> to classify annotations to Homer <a href="https://t.co/8SguHE71Q6">pic.twitter.com/8SguHE71Q6</a></p>&mdash; Gregory Crane (@PhilologistGRC) <a href="https://twitter.com/PhilologistGRC/status/857339898319302657">April 26, 2017</a></blockquote> <script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

_ _ _ _ _

#### DARIAH-HR Digital Philology workshop in Split

Thanks to Neven Jovanovic of the University of Zagreb and [DARIAH-HR](http://dariah.hr/en/home/), I was invited to give my first longer workshop on ToPān at the University of Split. The two-day workshop also had complementary streams introducing the participants to the work of Transcribus, Chris Blackwell, and Matthias Schlögl. Günter Mühlberger of Transcribus indicated some interest in combining Transcribus results with my Topic Modelling work. Watch this space.

_ _ _ _ _

#### Version 0.3: Romulus

Just before the workshop in Split, I finished my work on version 0.3. I named it [Romulus](https://zenodo.org/record/556708#.WRGT8rF7ElI), because it was released on April 21 and hopefully it is also a foundation for further improvement. You can cite it like this:

Thomas Köntges. (2017). ThomasK81/ToPan: Romulus. Zenodo. http://doi.org/10.5281/zenodo.556708

_ _ _ _ _

### March 2017

_ _ _ _ _

#### Visit to Mount Allison University, Canada & Visualisations with Tableau Public

I gave a talk at Mount Allison University titled [A NEEDLE IN THE HAYSTACK: FINDING THE INTERESTING BITS IN 30 MILLION WORDS OF LATIN](https://drive.google.com/file/d/0BzNW0LZy0RUOODFXbFZqRGV6S2M/view?usp=sharing). I think the talk serves well as a summary of the advantages and opportunities offered by topic modelling and ToPān.

Instead of using my go-to R-libraries for visualisations, I decided to use other open tools to prove that really anyone can use topic-modelling results to communicate research findings. Unfortunately, my topic-document matrix was too big for GoogleFusion or any traditional spreadsheet GUIs. So instead I started to experiment with [Tableau Public](https://public.tableau.com/profile/thomas.koentges#!/), which has no problem generating a bunch of generic visualisations from large tables. Here are some examples:

It is very obvious for Latin scholars that although Livy and Virgil wrote about similar topics, they used different vocabulary.

![Comparison Livy vs. Virgil](http://drive.google.com/uc?export=view&id=0BzNW0LZy0RUOUkxOSEVRSUd1VUU)

This trend is observable throughout all Latin historical and poetic works.

![Comparison History vs. Epic](http://drive.google.com/uc?export=view&id=0BzNW0LZy0RUOMTgwaVVkZTJJT2M)

While this is certainly not surprising for scholars, the interesting fact is that the machine did not need training in Latin literature or any prior annotations to detect this difference. That said, while we can use our knowledge of well-known texts to evaluate computational results, once evaluated, the same results become interesting for less-well-studied texts, e.g the Latin panegyrics.

![Comparison History vs. Panegyric](http://drive.google.com/uc?export=view&id=0BzNW0LZy0RUOVE5wVHdZRTZMUGc)

_ _ _ _ _

#### Version 0.2: Easy Kiwi

I used Github and Zenodo to publish the 0.2 release of ToPān. It has a DOI and is archived securely. I named it [Easy Kiwi](https://zenodo.org/record/400253) because its ability to fly is still fairly limited. You can cite it like this:

Thomas Köntges. (2017). ThomasK81/ToPan: EasyKiwi. Zenodo. http://doi.org/10.5281/zenodo.400253

_ _ _ _ _

### February 2017

_ _ _ _ _

#### Initial results topic modelling 30 million words of Latin

Paul Dilley and his team at the University of Iowa contacted me last year to work on topic modelling their huge corpus that has been built by trying to combine all openly available Latin literature on the web. A morphologoically normalised version was produced by Patrick Burns. I am working with both data-sets. While we are currently working on publishing more serious research output, here is a [TSNE](https://drive.google.com/file/d/0BzNW0LZy0RUON0ZNS2t6dVVOaGs/view?usp=sharing) of the corpus using LDA with k = 100.

![TSNE coloured by century](http://drive.google.com/uc?export=view&id=0BzNW0LZy0RUOc3BjZS00dlA1X0U)

In this accessible visualisation, initial results also seem to indicate that Caesar is boring (just joking!!).

![Caesar vs. Petronius](http://drive.google.com/uc?export=view&id=0BzNW0LZy0RUObHBMRUYtS0VGems)

_ _ _ _ _

### October 2016

_ _ _ _ _

#### There might be little work on ToPān this winter.

Our new B.Sc. in Digital Humanities took off this winter. I will be teaching the mandatory module Introduction to DH and we have a lot more students than expected!

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">DH people: The new B.Sc. DH in Leipzig will have &gt;100 students enrolled. Could anyone point me to numbers from other unis? Thanks!</p>&mdash; Thomas Koentges (@ThomasKoentges) <a href="https://twitter.com/ThomasKoentges/status/783678040664670208">October 5, 2016</a></blockquote> <script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

_ _ _ _ _

### September 2016

_ _ _ _ _

#### Topic Modelling the Patrologia Graeca with David Mimno

During this two-day collaboration I certainly had the chance to learn a lot. While the master performed the surgery, I only did some stitching up. We used (messy) OCR of the multilingual Patrologia Graeca. I wrote a short script that separated Greek from Latin lines and from the critical apparatus. We developed some criteria for version selection and pushed the topic modelling to David's cluster. The results can be seen [here](https://mimno.infosci.cornell.edu/patgrec/volumes.html) and [here](https://mimno.infosci.cornell.edu/patgrec/).

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr"><a href="https://twitter.com/BabelAlexandria">@BabelAlexandria</a> Close! Latin translations of PG.</p>&mdash; David Mimno (@dmimno) <a href="https://twitter.com/dmimno/status/776453316247158784">September 15, 2016</a></blockquote> <script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

_ _ _ _ _

#### Holy Cross visit

I [visited Holy Cross](http://classics.me.holycross.edu/2016/10/04/thomas-kontges-presents-on-petronian-puzzles-and-possibilities/) this week to give an old-fashioned talk on Petronius' *Satyrica*, but also to give an introduction to topic modelling in Neel Smith's distant reading class. I am looking forward to the students' application of ToPān.

_ _ _ _ _

### July 2016

_ _ _ _ _

#### Amphora publication

The Melbourne Historical Journal invited me to write an article on the impact of [digital methods in Classics](https://www.academia.edu/26647479/Classical_text_and_the_digital_revolution) for their Amphora Issue. Now it has been published:
<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">Bringing the <a href="https://twitter.com/hashtag/DigitalHumanities?src=hash">#DigitalHumanities</a> to the world of classical texts in our latest issue is <a href="https://twitter.com/ThomasKoentges">@ThomasKoentges</a> <a href="https://t.co/6oBoiRJ0JG">pic.twitter.com/6oBoiRJ0JG</a></p>&mdash; Amphora Issue (@AmphoraIssue) <a href="https://twitter.com/AmphoraIssue/status/757743842930012160">July 26, 2016</a></blockquote> <script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

_ _ _ _ _

#### DH2016 Krakow: ToPān v.0.1 launches

I didn't name this alpha release; but if I were to name it, it should probably be called *Buggy Bastard*. Nevertheless, I introduced people to the first version of ToPān at DH2016 Krakow and it was well received (selection of tweets):

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">ToPān (Topic-Modelling for everyone) by <a href="https://twitter.com/ThomasKoentges">@ThomasKoentges</a> <a href="https://twitter.com/hashtag/dh2016?src=hash">#dh2016</a><a href="https://t.co/vmpCYeZwcu">https://t.co/vmpCYeZwcu</a></p>&mdash; DH Group at FBK (@DH_FBK) <a href="https://twitter.com/DH_FBK/status/753499595016179712">July 14, 2016</a></blockquote> <script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">.<a href="https://twitter.com/ThomasKoentges">@thomaskoentges</a>: If you haven’t left yet, I’m a classicist talking about building a GUI. You might want to leave. <a href="https://twitter.com/hashtag/dh2016?src=hash">#dh2016</a></p>&mdash; Brian Croxall (@briancroxall) <a href="https://twitter.com/briancroxall/status/753496715995537409">July 14, 2016</a></blockquote> <script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">I have a feeling my students will get to try <a href="https://twitter.com/ThomasKoentges">@ThomasKoentges</a> topic modelling tool in a couple of months… <a href="https://t.co/OpEjPbu4Xu">https://t.co/OpEjPbu4Xu</a> <a href="https://twitter.com/hashtag/dh2016?src=hash">#dh2016</a></p>&mdash; Aurélien Berra (@aurelberra) <a href="https://twitter.com/aurelberra/status/753500593809920000">July 14, 2016</a></blockquote> <script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr"><a href="https://twitter.com/ThomasKoentges">@ThomasKoentges</a> work on GUI for multilingual topic modelling is deeply interesting. Did anyone catch the url? <a href="https://twitter.com/hashtag/A51?src=hash">#A51</a> <a href="https://twitter.com/hashtag/DH2016?src=hash">#DH2016</a></p>&mdash; Purdom Lindblad (@Purdom_L) <a href="https://twitter.com/Purdom_L/status/753499763153203204">July 14, 2016</a></blockquote> <script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

_ _ _ _ _

#### DH2016 Krakow: 82X is born (a few hours later renamed to 82XF)

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">Looking forward to <a href="https://twitter.com/ThomasKoentges">@ThomasKoentges</a> this morning and seeing <a href="https://twitter.com/hashtag/82XF?src=hash">#82XF</a> support in his shiny topic modelling app! <a href="https://twitter.com/hashtag/dh2016?src=hash">#dh2016</a></p>&mdash; Neel Smith (@neelsmithhc) <a href="https://twitter.com/neelsmithhc/status/753480579555917824">July 14, 2016</a></blockquote> <script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr"><a href="https://twitter.com/ThomasKoentges">@ThomasKoentges</a> proposes 82X for &quot;OHCO2 eXchange&quot;! Cleverly succinct.</p>&mdash; Neel Smith (@neelsmithhc) <a href="https://twitter.com/neelsmithhc/status/752447996051193856">July 11, 2016</a></blockquote> <script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr"><a href="https://twitter.com/neelsmithhc">@neelsmithhc</a> <a href="https://twitter.com/CWBlackwell">@CWBlackwell</a> I cannot take credit for what is build on many conversations with you. But I happily take credit for the name: 82X</p>&mdash; Thomas Koentges (@ThomasKoentges) <a href="https://twitter.com/ThomasKoentges/status/752542003284602880">July 11, 2016</a></blockquote> <script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

_ _ _ _ _

### February–April 2016

_ _ _ _ _

#### Topic Modelling Medieval Persian

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">Working with <a href="https://twitter.com/ThomasKoentges">@ThomasKoentges</a> on topic modelling medieval Persian poetry today <a href="https://twitter.com/roshaninstitute">@roshaninstitute</a> <a href="https://twitter.com/PersDig_UMD">@PersDig_UMD</a>! <a href="https://twitter.com/hashtag/topicmodelling?src=hash">#topicmodelling</a> <a href="https://twitter.com/hashtag/PersoArabicDH?src=hash">#PersoArabicDH</a></p>&mdash; M.T.Millerم.ت.میلر (@M_T_Miller) <a href="https://twitter.com/M_T_Miller/status/705755399081873412">March 4, 2016</a></blockquote> <script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

_ _ _ _ _

#### Teaching App used at Tufts University

I used topic modelling results to build an Ancient Greek [teaching app](https://thomask81.shinyapps.io/sightreading_app/) that was used in teaching Thucydides at Tufts University. The App looks for sentences similar to those already read by students. Unknown vocabulary is linked to a dictionary at the [Perseus Digital Library](http://www.perseus.tufts.edu/hopper/).

![SightReading App](http://drive.google.com/uc?export=view&id=0BzNW0LZy0RUOang2WGxYV0NPb0E)

_ _ _ _ _

### The Beginnings 2014–2015
_ _ _ _ _

The beginnings of *ToPān* are rooted in the many [INNZ topic models](http://thomask81.github.io/INNZ_tm_vis/index.html#topic=5&amp;lambda=0.6&amp;term=) I have built and visualised with Sievert's [LDAvis package](https://cran.r-project.org/web/packages/LDAvis/index.html) and [shinyapps](https://thomask81.shinyapps.io/TopicModellingBrowser/). From there I have applied those skills to Latin, [Ancient Greek](http://thomask81.github.io/Greek_vis/index.html#topic=4&amp;lambda=0.58&amp;term=), and [translations](http://thomask81.github.io/GreekTrans_vis/index.html#topic=4&amp;lambda=0.58&amp;term=). After interest from multiple scholars dealing with historical languages, I summarised some early observations in a [blog](http://www.dh.uni-leipzig.de/wo/topic-modelling-of-historical-languages-in-r/) to enable others to employ similar methods. It also became clear that something more intuitive was wanted.

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">Great post by <a href="https://twitter.com/ThomasKoentges">@ThomasKoentges</a> on &quot;Topic Modelling Historical Languages in R&quot; <a href="https://t.co/joBhrcPptw">https://t.co/joBhrcPptw</a> <a href="https://twitter.com/hashtag/PersoArabicDH?src=hash">#PersoArabicDH</a> <a href="https://t.co/jA6lfosctW">pic.twitter.com/jA6lfosctW</a></p>&mdash; M.T.Millerم.ت.میلر (@M_T_Miller) <a href="https://twitter.com/M_T_Miller/status/699224808354000896">February 15, 2016</a></blockquote> <script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr"><a href="https://twitter.com/M_T_Miller">@M_T_Miller</a> <a href="https://twitter.com/ThomasKoentges">@ThomasKoentges</a> Knowing Thomas, he&#39;ll be happy to see his code working for yet another language!</p>&mdash; Maxim Romanov (@maximromanov) <a href="https://twitter.com/maximromanov/status/655068913320001536">October 16, 2015</a></blockquote> <script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

_ _ _ _ _
