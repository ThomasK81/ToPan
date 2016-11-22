##### (Meletē)ToPān v.0.1

The name (Meletē)ToPān v.0.1 is based on the Greek principle μελέτη τὸ πᾶν which roughly translate to "take into care everything". I decided for the name because Topic-Modelling performs well on large amounts of logically structured chunks of texts and it helps selecting the interesting bits in a large corpus of text by technically having looked at everything. The butterfly in the logo is of the species Melete. The original photograph is by Didier Descouens and he has licensed it under CC BY-SA 4.0. I changed the image for the logo slightly. I'd strongly suggest to start with the <a href="https://commons.wikimedia.org/wiki/File:Melete_leucadia_MHNT_dos.jpg" target="_blank">original</a> if you want to use it, but you can also use this now slightly modified logo under CC BY-SA 4.0 license as I am required to share it under the same license as the original image.

ToPān is Topic-Modelling for everyone: from people without programming knowledge to people that want to build teaching and text-reuse tools and apps based on Topic-Modelling data without having to develop their own tool or having to majorly restructure their textual data. ToPān is made to be shared and used. That is why I tried to modularise ToPān in a way that in each step you could ingest your own data. It works best however, if you work your way from left to right: from "Data Input" to "LDA Tables" (please find more details under "Instructions"). ToPān works best with files that are structured according to the <a href="http://cite-architecture.github.io" target="_blank">CTS/CITE architecture</a>.

ToPān is also still under active development. This is an alpha release. More features will be added and you are encouraged to roadtest ToPān and send me feedback or report bugs.

*Please note: ToPān is written in RShiny and uses a number of other R libraries as well. In order to get ToPān working, please follow the instructions given below and Bob's your uncle!*

##### Requirements

Given the necessary operating system dependencies are installed, ToPān will install all R dependencies on its first run. If this fails, carefully read the output looking for missing operating system libraries. Install as required and repeat trying to run ToPān until successful.

##### Running ToPān

1. Install [R](https://www.r-project.org)
2. Optionally [RStudio](https://www.rstudio.com), which is a great help for working with R projects
3. `git clone` this repository
4. In your ToPān folder, type `./to_pan`
5. Enjoy!
