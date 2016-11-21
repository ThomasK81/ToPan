#### 0. Preliminary Notes

ToPān is made to be shared and used. That is why I modularised ToPān in such a way that in each step you could ingest your own data. It works best however, if you work your way from left to right: from "Data Input" to "LDA Tables". ToPān works best with files that are structured according to the <a href="http://cite-architecture.github.io" target="_blank">CTS/CITE architecture</a>. **However, you can also ignore all this technical stuff and just use a 2-column CSV file with the identifiers in the first column and the corresponding passages in the second.** I have written up some brief instructions to get you started and I recommend reading them. However, if you feel confident then work by trial and error. The worst thing that could happen is that you send a lot of server requests to the good people at Perseids and thus have to wait a long time until everything runs through.

*NB: Currently ToPān has a download function for .82XF-files which is an exchange format for OHCO2 compliant texts. However, it will save CSV-files and R-binaries for each corpus in "./ToPan/www/".*

