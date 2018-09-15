# Shakespeare
Natural language processing on Shakespeare plays

The first things I've made is a [Shiny app that uses tf-idf to compare the most distinctive words in different plays](https://sahelanth.shinyapps.io/Shakespeare/). 

The second is a way of producing a rhyming dictionary of Shakespeare characters; and of matching them to elements with similar pronunciation to make a riff on [Tom Lehrer's Elements song](https://www.youtube.com/watch?v=DYW50F42ss8). The rhyming is fine, the current implementation of matching character names to rhyming element names works but not well. 

Further goals:
1. I decided to start with informal pronunciation guides since I don't know International Phonetic Alphabet, and in so doing learned why IPA is useful. The last third of	Shakespeare rhyme analysis.R is flailing attempts to match informal pronunciations to each other better.

2. Allowing users to customize character and place names in the Shiny app to refine their analysis.

3. Using the Folger Digital [XML versions](https://www.folgerdigitaltexts.org/download/xml.html) rather than [TXT](https://www.folgerdigitaltexts.org/download/txt/FolgerDigitalTexts_TXT_Complete.zip) versions, to allow analyzing features like speaker, stage directions, and act and scene boundaries.
