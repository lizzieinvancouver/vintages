Started 9 December 2014
By Lizzie, with great help from Jehane!

See also: cleanataglance.R

This readme discusses my work getting together a wine quality star rating for (initially at least) Ben’s project on winegrape harvest records.

The source is the book “Michael Broadbent's Vintage Wine” and the steps were:

(1) Sally Gee (SG) and Harold Eyster (HE) both entered all the ‘At a glance’ data — these are wines with 3, 4 or 5 stars (good/very good/excellent).

(2) I worked (am working!) on cleaning there data so it agrees (see cleanataglance.R)

(3) I realized we really need the 0, 1, 2 data (and to know when there is no data from the book) so I asked Jehane to transcribe it on 8-9 December 2014 for Alsace, Champagne, Bordeaux and Burgundy. We had some issues because I initially lumped white and red (for Bordeaux and Burgundy) 

(4) She sent three files: 
	(a) needstars_full.csv which think is just Bordeaux and Burgundy, broken down by color with levels 0-5 (though below she says it’s all regions)
	(b) needstars.csv which is the full file! All four regions, with the colours for B and B regions
	(c) needstarscolor.csv which is Bordeaux and Burgundy with just stars less than 3 I think.

** She had some important notes on them, see below. **




Email from Jehane (9 December 2014):

Here are the two sheets you sent, and then a sheet with the regions all
compiled onto one sheet (needstars_full.csv).
In the alsace region, there are 5 rows that I assigned a 0 based on his
notes that the year was so poor that no vintage was shipped (or is likely
to be shipped, in the case of 2000).
There were a few instances of a 1 to 2 star range, in which case I
assigned 1.5 and made a note. You may want to choose either 1 or 2 for
these years, I¹m not sure. Much of the ³at a glance² data had such
variation listed in the text, but in ³at a glance² the book would give
just one number and note ³v² for variable.
1937 red bordeaux is an odd case‹ apparently was good (4 stars) but all of
the good stuff was guzzled in the war, and the rest aged badly? So now the
vintages are 2 stars. I gave it 2 stars, because that seems consistent
with the rest of the data‹ since all of this data is reporting his modern
tasting notes on these old wines. But I¹m not sure if that is the right
choice.
For 1982 red bordeaux, I happened to notice a discrepancy between the ³at
a glance² ranking and the text ranking (3 vs. 2). This seems to be a typo
on the book¹s part, and I¹m not sure which number to take. Hopefully there
are not more discrepancies like this! I¹d like to think the writers
compiled the ³at a glance² section accurately.

Can¹t wait to see what you find! Let me know if you need anything else.