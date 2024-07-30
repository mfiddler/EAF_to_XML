# EAF_to_XML

# Overview
This script takes as input an ELAN annotation file (extension .eaf) containing a transcript of an interaction between two speakers and returns an .xml file with the tiers in interlinear format suitable for displaying on a web page. It assumes the ELAN file contains the following tiers:
 - a primary tier for Speaker A and Speaker B containing full sentences/utterances/intonation units
 - a tier with each word (a dependent of the intonation unit tier)
 - a tier with each morpheme (a dependent of the word tier)
 - a tier with a gloss for each morpheme (a dependent of the morpheme tier)
 - a tier with translation of each intonation unit (not dependent on any other tier)

# Notes
 - The script assumes that all tiers are completely filled in--e.g. there should not be any words that don't have morpheme or gloss annotations associated with them. 
 - The script was written for a transcript with two speakers and the tier structure outlined above, but it could be adapted to run on transcripts with more speakers, different numbers of tiers, etc.
