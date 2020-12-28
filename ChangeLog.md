# Revision history for phonetic-languages-ukrainian

## 0.1.0.0 -- 2020-09-07

* First version. Released on an unsuspecting world.

## 0.2.0.0 -- 2020-09-09

* Second version. Added new functions aux4 and aux0 to be used after convertToProperUkrainian from the Melodics.Ukrainian module from the mmsyn6ukr package.

## 0.2.1.0 -- 2020-09-25

* Second version revised A. Changed the behaviour of the Languages.Phonetic.Ukrainian.PrepareText.splitLines function so that it splits the lines of the arbitrary length (in words) for the further analysis
by the uniqueness-periods-vector series of programs. This is needed for non-poetic texts processment.

## 0.2.2.0 -- 2020-09-25

* Second version revised B. Fixed issue with Languages.Phonetic.Ukrainian.PrepareText.splitLines functions in division of the substrings by their length not by their words count (as is required).

## 0.2.3.0 -- 2020-10-06

* Second version revised C. Fixed issues with some functions.

## 0.3.0.0 -- 2020-11-28

* Third version. Added new generalized functions splitLinesN and prepareTextN.
