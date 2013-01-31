IdConverter
===========

Introduction
===========

Sometimes we need to hide the real id of the database to the user of a web 
application. The following implementation allow us to apply a completely random 
biyective function to a given ID to show it to the user and get it back again 
applying the inverse function.

How it works
===========

The ids of an Sql database are base 10 integers, to get a shorter id we need to 
write that number in a higher base. To accomplish that we need to define an 
alphabet representing each digit of another numeric base. If we choose base 64 
we need to map numbers 0 to 63 each one to a symbol in our new alphabet. The 
symbols in the new alphabet will be used to "create" the id in base 64. To 
obtain the corresponding id in base 64 we just convert the original base 10 id 
to base 64 using the mapped characters applying the following algorithm:

<code>pre: numberToConver >= 0 and baseToConvert > 1 and DigitsList.length = 0</code>

<code>post: DigitList contains each digit of the numberToConvert in base baseToConvert</code>

<code>num = numberToConvert</code>

<code>while num > 0</code>

<code>	remainder = modulo(num, baseToConvert)</code>

<code>	DigitsList.add(remainder)</code>

<code>	num = num / baseToConvert</code>

<code> return DigitsList.reverse</code>


Then for each element in the DigitsList we must get the corresponding symbol in our alphabet.
The inverse function is analogous.

Requirements
===========

* We must to be capable to define several alphabets to enconde our ids to be used in differents parts of our web application.
* We must get shorten ids than the originals.
* We must broke the current ids from MongoDB.
* We must define random alphabets from the ascii table and to be able to exclude certain characters.
* We need a way to identify each alphabet and the converted ids.

Analysis
===========

* We need some data structures to work with differents alphabets
* We need a way to identify the encoded ids, the alphabet and the base
* We need a way to broke the string of the original id
* We need an algorithm to encode and decode the ids
* We need a way to ask for a new alphabet when needed
* We need to save and load alphabets from a database

Design
===========

* We need an entity that represents the alphabet and has: a prefix of the alphabet to include in the converted id, the alphabet with his mapping, and the base of the encoding
* We need one algorithm to encode and other to decode
* We need to save the alphabets

