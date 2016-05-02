# Evolution

This repo is the core of my implementation of the Evolution game for [Software Development @
NU CCIS S'2016](http://www.ccs.neu.edu/home/matthias/4500-s16/). It does not include the
test harnesses and the test-generating pieces. 

The course did not use the official rules of the game but a [simplified and modified
version](http://www.ccs.neu.edu/home/matthias/4500-s16/evolution.html) [also see the
students' frequently asked question and my
answers](http://www.ccs.neu.edu/home/matthias/4500-s16/faq.html).

The Evolution game is run as a series of turns, with almost no setup
required. Each turn proceeds in four stages:
1. the dealer ensures that every player has one species board and cards to play with 
2. the players pick one card to contribute to the common food supply 
3. they use (some of) their other cards to perform actions on their species boards
4. finally they feed their species. 
The clean-up phase consolidates the players' gains and awards cards for
lost species boards. 

The dealer communicates with the players and enforces the rules. The
implementation of a turn consists of three steps: the dealer starts each
player, then asks for card actions, and finally runs a feeding cycle,
asking each player what to feed next. 

This implementation supports both a sequential implementation that integrates Racket-based
player implementations statically and a distributed implementation for interacting with
players implemented in other languages.

For simple, deterministic runs, use xmain on the command line and supply a number
between 3 and 8. 

TO DO
-----

4. bring back gui in xmain and xserver 
5. put json-> for test harnesses in proper place (test modules)
7. re-think the case when a player cannot eat now, but later when a "victim" becomes available 
9. document JSON in this subdirectory's README 
A. add xdist-bad-clients to test error handling 
B. decouple printing from main & dealer so that it becomes easier to develop a benchmark
C. submit json-pretty to racket/json 


Files, Modules and Dependencies 
-------------------------------

| name              | purpose                                                            |
| ----------------- | ------------------------------------------------------------------ |
|
[doc-relations](https://github.com/mfelleisen/Evolution/blob/master/doc-relations.txt) | what kind of objects exist and how are they related and how they interact|
|           	    | 	     		       		     	  			 |
| dealer	    | dealer data representation and functionality			 |
| [doc-players](https://github.com/mfelleisen/Evolution/blob/master/doc-players.txt) | a diagram ow how the following three files are related     |
| player-base       | basic player data and functionality				 |
| player-external   | a representation of a specific "silly" external player		 |
| player-internal   | a representation of the internal player with index-based API  	 |
| internal-external | functions & syntax for communicating between X and I players  	 |
| next    	    | a representation of dealer-player dialogues 			 |
| board 	    | a data representation of species boards          			 |
| traits  	    | a data representation of traits         				 |
| cards   	    | a data representation of cards          				 |
| gui     	    | a gui mixin for displaying 2htdp/images 				 |
| basics     	    | functions, contracts, syntax that could come from Racket		 |
| common     	    | some project-cross-cutting things					 |
| 		    | 	       	   	      						 |
| xmain 	    | an executable for running a sequential game for n players		 |


The Distributed Version
-----------------------

| name              | purpose                                                            |
| ----------------- | ------------------------------------------------------------------ |
| [Xternal](https://github.com/mfelleisen/Evolution/tree/master/Xternal) | directory for playing in distributed mode 			 |

Files to Migrate (into the Racket repo)
---------------------------------------

| name              | purpose                                                            |
| ----------------- | ------------------------------------------------------------------ |
| json-pretty       | this must go to json library					 |

Reading the Code (in General)
-----------------------------

All code files consist of three segments: 

1. an interface, which specifies the exported services;
2. require dependencies for the code proper and the unit tests;
3. an implementation, which may consist of subsections. 

Most module come with a fourth section: 

4. some test suites; section 2 indicates whether this section exits
   with a require for rackunit. 

To read any file, open it in DrRacket. We strongly recommend reading the
program from the top down, starting with the provide specs. 

Reading this Code (Specifically)
--------------------------------

Make sure to read the rules of Evolution and the FAQ. 

Start with doc-relations.txt. The file provides an overview of the key
pieces: the dealer, its representation of players, the external players
(mostly strategies), and the communication layer between them. 

Now scan sections 1 of next.rkt, dealer.rkt, and player-external.rkt. 
These interfaces explain the communication layer. 

The doc-players.txt files comes next. The file lays out how the player
infrastructure is arranged.

Compiling 
---------

Use raco to compile the clients:

> raco make xmain

Compiling is optional in Racket. It reduces the start-up cost.

Testing
-------

Use raco to run the unit tests, e.g., 

> raco test dealer.rkt 

> ./xmain 8

See all 1s. 

