# Evolution

This repo represents my implementation of the Evolution game for [Software Development @
NU CCIS S'2016](http://www.ccs.neu.edu/home/matthias/4500-s16/). The course did not use
the official rules of the game but a [simplified and modified
version](http://www.ccs.neu.edu/home/matthias/4500-s16/evolution.html) [also see the
students' frequently asked question and my answers](http://www.ccs.neu.edu/home/matthias/4500-s16/faq.html).

The implementation supports both a sequential implementation that integrates Racket-based
player implementations statically and a distributed implementation for interacting with
players implemented in other languages.

For simple, deterministic runs, use xmain on the command line and supply a number
between 3 and 8. 

TO DO
-----

2. clean up common
4. bring back gui in xmain and xserver 
5. put json-> for test harnesses in proper place (test modules)
7. re-think the case when a player cannot eat now, but later when a "victim" becomes available 
9. document JSON in this subdirectory's README 
A. add xdist-bad-clients to test error handling 

Files, Modules and Dependencies 
-------------------------------

| name              | purpose                                                            |
| ----------------- | ------------------------------------------------------------------ |
|
[spec-relations](https://github.com/mfelleisen/Evolution/blob/master/spec-relations.txt) | what kind of objects exist and how are they related and how they interact|
|           	    | 	     		       		     	  			 |
| dealer	    | dealer data representation and functionality			 |
| [player-implementation.txt](https://github.com/mfelleisen/Evolution/blob/master/player-implementation.txt) | a diagram ow how the following three files are related     |
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
| 		    | 	       	   	      						 |
| xmain 	    | an executable for running a sequential game for n players		 |
| 		    | 	       	   	      						 |
| Xternal	    | directory for playing in distributed mode 			 |
| 		    | 	       	   	      						 |
| common     	    | some project-cross-cutting things					 |
| json-pretty       | this must go to json library					 |

Reading the Code 
----------------

All files consist of three (or more) segments: 

1. an interface, which specifies the exported services;
2. require dependencies for the code proper and the unit tests;
3. an implementation, which may consist of subsections. 

Most module come with a fourth section: 

4. some test suites; section 2 indicates whether this section exits
   with a require for rackunit. 

To read any file, open it in DrRacket. We strongly recommend reading the
program from the top down, starting with the provide specs. 

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

