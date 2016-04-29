# Evolution

This repo represents my implementation of the Evolution game for [Software Development @
NU CCIS S'2016](http://www.ccs.neu.edu/home/matthias/4500-s16/). 

The implementation supports both a sequential implementation that integrates Racket-based
player implementations statically and a distributed implementation for interacting with
players implemented in other languages.

TOOD
----

1. clean up basic
2. clean up common
3. clean up add a description of communication between pieces 
4. bring back gui in xserver 

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
| next    	    | a data representation of feed-next responses 			 |
| board 	    | a data representation of species boards          			 |
| traits  	    | a data representation of traits         				 |
| cards   	    | a data representation of cards          				 |
| gui     	    | a gui mixin for displaying 2htdp/images 				 |
| basics     	    | functions, contracts, syntax that could come from Racket		 |
| 	      	    |									 |
| proxy-player 	    | a proxy player for the server to connect to remote clients 	 |
| proxy-dealer 	    | a proxy dealer for the client to connect to remote servers    	 |
| proxy-json-tests  | a unit test suite for relation json-> and ->json functions	 |
| proxy-server 	    | a server for running Evolution remotely  	  	 		 |
| proxy-client 	    | a client for generating Evolution clients				 |
| 		    | 	       	   	      						 |
| xmain 	    | an executable for running a sequential game for n players		 |
| 		    | 	       	   	      						 |
| xdist 	    | an executable for running a distributed game for n players	 |
| xclients 	    | an executable for running a bunch of clients in threads 		 |
| xserver 	    | an executable for running server	   	      			 |


Compiling 
---------

Use raco to compile the clients:

> raco make xmain

> raco make xclients 

> raco make xserver

> raco make xdist 

Compiling is optional in Racket. It reduces the start-up cost.

Testing
-------

Use raco to run the unit tests, e.g., 

> raco test dealer.rkt 

Running the Distributed Version
-------------------------------

Open two shells, in the first run: 
> ./xproxy-server 45678 

in the second one, run 
> ./xclients 3 ""

This will run one complete game on local host.

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
