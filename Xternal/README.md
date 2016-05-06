# Evolution, the distributed part 

This sub-directory equips the Evolution system with the necessary
serialization mechanics for GUI rendering and distributed execution. 

Each module and class in the Evolution system is extended with functions
and methods, respectively, that can turn structures and objects into JSON. 

In addition, the dealer component is extended with a method for registering
and calling observers. The observer module provides the necessary
functionality. 

TO DO
-----

12. merge common and messaging ? 

4. bring back gui in xmain and xserver 
9. document JSON in this subdirectory's README 
10. add xdist-bad-clients to test error handling 

Files, Modules and Dependencies 
-------------------------------


| name              | purpose                                                               |
| ----------------- | --------------------------------------------------------------------- |
| dealer            | equip dealer with json/c and observer/c interfaces for GUI only	    |
| player-external   | overrides feed-* creators in external% with serializable constructors |
| player-internal   | overrides species creator in internal% with serializable constructors |
| next		    | equips feed-* classes and creators with serialization    		    |
| board		    | equips species class and creators with serialization    		    |
| cards		    | equips card struct with serialization  				    |
| 		    | 	     	  	      						    |
| common	    | add testing facilities for serialization functions and methods	    |
| 		    | 	    	      		 					    |
| gui     	    | a gui for displaying 2htdp/images from JSON serialization 	    |	
| observer     	    | set up observer connection between dealer and players		    |
| 		    | 	    	      		 					    |
| messaging	    | sending and receiving messages for dist. impl. 			    |
| proxy-player 	    | a proxy player for the server to connect to remote clients 	    |
| proxy-dealer 	    | a proxy dealer for the client to connect to remote servers    	    |
| proxy-json-tests  | a unit test suite for relation json-> and ->json functions	    |
| 	      	    |									    |
| xdist 	    | an executable for running a distributed game for n players	    |
| client 	    | a client for generating dist. Evolution clients			    |
| xclients 	    | an executable for running a bunch of clients in threads 		    |
| xserver 	    | an executable for running server	   	      			    |

Running the Distributed Version
-------------------------------

Open two shells, in the first run: 
> ./xproxy-server 45678 

in the second one, run 
> ./xclients 3 ""

This will run one complete game on local host. It also runs across the network.

Compiling 
---------

Use raco to compile the distributed system:

> raco make xdist 

Compiling is optional in Racket. It reduces the start-up cost.

Testing
-------

> raco test *.rkt 

> ./xdist 8

See all 1s. 


