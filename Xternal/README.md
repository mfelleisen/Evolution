# Evolution, the distributed part 

TO DO
-----

4. bring back gui in xmain and xserver 
9. document JSON in this subdirectory's README 
10. add xdist-bad-clients to test error handling 

Files, Modules and Dependencies 
-------------------------------

| name              | purpose                                                            |
| ----------------- | ------------------------------------------------------------------ |
| gui     	    | a gui mixin for displaying 2htdp/images 				 |	
| 		    | 	    	      		 					 |
| proxy-player 	    | a proxy player for the server to connect to remote clients 	 |
| proxy-dealer 	    | a proxy dealer for the client to connect to remote servers    	 |
| proxy-json-tests  | a unit test suite for relation json-> and ->json functions	 |
| 	      	    |									 |
| xdist 	    | an executable for running a distributed game for n players	 |
| client 	    | a client for generating Evolution clients				 |
| xclients 	    | an executable for running a bunch of clients in threads 		 |
| xserver 	    | an executable for running server	   	      			 |


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

> ./xdist 8

See all 1s. 


