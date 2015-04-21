# simpleserver
  A simple Erlang server for purely educational and testing purposes.
  
  Features:
  * REST interface using Cowboy http server
  * Non-blocking backend generic erlang server handling the requests

##Installing
    $ git clone https://github.com/nkmprog/simpleserver.git
    $ cd simpleserver
    $ rebar get-deps
    $ rebar compile

##Running
  To run simpleserver start an Erlang node with simpleserver library in its code path.

    $ erl -pa $PATH_TO_SIMPLESERVER/ebin/ $PATH_TO_SIMPLESERVER/deps/*/ebin/
    
    1> myapp_app:start().
    {ok,[ranch,crypto,cowlib,cowboy,myapp]}

##Testscript
  A simple testscript is included in the repository. Running it will produce an output.txt file with the result.

##Example Requests
    $ curl -v -X POST http://localhost:8001/communication -d '{"provider": $provider}'
    > {"request_id":"63596864760WCL5kWtLxxDgKtJn"}

    $ curl -v -X GET http://localhost:8001/stats
    > {
      "data" : [
      	{
	  "req_id" : "63596864646y6AlQEnplYExYsSk",
      	  "provider" : "google",
      	  "finish_time" : 1429634646494910,
      	  "response_time" : 424654,
      	  "total_time" : 424687
    	},
        ...
      ]}

    $ curl -v -X GET http://localhost:8001/stats?id=$reqid
    > {
       "req_id" : "63596864760WCL5kWtLxxDgKtJn",
       "provider" : "google",
       "finish_time" : 1429634760757794,
       "response_time" : 124901,
       "total_time" : 124973
      }

  provider = google | youtube  
  reqid = as returned by the communication rest method
