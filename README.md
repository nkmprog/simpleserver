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
    $ curl -v -X GET http://localhost:8001/stats
    $ curl -v -X GET http://localhost:8001/stats?id=$reqid

  provider = google | youtube
  reqid = as returned by the communication rest method
