# simpleserver
  A simple server Erlang server for purely educational and testing purposes.
  
  Features:
  * REST interface using Cowboy http server
  * Non-blocking backend generic erlang server handling the requests

##Installing
    $ git clone https://github.com/nkmprog/simpleserver.git
    $ cd simpleserver
    $ rebar compile

##Running
  To run simpleserver start an Erlang node with simpleserver library in it's code path.

    $ erl -pa $PATH_TO_SIMPLESERVER/ebin/ $PATH_TO_SIMPLESERVER/deps/*/ebin/
    
    1> myapp_app:start().
    {ok,[ranch,crypto,cowlib,cowboy,myapp]}

##Testscript
  A simple testscript is included in the repository. Running it will produce an output.txt file with the result.
