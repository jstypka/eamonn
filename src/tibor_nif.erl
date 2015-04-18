-module(tibor_nif).
-export([get/1]).
-on_load(init/0).

-define(LIBNAME, "tibor_nif").

-spec init() -> ok.
init() ->
	ProjectPath = filename:dirname(filename:dirname(code:which(?MODULE))),
    SOName = filename:join([ProjectPath, priv, ?LIBNAME]),
    ok = erlang:load_nif(SOName, 0).


-spec get(string()) -> binary().
get(_X) ->
    erlang:nif_error(nif_not_loaded).