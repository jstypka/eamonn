-module(tibor_nif).
-export([init/0, get/1]).
-on_load(on_load/0).

-define(LIBNAME, "tibor_nif").

-spec on_load() -> ok.
on_load() ->
	ProjectPath = filename:dirname(filename:dirname(code:which(?MODULE))),
    SOName = filename:join([ProjectPath, priv, ?LIBNAME]),
    ok = erlang:load_nif(SOName, 0).


-spec init() -> ok.
init() ->
    erlang:nif_error(nif_not_loaded).


-spec get(string()) -> binary().
get(_X) ->
    erlang:nif_error(nif_not_loaded).