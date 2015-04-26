-module(eamonn_loadtest).

-export([run/1]).

-define(DATASET_SIZE, 80).
-define(TIMEOUT_TIME, 2000000).
-define(ADDRESS, "http://localhost:3000").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run(Processes) ->
    register(?MODULE, self()),
    Start = os:timestamp(),
    [spawn(fun client/0) || _ <- lists:seq(1, Processes)],
    Results = [receive_acks() || _ <- lists:seq(1, Processes)],
    End = os:timestamp(),
    {Success, Failed} = lists:partition(fun(X) -> X == ok end, Results),
    io:format("Time: ~p ms~nResults: ~p served correctly, ~p timed out~n",
              [timer:now_diff(End, Start) div 1000,
               length(Success),
               length(Failed)]),
    unregister(?MODULE),
    success.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
receive_acks() ->
    receive
        X -> X
    after 3000 ->
            error(testing_process_timeout)
    end.

client() ->
    inets:start(),
    Start = os:timestamp(),
    random:seed(Start),
    ImgNr = random_exponential(),
    Address = ?ADDRESS ++ "/img/" ++ integer_to_list(ImgNr) ++ ".png",
    case httpc:request(Address) of
        {http, {_RequestId, {error, {Reason, _Rest}}}} ->
            error(Reason);
        {ok, {{_, 200, _}, _Headers, _Body}} ->
            ok
    end,
    End = os:timestamp(),
    Answer = case timer:now_diff(End, Start) of
                 X when X > ?TIMEOUT_TIME ->
                     timeout;
                 _ -> ok
             end,
    ?MODULE ! Answer.

random_uniform() ->
    random:uniform(?DATASET_SIZE).

random_exponential() ->
    R = random:uniform(),
    trunc(R * R * ?DATASET_SIZE) + 1.