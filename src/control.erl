%%%
%%% Control Functions
%%%
-module(control).

%% API
-export([do_i/3]).
-export([foreach_i/2, foreach_i/3, foreach_i/4]).
-export([map_i/2, map_i/3]).

%% API
do_i(F, Args, 0) when is_function(F), is_list(Args) ->
  ok;
do_i(F, Args, X) when is_function(F), is_list(Args), X > 0 ->
  erlang:apply(F, Args),
  do_i(F, Args, X - 1).

foreach_i(F, N) ->
  foreach_i(F, 1, N).

foreach_i(F, Step, N) ->
  foreach_i(F, 1, Step, N).

foreach_i(F, X, _Step, N) when is_function(F), X >= N ->
  ok;
foreach_i(F, X, Step, N) when is_function(F), X < N ->
  F(X),
  foreach_i(F, X + Step, Step, N).

map_i(F, X) ->
  map_i(F, X, []).

map_i(F, 0, Acc) when is_function(F) ->
  Acc;
map_i(F, X, Acc) when is_function(F), X > 0 ->
  map_i(F, X - 1, [F(X) | Acc]).
