%%%
%%% Control Functions
%%%
-module(control).

%% API
-export([do/3]).
-export([foreach/2, foreach/3, foreach/4]).
-export([map/2, map/3]).

%% API
do(F, Args, 0) when is_function(F), is_list(Args) ->
  ok;
do(F, Args, X) when is_function(F), is_list(Args), X > 0 ->
  erlang:apply(F, Args),
  do(F, Args, X - 1).

foreach(F, N) ->
  foreach(F, 1, N).

foreach(F, Step, N) ->
  foreach(F, 1, Step, N).

foreach(F, X, _Step, N) when is_function(F), X >= N ->
  ok;
foreach(F, X, Step, N) when is_function(F), X < N ->
  F(X),
  foreach(F, X + Step, Step, N).

map(F, X) ->
  map(F, X, []).

map(F, 0, Acc) when is_function(F) ->
  Acc;
map(F, X, Acc) when is_function(F), X > 0 ->
  map(F, X - 1, [F(X) | Acc]).
