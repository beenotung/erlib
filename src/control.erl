%%%
%%% Control Functions
%%%
-module(control).

-export([do/3]).
-export([foreach/2, foreach/3, foreach/4]).
-export([map/2, map/3]).
-export([acc/3, acc/4, acc/5]).

%% repeat execution X times
do(F, Args, 0) when is_function(F), is_list(Args) ->
  ok;
do(F, Args, X) when is_function(F), is_list(Args), X > 0 ->
  erlang:apply(F, Args),
  do(F, Args, X - 1).

%% like call in for loop
foreach(F, N) ->
  foreach(F, 1, N).

foreach(F, Start, End) ->
  foreach(F, Start, 1, End).

foreach(F, Start, _Step, End) when is_function(F), Start > End ->
  ok;
foreach(F, Start, Step, End) when is_function(F), Start =< End ->
  F(Start),
  foreach(F, Start + Step, Step, End).

%% construct a list using mapper function and index (without lists:seq)
map(F, X) ->
  map(F, X, []).

map(F, 0, Acc) when is_function(F) ->
  Acc;
map(F, X, Acc) when is_function(F), X > 0 ->
  map(F, X - 1, [F(X) | Acc]).

%% like list:seq > list:fold, but use less memory
acc(F, Acc, N) ->
  acc(F, Acc, 1, N).

acc(F, Acc, Start, End) ->
  acc(F, Acc, Start, 1, End).

acc(F, Acc, Start, _Step, End) when is_function(F), Start > End ->
  Acc;
acc(F, Acc, Start, Step, End) when is_function(F), Start =< End ->
  acc(F, F(Acc, Start), Start + Step, Step, End).
