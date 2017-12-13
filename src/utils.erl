%%%
%%% Utils Functions
%%%
-module(utils).

%% OTP functions
-export([get_sup_child/2]).

-spec get_sup_child(module(), atom()) -> {error, not_found}|{ok, pid()}.

%% OTP functions
get_sup_child(Sup_Mod, Child_Name) ->
  Sup = whereis(Sup_Mod),
  Childs = supervisor:which_children(Sup),
  case lists:keyfind(Child_Name, 1, Childs) of
    false ->
      {error, not_found};
    {Child_Name, Pid, _, _} ->
      {ok, Pid}
  end.
