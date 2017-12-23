%%%
%%% Utils Functions
%%%
-module(erlib).

%% OTP functions
-export([get_sup_child/2]).
-export([gen_child_spec/1, gen_child_spec/2, gen_child_spec/3]).

-spec get_sup_child(module(), atom()) -> {error, not_found}|{ok, pid()}.

%%% ------------------------------------------------------
%%% OTP functions
%%% ------------------------------------------------------
get_sup_child(Sup_Mod, Child_Name) ->
  Sup = whereis(Sup_Mod),
  Children = supervisor:which_children(Sup),
  case lists:keyfind(Child_Name, 1, Children) of
    false ->
      {error, not_found};
    {Child_Name, Pid, _, _} ->
      {ok, Pid}
  end.

%% build general worker child_spec
gen_child_spec(Mod) when is_atom(Mod) ->
  gen_child_spec(Mod, []);
gen_child_spec(Opts) when is_list(Opts) ->
  gen_child_spec(maps:from_list(Opts));
gen_child_spec(Opts) when is_map(Opts) ->
  Mod = maps:get(mod, Opts),
  Args = maps:get(args, Opts, []),
  Func_Name = maps:get(func, Opts, start_link),
  case maps:get(global, Opts) of
    true ->
      {Mod, {{global, Mod}, Func_Name, Args},
        permanent, 1000, worker, [Mod]};
    _ ->
      {Mod, {Mod, Func_Name, Args},
        permanent, 1000, worker, [Mod]}
  end.
gen_child_spec(Mod, Args) when is_atom(Mod), is_list(Args) ->
  gen_child_spec(Mod, start_link, Args).
gen_child_spec(Mod, Func_Name, Args) when is_atom(Mod), is_atom(Func_Name), is_list(Args) ->
  {Mod, {Mod, Func_Name, Args},
    permanent, 1000, worker, [Mod]}.
