%%------------------------------------------------------------------------------
%% Copyright (c) 2015, Vasily Demidenok <define.null@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%------------------------------------------------------------------------------
%% The code for semver parsing is copied from https://github.com/nox/mouture/
%% See copiright below
%%------------------------------------------------------------------------------
%% Copyright (c) 2014, Anthony Ramine <n.oxyde@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%------------------------------------------------------------------------------

-module(rebar_version_checker).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export(['vsn-check'/2
         %% ,'post_get-deps'/2,
         %% ,'post_update-deps'/2
        ]).

-export([check/2,
         compare/2
        ]).

-type version() :: {core(),pre(),meta()}.

-type   core()    :: {non_neg_integer(),non_neg_integer(),non_neg_integer()}.
-type   pre()     :: [binary() | non_neg_integer()].
-type   meta()    :: [binary()].

%% {vsn_check, [{app, constraints},
%%              {app, constraints}
%%             ]}.

%% 'post_get-deps'(Config, AppFile) ->
%%     'vsn-check'(Config, AppFile).

%% 'post_update-deps'(Config, AppFile) ->
%%     'vsn-check'(Config, AppFile).

'vsn-check'(Config, _AppFile) ->
    case rebar_app_utils:is_app_dir() of
        false -> ok;
        {true, _} ->
            VsnChecks = rebar_config:get_local(Config, vsn_check, []),

            lists:foreach(fun({App, Constraint}) ->
                                  AppVsn = case app_version(Config, App) of
                                               {_, [$v | AppVsn]} -> AppVsn;
                                               {_, AppVsn} -> AppVsn
                                           end,
                                                                         
                                  case check(AppVsn, Constraint) of
                                      true  -> ok;
                                      {false, Op, Cons} ->
                                          io:format("App ~p ~p is not matching constraint \"~s ~s\" in ~p~n",
                                                    [App, AppVsn, Op, unparse(Cons), rebar_utils:get_cwd()]),
                                          halt(2)
                                  end
                          end, VsnChecks),
            ok
    end.

app_version(Config, App) ->
    {true, AppDir} = get_deps_dir(Config, App),
    case rebar_app_utils:is_app_dir(AppDir) of
        {true, AppFile} ->
            rebar_app_utils:app_vsn(Config, AppFile);
        false ->
            io:format("App vsn not found ~p~n", [App]),
            halt(1)            
    end.

get_deps_dir(Config, App) ->
    BaseDir = rebar_config:get_xconf(Config, base_dir, []),
    DepsDir = get_shared_deps_dir(Config),
    {true, filename:join([BaseDir, DepsDir, App])}.

get_shared_deps_dir(Config) ->
    rebar_config:get_xconf(Config, deps_dir, "deps").

-spec check(string(), [string()]) -> boolean().
check(Vsn, [Constraint|_] = VsnConstraints)
  when is_list(Constraint) ->
    OrigVsn = parse(Vsn),
    ParsedCond = parse_constraints(VsnConstraints),
    
    verify_constraints(OrigVsn, ParsedCond).

compare(Vsn1, Vsn2) ->
    V1 = parse(Vsn1),
    V2 = parse(Vsn2),
    compare_imp(V1, V2).

%%------------------------------------------------------------------------------
%% Constraints
%%------------------------------------------------------------------------------

parse_constraints(Constraints) ->
    parse_constraints(Constraints, []).

parse_constraints([], Acc) -> Acc;
parse_constraints([ Cons | T ], Acc) ->
    {Op, Vsn1} = case string:strip(Cons) of
                     [$=,$>|Vsn] -> {'>=', Vsn};
                     [$>,$=|Vsn] -> {'>=', Vsn};
                     [$>|   Vsn] -> {'>',  Vsn};
                     [$=,$<|Vsn] -> {'<=', Vsn};
                     [$<,$=|Vsn] -> {'<=', Vsn};
                     [$=|   Vsn] -> {'=',  Vsn};
                     [$<|   Vsn] -> {'<',  Vsn};
                     _ ->
                         erlang:error({invalid_constraint, Cons})
                 end,
    parse_constraints(T, [{Op, parse(string:strip(Vsn1))} | Acc]).

-spec verify_constraints(version(), list()) -> boolean() | no_return().
verify_constraints(_Vsn, []) -> true;
verify_constraints(Vsn, [{Op, ConVsn} | T]) ->
    Res = case {Op, compare_imp(Vsn, ConVsn)} of
              {'>=', Q } when (Q==eq) or (Q == gt) -> true;
              {'>' , gt} -> true;
              {'=' , eq} -> true;
              {'<=', Q } when (Q == eq) or (Q == lt) -> true;
              {'<' , lt} -> true;
              _ -> false
          end,
    case Res of
        true  -> verify_constraints(Vsn, T);
        false -> {false, Op, ConVsn}
    end.    

%%------------------------------------------------------------------------------
%% Semver parsing
%%------------------------------------------------------------------------------

-spec parse(iodata()) -> version().
parse(Input) when is_list(Input) ->
  parse(iolist_to_binary(Input));
parse(Input) when is_binary(Input) ->
    juxta(Input, [fun core/2,
                  fun preversion/2,
                  fun metadata/2],
        fun (<<>>, [Core,Pre,Meta]) -> {Core,Pre,Meta} end).

-spec unparse(version()) -> binary().
unparse({{X,Y,Z},Pre,Meta}) ->
    <<(integer_to_binary(X))/binary,$.,
      (integer_to_binary(Y))/binary,$.,
      (integer_to_binary(Z))/binary,
      (unparse_ext($-, fun unparse_pre_seg/1, Pre))/binary,
      (unparse_ext($+, fun unparse_meta_seg/1, Meta))/binary>>.

%% Comparison and compatibility

-spec compare_imp(version(), version()) -> lt | eq | gt.
compare_imp({Core,Pre,_}, {Core,Pre,_}) ->
    eq;
compare_imp({Core,[],_}, {Core,_,_}) ->
    gt;
compare_imp({Core,_,_}, {Core,[],_}) ->
    lt;
compare_imp({Core,PreA,_}, {Core,PreB,_})
  when PreA < PreB ->
    lt;
compare_imp({CoreA,_,_}, {CoreB,_,_})
  when CoreA < CoreB ->
    lt;
compare_imp({_,_,_}, {_,_,_}) ->
    gt.

%% Main parts

-spec core(binary(), cont(core(), A)) -> A.
core(Bin, Cont) ->
    dot(Bin, fun integer/2, fun (Rest, [X,Y,Z]) -> Cont(Rest, {X,Y,Z}) end).

-spec preversion(binary(), cont(pre(), A)) -> A.
preversion(<<$-,Rest/binary>>, Cont) ->
    dot(Rest, fun ident_or_integer/2, Cont);
preversion(Bin, Cont) ->
    Cont(Bin, []).

-spec metadata(binary(), cont(meta(), A)) -> A.
metadata(<<$+,Rest/binary>>, Cont) ->
    dot(Rest, fun ident/2, Cont);
metadata(Bin, Cont) ->
    Cont(Bin, []).

%% Base parsers

-type cont(A, B) :: fun((binary(), A) -> B).

-spec ident_or_integer(binary(), cont(binary() | non_neg_integer(), A)) -> A.
ident_or_integer(Bin, Cont) ->
    ident(Bin, fun (Rest, <<$0>>) ->
                       Cont(Rest, 0);
                   (Rest, <<C,_/binary>>=Ident) when C >= $0, C =< $9 ->
                       Cont(Rest, try binary_to_integer(Ident) of
                                      Int when Int =/= 0; C =/= $0 -> Int
                                  catch error:badarg -> Ident end);
                   (Rest, Ident) ->
                       Cont(Rest, Ident)
               end).

-spec ident(binary(), cont(binary(), A)) -> A.
ident(<<C,Rest/binary>>, Cont)
  when C >= $0, C =< $9; C >= $a, C =< $z;
       C >= $A, C =< $Z; C =:= $- ->
    ident(Rest, <<C>>, Cont).

ident(<<C,Rest/binary>>, Ident, Cont)
  when C >= $0, C =< $9; C >= $a, C =< $z;
                                           C >= $A, C =< $Z; C =:= $- ->
    ident(Rest, <<Ident/binary,C>>, Cont);
ident(Bin, Ident, Cont) ->
    Cont(Bin, Ident).

-spec integer(binary(), cont(non_neg_integer(), A)) -> A.
integer(<<$0,Rest/binary>>, Cont) ->
    Cont(Rest, 0);
integer(<<C,Rest/binary>>, Cont)
  when C >= $1, C =< $9 ->
    integer(Rest, C - $0, Cont).

integer(<<C,Rest/binary>>, Acc, Cont)
  when C >= $0, C =< $9 ->
    integer(Rest, Acc * 10 + C - $0, Cont);
integer(Bin, Acc, Cont) ->
    Cont(Bin, Acc).

%% Combinators

-spec juxta(binary(), [fun((binary(), cont(_, A)) -> A)], cont([A], B)) -> B.
juxta(Bin, Fs, Cont) ->
    juxta(Bin, [], Cont, Fs).

juxta(Bin, Acc, Cont, [F|Fs]) ->
    F(Bin, fun (Rest, X) -> juxta(Rest, [X|Acc], Cont, Fs) end);
juxta(Bin, Acc, Cont, []) ->
    Cont(Bin, lists:reverse(Acc)).

-spec dot(binary(), fun((binary(), cont(_, A)) -> A), cont([A], B)) -> B.
dot(Bin, F, Cont) ->
    F(Bin, fun (Rest, X) -> dot(Rest, [X], F, Cont) end).

dot(<<$.,Rest0/binary>>, Acc, F, Cont) ->
    F(Rest0, fun (Rest1, X) -> dot(Rest1, [X|Acc], F, Cont) end);
dot(Bin, Acc, _, Cont) ->
    Cont(Bin, lists:reverse(Acc)).

%% Extensions' unparser

-spec unparse_ext(byte(), fun((A) -> binary()), [A]) -> binary().
unparse_ext(_, _, []) ->
    <<>>;
unparse_ext(Prefix, F, [H|Rest]) ->
    <<Prefix,(F(H))/binary,
      (<< <<$.,(F(Seg))/binary>> || Seg <- Rest >>)/binary>>.

-spec unparse_pre_seg(binary() | non_neg_integer()) -> binary().
unparse_pre_seg(Seg) when is_binary(Seg) ->
    Seg;
unparse_pre_seg(Seg) when is_integer(Seg), Seg >= 0 ->
    integer_to_binary(Seg).

-spec unparse_meta_seg(binary()) -> binary().
unparse_meta_seg(Seg) when is_binary(Seg) ->
    Seg.

%%------------------------------------------------------------------------------
%% Eunit
%%------------------------------------------------------------------------------

-ifdef(TEST).

comparation_test() ->
    ?_assertEqual(eq, compare("1.2.3", "1.2.3")),
    ?_assertEqual(gt, compare("1.0.0", "0.0.1")),
    ?_assertEqual(lt, compare("1.0.1", "1.1.0")),
    
    ?_assertEqual(lt, compare("1.0.0-alpha",      "1.0.0-alpha.1")),
    ?_assertEqual(lt, compare("1.0.0-alpha.1",    "1.0.0-alpha.beta")),
    ?_assertEqual(lt, compare("1.0.0-alpha.beta", "1.0.0-beta")),
    ?_assertEqual(lt, compare("1.0.0-beta",       "1.0.0-beta.2")),
    ?_assertEqual(lt, compare("1.0.0-beta.2",     "1.0.0-beta.11")),
    ?_assertEqual(lt, compare("1.0.0-beta.11",    "1.0.0-rc.1")),
    ?_assertEqual(lt, compare("1.0.0-rc.1",       "1.0.0")),
    ok.    
     
check_single_constraints_test() ->

    ?_assertEqual(true, check("1.2.3", [">= 1.2.3"])),
    ?_assertEqual(true, check("1.2.3", ["> 1.2.2"])),
    ?_assertEqual(true, check("1.2.3", ["= 1.2.3"])), 
    ?_assertEqual(true, check("1.2.3", ["<= 1.2.3"])), 
    ?_assertEqual(true, check("1.2.3", ["< 1.2.4"])),   
    ?_assertEqual(false, check("1.2.3", ["< 1.2.3"])),
    ?_assertEqual(false, check("1.3.0", ["< 1.2.3"])),
    ?_assertEqual(false, check("2.0.0", ["< 1.2.3"])),
    ?_assertEqual(false, check("1.2.3-rc1", [">= 1.2.3"])),
    ?_assertEqual(true,  check("1.2.3+some-extra-info", ["= 1.2.3"])),
    
    ok.

check_multiple_constraints_test() ->
    ?_assertEqual(true, check("1.2.3", [">= 1.0.0", "<= 2.0.0"])),
    ?_assertEqual(false, check("1.2.3", [">= 1.0.0", "< 1.2.0"])),
    
    ok.

-endif.

