%% Copyright 2024 Jesse Gumm
%% MIT LICENSE

-module(ds_syntax).

%% API exports
-export([parse_transform/2, init/0, arrow/1, bracket/1, is_initialized/0]).

-compile({nowarn_unused_function, log/1}).
-compile({nowarn_unused_function, log/2}).
-compile({nowarn_unused_function, log_file/1}).
-compile({nowarn_unused_function, log_file/2}).
-dialyzer({nowarn_function, log/1}).
-dialyzer({nowarn_function, log/2}).
-dialyzer({nowarn_function, log_file/1}).
-dialyzer({nowarn_function, log_file/2}).

-ifdef(debug_parser).
-define(pr(CT, H), io:format("(L#: ~p) (CT: ~p) ~p = ~p~n", [?LINE, CT, ??H, H])).
-else.
-define(pr(CT, H), ok).
-endif.


log(Msg) ->
    io:format(["Erlang DS: ", Msg, "\n"]).

log(Msg, Args) ->
    log(io_lib:format(Msg, Args)).

log_file(Msg) ->
    File = get_filename(),
    FullMsg = [io_lib:format("(~ts): ",[File]), Msg],
    log(FullMsg).

log_file(Msg, Args) ->
    FullMsg = io_lib:format(Msg, Args),
    log_file(FullMsg).

init() ->
    case is_initialized() of
        false ->
            log("Initializing Syntax Parse Transform"),
            log("Hijacking erl_parse:parse_form/1"),
            meck:new(erl_parse, [no_link, unstick, passthrough]),
            meck:expect(erl_parse, parse_form, fun(Tokens) ->
                save_tokens(Tokens),
                maybe_update_filename(Tokens),
                case is_pt_enabled(Tokens) of
                    true ->
                        %log_file("Preprocessing Arrow Syntax"),
                        Tokens2 = ?MODULE:arrow(Tokens),
                        meck:passthrough([Tokens2]);
                    false ->
                        %log_file("Skipping Arrow Syntax for ~p", [Tokens]),
                        meck:passthrough([Tokens])
                end
            end),
            log("Initialization Complete"),
            set_initialized(),
            ok;
        true ->
            ok
    end.

is_initialized() ->
    persistent_term:get(erlang_ds_syntax_enabled, false).

set_initialized() ->
    persistent_term:put(erlang_ds_syntax_enabled, true).

get_filename() ->
    erlang:get(ds_syntax_parse_filename).

maybe_update_filename(Tokens) ->
    case get_file_line(Tokens) of
        undefined -> ok;
        Filename ->
            put(ds_syntax_parse_filename, Filename),
            ok
            %log_file("Open file")
    end.

is_pt_enabled(Tokens) ->
    case get_filename() of
        undefined -> false;
        Filename ->
            case was_pt_enabled_for_this_file(Filename) of
                true -> true;
                false ->
                    %log("Looking for parse transform"),
                    case has_pt_line(Tokens) of
                        false -> false;
                        true -> 
                            set_pt_enabled_for_this_file(Filename),
                            true
                    end
            end
    end.

was_pt_enabled_for_this_file(Filename) ->
    case get({erlang_ds_file_has_pt, Filename}) of
        undefined -> false;
        Res -> Res
    end.

set_pt_enabled_for_this_file(Filename) ->
    %log_file("Has Syntax rules for this file"), 
    put({erlang_ds_file_has_pt, Filename}, true).

has_pt_line([]) ->
    false;
has_pt_line([
             {'-', _},
             {atom, _, compile},
             {'(', _},
             {'{', _},
             {atom,_,parse_transform},
             {',',_},
             {atom,_,ds_syntax},
             {'}', _},
             {')', _},
             {'dot', _} | _]) ->
    true;
has_pt_line([_|T]) ->
    has_pt_line(T).


get_file_line([]) ->
    undefined;
get_file_line([
               {'-', _},
               {atom, _, file},
               {'(',_},
               {string, _, Filename} | _]) ->
    Filename;
get_file_line([_|T]) ->
    get_file_line(T).

%%====================================================================
%% API functions
%%====================================================================

parse_transform(Forms0, _Options) ->
    case is_initialized() of
        true ->
            Forms0;
        false ->
            logger:error("The ds_syntax parse_transform will not work without it being included as a rebar3 plugin and provider."),
            exit(1)
    end.
   % io:format("Applying PT~n"),
    %file:write_file("/tmp/forms.erl", io_lib:format("~p",[Forms0])),
    %NewForms = load_file_forms(Forms0),
    %file:write_file("/tmp/newforms.erl", io_lib:format("~p", [NewForms])),
    %Forms.

%load_file_forms([{attribute, Anno, file, {Filename, _}} | MoreForms]) ->
%    {ok, Bin} = file:read_file(Filename),
%    Str = binary_to_list(Bin),
%    Opts = get_erl_scan_opts(MoreForms),
%    {ok, Tokens, _} = erl_scan:string(Str, Anno, Opts),
%    Tokens2 = arrow(Tokens),
%    %io:format("Converted Tokens: ~p~n", [Tokens2]),
%    Exprs = split_at_dots(Tokens2),
%    Forms = exprs_to_forms(Exprs),
%
%   % {ok, Forms} = erl_parse:parse_exprs(Tokens2),
%    Forms;
%load_file_forms(Forms) ->
%    Forms.
%  
%get_erl_scan_opts([]) ->
%    [];
%get_erl_scan_opts([{attribute, _, epp_erl_scan_opts, Bin}|_]) ->
%    binary_to_term(Bin);
%get_erl_scan_opts([_|T]) ->
%    get_erl_scan_opts(T).

%% Peel of Name[SingleTerm] Exressions.
bracket([V={var, Anno, _}, {'[',_}, Key, {']', _} | Rest]) ->
    NewExpr = [
        {atom, Anno, 'ds'},
        {':', Anno},
        {atom, Anno, 'get'},
        {'(', Anno},
        V,
        {',', Anno},
        Key,
        {')', Anno}
    ],
    NewExpr ++ bracket(Rest);
bracket([H|T]) ->
    [H | bracket(T)];
bracket([]) ->
    [].

save_tokens(Tokens) ->
    file:write_file("/tmp/tokens.erl", io_lib:format("~p",[Tokens])).

arrow([V={var, Anno, _}, {'->', _} | Rest]) ->
    {Captured, NewRest} = capture_rest_of_expr(none, [], Rest),
    GetFun = case hd(Captured) of
        {'[', _} -> get_list;
        _ -> get
    end,
    %io:format("Captured: ~p~n",[Captured]),
    NewExpr = lists:flatten([
        {atom, Anno, 'ds'},
        {':', Anno},
        {atom, Anno, GetFun},
        {'(', Anno},
        V,
        {',', Anno},
        Captured,
        {')', Anno}
    ]),
    NewExpr ++ arrow(NewRest);
arrow([H|T]) ->
    [H | arrow(T)];
arrow([]) ->
    [].


capture_rest_of_expr(none, Captured, T=[{',',_}|_]) ->
    ?pr(none, ','),
    {Captured, T};
capture_rest_of_expr(none, Captured, T=[{';',_}|_]) ->
    ?pr(none, ';'),
    {Captured, T};
capture_rest_of_expr(_CaptureType, Captured, T=[{'dot',_}|_]) ->
    ?pr(none, '.'),
    {Captured, T};
%%capture_rest_of_expr(CaptureType, Captured, [H1={'?',_},H2={'?',_},H3={AorV, _, _} | T]) when AorV=='var'; AorV=='atom' ->
%%    %% Macro in the form: ??SOMETHING
%%    ?pr(CaptureType, [H1,H2,H3]),
%%    capture_rest_of_expr(CaptureType, Captured ++ [H1,H2,H3], T);
%%capture_rest_of_expr(CaptureType, Captured, [H1={'?',_},H2={AorV, _, _},H3={':', _} |T]) when AorV=='var'; AorV=='atom' ->
%%    ?pr(CaptureType, [H1,H2,H3]),
%%    %% Macro in the form: ?SOMETHING:whatever
%%    capture_rest_of_expr(CaptureType, Captured ++ [H1,H2,H3], T);
%%capture_rest_of_expr(CaptureType, Captured, [H1={'?',_},H2={AorV, _, _} | T]) when AorV=='var'; AorV=='atom' ->
%%    %% Macro in the form: ?SOMETHING
%%    ?pr(CaptureType, [H1,H2]),
%%    capture_rest_of_expr(CaptureType, Captured ++ [H1,H2], T);
capture_rest_of_expr(none, Captured, [H={AorV, _Anno, _}|Rest]) when AorV=='atom';
                                                                      AorV=='var';
                                                                      AorV=='binary';
                                                                      AorV=='integer';
                                                                      AorV=='string';
                                                                      AorV=='float' ->
    ?pr(none, [H]),
    {Captured ++ [H], Rest};


capture_rest_of_expr(CaptureType, Captured, [H={'(',_}|T]) ->
    ?pr(CaptureType, [H]),
    {NewCaptured, T2} = capture_rest_of_expr(paren, Captured ++ [H], T),
    capture_rest_of_expr(CaptureType, NewCaptured, T2);
capture_rest_of_expr(CaptureType, Captured, [H={'[',_}|T]) ->
    ?pr(CaptureType, [H]),
    {NewCaptured, T2} = capture_rest_of_expr(square, Captured ++ [H], T),
    capture_rest_of_expr(CaptureType, NewCaptured, T2);
capture_rest_of_expr(CaptureType, Captured, [H={'{',_}|T]) ->
    ?pr(CaptureType, [H]),
    {NewCaptured, T2} = capture_rest_of_expr(curly, Captured ++ [H], T),
    capture_rest_of_expr(CaptureType, NewCaptured, T2);

capture_rest_of_expr(paren, Captured, [H={')',_}|T]) ->
    ?pr(paren, [H]),
    {Captured ++ [H], T};
capture_rest_of_expr(square, Captured, [H={']',_}|T]) ->
    ?pr(square, [H]),
    {Captured ++ [H], T};
capture_rest_of_expr(curly, Captured, [H={'}',_}|T]) ->
    ?pr(curly, [H]),
    {Captured ++ [H], T};
capture_rest_of_expr(CaptureType, Captured, [H|T]) when CaptureType=/=none ->
    ?pr(CaptureType, [H]),
    capture_rest_of_expr(CaptureType, Captured ++ [H], T).


%% split_at_dots(Tokens) ->
%%     split_at_dots([], Tokens).
%% 
%% 
%% split_at_dots(Exprs, []) ->
%%     Exprs;
%% split_at_dots(Exprs, Tokens) ->
%%     {Expr, RemainingTokens} = first_expr(Tokens),
%%     Exprs2 = Exprs ++ [Expr],
%%     split_at_dots(Exprs2, RemainingTokens).
%% 
%% first_expr(Tokens) ->
%%     first_expr([], Tokens).
%% 
%% first_expr(Acc, [Dot = {dot, _} | Rest]) ->
%%     Acc2 = Acc ++ [Dot],
%%     {Acc2, Rest};
%% first_expr(Acc, [H | T]) ->
%%     Acc2 = Acc ++ [H],
%%     first_expr(Acc2, T);
%% first_expr(Acc, []) ->
%%     Acc.
%% 
%% 
%% exprs_to_forms([]) ->
%%     [];
%% exprs_to_forms([Expr|Rest]) ->
%%     {ok, Form} = erl_parse:parse_form(Expr),
%%     [Form | exprs_to_forms(Rest)].

