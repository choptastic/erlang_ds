%% Copyright 2024 Jesse Gumm
%% MIT LICENSE

-module(ds_syntax).

%% API exports
-export([
    parse_transform/2,
    init/0,
    arrow/1,
    bracket/1,
    is_initialized/0
]).

-compile({nowarn_unused_function, log/1}).
-compile({nowarn_unused_function, log/2}).
-compile({nowarn_unused_function, log_file/1}).
-compile({nowarn_unused_function, log_file/2}).
-compile({nowarn_unused_function, run_id/0}).
-compile({nowarn_unused_function, timestamp/0}).
-compile({nowarn_unused_function, filename/1}).
-dialyzer({nowarn_function, log/1}).
-dialyzer({nowarn_function, log/2}).
-dialyzer({nowarn_function, log_file/1}).
-dialyzer({nowarn_function, log_file/2}).
-dialyzer({nowarn_function, run_id/0}).
-dialyzer({nowarn_function, timestamp/0}).
-dialyzer({nowarn_function, filename/1}).
-dialyzer({nowarn_function, set_initialized/0}).

-define(debug_parser, true).

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

run_id() ->
    {Y,M,D} = date(),
    {H,Mi,S} =  time(),
    lists:flatten(io_lib:format("~p-~p-~p.~p-~p-~p", [Y,M,D,H,Mi,S])).

timestamp() ->
    {_MS, S, Us} = os:timestamp(),
    Timestamp = S*1000 + Us,
    lists:flatten(io_lib:format("~p", [Timestamp])).


filename(Prefix) ->
    %"/tmp/tokens.erl".
    lists:flatten(["/tmp/tokens-",Prefix,"-",run_id(),".",timestamp(),".erl"]).


%% This function gets called by the ds_syntax plugin for each module that it
%% compiles.  But this has an initialization checker using persistent_term so
%% once its initialized, it doesn't have to initialize again.
init() ->
    case is_initialized() of
        false ->
            log("Initializing Syntax Parse Transform"),
            log("Hijacking erl_parse:parse_form/1"),

            %% Create a new erl_parse module that won't unload (no_link), is
            %% allowed to replace a system module (unstick), and retains the
            %% original functionality of the original (passthrough)
            meck:new(erl_parse, [no_link, unstick, passthrough]),

            %% Now w're going to rewrite erl_parse:parse_form/1 to look for the
            %% ds_syntax parse transform, and if it's found in the module being
            %% parsed, convert the arrows and brackets
            meck:expect(erl_parse, parse_form, fun(Tokens) ->
                %% For debugging purposes, this just saves the tokens term to
                %% /tmp/tokens.erl so it can be inspected in the event of a
                %% crash or whatever. (You know, the things you do debugging
                %% for).
                save_tokens("orig", Tokens),

                %% If the module has as -file attribute, we need to track that here.
                maybe_update_filename(Tokens),

                %% If the module has the ds_syntax parse transform, let's do some magic
                case is_pt_enabled(Tokens) of
                    true ->
                        %log_file("Preprocessing Arrow Syntax"),

                        %% Preprocess the for the relevant arrow tokens.
                        Tokens2 = ?MODULE:arrow(Tokens),
                        case Tokens/=Tokens2 of
                            true ->
                                save_tokens("changed", Tokens2);
                            false ->
                                ok
                        end,

                        
                        %% Then run the original erl_parse:parse_form.
                        %% meck:passthrough/1 is meck's function to "call the
                        %% rest of the function you hijacked".  It's quite
                        %% clever, and my hat is off to @eproxus
                        meck:passthrough([Tokens2]);
                    false ->
                        %log_file("Skipping Arrow Syntax for ~p", [Tokens]),

                        %% No parse-transform found, so just go straight to
                        %% calling the original erl_parse:parse_form.
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
%bracket(Tuple) when is_tuple(Tuple) ->
%    List = tuple_to_list(Tuple) ->
bracket(X) ->
    X.
%bracket([]) ->
%    [].

-ifdef(debug_parser).
save_tokens(Prefix,Tokens) ->
    FN = filename(Prefix),
    io:format("Writing Tokens to ~p~n", [FN]),
    file:write_file(FN, io_lib:format("~p",[Tokens])).
-else.
save_tokens(_, _) ->
    ok.
-endif.


arrow(Tokens) ->
    arrow([], Tokens).

arrow(Stack, [V={var, _, _}, {'->', _} | Rest]) -> %when Stack==[];
                                                   %  hd(Stack)=/='if' ->
    ?pr(var_equals, V),
    {NewExpr, NewRest} = replace_arrow_arg(V, Rest),
    NewExpr ++ arrow(Stack, NewRest);
arrow(Stack, [H={Tok,_}|Rest]) when Tok=='if';
                                    Tok=='receive';
                                    Tok=='case';
                                    Tok=='try' ->
    NewStack = [Tok | Stack],
    ?pr(stack, NewStack),
    {Guard, NewRest} = capture_until('->', [], Rest),
    [H] ++ Guard ++ arrow(NewStack, NewRest);
arrow(Stack, [H = {'->', _}| Rest]) ->
    ?pr(arrow_no_update, ""),
    [H] ++ arrow(Stack, Rest);
arrow(Stack = [Block | _], [H = {';', _}| Rest]) when Block=='if';
                                                      Block=='receive';
                                                      Block=='case';
                                                      Block=='try' ->
    ?pr(end_of_clause, ""),
    {Guard, NewRest} = capture_until('->', [], Rest),
    [H] ++ Guard ++ arrow(Stack, NewRest);
arrow(Stack = ['receive' | _], [H = {'after', _} | Rest]) ->
    {Guard, NewRest} = capture_until('->', [], Rest),
    ?pr(receive_after, Guard),
    [H] ++ Guard ++ arrow(Stack, NewRest);
arrow(Stack, [H = {Tok, _} | Rest]) when Tok=='try';
                                         Tok=='case';
                                         Tok=='fun';
                                         Tok=='receive';
                                         Tok=='begin' ->
    NewStack = [Tok | Stack],
    ?pr(stack, NewStack),
    [H] ++ arrow(NewStack, Rest);
arrow([Block | NewStack], [H = {'end', _} | Rest]) when Block=='if';
                                                      Block=='try';
                                                      Block=='case';
                                                      Block=='begin';
                                                      Block=='receive';
                                                      Block=='fun'->
    ?pr(encountered_end, NewStack),
    [H] ++ arrow(NewStack, Rest);
arrow(Stack, [H = {'when',_}|Rest]) ->
    {Captured, NewRest} = capture_until('->', [H], Rest),
    ?pr(when_clause, Captured),
    %% Captured here will be the whole 'when' expression (from 'when' to '->').
    %% and since non-BIFs can't be called in guards, we will skip those altogether.
    Captured ++ arrow(Stack, NewRest);
arrow(Stack, [H|T]) ->
    ?pr(ignoring_head, H),
    [H | arrow(Stack, T)];
arrow(_Stack, X) ->
    ?pr(ignoring_end_of_list, X),
    X.

replace_arrow_arg(V = {var, Anno, _}, Tokens) ->
    {Captured, NewRest} = capture_rest_of_expr(none, [], Tokens),
    ?pr(captured, Captured),
    GetFun = case hd(Captured) of
        {'[', _} -> 
            get_list;
        _ -> get
    end,
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
    {NewExpr, NewRest}.

%% Here, we're looking for the provided token (usually '->'). When it's
%% encountered, we return everything we've captured up until that point (hence,
%% why Token is repeated).
capture_until(Token, Captured, [H={Token,_}|T]) ->
    {Captured ++ [H], T};
%% We didn't find token, so just capture H and tack it to the list. I'm not
%% terrible concerned about performance here, so using ++ instead of prepending
%% then reversing is fine
capture_until(Token, Captured, [H|T]) ->
    capture_until(Token, Captured ++ [H], T);
%% We've reached the end of the file. There is very likely a syntax error, so we'll just return the whole captured term and leave it like that.
capture_until(_Token, Captured, []) ->
    {Captured, []}.



capture_rest_of_expr(none, Captured, T=[{Tok,_}|_]) when Tok==',';
                                                             Tok==';';
                                                             Tok=='end';
                                                             Tok=='after';
                                                             Tok=='catch' ->
    ?pr(none, Tok),
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

capture_rest_of_expr(CaptureType, Captured, [H={Tok,_}|T]) when Tok=='fun';
                                                                Tok=='case';
                                                                Tok=='begin';
                                                                Tok=='if';
                                                                Tok=='try';
                                                                Tok=='receive' ->
    ?pr(CaptureType, [H]),
    {NewCaptured, T2} = capture_rest_of_expr('looking_for_end', Captured ++ [H], T),
    capture_rest_of_expr(CaptureType, NewCaptured, T2);
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
capture_rest_of_expr('looking_for_end', Captured, [H={'end',_} | T]) ->
    ?pr('looking_for_end', [H]),
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

