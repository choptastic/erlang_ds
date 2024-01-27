%% Copyright 2024 Jesse Gumm
%% MIT LICENSE

-module(ds_helper).

%% API exports
-export([parse_transform/2]).

%%====================================================================
%% API functions
%%====================================================================

parse_transform(Forms0, _Options) ->
    io:format("Applying PT~n"),
    file:write_file("/tmp/forms.erl", io_lib:format("~p",[Forms0])),
    NewForms = load_file_forms(Forms0),
    file:write_file("/tmp/newforms.erl", io_lib:format("~p", [NewForms])),
    NewForms.

load_file_forms([{attribute, _, file, {Filename, _}} |_]) ->
    {ok, Bin} = file:read_file(Filename),
    Str = binary_to_list(Bin),
    {ok, Tokens, _} = erl_scan:string(Str),
    Tokens2 = replace_var_arrow(Tokens),
    io:format("Converted Tokens: ~p~n", [Tokens2]),
    Exprs = split_at_dots(Tokens2),
    Forms = exprs_to_forms(Exprs),

   % {ok, Forms} = erl_parse:parse_exprs(Tokens2),
    Forms;
load_file_forms(Forms) ->
    Forms.
    

%% Peel of Name[SingleTerm] Exressions.
replace_var_bracket([V={var, Anno, _}, {'[',_}, Key, {']', _} | Rest]) ->
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
    NewExpr ++ replace_var_bracket(Rest);
replace_var_bracket([H|T]) ->
    [H | replace_var_bracket(T)];
replace_var_bracket([]) ->
    [].

replace_var_arrow([V={var, Anno, _}, {'->', _} | Rest]) ->
    {Captured, NewRest} = capture_rest_of_expr(none, [], Rest),
    GetFun = case hd(Captured) of
        {'[', _} -> get_list;
        _ -> get
    end,
    io:format("Captured: ~p~n",[Captured]),
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
    NewExpr ++ replace_var_arrow(NewRest);
replace_var_arrow([H|T]) ->
    [H | replace_var_arrow(T)];
replace_var_arrow([]) ->
    [].


capture_rest_of_expr(none, Captured, T=[{',',_}|_]) ->
    {Captured, T};
capture_rest_of_expr(none, Captured, T=[{';',_}|_]) ->
    {Captured, T};
capture_rest_of_expr(none, Captured, T=[{'dot',_}|_]) ->
    {Captured, T};
capture_rest_of_expr(none, Captured, [Expr={AorV, Anno, _}|Rest]) when AorV=='atom';
                                                                      AorV=='var';
                                                                      AorV=='binary';
                                                                      AorV=='integer';
                                                                      AorV==foat ->
    {Captured ++ [Expr], Rest};


capture_rest_of_expr(CaptureType, Captured, [H={'(',_}|T]) ->
    {NewCaptured, T2} = capture_rest_of_expr(paren, Captured ++ [H], T),
    capture_rest_of_expr(CaptureType, NewCaptured, T2);
capture_rest_of_expr(CaptureType, Captured, [H={'[',_}|T]) ->
    {NewCaptured, T2} = capture_rest_of_expr(square, Captured ++ [H], T),
    capture_rest_of_expr(CaptureType, NewCaptured, T2);
capture_rest_of_expr(CaptureType, Captured, [H={'{',_}|T]) ->
    {NewCaptured, T2} = capture_rest_of_expr(curly, Captured ++ [H], T),
    capture_rest_of_expr(CaptureType, NewCaptured, T2);

capture_rest_of_expr(paren, Captured, [H={')',_}|T]) ->
    {Captured ++ [H], T};
capture_rest_of_expr(square, Captured, [H={']',_}|T]) ->
    {Captured ++ [H], T};
capture_rest_of_expr(curly, Captured, [H={'}',_}|T]) ->
    {Captured ++ [H], T};
capture_rest_of_expr(CaptureType, Captured, [H|T]) when CaptureType=/=none ->
    capture_rest_of_expr(CaptureType, Captured ++ [H], T).
            
            

%opener_type({'(', _}) -> paren;
%opener_type({'[', _}) -> square;
%opener_type({'{', _}) -> curly;
%opener_type({'#{', _}) -> map;
%opener_type(_) -> unknown.
%
%is_open_and_close(paren, {')', _}) -> true;
%is_open_and_close(square, {']', _}) -> true;
%is_open_and_close(curly, {'}', _}) -> true;
%is_open_and_close(map, {'}', _}) -> true;
%is_opener(_) -> false.



split_at_dots(Tokens) ->
    split_at_dots([], Tokens).


split_at_dots(Exprs, []) ->
    Exprs;
split_at_dots(Exprs, Tokens) ->
    {Expr, RemainingTokens} = first_expr(Tokens),
    Exprs2 = Exprs ++ [Expr],
    split_at_dots(Exprs2, RemainingTokens).

first_expr(Tokens) ->
    first_expr([], Tokens).

first_expr(Acc, [Dot = {dot, _} | Rest]) ->
    Acc2 = Acc ++ [Dot],
    {Acc2, Rest};
first_expr(Acc, [H | T]) ->
    Acc2 = Acc ++ [H],
    first_expr(Acc2, T);
first_expr(Acc, []) ->
    Acc.


exprs_to_forms([]) ->
    [];
exprs_to_forms([Expr|Rest]) ->
    io:format("Converting to Form: ~p~n",[Expr]),
    {ok, Form} = erl_parse:parse_form(Expr),
    [Form | exprs_to_forms(Rest)].

%% maybe try going to 


%%====================================================================
%% Internal functions
%%====================================================================



transform_until_done(Elements, Forms) when is_list(Forms) ->
    [transform_until_done(Elements, F) || F <- Forms];
transform_until_done(Elements, Form = {tuple, _Anno, Fields}) ->
    NewFields = transform_until_done(Elements, Fields),
    setelement(3, Form, NewFields);
transform_until_done(Elements, Form = {clause, _Anno, _Left, _Guards, Body}) ->
    NewBody = transform_until_done(Elements, Body),
    setelement(5, Form, NewBody);
transform_until_done(Elements, Form = {match, _Anno, _Left, Right}) ->
    %% Because we're doing a pattern match/bind, we don't want to do any munging of the left-side, so we'll leave it completly untouched.
    NewRight = transform_until_done(Elements, Right),
    setelement(4, Form, NewRight);
    
transform_until_done(Elements, Form = {record, Anno, RecordName, Attrs}) ->
    NewAttrs = case is_element_record(Elements, RecordName) of
        true ->
            %% This is a nitrogen element - let's handle it more closely
            transform_attrs(Elements, Anno, RecordName, Attrs);
        false ->
            %% This is not a nitrogen element, just transform any subdata and move on
            transform_until_done(Elements, Attrs)
    end,
    _NewForm = setelement(4, Form, NewAttrs);
transform_until_done(Elements, Form) when is_tuple(Form),
                                          tuple_size(Form) >= 3 ->
    [Tag, Anno | Content] = tuple_to_list(Form),
    NewContent = transform_until_done(Elements, Content),
    list_to_tuple([Tag, Anno | NewContent]);

transform_until_done(_Elements, Form) ->
    Form.

transform_attrs(Elements, RecAnno, RecordName, Attrs) when is_list(Attrs) ->
    %% AccRecAttrs are going to be legit attributes
    %% AccHtmlAttr are going to be new values to add the the html attrs field - basically any missing values
    {RecAttrs, HtmlAttrs, HtmlAnno2} = lists:foldl(fun(Attr, {AccRecAttrs, AccHtmlAttr, HtmlAnno}) ->
        case validate_and_transform_attr(Elements, RecordName, Attr) of
            {valid, GoodAttr} ->
                {AccRecAttrs ++ [GoodAttr], AccHtmlAttr, HtmlAnno};
            {invalid, BadAttr} ->
                % Add the invalid ones atr the end
                {AccRecAttrs, append_to_list(AccHtmlAttr, BadAttr), HtmlAnno};
            {attrs, OrigHtmlAttrs, NewHtmlAnno} ->
                % The originally specified ones should be at the front
                {AccRecAttrs, append_to_list(OrigHtmlAttrs, AccHtmlAttr), NewHtmlAnno}
        end
    end, {[], [], RecAnno}, Attrs),
    FinalAttrs = case HtmlAttrs of
        [] ->
            RecAttrs;
        _ ->
            HtmlAttrFinal = {record_field, HtmlAnno2, {atom, HtmlAnno2, attrs}, HtmlAttrs},
            [HtmlAttrFinal | RecAttrs]
    end,
    FinalAttrs.

append_to_list(Item, []) ->
    Anno = element(2, Item),
    {cons, Anno, Item, {nil, Anno}};
append_to_list([], Item) ->
    Anno = element(2, Item),
    {cons, Anno, Item, {nil, Anno}};
append_to_list({cons, Anno, Hd, Tl}, Item) ->
    % if the current element is a cons, let's dive deeper toward the end of the
    % list
    {cons, Anno, Hd, append_to_list(Tl, Item)};
append_to_list({nil, _Anno}, Item = {cons, _, _, _}) ->
    %% We're at the end of the list, and the element to append is a list
    %% already, so we can safely append it
    Item;
append_to_list({nil, Anno}, Item) ->
    %% We're at the end of the list, and the item to append is not a list, so
    %% let's make sure we make a proper list (ending with a nil)
    {cons, Anno, Item, {nil, Anno}};
append_to_list(OtherTerm, Item = {cons, Anno, _, _}) ->
    %% We don't know what kind of term the first term is, but we know the item
    %% to add is a cons (a list), so let's make the "Other Term" the first
    %% element of the list, and then append the Item
    {cons, Anno, OtherTerm, Item};
append_to_list(OtherTerm, Item) ->
    Anno = element(2, OtherTerm),
    %% Finally, we don't know what's going on with these terms but the first item  so we're just
    %% going to make a simple list where the first term becomes the first
    %% element of the list and the item to append becomse the second item of
    %% the list (again, making sure it's a proper list)
    {cons, Anno, OtherTerm, {cons, Anno, Item, {nil, Anno}}}.


validate_and_transform_attr(Elements, _RecordName, {record_field, Anno, {atom, _, attrs}, Val})  ->
    NewVal = transform_until_done(Elements, Val),
    %% It's a nitrogen element, so we can work on it
    {attrs, NewVal, Anno};

validate_and_transform_attr(Elements, RecordName, Form = {record_field, _, F = {atom, Anno, FieldName}, Val})  ->
    NewVal = transform_until_done(Elements, Val),
    case validate_element_field(Elements, RecordName, FieldName) of
        true ->
            NewForm = setelement(4, Form, NewVal),
            {valid, NewForm};
        false ->
            Tuple = {tuple, Anno, [F, NewVal]},
            {invalid, Tuple}
    end.

is_element_record(Elements, Recordname) ->
    case lists:keyfind(Recordname, 1, Elements) of
        false -> false;
        _ -> true
    end.


validate_element_field(Elements, RecordName, FieldName) ->
    case lists:keyfind(RecordName, 1, Elements) of
        {_, Fields} ->
            case lists:keyfind(FieldName, 1, Fields) of
                false ->
                    %% This time, we've validated that it's an element, but the
                    %% field specified is not found
                    false;
                _  ->
                    true
            end;
        false ->
            %% This is not even an element, so we're not going to mess with
            %% record validation - that said, this code should never be run and
            %% will run a warning
            logger:warning("Record: ~p and Fieldname: ~p some how is being validated as an element though it shouldn't be!", [RecordName, FieldName]),
            true
    end.

extract_record_defs(Forms) ->
    [extract_record_def(Rec) || {attribute, _, record, Rec} <- Forms].

extract_record_def(Rec) ->
    Name = extract_record_name(Rec),
    Attrs = extract_fields(Rec),
    %io:format("Attrs: ~p~n",[Attrs]),
    {Name, Attrs}.

extract_fields(Rec) ->
    Attrs = element(2, Rec),
    [extract_field_details(Attr) || Attr <- Attrs].

extract_record_name(Rec) ->
    element(1, Rec).

extract_field_details(_Raw = {typed_record_field, Rec, TypeDetails}) ->
    %io:format("TypeDetails: ~p~n",[TypeDetails]),
    {FieldName, Details} = extract_field_details(Rec),

    Details2 = Details#{
        %raw=>Raw,
        type=>extract_type_details(TypeDetails)
    },

    {FieldName, Details2};
extract_field_details(_Raw={record_field, _, {atom, _, FieldName}}) ->
    Details = #{
        default=>undefined,
        type=>any
        %raw=>Raw
    },
    {FieldName, Details};
extract_field_details(_Raw={record_field, _, {atom, _, FieldName}, Default}) ->
    Details = #{
        default=>extract_default_value(Default),
        type=>any
        %raw=>Raw
    },
    {FieldName, Details}.

extract_default_value({atom, _, DefaultValue}) ->
    DefaultValue;
extract_default_value({nil, _}) ->
    [];
extract_default_value({string, _, Val}) ->
    Val;
extract_default_value(_) ->
    undefined.

extract_type_details({type, _, union, UnionTypes}) ->
    [extract_type_details(T) || T <- UnionTypes];
extract_type_details({type, _, Type, _}) ->
    Type;
extract_type_details({user_type, _, UserType, _}) ->
    {user_type, UserType};
extract_type_details(_) ->
    undefined.
%extract_type_details({atom, _, undefined}) ->
%    undefined;
%extract_type_details({remote_type, _, [{_, _, Mod}, {_, _, TypeName} | _]}) ->
%    undefined;
