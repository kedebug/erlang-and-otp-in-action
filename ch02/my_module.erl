-module(my_module).

-export([pie/0, either_or_both/2, area/1, sign/1, 
         yesno/1, render/0, sum/1, reverse/1]).


pie() ->
    3.14.


either_or_both(true, A) when is_boolean(A) ->
    true;
either_or_both(B, true) when is_boolean(B) ->
    true;
either_or_both(false, false) ->
    false.


area({circle, Radius}) ->
    Radius * Radius * math:pi();
area({square, Side}) ->
    Side * Side;
area({rectangle, Height, Width}) ->
    Height * Width.


sign(N) when is_number(N) ->
    if
        N > 0 -> positive;
        N < 0 -> negative;
        true  -> zero
    end.


yesno(F) ->
    case F(true, false) of 
        true  -> io:format("yes~n");
        false -> io:format("no~n")
    end.


to_html(Items, F) ->
    ["<dl>\n",
     [io_lib:format("<dt>~s:\n<dd>~s\n", [F(T), F(D)]) || {T, D} <- Items],
     "</dl>"
    ].

render(Items, Em) ->
    to_html(Items,
            fun (Text) ->
                "<" ++ Em ++ ">" ++ Text ++ "</" ++ Em ++ ">"
            end).

render() ->
    render([{"D&D", "Dungeons and Dragons"}], "b").


sum(N) -> do_sum(N, 0).

do_sum(0, Total) -> Total;
do_sum(N, Total) -> do_sum(N-1, Total+N).


reverse(List) -> reverse(List, []).

reverse([], Acc) -> Acc;
reverse([X | XS], Acc) -> reverse(XS, [X | Acc]).