%%==========================================================================
%% Copyright (C) 2003 Joe Armstrong
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to permit
%% persons to whom the Software is furnished to do so, subject to the
%% following conditions:
%% 
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%%
%% Author: Joe Armstrong <joe@sics.se>
%%==========================================================================

%% ============================================================================
%% load hyph_*.dic files and create matching eg_hyphen_rules_*.erl 
%% ============================================================================
%% @private

-module(eg_mk_hyphen).

-export([start/0,
	 start/1,
	 start/2
	]).

%% ============================================================================

start() ->
    start([orig]),

    start([en_GB]),
    start([sv_SE]),
    start([da_DK]),
    start([fi_FI]),
    start([nb_NO]).

start([orig]) -> start_original();

%% pass .dic file name and lc_CC (language code and country code) to use
%% in generated eg_hyphen_rules_lc_CC.erl files
start([en_GB]) -> start("hyph_en_GB.dic", "en_GB");
start([sv_SE]) -> start("hyph_sv_SE.dic", "sv_SE");
start([da_DK]) -> start("hyph_da_DK.dic", "da_DK");
start([fi_FI]) -> start("hyph_fi_FI.dic", "fi_FI");
start([nb_NO]) -> start("hyph_nb_NO.dic", "nb_NO").


%% create eg_hyphen_rules_en.erl
%%
%% previous version of start - can only process ukhyphen.tex 
%% this was keept for this for testing purpouses
start_original() ->
    {ok, Bin} = file:read_file("../priv/hyphenation/ukhyphen.tex"),
    L = binary_to_list(Bin),
    Toks = string:tokens(L, "\n\r"),
    L1 = split1(Toks),
    {L2,L3} = split2(L1, []),
    ErlFileName = "eg_hyphen_rules",
    {ok, O} = file:open("../src/" ++ ErlFileName ++ ".erl", [write]),
    io:format(O, "-module(" ++ ErlFileName ++ ").~n", []),
    io:format(O, "%% autogenerated do not edit~n",[]),
    io:format(O, "%%[{Char,Val}] = after char Char Val is inserted~n",[]),
    io:format(O, "-export([hyphens/1, exception/1]).~n~n",[]),
    Except = lists:map(fun(I) -> clean(I) end, L3),
    lists:foreach(fun(I) -> mk_exception(O, I) end, Except),
    io:format(O, "exception(_) -> no.~n~n", []),
    lists:foreach(fun(I) -> jiggle(O, I) end, L2),
    io:format(O, "hyphens(_) -> [].~n",[]),
    file:close(O).


%% * File is expected to start with a "charset ISO8859-1 ..." or "ISO8859-1 ..."
%%   line. Telling us that the file contains ISO8895-1 / latin-1 encoded text.
%% * Each NL or CR separated line may contain one hyphen rule.
%% * Trailing and intial spaces are discarded before the rule is processed.
%% * Anything after and including a "%" is treated as a comment an therfore 
%%   discarded. Lines that are or become blank this way are also discarded.
%% * "\hyphenation" is treated as a special rule that tells us that all rules
%%   following it are exceptions to the normal hyphen rules - each such rules
%%   contains one hyphenated word e.g. "uni-ver-sity"
%% 
%% FileName    = string() 
%% LangCodeStr = string()
start(FileName, LangCode) ->
    {ok, Bin} = file:read_file(filename:join("../priv/hyphenation/", FileName)),
    L = binary_to_list(Bin),
    Toks = string:tokens(L, "\n\r"),
    %% get lines after initial header
    L1 = lines_after_header(Toks),

    %% Find hyphen rules and exceptions
    {L2, L3} = split(clear_comments_and_sp(L1)),

    ErlFileName = "eg_hyphen_rules_"++LangCode,
    {ok, O} = file:open(filename:join("../src/", ErlFileName++".erl"), [write]),
    io:format(O, "-module(" ++ ErlFileName ++ ").~n", []),
    io:format(O, "%% autogenerated do not edit~n",[]),
    io:format(O, "%%[{Char,Val}] = after char Char Val is inserted~n",[]),
    io:format(O, "-export([hyphens/1, exception/1]).~n~n",[]),

    %% * sort to ensure that alphabetical order
    %% * reverse so that exception(...) clauses are generated with the largest 
    %%   match first
    Except = lists:reverse(lists:sort(L3)),
    lists:foreach(fun(I) -> mk_exception(O, I) end, Except),
    io:format(O, "exception(_) -> no.~n~n", []),

    %% * sort to ensure that alphabetical order
    %% * reverse so that hyphens(...) clauses are generated with the largest 
    %%   match first
    HyphenClauses1 = [jiggle(I) || I <- L2],
    HyphenClauses2 = lists:reverse(lists:sort(HyphenClauses1)), 
    lists:foreach(
      fun({S1,S2}) -> io:format(O, "hyphens(\"~s\" ++ _)->~p;~n",[S1,S2]) end,
      HyphenClauses2),
    io:format(O, "hyphens(_) -> [].~n",[]),

    file:close(O).


%% keep only non-empty non-comment content lines
clear_comments_and_sp(Lines) ->
    ClearedLines = [string:strip(clear_comments(Line)) || Line <- Lines],
    %% discard empty lines
    [Line || Line <- ClearedLines, Line /= ""].


%% "% ...."             -> ""
%% " foo bar % foo bar" -> " foo bar "
clear_comments([]) -> [];
clear_comments([$% | _]) -> [];
clear_comments([C | R]) -> [C | clear_comments(R)].

%% @split(RuleLines::[string()]) -> {HyhpenRules::[string()], 
%%                                   Exceptions::[string()]}
%% extract the hyphen rules and rule exception lines
split(RuleLines) ->
    split(RuleLines, []).

%% only found hyphen rules
split([], HyphenRules) -> {lists:reverse(HyphenRules), []};
%% hyphen rules and rule exceptions found
split(["\\hyphenation" | ExcpL], HyphenRules) -> {lists:reverse(HyphenRules), 
						 ExcpL};
split(["\\" ++ _ | _ExcpL], _) -> throw(unkown_dic_syntax);
split([HyphenRule | R], HyphenRules) -> split(R, [HyphenRule | HyphenRules]).
    


mk_exception(Stream, H) ->
    X = remove_hyphens(H),
    io:format(Stream, "exception(\"~s\") -> \"~s\";~n",[X, H]).

remove_hyphens([$-|T]) -> remove_hyphens(T);
remove_hyphens([H|T])  -> [H|remove_hyphens(T)];
remove_hyphens([])     -> [].

%% for use by start_original(...)
jiggle(O, X) ->
    {L1, L2} = jiggle(X, 0, [], []),
    %% io:format("~s~n",[X]),
    io:format(O, "hyphens(\"~s\" ++ _)->~p;~n",[L1,L2]).

jiggle(X) ->
    jiggle(X, 0, [], []).


jiggle([], _, L1, L2) ->
    {lists:reverse(L1), L2};
jiggle([H|T], N, L1, L2) when H >= $1, H =< $9 ->
    jiggle(T, N, L1, [{N,H-$0}|L2]);
jiggle([$0|T], N, L1, L2) ->
    io:format("****?~n"),
    jiggle(T, N, L1, L2);
jiggle([H|T], N, L1, L2) ->
    jiggle(T, N+1, [H|L1], L2).
    

split1(["\\patterns" ++ _|T]) -> T;
split1([_|T])                 -> split1(T).

%% return: {[Hypen::string()], [Exception::string()]}
split2(["}" ++ _,"\\hyphenation"++_|T], L) -> {L, T};
split2([H|T], L)                           -> split2(T, [H|L]).

clean("}" ++ _) -> [];
clean([H|T])    -> [H|clean(T)];
clean([])       -> [].

    
%% we allow any header that ends with a "charset ISO8859-1 ...." or 
%% "ISO8859-1 ...." line 
lines_after_header(["charset ISO8859-1" ++ _|T]) -> T;
lines_after_header(["ISO8859-1" ++ _|T]) -> T;
lines_after_header([_|T])                 -> lines_after_header(T).

