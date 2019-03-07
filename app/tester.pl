% note: all the predicates defined in this file start with the prefix test_
%       you are not allowed to reference any of these predicates in your own
%       solution.  if test_ appears anywhere in your lib/due190307.pl file
%       (even in a comment or string constant), it will be assumed you are 
%       trying to compromise the testing process and marked accordingly.

% A scanner will be defined as a list of scanner terms
%     where a scanner term is scanner(Token, RegularExpression)
%        where Token is the name the scanner returns if RegularExpression
%              matches an input string.  if there are multiple such matches,
%              the Token in the first scanner term in the list to match is
%              the one that should match.
%        where RegularExpression is a regular expression term defined later.

% from NFA definition in text:
%     corresponds to term nfa(Start, States, Transitions)
%       Start is a state
%       States is a list of state terms
%       Transitions is a list of next terms
%     a state term has the form state(Name, Status)
%       where Name is a name given the state
%             Status is either
%                not_accepting if it is not an accepting state
%                Token         if it is an accepting state, when which Token
%                                 has been matched at this point.
%     a next term has the form next(From, To, Category)
%       where From is a state term in States
%             To is a state term in States
%             Category is a character category or the term epsilon
%               the categories will be 
%                   letter
%                   digit
%                   white_space
%                   question_mark
%                   exclamation_mark
%                   period
%                   comma
%                   colon
%                   semicolon
%                   minus_sign
%                   plus_operator
%                   binary_operators  for  * / % ^
%                   left_parenthesis
%                   right_parenthesis
%                   equal
%                   less_than
%                   greater_than
%                   undefined
%  the regular expression operators will be:
%    concat for concatenation as a binary infix operator
%    plus for one or more copies of its parameter as a postfix operator
%    star for zero or more copies of its parameter as a postfix operator
%    opertional for zero or one copies of its parameter as a postfix operator
%    or for from one or the other as a binary infix operator
%    (they are suffixed with underscore to avoid conflicts with existing operators)

:- op(300, yfx, 'concat').
:- op(500, yfx, 'or').
:- op(400, yf, 'plus').
:- op(400, yf, 'star').
:- op(400, yf, 'optional').

test_categorize([], []).
test_categorize([H_in|T_in], [H_result| T_result]) :- test_category(H_in, H_result), test_categorize(T_in, T_result).

test_category(X,Y) :- test_category_helper(X,Y).
test_category(X,undefined) :- \+ test_category_helper(X,_).


test_category_helper(Char, letter) :- [ACode] = "a", ACode @=< Char, [ZCode] = "z", Char @=< ZCode.
test_category_helper(Char, letter) :- [ACode] = "A", ACode @=< Char, [ZCode] = "Z", Char @=< ZCode.
test_category_helper(Char, digit) :- [ZeroCode] = "0", ZeroCode @=< Char, [NineCode] = "9", Char @=< NineCode.
test_category_helper(Char, white_space) :- [Char] = " ".
test_category_helper(Char, white_space) :- [Char] = "\n".
test_category_helper(Char, question_mark) :- [Char] = "?".
test_category_helper(Char, exclamation_mark) :- [Char] = "!".
test_category_helper(Char, period) :- [Char] = ".".
test_category_helper(Char, comma) :- [Char] = ",".
test_category_helper(Char, colon) :- [Char] = ":".
test_category_helper(Char, semicolon) :- [Char] = ";".
test_category_helper(Char, minus_sign) :- [Char] = "-".
test_category_helper(Char, plus_operator) :- [Char] = "+".
test_category_helper(Char, binary_operators) :- [Char] = "*".
test_category_helper(Char, binary_operators) :- [Char] = "/".
test_category_helper(Char, binary_operators) :- [Char] = "%".
test_category_helper(Char, binary_operators) :- [Char] = "^".
test_category_helper(Char, left_parenthesis) :- [Char] = "(".
test_category_helper(Char, right_parenthesis) :- [Char] = ")".
test_category_helper(Char, equal) :- [Char] = "=".
test_category_helper(Char, less_than) :- [Char] = "<".
test_category_helper(Char, greater_than) :- [Char] = ">".

test_count_successful_tests(N) :- setof(A, test_example_run_tests(A), L), length(L,N).

test_example_run_tests(Status) :-
                  test_case(InputString, RegularExpression, my_succeed), 
                  Result = my_succeed,
                  regular_expression_to_nfa(RegularExpression, NFA), 
                  test_categorize(InputString, TransformedString), 
                  test_nfa_match(TransformedString, NFA),
                  Status = [InputString, RegularExpression, Result].

test_example_run_tests(Status) :- 
                  test_case(InputString, RegularExpression, my_fail), 
                  Result = my_fail,
                  regular_expression_to_nfa(RegularExpression, NFA), 
                  test_categorize(InputString, TransformedString), 
                  \+ test_nfa_match(TransformedString, NFA),
                  Status = [InputString, RegularExpression, Result].

test_debug_run_tests(Status) :-
                  test_case(InputString, RegularExpression, my_succeed), 
                  Result = my_succeed,
                  regular_expression_to_nfa(RegularExpression, NFA), 
                  test_categorize(InputString, TransformedString), 
                  test_nfa_match(TransformedString, NFA),
                  Status = [InputString, RegularExpression, TransformedString, NFA, Result].

test_debug_run_tests(Status) :- 
                  test_case(InputString, RegularExpression, my_fail), 
                  Result = my_fail,
                  regular_expression_to_nfa(RegularExpression, NFA), 
                  test_categorize(InputString, TransformedString), 
                  \+ test_nfa_match(TransformedString, NFA),
                  Status = [InputString, RegularExpression, TransformedString, NFA, Result].

test_nfa_match(String, nfa(start(State),X,Y)) :- test_nfa_match_helper(String, nfa(start(State),X,Y), State).

test_nfa_match_helper("", NFA, State) :- epsilon_closure(State, NFA, Closure), 
     member(PossibleFinish, Closure),
     accepting(NFA, PossibleFinish).

test_nfa_match_helper([H|T], nfa(X,Y,  transitions(Transitions)), State) :- 
     member(next(State, NextState, H), Transitions),
     test_nfa_match_helper(T, nfa(X,Y, transitions(Transitions)), NextState).

test_nfa_match_helper(String, nfa(X,Y,transitions(Transitions)), State) :- 
     epsilon_closure(State, nfa(X,Y,transitions(Transitions)), Closure),
     member(NextState, Closure),     
     NextState \= State,
     member(next(State, NextState, epsilon), Transitions),
     test_nfa_match_helper(String, nfa(X,Y,transitions(Transitions)), NextState).

test_case("a",letter concat (letter star), my_succeed).

test_case("a",letter concat letter star, my_fail).

% test_case("a",(letter concat letter) star, my_fail).
% identical to  previous test case after operator processing and so doesn't
% show up as distinct in count of tests passed.

