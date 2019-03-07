state(first, non_accepting).
state(last, accepting).

test(X, A, B, Op) :- (
(X = star(A), Op = st);
(X = plus(A), Op = pl);
(X = optional(A), Op = opt);
(X = or(A, B), Op = o);
(X = concat(A, B), Op = cc)).

regular_expression_to_nfa(RegularExpression, NFA) :- 
StartState = state(start, non_accepting),
FinishState = state(second, accepting), 
recursive_nfa(StartState, FinishState, RegularExpression, _, Transitions, 0, Next),
NFA = nfa(start(StartState), [], Transitions).


recursive_nfa(S, F, RegEx, TransitionsNext, Transitions, CurrentIndex, NextIndex) :-
FirstSub is CurrentIndex + 1,
Initial = state(FirstSub, non_accepting), 
(atomic(RegEx) *-> (FinishSub is FirstSub + 1, NextIndex = FinishSub, Final = state(FinishSub, non_accepting), append([], [next(S, Initial, epsilon), next(Initial, Final, RegEx), next(Final, F, epsilon)], Transitions));
(test(RegEx, A, B, Op),
((Op == st) *-> (FinishSub is FirstSub + 1, Final = state(FinishSub, non_accepting), recursive_nfa(Initial, Final, A, Transitions2, Transitions1, FinishSub, ReturnIndex), NextIndex = ReturnIndex, append(Transitions1, [next(S, F, epsilon), next(Final, Initial, epsilon), next(S, Initial, epsilon), next(Final, F, epsilon)], Transitions));
(Op == pl) *-> (FinishSub is FirstSub + 1, Final = state(FinishSub, non_accepting), recursive_nfa(Initial, Final, A, Transitions2, Transitions1, FinishSub, ReturnIndex), NextIndex = ReturnIndex, append(Transitions1, [next(Final, Initial, epsilon), next(S, Initial, epsilon), next(Final, F, epsilon)], Transitions));
(Op == opt) *-> (FinishSub is FirstSub + 1, Final = state(FinishSub, non_accepting), recursive_nfa(Initial, Final, A, Transitions2, Transitions1, FinishSub, ReturnIndex), NextIndex = ReturnIndex, append(Transitions1, [next(S, F, epsilon), next(S, Initial, epsilon), next(Final, F, epsilon)], Transitions));
(Op == cc) *-> (InterSub is FirstSub + 1, FinishSub is InterSub + 1, Final = state(FinishSub, non_accepting), Intermediate = state(InterSub, non_accpeting), recursive_nfa(Initial, Intermediate, A, Transitions2, Transitions1, FinishSub, FirstNextInd), recursive_nfa(Intermediate, Final, B, Transitions4, Transitions3, FirstNextInd, SecondNextInd), NextIndex = SecondNextInd, append(Transitions1, [next(Final, F, epsilon), next(S, Initial, epsilon)], TransitionsTemp), append(TransitionsTemp, Transitions3, Transitions));
((Op == o) -> (StartBSub is FirstSub + 1, FinishBSub is StartBSub + 1, FinishSub is FinishBSub + 1, Final = state(FinishSub, non_accepting), FinalB = state(FinishBSub, non_accepting), InitialB = state(StartBSub, non_accepting), recursive_nfa(Initial, Final, A, Transitions2, Transitions1, FinishSub, FirstNextInd), recursive_nfa(InitialB, FinalB, B, Transitions4, Transitions3, FirstNextInd, SecondNextInd), append(Transitions1, [next(S, Initial, epsilon), next(Final, F, epsilon), next(S, InitialB, epsilon), next(FinalB, F, epsilon)], TransitionsTemp), append(TransitionsTemp, Transitions3, Transitions), NextIndex = SecondNextInd))))).


