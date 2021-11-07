:- op(600, xfx, /).
:- op(600, xfx, \).
:- op(700, xfx, by).

:- if(current_prolog_flag(dialect, swi)).
iparse5(Grammar, N, XS, T) :-
  first_solution(T, [
    iparse1(Grammar, N, XS, T),
    iparse2(Grammar, N, XS, T),
    iparse3(Grammar, N, XS, T),
    iparse4(Grammar, N, XS, T)
  ], []).

:- table iparse6/4.
iparse6(_, _, [T], T) :-
  acyclic_term(T).
iparse6(Grammar, N, L, T) :-
  call(Grammar, Rule1, Rule2),
  append(XS, [X], L),
  iparse6(Grammar, N, XS, S),
  between(0, N, I),
  unary(I, S, A, Rule1),
  between(0, N, J),
  unary(J, X, B, Rule1),
  binary(A, B, T, Rule2).

:- endif.

iparse4(_, _, [T], T) :-
  acyclic_term(T).
iparse4(Grammar, N, L, T) :-
  call(Grammar, Rule1, Rule2),
  between(0, N, I),
  M is N - I,
  between(0, M, J),
  K is M - J,
  append(XS, [X], L),
  iparse4(Grammar, K, XS, S),
  unary(I, S, A, Rule1),
  unary(J, X, B, Rule1),
  binary(A, B, T, Rule2).

iparse3(_, _, [T], T) :-
  acyclic_term(T).
iparse3(Grammar, N, L, T) :-
  call(Grammar, Rule1, Rule2),
  between(0, N, I),
  between(0, N, J),
  append(XS, [X], L),
  iparse3(Grammar, N, XS, S),
  unary(I, S, A, Rule1),
  unary(J, X, B, Rule1),
  binary(A, B, T, Rule2).

iparse2(_, _, [T], T) :-
  acyclic_term(T).
iparse2(Grammar, N, [X, Y|XS], T) :-
  call(Grammar, Rule1, Rule2),
  between(0, N, I),
  unary(I, X, A, Rule1),
  M is N - I,
  between(0, M, J),
  unary(J, Y, B, Rule1),
  K is M - J,
  binary(A, B, C, Rule2),
  iparse2(Grammar, K, [C|XS], T).

iparse1(_, _, [T], T) :-
  acyclic_term(T).
iparse1(Grammar, N, [X, Y|XS], T) :-
  call(Grammar, Rule1, Rule2),
  between(0, N, I),
  unary(I, X, A, Rule1),
  between(0, N, J),
  unary(J, Y, B, Rule1),
  binary(A, B, C, Rule2),
  iparse1(Grammar, N, [C|XS], T).

parse(_, _, [T], T) :-
  acyclic_term(T).
parse(Grammar, N, XS, T) :-
  append([HD, [X, Y], TL], XS),
  call(Grammar, Rule1, Rule2),
  between(0, N, I),
  unary(I, X, A, Rule1),
  between(0, N, J),
  unary(J, Y, B, Rule1),
  binary(A, B, C, Rule2),
  append([HD, [C], TL], YS),
  parse(Grammar, N, YS, T).

qccg([ruleT, ruleD, ruleDx, ruleQ, ruleQx], [ruleA, ruleB, ruleBx]).
ccg([ruleT], [ruleA, ruleB, ruleBx]).
cg([], [ruleA]).

qccg(N, A, B, C) :-
  between(0, N, I),
  unary(I, A, X, [ruleT, ruleD, ruleDx, ruleQ, ruleQx]),
  between(0, N, J),
  unary(J, B, Y, [ruleT, ruleD, ruleDx, ruleQ, ruleQx]),
  binary(X, Y, C, [ruleA, ruleB, ruleBx]).

ccg(N, A, B, C) :-
  between(0, N, I),
  unary(I, A, X, [ruleT]),
  between(0, N, J),
  unary(J, B, Y, [ruleT]),
  binary(X, Y, C, [ruleA, ruleB, ruleBx]).

cg(_, A, B, C) :-
  binary(A, B, C, [ruleA]).

unary(0, A, A, _) :- !.
unary(N, A, C, Rules) :-
  member(Rule, Rules),
  call(Rule, A, B),
  M is N - 1,
  unary(M, B, C, Rules).

binary(A, B, C, Rules) :-
  member(Rule, Rules),
  call(Rule, A, B, C).

ruleA(A, B, C) :-
  A = node(X/Y by _, _),
  B = node(Y by _, _),
  C = node(X by '>A', [A, B]).
ruleA(A, B, C) :-
  A = node(X by _, _),
  B = node(X\Y by _, _),
  C = node(Y by '<A', [A, B]).
ruleB(A, B, C) :-
  A = node(X/Y by _, _),
  B = node(Y/Z by _, _),
  C = node(X/Z by '>B', [A, B]).
ruleB(A, B, C) :-
  A = node(X\Y by _, _),
  B = node(Y\Z by _, _),
  C = node(X\Z by '<B', [A, B]).
ruleBx(A, B, C) :-
  A = node(X/Y by _, _),
  B = node(Z\Y by _, _),
  C = node(Z\X by '>Bx', [A, B]).
ruleBx(A, B, C) :-
  A = node(X/Y by _, _),
  B = node(X\Z by _, _),
  C = node(Z/Y by '<Bx', [A, B]).

ruleT(A, B) :-
  A = node(X by _, _),
  B = node(Y/(X\Y) by '>T', [A]).
ruleT(A, B) :-
  A = node(X by _, _),
  B = node((Y/X)\Y by '<T', [A]).
ruleD(A, B) :-
  A = node(X/Y by _, _),
  B = node((X/Z)/(Y/Z) by '>D', [A]).
ruleD(A, B) :-
  A = node(X\Y by _, _),
  B = node((Z\X)\(Z\Y) by '<D', [A]).
ruleDx(A, B) :-
  A = node(X/Y by _, _),
  B = node((Z\X)/(Z\Y) by '>Dx', [A]).
ruleDx(A, B) :-
  A = node(X\Y by _, _),
  B = node((X/Z)\(Y/Z) by '<Dx', [A]).
ruleQ(A, B) :-
  A = node(X/Y by _, _),
  B = node((Z/X)\(Z/Y) by '>Q', [A]).
ruleQ(A, B) :-
  A = node(X\Y by _, _),
  B = node((X\Z)/(Y\Z) by '<Q', [A]).
ruleQx(A, B) :-
  A = node(X\Y by _, _),
  B = node((Z/Y)\(X\Z) by '>Qx', [A]).
ruleQx(A, B) :-
  A = node(X/Y by _, _),
  B = node((Z/Y)/(X\Z) by '<Qx', [A]).

node(node(_ by _, C)) :- maplist(node, C).
axiom(A, node(A by axiom, [])).

% 先頭からN個取得する
take(0, _, []).
take(N, [X|XS], [X|YS]) :-
  M is N - 1,
  take(M, XS, YS).

% アルファベット
alphabets([a, b, c, d, e, f, g, h, i, j, k, l, m,
           n, o, p, q, r, s, t, u, v, w, x, y, z]).

% 変数を定数に置換する
freeze(X) :-
  term_variables(X, V),
  length(V, N),
  alphabets(A),
  take(N, A, B),
  V = B.
