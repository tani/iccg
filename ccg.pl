:- op(600, xfx, /).
:- op(600, xfx, \).
:- op(700, xfx, by).

foldll(_, V, [], V).
foldll(F, V0, [X|XS], V) :-
  call(F, V0, X, Y),
  foldll(F, Y, XS, V).

foldrr(F, [X, Y], Z) :- !,
  call(F, X, Y, Z).
foldrr(F, L, V0) :-
  append(XS, [X], L),
  call(F, Y, X, V0),
  foldrr(F, XS, Y).

:- if(current_prolog_flag(dialect, swi)).
iparse(Grammar, XS, T) :-
  first_solution(T, [
    ilparse(Grammar, XS, T),
    irparse(Grammar, XS, T)
  ], []).
:-endif.

irparse(Grammar, XS, T) :-
  T = node(_ by _, _),
  foldrr(Grammar, XS, T),
  acyclic_term(T).

ilparse(Grammar, [X|XS], T) :-
  foldll(Grammar, X, XS, T),
  acyclic_term(T).

parse(_, [T], T).
parse(Grammar, XS, T) :-
  append([HD, [X, Y], TL], XS),
  call(Grammar, X, Y, Z),
  append([HD, [Z], TL], YS),
  parse(Grammar, YS, T).

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
