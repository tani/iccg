:- op(600, xfx, /).
:- op(600, xfx, \).
:- op(700, xfx, by).

parse(Grammar, L, XS, T) :-
  between(1, L, N),
  call(Grammar, N, XS, T),
  acyclic_term(T).

iqccg(_, [T], T) :- !.
iqccg(N, XS, T) :-
  uccg(N, XS, M, YS, [ruleT, ruleD, ruleDx, ruleQ, ruleQx]),
  bccg(M, YS, K, ZS, [ruleA, ruleB, ruleBx]),
  iqccg(K, ZS, T).

iccg(_, [T], T) :- !.
iccg(N, XS, T) :-
  uccg(N, XS, M, YS, [ruleT]),
  bccg(M, YS, K, ZS, [ruleA, ruleB, ruleBx]),
  iccg(K, ZS, T).

icg(_, [T], T) :- !.
icg(N, XS, T) :-
  uccg(N, XS, M, YS, []),
  bccg(M, YS, K, ZS, [ruleA]),
  icg(K, ZS, T).

bccg(N, [A, B|CS], M, [C|CS], Rules) :-
  binary(A, B, C, Rules),
  M is N - 1.

uccg(N, [A, B|CS], M, [C, D|CS], Rules) :-
  between(0, N, I),
  unary(I, A, C, Rules),
  K is N - I,
  between(0, K, J),
  unary(J, B, D, Rules),
  M is N - I - J.

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

% CCGで生成できる文とその導出木を作る
ccg(_, [T], T) :- !.
ccg(0, _, _) :- !, fail.
ccg(N, XS, T) :-
  append([HD, [A], TL], XS),
  unary(1, A, C, [ruleT]),
  append([HD, [C], TL], ZS),
  M is N - 1,
  ccg(M, ZS, T).
ccg(N, XS, T) :-
  append([HD, [A, B], TL], XS),
  binary(A, B, C, [ruleA, ruleB, ruleBx]),
  append([HD, [C], TL], ZS),
  M is N - 1,
  ccg(M, ZS, T).

node(node(_ by _, C)) :- maplist(node, C).
axiom(A, node(A by axiom, [])).
axiom(node(_ by axiom, [])).

% CGで生成できる文とその導出木を作る
cg(_, [T], T) :- !.
cg(_, XS, T) :-
  append([HD, [A, B], TL], XS),
  binary(A, B, C, [ruleA]),
  append([HD, [C], TL], ZS),
  cg(_, ZS, T).

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
