:- op(600, xfx, /).
:- op(600, xfx, \).
:- op(650, xfx, by).

% 
% For GNU Prolog
%
% write_length(X, N, _) :-
%   write_to_atom(A, X),
%   atom_length(A, N).
% foldl(_,[],V,V).
% foldl(Goal,[H|T],V0,V) :-
%   call(Goal,H,V0,V1),foldl(Goal,T,V1,V).
% append(X, L) :- foldl(append, X, [], L).

iccg(_, [T], T) :- T = node(s by _, _).
iccg(0, _, _) :- !, fail.

iccg(N, [A, B|CS], T) :-
  A = node(X/Y by _, _),
  B = node(Y by _, _),
  C = node(X by ">", [A, B]),
  M is N - 1,
  iccg(M, [C|CS], T).
iccg(N, [A, B|CS], T) :-
  A = node(X by _, _),
  B = node(X\Y by _, _),
  C = node(Y by "<", [A, B]),
  M is N - 1,
  iccg(M, [C|CS], T).

iccg(N, [A, B|CS], T) :-
  A = node(X/Y by _, _),
  B = node(Y/Z by _, _),
  C = node(X/Z by ">B", [A, B]),
  M is N - 1,
  iccg(M, [C|CS], T).
iccg(N, [A, B|CS], T) :-
  A = node(X\Y by _, _),
  B = node(Y\Z by _, _),
  C = node(X\Z by "<B", [A, B]),
  M is N - 1,
  iccg(M, [C|CS], T).

iccg(N, [A, B|CS], T) :-
  A = node(X/Y by _, _),
  B = node(Z\Y by _, _),
  C = node(Z\X by ">Bx", [A, B]),
  M is N - 1,
  iccg(M, [C|CS], T).
iccg(N, [A, B|CS], T) :-
  A = node(X/Y by _, _),
  B = node(X\Z by _, _),
  C = node(Z/Y by "<Bx", [A, B]),
  M is N - 1,
  iccg(M, [C|CS], T).

iccg(N, [A, B|CS], T) :-
  between(0, N, M),
  lift(M, A, C),
  between(0, N - M, L),
  lift(L, B, D),
  iccg(N - M - L, [C, D|CS], T).

iccg(N, [A, B|CS], T) :-
  A = node(X by _, _),
  C = node((Y/X)\Y by ">T", [A]),
  M is N - 1,
  iccg(M, [C, B|CS], T).
iccg(N, [A, B|CS], T) :-
  A = node(X by _, _),
  C = node(Y/(X\Y) by "<T", [A]),
  M is N - 1,
  iccg(M, [C, B|CS], T).
iccg(N, [B, A|CS], T) :-
  A = node(X by _, _),
  C = node((Y/X)\Y by ">T", [A]),
  M is N - 1,
  iccg(M, [B, C|CS], T).
iccg(N, [B, A|CS], T) :-
  A = node(X by _, _),
  C = node(Y/(X\Y) by "<T", [A]),
  M is N - 1,
  iccg(M, [B, C|CS], T).

iccg(N, [A, B|CS], T) :-
  A = node(X/Y by _, _),
  C = node((X/Z)/(Y/Z) by ">D", [A]),
  M is N - 1,
  iccg(M, [C, B|CS], T).
iccg(N, [A, B|CS], T) :-
  A = node(X\Y by _, _),
  C = node((Z\X)\(Z\Y) by "<D", [A]),
  M is N - 1,
  iccg(M, [C, B|CS], T).
iccg(N, [B, A|CS], T) :-
  A = node(X/Y by _, _),
  C = node((X/Z)/(Y/Z) by ">D", [A]),
  M is N - 1,
  iccg(M, [B, C|CS], T).
iccg(N, [B, A|CS], T) :-
  A = node(X\Y by _, _),
  C = node((Z\X)\(Z\Y) by "<D", [A]),
  M is N - 1,
  iccg(M, [B, C|CS], T).

iccg(N, [A, B|CS], T) :-
  A = node(X/Y by _, _),
  C = node((Z\X)/(Z\Y) by ">Dx", [A]),
  M is N - 1,
  iccg(M, [C, B|CS], T).
iccg(N, [A, B|CS], T) :-
  A = node(X\Y by _, _),
  C = node((X/Z)\(Y/Z) by "<Dx", [A]),
  M is N - 1,
  iccg(M, [C, B|CS], T).
iccg(N, [B, A|CS], T) :-
  A = node(X/Y by _, _),
  C = node((Z\X)/(Z\Y) by ">Dx", [A]),
  M is N - 1,
  iccg(M, [B, C|CS], T).
iccg(N, [B, A|CS], T) :-
  A = node(X\Y by _, _),
  C = node((X/Z)\(Y/Z) by "<Dx", [A]),
  M is N - 1,
  iccg(M, [B, C|CS], T).

iccg(N, [A, B|CS], T) :-
  A = node(X/Y by _, _),
  C = node((Z/X)\(Z/Y) by ">Q", [A]),
  M is N - 1,
  iccg(M, [C, B|CS], T).
iccg(N, [A, B|CS], T) :-
  A = node(X\Y by _, _),
  C = node((X\Z)/(Y\Z) by "<Q", [A]),
  M is N - 1,
  iccg(M, [C, B|CS], T).
iccg(N, [B, A|CS], T) :-
  A = node(X/Y by _, _),
  C = node((Z/X)\(Z/Y) by ">Q", [A]),
  M is N - 1,
  iccg(M, [B, C|CS], T).
iccg(N, [B, A|CS], T) :-
  A = node(X\Y by _, _),
  C = node((X\Z)/(Y\Z) by "<Q", [A]),
  M is N - 1,
  iccg(M, [B, C|CS], T).

iccg(N, [A, B|CS], T) :-
  A = node(X\Y by _, _),
  C = node((Z/Y)\(X\Z) by ">Qx", [A]),
  M is N - 1,
  iccg(M, [C, B|CS], T).
iccg(N, [A, B|CS], T) :-
  A = node(X/Y by _, _),
  C = node((Z/Y)/(X\Z) by "<Qx", [A]),
  M is N - 1,
  iccg(M, [C, B|CS], T).
iccg(N, [B, A|CS], T) :-
  A = node(X\Y by _, _),
  C = node((Z/Y)\(X\Z) by ">Qx", [A]),
  M is N - 1,
  iccg(M, [B, C|CS], T).
iccg(N, [B, A|CS], T) :-
  A = node(X/Y by _, _),
  C = node((Z/Y)/(X\Z) by "<Qx", [A]),
  M is N - 1,
  iccg(M, [B, C|CS], T).

iccg(XS, T) :-
  between(1, 16, N),
  iccg(N, XS, T), acyclic_term(T), !.

lift(0, A, A).
lift(N, A, C) :-
  ((A = node(X by _, _),
    B = node(Y/(X\Y) by ">T", [A]);
   (A = node(X by _, _),
    B = node((Y/X)\Y by "<T", [A]));
   (A = node(X/Y by _, _),
    B = node((X/Z)/(Y/Z) by ">D", [A]);
   (A = node(X\Y by _, _),
    B = node((Z\X)\(Z\Y) by "<D", [A]);
   (A = node(X/Y by _, _),
    B = node((Z\X)/(Z\Y) by ">Dx", [A]);
   (A = node(X\Y by _, _),
    B = node((X/Z)\(Y/Z) by "<Dx", [A]);
   (A = node(X/Y by _, _),
    B = node((Z/X)\(Z/Y) by ">Q", [A]);
   (A = node(X\Y by _, _),
    B = node((X\Z)/(Y\Z) by "<Q", [A]);
   (A = node(X\Y by _, _),
    B = node((Z/Y)\(X\Z) by ">Qx", [A]);
   (A = node(X/Y by _, _),
    B = node((Z/Y)/(X\Z) by "<Qx", [A])),
  M is N - 1,
  lift(M, B, C).

% CGで生成できる文とその導出木を作る
cg([node(s by L, CS)], node(s by L, CS)).
cg(XS, T) :-
  append([HD, [node(X by L, CS), node(Y by R, DS)], TL], XS),
  (var(CS) -> CS = [], L = axiom; true),
  (var(DS) -> DS = [], R = axiom; true),
  ((X = Z/Y, append([HD, [node(Z by lapp, [node(X by L, CS), node(Y by R, DS)])], TL], ZS), cg(ZS, T));
   (Y = X\Z, append([HD, [node(Z by rapp, [node(X by L, CS), node(Y by R, DS)])], TL], ZS), cg(ZS, T))).

% 先頭からN個取得する
take(0, _, []).
take(N, [X|XS], [X|YS]) :-
  M is N - 1,
  take(M, XS, YS).

% アルファベット
alphabets([a, b, c, d, e, f, g, h, i, j, k, l,
           m, n, o, p, q, r, t, u, v, w, x, y, z]).

% 変数を定数に置換する
freeze(X) :-
  term_variables(X, V),
  length(V, N),
  alphabets(A),
  take(N, A, B),
  V = B.

% 全ての枝を同じ深さにする
% 深さが足りない場合はダミーとして空ノードを加える
complete_tree(1, node(L, []), node(L, [])) :- !.
complete_tree(N, node(L, []), node(L, [C])) :- !,
  M is N - 1,
  complete_tree(M, node(' ', []), C).
complete_tree(N, node(L, C), node(L, D)) :-
  M is N - 1,
  maplist(complete_tree(M), C, D).

% 深さNにある子ノードを全て取得する
children(1, T, [T]) :- !.
children(N, node(_, CS, _), DS) :-
  M is N - 1,
  maplist(children(M), CS, DSS),
  append(DSS, DS).

% 木の深さを取得する
depth(node(_, []), 1) :- !.
depth(node(_, CS), N) :-
  maplist(depth, CS, NS),
  max_list(NS, M),
  N is M + 1.

% 与えられた導出木を描画するのに最低限必要な文字幅を計算する
min_width(node(X, CS), N) :-
  write_length(X, M, []),
  maplist(min_width, CS, NS),
  sum_list(NS, S),
  length(CS, D),
  N is max(S + (D - 1) * 2, M).

add(A, B, C) :- C is A + B.

% 導出木を描画するのに最低限必要な文字幅もしくは
% 親ノードの長さのN等分 (M) のどちらかのうち大きい方を
% ノードの幅とする
render_tree(node(X, CS), node(X, DS, W), M) :-
  maplist(min_width, CS, WS),
  length(CS, N),
  sum_list(WS, S),
  W is max(M, S + (N - 1) * 2),
  (N = 0 -> D = 0; D is floor((W - (S + (N  - 1) * 2)) / N)),
  maplist(add(D), WS, MS),
  maplist(render_tree, CS, DS, MS).

% 与えられた文字をN個出力する
write_chars(0, _) :- !.
write_chars(N, C) :-
  M is N - 1,
  write(C),
  write_chars(M, C).

% 同じ深さのノードのラベルを画面に出力する
draw_labels([node(X, _, W)|CS]) :-
  write_length(X, N, []),
  P is floor((W - N)/2),
  L is ceiling((W - N)/2) + 2,
  write_chars(P, ' '),
  write(X),
  write_chars(L, ' '),
  draw_labels(CS).
draw_labels([]) :- nl.

% 同じ深さのノードでの区切り線を画面に出力する
draw_hline([node(_, [node(' ', _, _)], W)|CS]) :- !,
  write_chars(W, ' '),
  write(' '),
  write(' '),
  draw_hline(CS).
draw_hline([node(_, [], W)|CS]) :- !,
  write_chars(W, ' '),
  write(' '),
  write(' '),
  draw_hline(CS).
draw_hline([node(_, _, W)|CS]) :-
  write_chars(W, '─'),
  %write_chars(W, '-'),
  write(' '),
  write(' '),
  draw_hline(CS).
draw_hline([]) :- nl.

% 木を画面に出力する
draw_tree(1, T) :- !,
  children(1, T, CS),
  draw_hline(CS),
  draw_labels(CS).
draw_tree(N, T) :-
  children(N, T, CS),
  draw_hline(CS),
  draw_labels(CS),
  M is N - 1,
  draw_tree(M, T).
draw_tree(T) :-
  depth(T, N),
  complete_tree(N, T, U),
  min_width(T, M),
  render_tree(U, S, M),
  draw_tree(N, S),
  nl.
