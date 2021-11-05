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

% 導出木を描画するのに最低限必要な文字幅もしくは
% 親ノードの長さのN等分 (M) のどちらかのうち大きい方を
% ノードの幅とする
render_tree(node(X, CS), node(X, DS, W), M) :-
  maplist(min_width, CS, WS),
  length(CS, N),
  sum_list(WS, S),
  W is max(M, S + (N - 1) * 2),
  (N = 0 -> D = 0; D is floor((W - (S + (N  - 1) * 2)) / N)),
  maplist(plus(D), WS, MS),
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
  %write_chars(W, '─'),
  write_chars(W, '-'),
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
