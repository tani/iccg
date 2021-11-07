:- include('ccg.pl').
:- include('compat.pl').
:- include('printer.pl').
:- initialization(main).

:- if(current_prolog_flag(dialect, swi)).

root(node('S' by _, _)).
print([X]) :-
  write(X),
  nl.
print([X|XS]) :-
  write(X),
  write(', '),
  print(XS).

main :-
  findall(
    N,
    (
      between(3, 5, I),
      length(G, I),
      maplist(axiom, G, N),
      T=node('S' by _, _),
      parse(cg, 1, N, T),
      freeze(G)
    ),
    NS
  ),
  concurrent_maplist(parse(cg, 1), NS, TS),
  length(NS, K),
  length(US, K),
  concurrent_maplist(root, US),
  concurrent_maplist(iparse7(qccg, 8), NS, US),
  forall(
    between(1, K, I),
    (
      nth1(I, NS, N),
      maplist(axiom, A, N),
      nth1(I, TS, T),
      nth1(I, US, U),
      write_chars(80, '='), nl,
      print(A),
      draw_tree(T),
      draw_tree(U)
    )
  ),
  halt.

%main :-
%  forall(
%    (between(3, 4, I), length(G, I), maplist(axiom, G, N), T=node('S' by _, _), parse(cg, 1, N, T), freeze(G)),
%    (write_chars(80, '='), nl, draw_tree(T), U=node('S' by _, _), iparse7(qccg, 8, N, U), draw_tree(U))
%  ),
%  halt.

:- else.

main :-
  forall(
    (between(3, 4, I), length(G, I), maplist(axiom, G, N), T=node('S' by _, _), parse(cg, 1, N, T), freeze(G)),
    (write_chars(80, '='), nl, draw_tree(T), U=node('S' by _, _), iparse2(qccg, 8, N, U), draw_tree(U))
  ),
  halt.

:- endif.
