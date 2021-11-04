:- include('ccg.pl').
:- include('compat.pl').
:- include('printer.pl').
:- initialization(main).

:- if(current_prolog_flag(dialect, swi)).

main :-
  concurrent_forall(
    (between(3, 5, I), length(G, I), maplist(axiom, G), cg(_, G, _), freeze(G)),
    (parse(iccg, 32, G, _))
  ),
  halt.

:- else.

main :-
  forall(
    (between(3, 6, I), length(G, I), maplist(axiom, G, N), cg(_, N, T), T=node('S' by _, _), freeze(G)),
    (write_chars(80, '='), nl, draw_tree(T), parse(iqccg, 16, N, U), U=node('S' by _, _), draw_tree(U))
  ),
  halt.

:- endif.
