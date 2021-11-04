:- include('ccg.pl').
:- include('compat.pl').
:- include('printer.pl').
:- initialization(main).

:- if(current_prolog_flag(dialect, swi)).

main :-
  findall(
    N,
    (between(3, 6, I), length(G, I), maplist(axiom, G, N), cg(_, N, T), T=node('S' by _, _), freeze(G)),
    NS
  ),
  concurrent_maplist(
    parse(cg, 16),
    NS,
    TS
  ),
  concurrent_maplist(
    parse(iqccg, 16),
    NS,
    US
  ),
  forall(
    between(1, N, I),
    (
      nth1(I, TS, T),
      nth1(I, US, U),
      write_chars(80, '='),
      draw_tree(T),
      draw_tree(U)
    )
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
